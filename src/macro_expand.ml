open Expr

module StringSet = Set.Make (String)

type matcher_elem =
  | MatcherToken of Lex.token_type
  | MatcherGroup of macro_delim * matcher_seq
  | MatcherCapture of string * macro_fragment_kind
  | MatcherRepeat of matcher_seq * Lex.token_type option * bool

and matcher_seq = matcher_elem list

type transcriber_elem =
  | TranscriberToken of Lex.token_type
  | TranscriberGroup of macro_delim * transcriber_seq
  | TranscriberVar of string
  | TranscriberRepeat of transcriber_seq * Lex.token_type option * bool

and transcriber_seq = transcriber_elem list

type macro_capture =
  | CaptureOne of macro_tt list
  | CaptureMany of macro_capture list

type capture_env = (string * macro_capture) list

type compiled_macro_arm = {
  matcher : matcher_seq;
  transcriber : transcriber_seq;
}

type macro_def = { arms : compiled_macro_arm list }

let path_to_key (path : string list) : string = String.concat "." path
let scoped_key (path : string list) (name : string) : string = path_to_key path ^ "|" ^ name

let drop_last (path : string list) : string list =
  match List.rev path with [] -> [] | _ :: t -> List.rev t

let rec nth_opt (n : int) (xs : 'a list) : 'a option =
  match (n, xs) with
  | n, _ when n < 0 -> None
  | 0, x :: _ -> Some x
  | _, [] -> None
  | n, _ :: rest -> nth_opt (n - 1) rest

let rec take (n : int) (xs : 'a list) : 'a list =
  if n <= 0 then []
  else
    match xs with
    | [] -> []
    | x :: rest -> x :: take (n - 1) rest

let rec drop (n : int) (xs : 'a list) : 'a list =
  if n <= 0 then xs
  else
    match xs with
    | [] -> []
    | _ :: rest -> drop (n - 1) rest

let rec flatten_macro_tts (tts : macro_tt list) : Lex.token_type list =
  let flatten_one (tt : macro_tt) : Lex.token_type list =
    match tt with
    | MacroTTToken tok -> [ tok ]
    | MacroTTGroup (MacroParen, inner) ->
        if inner = [] then [ Lex.Unit ]
        else Lex.LParen :: flatten_macro_tts inner @ [ Lex.RParen ]
    | MacroTTGroup (MacroBracket, inner) ->
        Lex.LBracket :: flatten_macro_tts inner @ [ Lex.RBracket ]
    | MacroTTGroup (MacroBrace, inner) ->
        Lex.LBrace :: flatten_macro_tts inner @ [ Lex.RBrace ]
  in
  List.concat_map flatten_one tts

let tokens_to_debug_string (tokens : Lex.token_type list) : string =
  String.concat " " (List.map Lex.string_of_token_type tokens)

let parse_all (p : Lex.token_type list -> ('a * Lex.token_type list) option)
    (tokens : Lex.token_type list) : 'a option =
  try
    match p tokens with
    | Some (v, []) -> Some v
    | _ -> None
  with _ -> None

let parse_expr_all (tokens : Lex.token_type list) : expr option =
  parse_all Parser.ExprParser.expr_parser tokens

let parse_path_all (tokens : Lex.token_type list) : string list option =
  parse_all Parser.ParserUtils.qualified_id_segments_parser tokens

let parse_type_all (tokens : Lex.token_type list) : compound_type option =
  parse_all Parser.CompoundTypeParser.compound_type_parser tokens

let parse_pat_all (tokens : Lex.token_type list) : pat option =
  parse_all Parser.PatParser.pat_parser tokens

let parse_item_all (tokens : Lex.token_type list) : defn option =
  parse_all Parser.DefnParser.defn_parser tokens

let expr_from_factor (f : factor) : expr =
  ConsExpr
    (DisjunctionUnderCons
       (ConjunctionUnderDisjunction
          (RelationUnderConjunction
             (ArithmeticUnderRelExpr
                (Term (Factor (FactorUnderApplication f)))))))

let int_expr (i : int) : expr = expr_from_factor (Integer i)
let string_expr (s : string) : expr = expr_from_factor (String s)
let list_expr (xs : expr list) : expr = expr_from_factor (ListSugar xs)

let rec expr_as_atomic_factor (e : expr) : factor option =
  match e with
  | ConsExpr
      (DisjunctionUnderCons
         (ConjunctionUnderDisjunction
            (RelationUnderConjunction
               (ArithmeticUnderRelExpr (Term (Factor (FactorUnderApplication f))))))) ->
      factor_as_atomic_factor f
  | _ -> None

and factor_as_atomic_factor (f : factor) : factor option =
  match f with
  | ParenFactor e -> expr_as_atomic_factor e
  | _ -> Some f

let expr_as_string_literal (e : expr) : string option =
  match expr_as_atomic_factor e with
  | Some (String s) -> Some s
  | _ -> None

let expr_is_literal (e : expr) : bool =
  match expr_as_atomic_factor e with
  | Some (Boolean _ | String _ | Unit | Integer _ | Char _ | FloatFactor _) ->
      true
  | _ -> false

let macro_kind_of_name (s : string) : macro_fragment_kind option =
  match s with
  | "expr" -> Some MacroExpr
  | "pat" -> Some MacroPat
  | "ty" | "type" -> Some MacroType
  | "ident" -> Some MacroIdent
  | "item" -> Some MacroItem
  | "tt" -> Some MacroTT
  | "literal" | "lit" -> Some MacroLiteral
  | "path" -> Some MacroPath
  | "block" -> Some MacroBlock
  | _ -> None

let macro_kind_of_name_exn (macro_name : string) (kind_name : string) :
    macro_fragment_kind =
  match macro_kind_of_name kind_name with
  | Some k -> k
  | None ->
      failwith
        ("forge: unknown macro fragment kind $:<"
        ^ kind_name
        ^ "> in macro_rules! "
        ^ macro_name)

let parse_repeat_suffix (macro_name : string) (context : string)
    (rest : macro_tt list) : Lex.token_type option * bool * macro_tt list =
  match rest with
  | MacroTTToken (Lex.Mulop "*") :: tail -> (None, false, tail)
  | MacroTTToken (Lex.Addop "+") :: tail -> (None, true, tail)
  | MacroTTToken sep :: MacroTTToken (Lex.Mulop "*") :: tail ->
      (Some sep, false, tail)
  | MacroTTToken sep :: MacroTTToken (Lex.Addop "+") :: tail ->
      (Some sep, true, tail)
  | _ ->
      failwith
        ("forge: malformed repetition in "
        ^ context
        ^ " for macro_rules! "
        ^ macro_name
        ^ " (expected $( ... )* / + with optional separator)")

let rec parse_matcher_seq (macro_name : string) (tts : macro_tt list) : matcher_seq =
  match tts with
  | [] -> []
  | MacroTTToken Lex.Dollar :: MacroTTGroup (MacroParen, inner) :: rest ->
      let sep, one_or_more, tail =
        parse_repeat_suffix macro_name "matcher" rest
      in
      MatcherRepeat (parse_matcher_seq macro_name inner, sep, one_or_more)
      :: parse_matcher_seq macro_name tail
  | MacroTTToken Lex.Dollar :: MacroTTToken (Lex.Id name)
    :: MacroTTToken Lex.Colon :: MacroTTToken (Lex.Id kind_name) :: rest ->
      MatcherCapture (name, macro_kind_of_name_exn macro_name kind_name)
      :: parse_matcher_seq macro_name rest
  | MacroTTGroup (delim, inner) :: rest ->
      MatcherGroup (delim, parse_matcher_seq macro_name inner)
      :: parse_matcher_seq macro_name rest
  | MacroTTToken tok :: rest ->
      MatcherToken tok :: parse_matcher_seq macro_name rest

let rec parse_transcriber_seq (macro_name : string) (tts : macro_tt list) :
    transcriber_seq =
  match tts with
  | [] -> []
  | MacroTTToken Lex.Dollar :: MacroTTGroup (MacroParen, inner) :: rest ->
      let sep, one_or_more, tail =
        parse_repeat_suffix macro_name "transcriber" rest
      in
      TranscriberRepeat
        (parse_transcriber_seq macro_name inner, sep, one_or_more)
      :: parse_transcriber_seq macro_name tail
  | MacroTTToken Lex.Dollar :: MacroTTToken (Lex.Id name) :: rest ->
      TranscriberVar name :: parse_transcriber_seq macro_name rest
  | MacroTTGroup (delim, inner) :: rest ->
      TranscriberGroup (delim, parse_transcriber_seq macro_name inner)
      :: parse_transcriber_seq macro_name rest
  | MacroTTToken tok :: rest ->
      TranscriberToken tok :: parse_transcriber_seq macro_name rest

let rec matcher_capture_names (seq : matcher_seq) : string list =
  List.concat_map
    (function
      | MatcherCapture (name, _) -> [ name ]
      | MatcherGroup (_, inner) | MatcherRepeat (inner, _, _) ->
          matcher_capture_names inner
      | MatcherToken _ -> [])
    seq

let rec transcriber_var_names (seq : transcriber_seq) : string list =
  List.concat_map
    (function
      | TranscriberVar name -> [ name ]
      | TranscriberGroup (_, inner) | TranscriberRepeat (inner, _, _) ->
          transcriber_var_names inner
      | TranscriberToken _ -> [])
    seq

let validate_macro_arm (macro_name : string) (arm : macro_arm) : compiled_macro_arm =
  let matcher_tts, transcriber_tts = arm in
  let matcher = parse_matcher_seq macro_name matcher_tts in
  let transcriber = parse_transcriber_seq macro_name transcriber_tts in
  let capture_names = matcher_capture_names matcher in
  let capture_names_uniq = List.sort_uniq String.compare capture_names in
  if List.length capture_names <> List.length capture_names_uniq then
    failwith
      ("forge: duplicate macro parameter in macro_rules! " ^ macro_name)
  else
    let captures =
      List.fold_left
        (fun acc n -> StringSet.add n acc)
        StringSet.empty capture_names_uniq
    in
    List.iter
      (fun n ->
        if not (StringSet.mem n captures) then
          failwith
            ("forge: unknown macro variable $"
            ^ n
            ^ " in transcriber of macro_rules! "
            ^ macro_name))
      (transcriber_var_names transcriber);
    { matcher; transcriber }

let fragment_matches (kind : macro_fragment_kind) (captured : macro_tt list) :
    bool =
  let tokens = flatten_macro_tts captured in
  match kind with
  | MacroTT -> List.length captured = 1
  | MacroIdent -> (
      match captured with
      | [ MacroTTToken (Lex.Id _) ] -> true
      | _ -> false)
  | MacroLiteral -> (
      match parse_expr_all tokens with
      | Some e -> expr_is_literal e
      | None -> false)
  | MacroPath -> parse_path_all tokens <> None
  | MacroType -> parse_type_all tokens <> None
  | MacroPat -> parse_pat_all tokens <> None
  | MacroExpr -> parse_expr_all tokens <> None
  | MacroItem -> parse_item_all tokens <> None
  | MacroBlock -> (
      match parse_expr_all tokens with Some (Block _) -> true | _ -> false)

let rec capture_equal (a : macro_capture) (b : macro_capture) : bool =
  match (a, b) with
  | CaptureOne xs, CaptureOne ys -> xs = ys
  | CaptureMany xs, CaptureMany ys ->
      List.length xs = List.length ys && List.for_all2 capture_equal xs ys
  | _ -> false

let bind_capture (env : capture_env) (name : string) (capture : macro_capture) :
    capture_env option =
  match List.assoc_opt name env with
  | None -> Some ((name, capture) :: env)
  | Some existing -> if capture_equal existing capture then Some env else None

let merge_env (base : capture_env) (extra : capture_env) : capture_env option =
  List.fold_left
    (fun acc_opt (name, cap) ->
      match acc_opt with
      | None -> None
      | Some acc -> bind_capture acc name cap)
    (Some base) extra

let capture_candidates (kind : macro_fragment_kind) (input : macro_tt list) :
    (macro_tt list * macro_tt list) list =
  match kind with
  | MacroTT -> (
      match input with h :: t -> [ ([ h ], t) ] | [] -> [])
  | MacroIdent -> (
      match input with
      | (MacroTTToken (Lex.Id _ as tok)) :: t -> [ ([ MacroTTToken tok ], t) ]
      | _ -> [])
  | _ ->
      let rec go n acc =
        if n > List.length input then acc
        else
          let captured = take n input in
          let rest = drop n input in
          let acc' =
            if captured <> [] && fragment_matches kind captured then
              (captured, rest) :: acc
            else acc
          in
          go (n + 1) acc'
      in
      (* [go] builds reverse-by-length so longest captures are tried first. *)
      go 1 []

let build_repeat_bindings (macro_name : string) (expected_names : string list)
    (iter_envs : capture_env list) :
    capture_env =
  let names_from_iters =
    iter_envs
    |> List.concat_map (fun env -> List.map fst env)
    |> List.sort_uniq String.compare
  in
  let names = List.sort_uniq String.compare (expected_names @ names_from_iters) in
  List.map
    (fun name ->
      let caps =
        List.map
          (fun env ->
            match List.assoc_opt name env with
            | Some cap -> cap
            | None ->
                failwith
                  ("forge: inconsistent repetition capture $"
                  ^ name
                  ^ " in macro_rules! "
                  ^ macro_name))
          iter_envs
      in
      (name, CaptureMany caps))
    names

let rec match_matcher_seq (macro_name : string) (seq : matcher_seq)
    (input : macro_tt list) (env : capture_env) :
    (capture_env * macro_tt list) list =
  match seq with
  | [] -> [ (env, input) ]
  | elem :: rest ->
      let elem_matches = match_matcher_elem macro_name elem input env in
      List.concat_map
        (fun (env', rem) -> match_matcher_seq macro_name rest rem env')
        elem_matches

and match_repeat (macro_name : string) (inner : matcher_seq)
    (sep : Lex.token_type option) (one_or_more : bool)
    (expected_names : string list) (input : macro_tt list) (env : capture_env) :
    (capture_env * macro_tt list) list =
  let min_count = if one_or_more then 1 else 0 in
  let rec loop (iter_envs_rev : capture_env list) (rem : macro_tt list)
      (count : int) (acc : (capture_env * macro_tt list) list) =
    let acc =
      if count >= min_count then
        let iter_envs = List.rev iter_envs_rev in
        let repeat_bindings =
          build_repeat_bindings macro_name expected_names iter_envs
        in
        match merge_env env repeat_bindings with
        | Some merged -> (merged, rem) :: acc
        | None -> acc
      else acc
    in
    let one_iter_matches = match_matcher_seq macro_name inner rem [] in
    List.fold_left
      (fun acc (iter_env, rem_after_iter) ->
        if List.length rem_after_iter = List.length rem then acc
        else
          let next_rems =
            match sep with
            | None -> [ rem_after_iter ]
            | Some sep_tok -> (
                match rem_after_iter with
                | MacroTTToken tok :: tail when tok = sep_tok ->
                    [ rem_after_iter; tail ]
                | _ -> [ rem_after_iter ])
          in
          List.fold_left
            (fun acc rem_next ->
              loop (iter_env :: iter_envs_rev) rem_next (count + 1) acc)
            acc next_rems)
      acc one_iter_matches
  in
  loop [] input 0 []

and match_matcher_elem (macro_name : string) (elem : matcher_elem)
    (input : macro_tt list) (env : capture_env) :
    (capture_env * macro_tt list) list =
  match elem with
  | MatcherToken tok -> (
      match input with
      | MacroTTToken head :: tail when head = tok -> [ (env, tail) ]
      | _ -> [])
  | MatcherGroup (delim, inner) -> (
      match input with
      | MacroTTGroup (got, inner_input) :: tail when got = delim ->
          match_matcher_seq macro_name inner inner_input env
          |> List.filter_map (fun (env', rem_inner) ->
                 if rem_inner = [] then Some (env', tail) else None)
      | _ -> [])
  | MatcherCapture (name, kind) ->
      capture_candidates kind input
      |> List.filter_map (fun (captured, rem) ->
             match bind_capture env name (CaptureOne captured) with
             | Some env' -> Some (env', rem)
             | None -> None)
  | MatcherRepeat (inner, sep, one_or_more) ->
      let expected_names = matcher_capture_names inner |> List.sort_uniq String.compare in
      match_repeat macro_name inner sep one_or_more expected_names input env

let rec strict_capture_at_path (cap : macro_capture) (path : int list) :
    macro_capture option =
  match path with
  | [] -> Some cap
  | i :: rest -> (
      match cap with
      | CaptureMany xs -> (
          match nth_opt i xs with
          | Some c -> strict_capture_at_path c rest
          | None -> None)
      | CaptureOne _ -> None)

let rec relaxed_capture_at_path (cap : macro_capture) (path : int list) :
    macro_capture option =
  match path with
  | [] -> Some cap
  | i :: rest -> (
      match cap with
      | CaptureMany xs -> (
          match nth_opt i xs with
          | Some c -> relaxed_capture_at_path c rest
          | None -> None)
      | CaptureOne _ -> relaxed_capture_at_path cap rest)

let join_tt_segments_with_commas (segments : macro_tt list list) : macro_tt list =
  let rec go acc = function
    | [] -> List.rev acc
    | [ seg ] -> List.rev_append acc seg
    | seg :: rest -> go (MacroTTToken Lex.Comma :: List.rev_append seg acc) rest
  in
  go [] segments

let rec capture_to_list_elem_tts (cap : macro_capture) : macro_tt list =
  match cap with
  | CaptureOne tts -> tts
  | CaptureMany caps ->
      [
        MacroTTGroup
          (MacroBracket, join_tt_segments_with_commas (List.map capture_to_list_elem_tts caps));
      ]

let capture_tokens_for_var (env : capture_env) (path : int list) (name : string) :
    macro_tt list =
  match List.assoc_opt name env with
  | None -> failwith ("forge: unknown macro variable $" ^ name)
  | Some cap -> (
      match relaxed_capture_at_path cap path with
      | Some (CaptureOne tts) -> tts
      | Some (CaptureMany _) ->
          (* Backward-compatible extension: using a repeated capture outside a
             transcriber repetition yields a list literal. *)
          (match relaxed_capture_at_path cap path with
          | Some (CaptureMany caps) ->
              [
                MacroTTGroup
                  ( MacroBracket,
                    join_tt_segments_with_commas
                      (List.map capture_to_list_elem_tts caps) );
              ]
          | _ -> [])
      | None ->
          failwith
            ("forge: macro variable $"
            ^ name
            ^ " cannot be resolved at this repetition depth"))

let repeat_count_for_transcriber (env : capture_env) (path : int list)
    (inner : transcriber_seq) : int =
  let vars = transcriber_var_names inner |> List.sort_uniq String.compare in
  let counts =
    List.filter_map
      (fun name ->
        match List.assoc_opt name env with
        | None -> None
        | Some cap -> (
            match strict_capture_at_path cap path with
            | Some (CaptureMany xs) -> Some (name, List.length xs)
            | _ -> None))
      vars
  in
  match counts with
  | [] ->
      failwith
        "forge: transcriber repetition must contain at least one repeated macro \
         variable"
  | (_, n) :: rest ->
      List.iter
        (fun (name, n') ->
          if n' <> n then
            failwith
              ("forge: repetition length mismatch for $"
              ^ name
              ^ " in transcriber"))
        rest;
      n

let rec expand_transcriber_seq (env : capture_env) (path : int list)
    (seq : transcriber_seq) : macro_tt list =
  match seq with
  | [] -> []
  | elem :: rest ->
      let head = expand_transcriber_elem env path elem in
      head @ expand_transcriber_seq env path rest

and expand_transcriber_elem (env : capture_env) (path : int list)
    (elem : transcriber_elem) : macro_tt list =
  match elem with
  | TranscriberToken tok -> [ MacroTTToken tok ]
  | TranscriberGroup (delim, inner) ->
      [ MacroTTGroup (delim, expand_transcriber_seq env path inner) ]
  | TranscriberVar name -> capture_tokens_for_var env path name
  | TranscriberRepeat (inner, sep, one_or_more) ->
      let count = repeat_count_for_transcriber env path inner in
      if one_or_more && count = 0 then
        failwith "forge: macro repetition with + produced zero expansions"
      else
        let rec build i acc =
          if i >= count then List.rev acc
          else
            let piece = expand_transcriber_seq env (path @ [ i ]) inner in
            let acc' =
              if i = 0 then piece :: acc
              else
                match sep with
                | Some tok -> (MacroTTToken tok :: piece) :: acc
                | None -> piece :: acc
            in
            build (i + 1) acc'
        in
        build 0 [] |> List.concat

let select_macro_arm (macro_name : string) (arms : compiled_macro_arm list)
    (args : macro_tt list) : (compiled_macro_arm * capture_env) option =
  let rec try_arms = function
    | [] -> None
    | arm :: rest ->
        let matches =
          match_matcher_seq macro_name arm.matcher args []
          |> List.filter (fun (_, rem) -> rem = [])
        in
        (match matches with
        | (env, _) :: _ -> Some (arm, env)
        | [] -> try_arms rest)
  in
  try_arms arms

let split_macro_args (args : macro_tt list) : macro_tt list list =
  if args = [] then []
  else
    let rec go curr_rev acc = function
      | [] ->
          let current = List.rev curr_rev in
          List.rev (current :: acc)
      | MacroTTToken Lex.Comma :: rest ->
          let current = List.rev curr_rev in
          go [] (current :: acc) rest
      | tt :: rest -> go (tt :: curr_rev) acc rest
    in
    let raw = go [] [] args in
    match List.rev raw with
    | [] -> []
    | [] :: rest_rev -> List.rev rest_rev
    | _ -> raw

let parse_expr_from_macro_tts (tts : macro_tt list) : expr =
  let tokens = flatten_macro_tts tts in
  match Parser.ExprParser.expr_parser tokens with
  | Some (e, []) -> e
  | Some (_, rem) ->
      failwith
        ("forge: macro expansion produced trailing tokens: "
        ^ tokens_to_debug_string rem)
  | None ->
      failwith
        ("forge: macro expansion did not produce a valid expression: "
        ^ tokens_to_debug_string tokens)

let parse_macro_args_as_exprs (args : macro_tt list) : expr list =
  split_macro_args args |> List.map parse_expr_from_macro_tts

let expand_proc_macro (name : string) (args : macro_tt list)
    (expanded_arg_exprs : expr list Lazy.t) : expr option =
  match name with
  | "count_args" -> Some (int_expr (List.length (split_macro_args args)))
  | "vec" -> Some (list_expr (Lazy.force expanded_arg_exprs))
  | "stringify" ->
      Some
        (string_expr
           (String.concat ", "
              (Lazy.force expanded_arg_exprs
              |> List.map Tostring.string_of_expr)))
  | "concat_str" | "concat" ->
      let parts =
        Lazy.force expanded_arg_exprs
        |> List.map expr_as_string_literal
      in
      if List.for_all (function Some _ -> true | None -> false) parts then
        Some
          (string_expr
             (parts |> List.filter_map (fun x -> x) |> String.concat ""))
      else
        failwith "forge: concat!/concat_str! expects only string literal arguments"
  | _ -> None

let collect_declared_macros (defns : defn list) : (string, macro_def) Hashtbl.t =
  let tbl = Hashtbl.create 128 in
  let rec go (path : string list) (defs : defn list) : unit =
    List.iter
      (function
        | MacroDef (name, arms) ->
            let compiled_arms = List.map (validate_macro_arm name) arms in
            let key = scoped_key path name in
            if Hashtbl.mem tbl key then
              failwith
                ("forge: duplicate macro definition '"
                ^ (if path = [] then name
                   else String.concat "." (path @ [ name ]))
                ^ "'")
            else Hashtbl.replace tbl key { arms = compiled_arms }
        | ModDef (name, nested) -> go (path @ [ name ]) nested
        | _ -> ())
      defs
  in
  go [] defns;
  tbl

let rec resolve_macro_from_scope (tbl : (string, macro_def) Hashtbl.t)
    (path : string list) (name : string) : macro_def option =
  match Hashtbl.find_opt tbl (scoped_key path name) with
  | Some m -> Some m
  | None ->
      if path = [] then None else resolve_macro_from_scope tbl (drop_last path) name

let max_macro_expansion_depth = 128

let rec expand_expr (tbl : (string, macro_def) Hashtbl.t) (path : string list)
    (depth : int) (e : expr) : expr =
  match e with
  | Function (p, ann, body) -> Function (p, ann, expand_expr tbl path depth body)
  | Ternary (a, b, c) ->
      Ternary
        (expand_expr tbl path depth a, expand_expr tbl path depth b, expand_expr tbl path depth c)
  | ConsExpr ce -> ConsExpr (expand_cons_expr tbl path depth ce)
  | Bind (p, ann, e1, e2, rt) ->
      Bind
        ( p,
          ann,
          expand_expr tbl path depth e1,
          expand_expr tbl path depth e2,
          rt )
  | BindRec (p, ann, e1, e2, rt) ->
      BindRec
        ( p,
          ann,
          expand_expr tbl path depth e1,
          expand_expr tbl path depth e2,
          rt )
  | BindMutRec (bindings, body) ->
      BindMutRec
        ( List.map
            (fun (p, ann, rhs, rt, n) ->
              (p, ann, expand_expr tbl path depth rhs, rt, n))
            bindings,
          expand_expr tbl path depth body )
  | Switch (scrut, branches) ->
      Switch
        ( expand_expr tbl path depth scrut,
          List.map (fun (p, rhs) -> (p, expand_expr tbl path depth rhs)) branches
        )
  | Block parts ->
      Block
        (List.map
           (function
             | Expr e0 -> Expr (expand_expr tbl path depth e0)
             | Definition d0 -> Definition (expand_local_defn tbl path depth d0))
           parts)

and expand_cons_expr (tbl : (string, macro_def) Hashtbl.t) (path : string list)
    (depth : int) (ce : cons_expr) : cons_expr =
  match ce with
  | Cons (d, ce') ->
      Cons (expand_disjunction tbl path depth d, expand_cons_expr tbl path depth ce')
  | DisjunctionUnderCons d -> DisjunctionUnderCons (expand_disjunction tbl path depth d)
  | Pipeline (a, b) ->
      Pipeline (expand_cons_expr tbl path depth a, expand_cons_expr tbl path depth b)

and expand_disjunction (tbl : (string, macro_def) Hashtbl.t) (path : string list)
    (depth : int) (d : disjunction) : disjunction =
  match d with
  | Disjunction (a, b) ->
      Disjunction
        (expand_conjunction tbl path depth a, expand_disjunction tbl path depth b)
  | ConjunctionUnderDisjunction a ->
      ConjunctionUnderDisjunction (expand_conjunction tbl path depth a)

and expand_conjunction (tbl : (string, macro_def) Hashtbl.t)
    (path : string list) (depth : int) (c : conjunction) : conjunction =
  match c with
  | Conjunction (a, b) ->
      Conjunction (expand_rel_expr tbl path depth a, expand_conjunction tbl path depth b)
  | RelationUnderConjunction a -> RelationUnderConjunction (expand_rel_expr tbl path depth a)

and expand_rel_expr (tbl : (string, macro_def) Hashtbl.t) (path : string list)
    (depth : int) (r : rel_expr) : rel_expr =
  match r with
  | Relation (op, a, b) ->
      Relation (op, expand_rel_expr tbl path depth a, expand_arith_expr tbl path depth b)
  | CustomRelExpr (op, a, b) ->
      CustomRelExpr
        (op, expand_rel_expr tbl path depth a, expand_arith_expr tbl path depth b)
  | ArithmeticUnderRelExpr a -> ArithmeticUnderRelExpr (expand_arith_expr tbl path depth a)

and expand_arith_expr (tbl : (string, macro_def) Hashtbl.t)
    (path : string list) (depth : int) (a : arith_expr) : arith_expr =
  match a with
  | Plus (l, r) ->
      Plus (expand_arith_expr tbl path depth l, expand_term tbl path depth r)
  | Minus (l, r) ->
      Minus (expand_arith_expr tbl path depth l, expand_term tbl path depth r)
  | CustomArithExpr (op, l, r) ->
      CustomArithExpr
        (op, expand_arith_expr tbl path depth l, expand_term tbl path depth r)
  | Term t -> Term (expand_term tbl path depth t)

and expand_term (tbl : (string, macro_def) Hashtbl.t) (path : string list)
    (depth : int) (t : term) : term =
  match t with
  | Mul (a, b) -> Mul (expand_term tbl path depth a, expand_app_factor tbl path depth b)
  | Div (a, b) -> Div (expand_term tbl path depth a, expand_app_factor tbl path depth b)
  | Mod (a, b) -> Mod (expand_term tbl path depth a, expand_app_factor tbl path depth b)
  | CustomTerm (op, a, b) ->
      CustomTerm
        (op, expand_term tbl path depth a, expand_app_factor tbl path depth b)
  | Factor af -> Factor (expand_app_factor tbl path depth af)

and expand_app_factor (tbl : (string, macro_def) Hashtbl.t) (path : string list)
    (depth : int) (af : app_factor) : app_factor =
  match af with
  | Application (a, b) ->
      Application
        (expand_app_factor tbl path depth a, expand_factor tbl path depth b)
  | FactorUnderApplication f -> FactorUnderApplication (expand_factor tbl path depth f)

and expand_factor (tbl : (string, macro_def) Hashtbl.t) (path : string list)
    (depth : int) (f : factor) : factor =
  match f with
  | ParenFactor e -> ParenFactor (expand_expr tbl path depth e)
  | Opposite f0 -> Opposite (expand_factor tbl path depth f0)
  | Vector es -> Vector (List.map (expand_expr tbl path depth) es)
  | ListSugar es -> ListSugar (List.map (expand_expr tbl path depth) es)
  | ListEnumeration (a, b) ->
      ListEnumeration (expand_expr tbl path depth a, expand_expr tbl path depth b)
  | ListComprehension (body, generators) ->
      ListComprehension
        ( expand_expr tbl path depth body,
          List.map (fun (p, ge) -> (p, expand_expr tbl path depth ge)) generators )
  | RecordLit fields ->
      RecordLit
        (List.map (fun (n, e0) -> (n, expand_expr tbl path depth e0)) fields)
  | RecordUpdate (base, fields) ->
      RecordUpdate
        ( expand_expr tbl path depth base,
          List.map (fun (n, e0) -> (n, expand_expr tbl path depth e0)) fields )
  | FieldAccess (base, fld) -> FieldAccess (expand_factor tbl path depth base, fld)
  | MacroInvoke (name, args) ->
      ParenFactor (expand_macro_call tbl path depth name args)
  | (Boolean _ | String _ | Unit | Integer _ | Char _ | FloatFactor _ | Id _ | Nil) as x ->
      x

and expand_macro_call (tbl : (string, macro_def) Hashtbl.t) (path : string list)
    (depth : int) (name : string) (args : macro_tt list) : expr =
  if depth >= max_macro_expansion_depth then
    failwith
      (Printf.sprintf "forge: macro expansion exceeded max depth (%d) at %s!"
         max_macro_expansion_depth name)
  else
    match resolve_macro_from_scope tbl path name with
    | None -> (
        let expanded_arg_exprs =
          lazy
            (parse_macro_args_as_exprs args
            |> List.map (expand_expr tbl path (depth + 1)))
        in
        match expand_proc_macro name args expanded_arg_exprs with
        | Some e -> expand_expr tbl path (depth + 1) e
        | None -> failwith ("forge: unknown macro " ^ name ^ "!"))
    | Some { arms } -> (
        match select_macro_arm name arms args with
        | None ->
            failwith
              (Printf.sprintf
                 "forge: no matching arm for macro %s! with %d token tree(s)" name
                 (List.length args))
        | Some (arm, env) ->
            let expanded_tts = expand_transcriber_seq env [] arm.transcriber in
            let parsed = parse_expr_from_macro_tts expanded_tts in
            expand_expr tbl path (depth + 1) parsed)

and expand_local_defn (tbl : (string, macro_def) Hashtbl.t) (path : string list)
    (depth : int) (d : defn) : defn =
  match d with
  | Defn (p, cs, ann, body, rt, n) ->
      Defn (p, cs, ann, expand_expr tbl path depth body, rt, n)
  | DefnRec (p, cs, ann, body, rt, n) ->
      DefnRec (p, cs, ann, expand_expr tbl path depth body, rt, n)
  | DefnMutRec defs ->
      DefnMutRec
        (List.map
           (fun (p, cs, ann, body, rt, n) ->
             (p, cs, ann, expand_expr tbl path depth body, rt, n))
           defs)
  | ClassDef (name, params, requires, items) ->
      let items' =
        List.map
          (function
            | TraitVal _ as i -> i
            | TraitLet (m, e) -> TraitLet (m, expand_expr tbl path depth e))
          items
      in
      ClassDef (name, params, requires, items')
  | InstanceDef (cls, head_ty, requires, impls) ->
      let impls' =
        List.map
          (fun (m, e) -> (m, expand_expr tbl path depth e))
          impls
      in
      InstanceDef (cls, head_ty, requires, impls')
  | MacroDef _ ->
      failwith "forge: macro_rules! is not supported inside expression blocks"
  | (TypeDef _ | SumTypeDef _ | SumTypeDefRec _ | SumTypeDefMutRec _ | ModDef _
    | UseDef _ | ImportDef _) as d ->
      d

let expand_top_defn_non_mod (tbl : (string, macro_def) Hashtbl.t)
    (path : string list) (d : defn) : defn list =
  match d with
  | Defn (p, cs, ann, body, rt, n) ->
      [ Defn (p, cs, ann, expand_expr tbl path 0 body, rt, n) ]
  | DefnRec (p, cs, ann, body, rt, n) ->
      [ DefnRec (p, cs, ann, expand_expr tbl path 0 body, rt, n) ]
  | DefnMutRec defs ->
      [
        DefnMutRec
          (List.map
             (fun (p, cs, ann, body, rt, n) ->
               (p, cs, ann, expand_expr tbl path 0 body, rt, n))
             defs);
      ]
  | ClassDef (name, params, requires, items) ->
      let items' =
        List.map
          (function
            | TraitVal _ as i -> i
            | TraitLet (m, e) -> TraitLet (m, expand_expr tbl path 0 e))
          items
      in
      [ ClassDef (name, params, requires, items') ]
  | InstanceDef (cls, head_ty, requires, impls) ->
      let impls' =
        List.map
          (fun (m, e) -> (m, expand_expr tbl path 0 e))
          impls
      in
      [ InstanceDef (cls, head_ty, requires, impls') ]
  | MacroDef _ -> []
  | (TypeDef _ | SumTypeDef _ | SumTypeDefRec _ | SumTypeDefMutRec _ | UseDef _
    | ImportDef _) as d ->
      [ d ]
  | ModDef _ -> []

let rec expand_top_defns (tbl : (string, macro_def) Hashtbl.t)
    (path : string list) (defns : defn list) : defn list =
  let rec go acc = function
    | [] -> List.rev acc
    | ModDef (name, nested) :: rest ->
        let nested_defs = expand_top_defns tbl (path @ [ name ]) nested in
        go (ModDef (name, nested_defs) :: acc) rest
    | d :: rest ->
        let defs = expand_top_defn_non_mod tbl path d in
        go (List.rev_append defs acc) rest
  in
  go [] defns

let expand_program (defns : defn list) : defn list =
  let macros = collect_declared_macros defns in
  expand_top_defns macros [] defns

let expand_expr_in_context (context_defns : defn list) (e : expr) : expr =
  let macros = collect_declared_macros context_defns in
  expand_expr macros [] 0 e
