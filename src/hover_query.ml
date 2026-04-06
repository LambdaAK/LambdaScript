open Lex
open Parser.ProgramParser
open Condense
open Typecheck
open Build_env
open Cexpr
open Ceval

let byte_offset_of_line_char (text : string) (line0 : int) (char0 : int) : int
    =
  let lines = String.split_on_char '\n' text in
  let n_lines = List.length lines in
  let rec line_start i acc =
    if i >= line0 || i >= n_lines then acc
    else line_start (i + 1) (acc + String.length (List.nth lines i) + 1)
  in
  line_start 0 0 + char0

let type_string_for_id (env : static_env) (type_env : type_env) (name : string)
    : string option =
  match Typecheck.type_of_c_expr env type_env (EId (name, None)) with
  | Ok ct -> Some (C_to_string.string_of_c_type ct)
  | Error _ -> None

(** Synthetic dictionary dispatch ids from [Condense.internal_tc_id]; not real
    source names — do not report [type_of_c_expr] on them for hover. *)
let hover_skip_internal_tc_app_name (name : string) : bool =
  String.starts_with ~prefix:"__forge_tc_" name

(** Minimum [byte_start] of any [EId (_, Some (lo, _))] in [e], or [None] if
    there are no spanned ids (used to tell whether the cursor is left of the
    whole argument when the callee id lost its span). *)
let rec min_lo_spanned_id_byte (e : c_expr) : int option =
  let min2 a b =
    match (a, b) with
    | None, x | x, None -> x
    | Some i, Some j -> Some (min i j)
  in
  match e with
  | EId (_, Some (lo, _)) -> Some lo
  | EId (_, None) -> None
  | EApp (a, b) -> min2 (min_lo_spanned_id_byte a) (min_lo_spanned_id_byte b)
  | EBop (_, a, b) -> min2 (min_lo_spanned_id_byte a) (min_lo_spanned_id_byte b)
  | ETernary (a, b, c) ->
      min2 (min_lo_spanned_id_byte a)
        (min2 (min_lo_spanned_id_byte b) (min_lo_spanned_id_byte c))
  | EFunction (_, _, body) -> min_lo_spanned_id_byte body
  | EBind (_, _, e1, e2, _) ->
      min2 (min_lo_spanned_id_byte e1) (min_lo_spanned_id_byte e2)
  | EBindRec (_, _, e1, e2, _) ->
      min2 (min_lo_spanned_id_byte e1) (min_lo_spanned_id_byte e2)
  | EBindMutRec (bs, body) ->
      List.fold_left
        (fun acc (_, _, e1, _, _) -> min2 acc (min_lo_spanned_id_byte e1))
        (min_lo_spanned_id_byte body) bs
  | EBlock parts ->
      List.fold_left
        (fun acc part ->
          match part with
          | Expr ex -> min2 acc (min_lo_spanned_id_byte ex)
          | Defn _ -> acc)
        None parts
  | ESwitch (scr, brs) ->
      List.fold_left
        (fun acc (_, be) -> min2 acc (min_lo_spanned_id_byte be))
        (min_lo_spanned_id_byte scr) brs
  | EVector es ->
      List.fold_left (fun acc e -> min2 acc (min_lo_spanned_id_byte e)) None es
  | EListEnumeration (a, b) ->
      min2 (min_lo_spanned_id_byte a) (min_lo_spanned_id_byte b)
  | EListComprehension (e0, gens) ->
      List.fold_left
        (fun acc (_, ge) -> min2 acc (min_lo_spanned_id_byte ge))
        (min_lo_spanned_id_byte e0) gens
  | ERecordLit fs ->
      List.fold_left
        (fun acc (_, ex) -> min2 acc (min_lo_spanned_id_byte ex))
        None fs
  | ERecordUpdate (base, upd) ->
      List.fold_left
        (fun acc (_, ex) -> min2 acc (min_lo_spanned_id_byte ex))
        (min_lo_spanned_id_byte base) upd
  | EFieldAccess (e0, _) -> min_lo_spanned_id_byte e0
  | EInt _ | EFloat _ | EBool _ | EString _ | EChar _ | EUnit | ENil -> None

let hover_arrow_dual (env : static_env) (type_env : type_env) (name : string)
    (e2 : c_expr) : string option =
  match
    ( Typecheck.type_of_c_expr env type_env (EApp (EId (name, None), e2)),
      Typecheck.type_of_c_expr env type_env e2 )
  with
  | Ok res_ct, Ok arg_ct -> (
      match
        ( Typecheck.instantiate_and_simplify arg_ct type_env,
          Typecheck.instantiate_and_simplify res_ct type_env )
      with
      | Ok arg_m, Ok res_m ->
          Some
            (C_to_string.string_of_c_type (Mono (FunctionType (arg_m, res_m))))
      | _ -> None )
  | _ -> None

(** Reject [EId] spans that cross the prelude/user boundary: prelude ids must
    not match a cursor in the user buffer and vice versa. *)
let cursor_in_id_span_ok (cursor : int) (user_byte_lo : int) (lo, hi) : bool =
  lo <= cursor && cursor < hi
  && if cursor < user_byte_lo then hi <= user_byte_lo else lo >= user_byte_lo

(** Lexical [Id] token covering [offset] in [full_source], if any. Disambiguates
    mis-queued AST spans (e.g. prelude id bytes attached to the wrong [EId]). *)
let id_name_at_byte_offset (tokens : Lex.token list) (offset : int) : string option
    =
  List.find_map
    (fun (t : Lex.token) ->
      if t.byte_start <= offset && offset < t.byte_end then
        match t.token_type with Lex.Id s -> Some s | _ -> None
      else None)
    tokens

let try_id_hover (env : static_env) (type_env : type_env) (user_byte_lo : int)
    (lex_id : string option) (offset : int) (e : c_expr) : string option =
  match e with
  | EId (name, Some (a, b)) -> (
      match lex_id with
      | Some s when s = name ->
          if cursor_in_id_span_ok offset user_byte_lo (a, b) then
            type_string_for_id env type_env name
          else None
      | _ -> None)
  | EId _ -> None
  | _ -> None

let rec names_bound_in_pat (p : c_pat) : string list =
  match p with
  | CIdPat s -> [ s ]
  | CConsPat (a, b) -> names_bound_in_pat a @ names_bound_in_pat b
  | CVectorPat ps -> List.concat (List.map names_bound_in_pat ps)
  | CRecordPat fs -> List.concat (List.map (fun (_, p0) -> names_bound_in_pat p0) fs)
  | CVariantPat (_, None) -> []
  | CVariantPat (_, Some sub) -> names_bound_in_pat sub
  | CWildcardPat | CIntPat _ | CBoolPat _ | CCharPat _ | CStringPat _ | CUnitPat
  | CNilPat ->
      []

(** [true] if [offset] is strictly before the first spanned [EId] in the RHS
    (so it likely lies in [let pat =] / [| Pat ->] pattern text, not the RHS). *)
let cursor_on_lhs_of_rhs (offset : int) (rhs : c_expr) : bool =
  match min_lo_spanned_id_byte rhs with
  | None -> true
  | Some m -> offset < m

let type_string_from_rhs_pat (env : static_env) (type_env : type_env)
    (pat : c_pat) (rhs : c_expr) (name : string) : string option =
  match Typecheck.type_of_c_expr env type_env rhs with
  | Ok ct -> (
      match bind_static pat ct with
      | Some bs -> (
          match List.assoc_opt name bs with
          | Some t -> Some (C_to_string.string_of_c_type t)
          | None -> None)
      | None -> None)
  | Error _ -> None

let try_rhs_pat_hover (env : static_env) (type_env : type_env)
    (lex_id : string option) (offset : int) (pat : c_pat) (rhs : c_expr) :
    string option =
  match lex_id with
  | None -> None
  | Some name -> (
      if not (List.mem name (names_bound_in_pat pat)) then None
      else if not (cursor_on_lhs_of_rhs offset rhs) then None
      else type_string_from_rhs_pat env type_env pat rhs name)

let try_defn_pat_hover (env : static_env) (type_env : type_env)
    (lex_id : string option) (offset : int) (pat : c_pat) (body : c_expr) :
    string option =
  match lex_id with
  | None -> None
  | Some name -> (
      if not (List.mem name (names_bound_in_pat pat)) then None
      else if not (cursor_on_lhs_of_rhs offset body) then None
      else type_string_for_id env type_env name)

let type_for_switch_pattern_binder (env : static_env) (type_env : type_env)
    (scr : c_expr) (pat : c_pat) (name : string) : string option =
  match Typecheck.type_of_c_expr env type_env scr with
  | Error _ -> None
  | Ok scrut_ct -> (
      match pat with
      | CVariantPat (cons_name, Some subpat)
        when List.mem name (names_bound_in_pat subpat) -> (
          match List.assoc_opt cons_name env with
          | Some cons_ct -> (
              match Typecheck.instantiate cons_ct with
              | FunctionType (payload_m, _) -> (
                  match bind_static subpat (Mono payload_m) with
                  | Some bs -> (
                      match List.assoc_opt name bs with
                      | Some t -> Some (C_to_string.string_of_c_type t)
                      | None -> None)
                  | None -> None)
              | _ -> None)
          | None -> None)
      | _ -> (
          match bind_static pat scrut_ct with
          | Some bs -> (
              match List.assoc_opt name bs with
              | Some t -> Some (C_to_string.string_of_c_type t)
              | None -> None)
          | None -> None))

let try_switch_pat_hover (env : static_env) (type_env : type_env)
    (lex_id : string option) (offset : int) (scr : c_expr) (pat : c_pat)
    (branch_e : c_expr) : string option =
  match lex_id with
  | None -> None
  | Some name -> (
      if not (List.mem name (names_bound_in_pat pat)) then None
      else if not (cursor_on_lhs_of_rhs offset branch_e) then None
      else type_for_switch_pattern_binder env type_env scr pat name)

let try_efunction_param_hover (env : static_env) (type_env : type_env)
    (lex_id : string option) (offset : int) (pat : c_pat) (ann : c_type option)
    (body : c_expr) : string option =
  match lex_id with
  | None -> None
  | Some name -> (
      if not (List.mem name (names_bound_in_pat pat)) then None
      else if not (cursor_on_lhs_of_rhs offset body) then None
      else
        match Typecheck.type_of_c_expr env type_env (EFunction (pat, ann, body)) with
        | Ok ct -> (
            let m = Typecheck.instantiate ct in
            match (pat, m) with
            | CIdPat _, FunctionType (dom, _) -> (
                match bind_static pat (Mono dom) with
                | Some bs -> (
                    match List.assoc_opt name bs with
                    | Some t -> Some (C_to_string.string_of_c_type t)
                    | None -> None)
                | None -> None)
            | _ -> None)
        | Error _ -> None)

let rec visit_expr (env : static_env) (type_env : type_env) (user_byte_lo : int)
    (lex_id : string option) (offset : int) (e : c_expr) : string option =
  (* Handle [EApp (EId _, _)] before [try_id_hover]: otherwise a generic
     [EApp (e1, e2)] visit visits the callee [EId] first and
     [type_string_for_id] reports the unconstrained scheme (e.g. [a -> a] for
     [println]) instead of the arrow at this call site. *)
  match e with
  | EApp (EId (name, pos_opt), e2) -> (
      if hover_skip_internal_tc_app_name name then
        visit_expr env type_env user_byte_lo lex_id offset e2
      else
        let on_fun =
          match pos_opt with
          | Some (lo, hi) ->
              lo <= offset && offset < hi
              && cursor_in_id_span_ok offset user_byte_lo (lo, hi)
              && (match lex_id with Some s -> s = name | None -> false)
          | None -> false
        in
        if on_fun then hover_arrow_dual env type_env name e2
        else
          match min_lo_spanned_id_byte e2 with
          | Some m when offset < m -> (
              match lex_id with
              | Some s when s = name -> hover_arrow_dual env type_env name e2
              | _ -> (
                  match visit_expr env type_env user_byte_lo lex_id offset e2 with
                  | Some _ as r -> r
                  | None -> (
                      match pos_opt with
                      | None -> (
                          match lex_id with
                          | Some s2 when s2 = name ->
                              hover_arrow_dual env type_env name e2
                          | _ -> None)
                      | Some _ -> None ) ) )
          | _ -> (
              match visit_expr env type_env user_byte_lo lex_id offset e2 with
              | Some _ as r -> r
              | None -> (
                  match pos_opt with
                  | None -> (
                      match lex_id with
                      | Some s when s = name -> hover_arrow_dual env type_env name e2
                      | _ -> None)
                  | Some _ -> None ) ) )
  | _ -> (
      match try_id_hover env type_env user_byte_lo lex_id offset e with
      | Some _ as r -> r
      | None -> (
          match e with
          | EApp (e1, e2) -> (
              match visit_expr env type_env user_byte_lo lex_id offset e1 with
              | Some _ as r -> r
              | None -> visit_expr env type_env user_byte_lo lex_id offset e2)
      | EBind (pat, _, e1, e2, _) -> (
          match try_rhs_pat_hover env type_env lex_id offset pat e1 with
          | Some _ as r -> r
          | None -> (
              match visit_expr env type_env user_byte_lo lex_id offset e1 with
              | Some _ as r -> r
              | None -> (
                  match Typecheck.type_of_c_expr env type_env e1 with
                  | Ok ct -> (
                      match bind_static pat ct with
                      | Some bindings ->
                          visit_expr (bindings @ env) type_env user_byte_lo lex_id offset e2
                      | None -> visit_expr env type_env user_byte_lo lex_id offset e2)
                  | Error _ -> visit_expr env type_env user_byte_lo lex_id offset e2)))
      | EBindRec (pat, _, e1, e2, _) -> (
          match pat with
          | CIdPat id ->
              let rec_ty = fresh_type_var () in
              let rec_env = (id, Mono rec_ty) :: env in
              ( match try_rhs_pat_hover rec_env type_env lex_id offset pat e1 with
              | Some _ as r -> r
              | None -> (
                  match visit_expr rec_env type_env user_byte_lo lex_id offset e1 with
                  | Some _ as r -> r
                  | None -> (
                      match Typecheck.type_of_c_expr rec_env type_env e1 with
                      | Ok gen_ct ->
                          visit_expr ((id, gen_ct) :: env) type_env user_byte_lo lex_id offset e2
                      | Error _ -> visit_expr env type_env user_byte_lo lex_id offset e2 ) ) )
          | _ -> (
              match visit_expr env type_env user_byte_lo lex_id offset e1 with
              | Some _ as r -> r
              | None -> visit_expr env type_env user_byte_lo lex_id offset e2))
      | EBindMutRec (bindings, body) -> (
          let ids_opt =
            List.fold_left
              (fun acc (pat, _, _, _, _) ->
                match (acc, pat) with
                | None, _ -> None
                | Some ids, CIdPat id -> Some (id :: ids)
                | Some _, _ -> None)
              (Some []) bindings
          in
          match ids_opt with
          | None -> visit_expr env type_env user_byte_lo lex_id offset body
          | Some function_ids ->
              let function_ids = List.rev function_ids in
              let fresh_types =
                List.map (fun _ -> fresh_type_var ()) function_ids
              in
              let mut_rec_env =
                List.fold_left2
                  (fun acc id ft -> (id, Mono ft) :: acc)
                  env function_ids fresh_types
              in
              match
                List.fold_left
                  (fun acc (pat, _, e1, _, _) ->
                    match acc with
                    | Some _ as r -> r
                    | None -> (
                        match try_rhs_pat_hover mut_rec_env type_env lex_id offset pat e1 with
                        | Some _ as r -> r
                        | None -> visit_expr mut_rec_env type_env user_byte_lo lex_id offset e1))
                  None bindings
              with
              | Some _ as r -> r
              | None -> (
                  let generalized_types =
                    List.map
                      (fun (_, _, e1, _, _) -> (
                        match Typecheck.type_of_c_expr mut_rec_env type_env e1 with
                        | Ok ct -> ct
                        | Error _ -> Mono (fresh_type_var ())))
                      bindings
                  in
                  let body_env =
                    List.fold_left2
                      (fun acc id gt -> (id, gt) :: acc)
                      env function_ids generalized_types
                  in
                  visit_expr body_env type_env user_byte_lo lex_id offset body))
      | EFunction (pat, ann, body) -> (
          match try_efunction_param_hover env type_env lex_id offset pat ann body with
          | Some _ as r -> r
          | None -> (
              match Typecheck.type_of_c_expr env type_env (EFunction (pat, ann, body)) with
              | Ok ct -> (
                  let m = Typecheck.instantiate ct in
                  match (pat, m) with
                  | CIdPat _, FunctionType (dom, _) -> (
                      match bind_static pat (Mono dom) with
                      | Some bindings ->
                          visit_expr (bindings @ env) type_env user_byte_lo lex_id offset body
                      | None -> visit_expr env type_env user_byte_lo lex_id offset body)
                  | _ -> visit_expr env type_env user_byte_lo lex_id offset body)
              | Error _ -> visit_expr env type_env user_byte_lo lex_id offset body))
      | ETernary (a, b, c) -> (
          match visit_expr env type_env user_byte_lo lex_id offset a with
          | Some _ as r -> r
          | None -> (
              match visit_expr env type_env user_byte_lo lex_id offset b with
              | Some _ as r -> r
              | None -> visit_expr env type_env user_byte_lo lex_id offset c))
      | EBop (_, a, b) -> (
          match visit_expr env type_env user_byte_lo lex_id offset a with
          | Some _ as r -> r
          | None -> visit_expr env type_env user_byte_lo lex_id offset b)
      | ESwitch (scr, branches) -> (
          match visit_expr env type_env user_byte_lo lex_id offset scr with
          | Some _ as r -> r
          | None ->
              List.fold_left
                (fun acc (pat, be) ->
                  match acc with
                  | Some _ as r -> r
                  | None -> (
                      let _pattern_type, pat_env, _ =
                        Typecheck.type_of_pat env type_env pat
                      in
                      let env_in_branch = pat_env @ env in
                      match try_switch_pat_hover env type_env lex_id offset scr pat be with
                      | Some _ as r -> r
                      | None ->
                          visit_expr env_in_branch type_env user_byte_lo lex_id offset be))
                None branches)
      | EVector es ->
          List.fold_left
            (fun acc e ->
              match acc with
              | Some _ as r -> r
              | None -> visit_expr env type_env user_byte_lo lex_id offset e)
            None es
      | EBlock parts ->
          let rec walk_block (benv : static_env) = function
            | [] -> None
            | Expr ex :: rest -> (
                match visit_expr benv type_env user_byte_lo lex_id offset ex with
                | Some _ as r -> r
                | None -> walk_block benv rest)
            | Defn d :: rest -> (
                match generate_defn benv type_env d with
                | Error _ -> None
                | Ok (nb, _, _) ->
                    let benv' = nb @ benv in
                    match hover_in_defn benv' type_env user_byte_lo lex_id offset d with
                    | Some _ as r -> r
                    | None -> walk_block benv' rest)
          in
          walk_block env parts
      | ERecordLit fs ->
          List.fold_left
            (fun acc (_, ex) ->
              match acc with
              | Some _ as r -> r
              | None -> visit_expr env type_env user_byte_lo lex_id offset ex)
            None fs
      | ERecordUpdate (base, upd) -> (
          match visit_expr env type_env user_byte_lo lex_id offset base with
          | Some _ as r -> r
          | None ->
              List.fold_left
                (fun acc (_, ex) ->
                  match acc with
                  | Some _ as r -> r
                  | None -> visit_expr env type_env user_byte_lo lex_id offset ex)
                None upd)
      | EFieldAccess (e0, _) -> visit_expr env type_env user_byte_lo lex_id offset e0
      | EListEnumeration (a, b) -> (
          match visit_expr env type_env user_byte_lo lex_id offset a with
          | Some _ as r -> r
          | None -> visit_expr env type_env user_byte_lo lex_id offset b)
      | EListComprehension (e0, gens) -> (
          match visit_expr env type_env user_byte_lo lex_id offset e0 with
          | Some _ as r -> r
          | None ->
              List.fold_left
                (fun acc (_, ge) ->
                  match acc with
                  | Some _ as r -> r
                  | None -> visit_expr env type_env user_byte_lo lex_id offset ge)
                None gens)
      | EInt _ | EFloat _ | EBool _ | EString _ | EChar _ | EUnit | ENil -> None
      | _ -> None ) )

and hover_in_defn (env : static_env) (type_env : type_env) (user_byte_lo : int)
    (lex_id : string option) (offset : int) (d : c_defn) : string option =
  match d with
  | CDefn (pat, _, _, body, _, _) -> (
      match try_defn_pat_hover env type_env lex_id offset pat body with
      | Some _ as r -> r
      | None -> visit_expr env type_env user_byte_lo lex_id offset body)
  | CDefnRec (pat, _, _, body, _, _) ->
      let pattern_type, pattern_env, _ =
        Typecheck.type_of_pat env type_env pat
      in
      let _ = pattern_type in
      let rec_type = fresh_type_var () in
      let rec_env =
        match pattern_env with
        | [] -> env
        | (id, _) :: _ -> (id, Mono rec_type) :: env
      in
      ( match try_defn_pat_hover env type_env lex_id offset pat body with
      | Some _ as r -> r
      | None -> visit_expr rec_env type_env user_byte_lo lex_id offset body )
  | CDefnMutRec defs ->
      let patterns_and_fresh_types =
        List.map
          (fun (pat, _, _, _, _, _) ->
            let _pattern_type, pattern_env, _ =
              Typecheck.type_of_pat env type_env pat
            in
            let fresh_type = fresh_type_var () in
            (pat, pattern_env, fresh_type))
          defs
      in
      let rec_env =
        List.fold_left
          (fun acc_env (_, pattern_env, fresh_type) ->
            List.map (fun (id, _) -> (id, Mono fresh_type)) pattern_env @ acc_env)
          env patterns_and_fresh_types
      in
      List.fold_left
        (fun acc (pat, _, _, body, _, _) ->
          match acc with
          | Some _ as r -> r
          | None -> (
              match try_defn_pat_hover env type_env lex_id offset pat body with
              | Some _ as r -> r
              | None -> visit_expr rec_env type_env user_byte_lo lex_id offset body))
        None defs
  | CClassDecl _ | CTypeAlias _ | CSumType _ | CSumTypeRec _ | CSumTypeRecMutRec _
    ->
      None

let rec typecheck_defns_hover static_env type_env ctor_env defns :
    (static_env * type_env * constructor_env, type_error) result =
  match defns with
  | [] -> Ok (static_env, type_env, ctor_env)
  | defn :: rest -> (
      match generate_defn static_env type_env defn with
      | Ok (nb, nte, nce) ->
          typecheck_defns_hover (nb @ static_env) (nte @ type_env)
            (nce @ ctor_env) rest
      | Error e -> Error e)

let rec walk_defns (acc_env : static_env) (acc_te : type_env) (user_byte_lo : int)
    (lex_id : string option) (offset : int) (defs : c_defn list) : string option =
  match defs with
  | [] -> None
  | d :: rest -> (
      match generate_defn acc_env acc_te d with
      | Error _ -> None
      | Ok (nb, _, _) ->
          let env_for_hover = nb @ acc_env in
          ( match hover_in_defn env_for_hover acc_te user_byte_lo lex_id offset d with
          | Some _ as r -> r
          | None -> walk_defns env_for_hover acc_te user_byte_lo lex_id offset rest ))

let hover_type_for_identifier ~(prelude : bool) ~(src_path : string)
    ~(source : string) ~(line0 : int) ~(char0 : int) : (string, string) result =
  let full_source = Prelude.prepend_to_source ~enabled:prelude ~src_path source in
  let user_off = byte_offset_of_line_char source line0 char0 in
  let delta = String.length full_source - String.length source in
  (* First byte of user [source] inside [full_source] after optional prelude. *)
  let user_byte_lo = delta in
  let offset = user_off + delta in
  let full_tokens = lex (full_source |> String.to_seq |> List.of_seq) in
  let lex_id_at_offset = id_name_at_byte_offset full_tokens offset in
  set_id_queue_from_tokens full_tokens;
  let tokens = List.map (fun t -> t.token_type) full_tokens in
  match program_parser tokens with
  | None ->
      clear_id_queue ();
      Error "parse failed"
  | Some (_, remaining) when remaining <> [] ->
      clear_id_queue ();
      Error "parse failed: extra tokens"
  | Some (program, _) -> (
      let condensed =
        condense_program
          ?user_id_byte_min_after_prelude:
            (if delta = 0 then None
             else Some (Prelude.defn_count_when_parsed (), delta))
          program
      in
      clear_id_queue ();
      let static_env = build_full_static_env () in
      let type_env : type_env = [] in
      let ctor_env : constructor_env = [] in
      match typecheck_defns_hover static_env type_env ctor_env condensed with
      | Error e -> Error (string_of_type_check_error e)
      | Ok (_env, te, _) -> (
          match walk_defns static_env te user_byte_lo lex_id_at_offset offset condensed with
          | Some s -> Ok s
          | None -> Error "no typed identifier at this position"
          ))
