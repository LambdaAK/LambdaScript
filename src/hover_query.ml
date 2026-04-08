open Lex
open Parser.ProgramParser
open Condense
open Typecheck
open Build_env
open Cexpr
open Ceval
open Typefixer

(** Pretty-print for IDE hover: rename solver metavariables ([t123], [TCtorApp]
    heads) to [a], [b], … like the rest of the typechecker UI. *)
let hover_string_of_c_type (ct : c_type) : string =
  C_to_string.string_of_c_type (fix_c_type ct)

let hover_string_of_mono (m : mono_type) : string =
  hover_string_of_c_type (Mono m)

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
  | Ok ct -> Some (hover_string_of_c_type ct)
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

(** Like [min_lo_spanned_id_byte], but only [EId] spans with [lo >= byte_min]
    (typically [byte_min = user_byte_lo] after prepending the prelude). Otherwise
    the minimum can be a prelude identifier's bytes, making [offset < m] false
    for user-parameter hovers on lines like [let map func l =]. *)
let rec min_lo_spanned_id_byte_from (byte_min : int) (e : c_expr) : int option =
  let min2 a b =
    match (a, b) with
    | None, x | x, None -> x
    | Some i, Some j -> Some (min i j)
  in
  match e with
  | EId (_, Some (lo, _)) ->
      if lo >= byte_min then Some lo else None
  | EId (_, None) -> None
  | EApp (a, b) ->
      min2 (min_lo_spanned_id_byte_from byte_min a) (min_lo_spanned_id_byte_from byte_min b)
  | EBop (_, a, b) ->
      min2 (min_lo_spanned_id_byte_from byte_min a) (min_lo_spanned_id_byte_from byte_min b)
  | ETernary (a, b, c) ->
      min2 (min_lo_spanned_id_byte_from byte_min a)
        (min2 (min_lo_spanned_id_byte_from byte_min b) (min_lo_spanned_id_byte_from byte_min c))
  | EFunction (_, _, body) -> min_lo_spanned_id_byte_from byte_min body
  | EBind (_, _, e1, e2, _) ->
      min2 (min_lo_spanned_id_byte_from byte_min e1) (min_lo_spanned_id_byte_from byte_min e2)
  | EBindRec (_, _, e1, e2, _) ->
      min2 (min_lo_spanned_id_byte_from byte_min e1) (min_lo_spanned_id_byte_from byte_min e2)
  | EBindMutRec (bs, body) ->
      List.fold_left
        (fun acc (_, _, e1, _, _) -> min2 acc (min_lo_spanned_id_byte_from byte_min e1))
        (min_lo_spanned_id_byte_from byte_min body) bs
  | EBlock parts ->
      List.fold_left
        (fun acc part ->
          match part with
          | Expr ex -> min2 acc (min_lo_spanned_id_byte_from byte_min ex)
          | Defn _ -> acc)
        None parts
  | ESwitch (scr, brs) ->
      List.fold_left
        (fun acc (_, be) -> min2 acc (min_lo_spanned_id_byte_from byte_min be))
        (min_lo_spanned_id_byte_from byte_min scr) brs
  | EVector es ->
      List.fold_left
        (fun acc e -> min2 acc (min_lo_spanned_id_byte_from byte_min e))
        None es
  | EListEnumeration (a, b) ->
      min2 (min_lo_spanned_id_byte_from byte_min a) (min_lo_spanned_id_byte_from byte_min b)
  | EListComprehension (e0, gens) ->
      List.fold_left
        (fun acc (_, gen_e) -> min2 acc (min_lo_spanned_id_byte_from byte_min gen_e))
        (min_lo_spanned_id_byte_from byte_min e0) gens
  | ERecordLit fs ->
      List.fold_left
        (fun acc (_, ex) -> min2 acc (min_lo_spanned_id_byte_from byte_min ex))
        None fs
  | ERecordUpdate (base, upd) ->
      List.fold_left
        (fun acc (_, ex) -> min2 acc (min_lo_spanned_id_byte_from byte_min ex))
        (min_lo_spanned_id_byte_from byte_min base) upd
  | EFieldAccess (e0, _) -> min_lo_spanned_id_byte_from byte_min e0
  | EInt _ | EFloat _ | EBool _ | EString _ | EChar _ | EUnit | ENil -> None

(** Any [EId] subexpression (spanned or not). Used with [min_lo_spanned_id_byte]:
    if the RHS has identifiers but none carry spans, [cursor_on_lhs_of_rhs] must
    not assume the cursor is on the lambda parameter for every offset. *)
let rec expr_contains_eid (e : c_expr) : bool =
  match e with
  | EId _ -> true
  | EApp (a, b) -> expr_contains_eid a || expr_contains_eid b
  | EBop (_, a, b) -> expr_contains_eid a || expr_contains_eid b
  | ETernary (a, b, c) ->
      expr_contains_eid a || expr_contains_eid b || expr_contains_eid c
  | EFunction (_, _, body) -> expr_contains_eid body
  | EBind (_, _, e1, e2, _) | EBindRec (_, _, e1, e2, _) ->
      expr_contains_eid e1 || expr_contains_eid e2
  | EBindMutRec (bs, body) ->
      List.exists (fun (_, _, e1, _, _) -> expr_contains_eid e1) bs
      || expr_contains_eid body
  | EBlock parts ->
      List.exists
        (function Expr ex -> expr_contains_eid ex | Defn _ -> false)
        parts
  | ESwitch (scr, brs) ->
      expr_contains_eid scr
      || List.exists (fun (_, be) -> expr_contains_eid be) brs
  | EVector es -> List.exists expr_contains_eid es
  | EListEnumeration (a, b) -> expr_contains_eid a || expr_contains_eid b
  | EListComprehension (e0, gens) ->
      expr_contains_eid e0
      || List.exists (fun (_, ge) -> expr_contains_eid ge) gens
  | ERecordLit fs -> List.exists (fun (_, ex) -> expr_contains_eid ex) fs
  | ERecordUpdate (base, upd) ->
      expr_contains_eid base || List.exists (fun (_, ex) -> expr_contains_eid ex) upd
  | EFieldAccess (e0, _) -> expr_contains_eid e0
  | EInt _ | EFloat _ | EBool _ | EString _ | EChar _ | EUnit | ENil -> false

(** Like [cursor_on_lhs_of_rhs] but conservative when the body has [EId] nodes
    with no spans (otherwise any cursor position is wrongly "left of RHS"). *)
let cursor_on_lambda_param_site (offset : int) (rhs : c_expr) : bool =
  match min_lo_spanned_id_byte rhs with
  | Some m -> offset < m
  | None -> not (expr_contains_eid rhs)

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
            (hover_string_of_mono (FunctionType (arg_m, res_m)))
      | _ -> None )
  | _ -> None

(** Reject [EId] spans that cross the prelude/user boundary: prelude ids must
    not match a cursor in the user buffer and vice versa. *)
let cursor_in_id_span_ok (cursor : int) (user_byte_lo : int) (lo, hi) : bool =
  lo <= cursor && cursor < hi
  && if cursor < user_byte_lo then hi <= user_byte_lo else lo >= user_byte_lo

(** Bound name for [(op)] / [val (op)] — same strings as parser
    [infix_id_parser] / [paren_infix_pat_parser]. *)
let lexer_operator_symbol (tt : Lex.token_type) : string option =
  match tt with
  | Relop s | Addop s | Mulop s | Logop s -> Some s
  | AND -> Some "&&"
  | OR -> Some "||"
  | ConsToken -> Some "::"
  | _ -> None

(** [(==)] is [LParen]; [Relop "=="]; [RParen]. Treat the whole group as the
    operator when the cursor is on [(], [)], or the middle token. *)
let rec paren_wrapped_operator_covering (offset : int) :
    Lex.token list -> (string * int * int) option = function
  | [] | [ _ ] | [ _; _ ] -> None
  | l :: op :: r :: _ as window -> (
      match (l.Lex.token_type, r.Lex.token_type) with
      | LParen, RParen -> (
          match lexer_operator_symbol op.Lex.token_type with
          | Some s
            when l.Lex.byte_start <= offset && offset < r.Lex.byte_end ->
              Some (s, l.Lex.byte_start, r.Lex.byte_end)
          | _ -> paren_wrapped_operator_covering offset (List.tl window))
      | _ -> paren_wrapped_operator_covering offset (List.tl window))

(** Lexical [Id] or parenthesized infix operator token covering [offset] in
    [full_source], if any. Disambiguates mis-queued AST spans (e.g. prelude id
    bytes attached to the wrong [EId]). *)
let id_name_at_byte_offset (tokens : Lex.token list) (offset : int) : string option
    =
  match
    List.find_map
      (fun (t : Lex.token) ->
        if t.byte_start <= offset && offset < t.byte_end then
          match t.token_type with
          | Id s -> Some s
          | tt -> lexer_operator_symbol tt
        else None)
      tokens
  with
  | Some _ as r -> r
  | None -> (
      match paren_wrapped_operator_covering offset tokens with
      | Some (s, _, _) -> Some s
      | None -> None)

(** Lexer [Id] or [(op)] operator that contains [offset], if any. *)
let lexer_id_covering_offset (tokens : Lex.token list) (offset : int) :
    (string * int * int) option =
  match
    List.find_map
      (fun (t : Lex.token) ->
        if t.byte_start <= offset && offset < t.byte_end then
          match t.token_type with
          | Id s -> Some (s, t.byte_start, t.byte_end)
          | tt -> (
              match lexer_operator_symbol tt with
              | Some s -> Some (s, t.byte_start, t.byte_end)
              | None -> None)
        else None)
      tokens
  with
  | Some _ as r -> r
  | None -> paren_wrapped_operator_covering offset tokens

(** Reject [EId] spans from the condensation id-queue when they disagree with
    the actual [Id] token at the cursor (prelude/user queue skew would otherwise
    match a prelude binder at a user position). *)
let spans_agree_at_cursor (tokens : Lex.token list) (offset : int) (name : string)
    ((a, b) : int * int) : bool =
  match lexer_id_covering_offset tokens offset with
  | Some (n, ta, tb) when n = name ->
      (* Exact bytes, or lexer [(op)] wraps the AST span, or the reverse. *)
      (ta = a && tb = b)
      || (ta <= a && b <= tb)
      || (a <= ta && tb <= b)
  | _ -> false

let try_id_hover (full_tokens : Lex.token list) (env : static_env)
    (type_env : type_env) (user_byte_lo : int) (lex_id : string option)
    (offset : int) (e : c_expr) : string option =
  match e with
  | EId (name, Some (a, b)) -> (
      match lex_id with
      | Some s when s = name ->
          if
            cursor_in_id_span_ok offset user_byte_lo (a, b)
            && spans_agree_at_cursor full_tokens offset name (a, b)
          then type_string_for_id env type_env name
          else None
      | _ -> None)
  | EId (name, None) -> (
      (* [(>>=)] parses as [Id] in the AST but is lexed as [Relop] / …, so it is
         not enqueued in the id span queue — use the lexer token at the cursor. *)
      match lex_id with
      | Some s when s = name -> (
          match lexer_id_covering_offset full_tokens offset with
          | Some (n, lo, hi) when n = name
            && cursor_in_id_span_ok offset user_byte_lo (lo, hi) ->
              type_string_for_id env type_env name
          | _ -> None)
      | _ -> None)
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
          | Some t -> Some (hover_string_of_c_type t)
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
      else
        match type_string_from_rhs_pat env type_env pat body name with
        | Some _ as r -> r
        | None -> type_string_for_id env type_env name)

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
                      | Some t -> Some (hover_string_of_c_type t)
                      | None -> None)
                  | None -> None)
              | _ -> None)
          | None -> None)
      | _ -> (
          match bind_static pat scrut_ct with
          | Some bs -> (
              match List.assoc_opt name bs with
              | Some t -> Some (hover_string_of_c_type t)
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

(** List comprehension generator [pat <- ge]: element types come from [ge]'s list
    type after inference. *)
let type_string_from_list_comp_gen (env : static_env) (type_env : type_env)
    (pat : c_pat) (ge : c_expr) (name : string) : string option =
  match Typecheck.type_of_c_expr env type_env ge with
  | Ok ct -> (
      let m = Typecheck.instantiate ct in
      match m with
      | CListType elem -> (
          match bind_static pat (Mono elem) with
          | Some bs -> (
              match List.assoc_opt name bs with
              | Some t -> Some (hover_string_of_c_type t)
              | None -> None)
          | None -> None)
      | _ -> None)
  | Error _ -> None

let try_list_comp_gen_pat_hover (env : static_env) (type_env : type_env)
    (lex_id : string option) (offset : int) (pat : c_pat) (ge : c_expr) :
    string option =
  match lex_id with
  | None -> None
  | Some name -> (
      if not (List.mem name (names_bound_in_pat pat)) then None
      else if not (cursor_on_lhs_of_rhs offset ge) then None
      else type_string_from_list_comp_gen env type_env pat ge name)

(** [__forge_tc_ClassName_method] from [Condense.internal_tc_id]. *)
let parse_internal_tc_id (id : string) : (string * string) option =
  let p = "__forge_tc_" in
  if not (String.starts_with ~prefix:p id) then None
  else
    let rest =
      String.sub id (String.length p) (String.length id - String.length p)
    in
    match String.index_opt rest '_' with
    | None -> None
    | Some i ->
        let cls = String.sub rest 0 i in
        let meth =
          String.sub rest (i + 1) (String.length rest - i - 1)
        in
        Some (cls, meth)

(** [impl] dict slots bind [__forge_tc_Class_meth = rhs]; hover on source [meth]
    uses the lexer name while the pattern is the internal id. *)
let try_dict_slot_bind_hover (env : static_env) (type_env : type_env)
    (lex_id : string option) (offset : int) (pat : c_pat) (rhs : c_expr) :
    string option =
  match (pat, lex_id) with
  | CIdPat binder, Some meth_name
    when String.starts_with ~prefix:"__forge_tc_" binder -> (
      match parse_internal_tc_id binder with
      | Some (_, m_internal)
        when String.equal m_internal (sanitize_method_internal meth_name)
             && cursor_on_lhs_of_rhs offset rhs -> (
          match Typecheck.type_of_c_expr env type_env rhs with
          | Ok ct -> Some (hover_string_of_c_type ct)
          | Error _ -> None)
      | _ -> None)
  | _ -> None

let rec peel_efunction_layers acc (e : c_expr) : (c_pat * c_type option * c_expr) list * c_expr
    =
  match e with
  | EFunction (pat, ann, body) -> peel_efunction_layers ((pat, ann, body) :: acc) body
  | _ -> (List.rev acc, e)

(** Strip top-level [PolyType]/[Constrained] to reach [Mono] (quantifiers kept
    abstract as [TypeVar] names in the spine — avoids [instantiate] merging
    independent binders into [a -> a] for hover). *)
let rec c_type_head_mono (t : c_type) : mono_type option =
  match t with
  | Mono m -> Some m
  | PolyType (_, inner) -> c_type_head_mono inner
  | Constrained (_, inner) -> c_type_head_mono inner

let rec function_type_dom_spine acc (m : mono_type) : mono_type list * mono_type =
  match m with
  | FunctionType (dom, cod) -> function_type_dom_spine (dom :: acc) cod
  | tail -> (List.rev acc, tail)

(** Cursor is on a curried parameter [name] only if the lexer sees that [Id] and
    the offset is left of the RHS body in a way consistent with nested lambdas
    (including [let f x y =] when inner [case x] reuses parameter names). *)
let cursor_on_curried_param ~(user_byte_lo : int) (full_tokens : Lex.token list)
    (offset : int) (param_pat : c_pat) (immediate_body : c_expr) : bool =
  let name_matches_lexer =
    match (param_pat, lexer_id_covering_offset full_tokens offset) with
    | CIdPat p, Some (n, lo, hi) ->
        n = p && cursor_in_id_span_ok offset user_byte_lo (lo, hi)
    | _ -> false
  in
  if not name_matches_lexer then false
  else
    match min_lo_spanned_id_byte_from user_byte_lo immediate_body with
    | Some m -> offset < m
    | None -> (
        match (param_pat, immediate_body) with
        | CIdPat p, ESwitch (EId (scr, _), _) when String.equal p scr -> true
        (* [let f x y =] peels as [EFunction(x, EFunction(y, rhs))]: when there
           are no spanned ids yet, [expr_contains_eid] on the inner lambda is
           still true, which wrongly rejects the outer parameter (e.g. [func] in
           [let map func l =]). A nested [EFunction] body is always entirely to
           the right of the outer parameter token. *)
        | _, EFunction (_, _, _) -> true
        | _, _ -> not (expr_contains_eid immediate_body))

(** Infer curried parameter types from [type_of_c_expr] without [instantiate]
    (which can unify distinct quantifiers and show [a -> a] for [map func]). *)
let try_curried_efunction_param_hover (env : static_env) (type_env : type_env)
    ~(user_byte_lo : int) (lex_id : string option) (offset : int)
    (full_tokens : Lex.token list) (e : c_expr) : string option =
  let layers, _ = peel_efunction_layers [] e in
  if List.length layers <= 1 then None
  else
    match lex_id with
    | None -> None
    | Some name -> (
        match Typecheck.type_of_c_expr env type_env e with
        | Error _ -> None
        | Ok ct -> (
            match Typecheck.simplify_type ct type_env with
            | Error _ -> None
            | Ok sch' -> (
                match c_type_head_mono sch' with
                | None -> None
                | Some m0 -> (
                    let doms, _ = function_type_dom_spine [] m0 in
                    let rec find_layer i = function
                      | [] -> None
                      | (pat, _ann, imm_body) :: rest ->
                          if List.mem name (names_bound_in_pat pat) then
                            Some (i, pat, imm_body)
                          else find_layer (i + 1) rest
                    in
                    match find_layer 0 layers with
                    | None -> None
                    | Some (idx, pat, imm_body) -> (
                        if idx >= List.length doms then None
                        else if
                          not
                            (cursor_on_curried_param ~user_byte_lo full_tokens offset pat
                               imm_body)
                        then None
                        else
                          let dom = List.nth doms idx in
                          match bind_static pat (Mono dom) with
                          | Some bs -> (
                              match List.assoc_opt name bs with
                              | Some t -> Some (hover_string_of_c_type t)
                              | None -> None)
                          | None -> None)))))

(** Like [try_curried_efunction_param_hover] but uses the bound scheme of
    [let f ... =] from [env] (matches the type shown when hovering [f]). *)
let try_curried_params_using_defn_scheme (env : static_env) (type_env : type_env)
    ~(user_byte_lo : int) (lex_id : string option) (offset : int)
    (full_tokens : Lex.token list) (pat : c_pat) (body : c_expr) : string option
    =
  match (pat, lex_id) with
  | CIdPat fn_name, Some param_name -> (
      let layers, _ = peel_efunction_layers [] body in
      if List.length layers < 2 then None
      else
        match List.assoc_opt fn_name env with
        | None -> None
        | Some sch -> (
            match Typecheck.simplify_type sch type_env with
            | Error _ -> None
            | Ok sch' -> (
                match c_type_head_mono sch' with
                | None -> None
                | Some m0 -> (
                    let doms, _ = function_type_dom_spine [] m0 in
                    let rec find_layer i = function
                      | [] -> None
                      | (p, _, imm) :: rest ->
                          if List.mem param_name (names_bound_in_pat p) then
                            Some (i, p, imm)
                          else find_layer (i + 1) rest
                    in
                    match find_layer 0 layers with
                    | None -> None
                    | Some (idx, p, imm) -> (
                        if idx >= List.length doms then None
                        else if
                          not
                            (cursor_on_curried_param ~user_byte_lo full_tokens offset p imm)
                        then None
                        else
                          let dom = List.nth doms idx in
                          match bind_static p (Mono dom) with
                          | Some bs -> (
                              match List.assoc_opt param_name bs with
                              | Some t -> Some (hover_string_of_c_type t)
                              | None -> None)
                          | None -> None)))))
  | _ -> None

let try_efunction_param_hover (env : static_env) (type_env : type_env)
    ~(user_byte_lo : int) (lex_id : string option) (offset : int)
    (full_tokens : Lex.token list) (pat : c_pat) (ann : c_type option)
    (body : c_expr) : string option =
  match lex_id with
  | None -> None
  | Some name -> (
      if not (List.mem name (names_bound_in_pat pat)) then None
      else if
        not (cursor_on_curried_param ~user_byte_lo full_tokens offset pat body)
      then None
      else
        match Typecheck.type_of_c_expr env type_env (EFunction (pat, ann, body)) with
        | Ok ct -> (
            match Typecheck.simplify_type ct type_env with
            | Error _ -> None
            | Ok sch' -> (
                match c_type_head_mono sch' with
                | None -> None
                | Some m -> (
                    match m with
                    | FunctionType (dom, _) -> (
                        match bind_static pat (Mono dom) with
                        | Some bs -> (
                            match List.assoc_opt name bs with
                            | Some t -> Some (hover_string_of_c_type t)
                            | None -> None)
                        | None -> None)
                    | _ -> None)))
        | Error _ -> None)

let rec visit_expr (env : static_env) (type_env : type_env) (user_byte_lo : int)
    (lex_id : string option) (offset : int) (full_tokens : Lex.token list)
    (e : c_expr) : string option =
  (* Handle [EApp (EId _, _)] before [try_id_hover]: otherwise a generic
     [EApp (e1, e2)] visit visits the callee [EId] first and
     [type_string_for_id] reports the unconstrained scheme (e.g. [a -> a] for
     [println]) instead of the arrow at this call site. *)
  match e with
  | EApp (EId (name, pos_opt), e2) -> (
      if hover_skip_internal_tc_app_name name then
        visit_expr env type_env user_byte_lo lex_id offset full_tokens e2
      else
        let on_fun =
          match pos_opt with
          | Some (lo, hi) ->
              lo <= offset && offset < hi
              && cursor_in_id_span_ok offset user_byte_lo (lo, hi)
              && (match lex_id with Some s -> s = name | None -> false)
              && spans_agree_at_cursor full_tokens offset name (lo, hi)
          | None -> false
        in
        if on_fun then hover_arrow_dual env type_env name e2
        else
          match min_lo_spanned_id_byte_from user_byte_lo e2 with
          | Some m when offset < m -> (
              match lex_id with
              | Some s when s = name -> hover_arrow_dual env type_env name e2
              | _ -> (
                  match visit_expr env type_env user_byte_lo lex_id offset full_tokens e2 with
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
              match visit_expr env type_env user_byte_lo lex_id offset full_tokens e2 with
              | Some _ as r -> r
              | None -> (
                  match pos_opt with
                  | None -> (
                      match lex_id with
                      | Some s when s = name -> hover_arrow_dual env type_env name e2
                      | _ -> None)
                  | Some _ -> None ) ) )
  | _ -> (
      match try_id_hover full_tokens env type_env user_byte_lo lex_id offset e with
      | Some _ as r -> r
      | None -> (
          match e with
          | EApp (e1, e2) -> (
              match visit_expr env type_env user_byte_lo lex_id offset full_tokens e1 with
              | Some _ as r -> r
              | None -> visit_expr env type_env user_byte_lo lex_id offset full_tokens e2)
      | EBind (pat, _, e1, e2, _) -> (
          match try_dict_slot_bind_hover env type_env lex_id offset pat e1 with
          | Some _ as r -> r
          | None -> (
              match try_rhs_pat_hover env type_env lex_id offset pat e1 with
              | Some _ as r -> r
              | None -> (
                  match visit_expr env type_env user_byte_lo lex_id offset full_tokens e1 with
                  | Some _ as r -> r
                  | None -> (
                      match Typecheck.type_of_c_expr env type_env e1 with
                      | Ok ct -> (
                          match bind_static pat ct with
                          | Some bindings ->
                              visit_expr (bindings @ env) type_env user_byte_lo lex_id offset
                                full_tokens e2
                          | None ->
                              visit_expr env type_env user_byte_lo lex_id offset full_tokens e2)
                      | Error _ ->
                          visit_expr env type_env user_byte_lo lex_id offset full_tokens e2))))
      | EBindRec (pat, _, e1, e2, _) -> (
          match try_dict_slot_bind_hover env type_env lex_id offset pat e1 with
          | Some _ as r -> r
          | None -> (
          match pat with
          | CIdPat id ->
              let rec_ty = fresh_type_var () in
              let rec_env = (id, Mono rec_ty) :: env in
              ( match try_rhs_pat_hover rec_env type_env lex_id offset pat e1 with
              | Some _ as r -> r
              | None -> (
                  match visit_expr rec_env type_env user_byte_lo lex_id offset full_tokens e1
                  with
                  | Some _ as r -> r
                  | None -> (
                      match Typecheck.type_of_c_expr rec_env type_env e1 with
                      | Ok gen_ct ->
                          visit_expr ((id, gen_ct) :: env) type_env user_byte_lo lex_id offset
                            full_tokens e2
                      | Error _ -> visit_expr env type_env user_byte_lo lex_id offset full_tokens e2 ) ) )
          | _ -> (
              match visit_expr env type_env user_byte_lo lex_id offset full_tokens e1 with
              | Some _ as r -> r
              | None -> visit_expr env type_env user_byte_lo lex_id offset full_tokens e2)))
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
          | None -> visit_expr env type_env user_byte_lo lex_id offset full_tokens body
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
                        | None ->
                            visit_expr mut_rec_env type_env user_byte_lo lex_id offset full_tokens
                              e1))
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
                  visit_expr body_env type_env user_byte_lo lex_id offset full_tokens body))
      | EFunction (pat, ann, body) as efun -> (
          match
            try_curried_efunction_param_hover env type_env ~user_byte_lo lex_id offset
              full_tokens efun
          with
          | Some _ as r -> r
          | None -> (
              match
                try_efunction_param_hover env type_env ~user_byte_lo lex_id offset full_tokens pat
                  ann body
              with
              | Some _ as r -> r
              | None -> (
                  match Typecheck.type_of_c_expr env type_env (EFunction (pat, ann, body)) with
                  | Ok ct -> (
                      match Typecheck.simplify_type ct type_env with
                      | Error _ ->
                          visit_expr env type_env user_byte_lo lex_id offset full_tokens body
                      | Ok sch' -> (
                          match c_type_head_mono sch' with
                          | None ->
                              visit_expr env type_env user_byte_lo lex_id offset full_tokens body
                          | Some m -> (
                              match m with
                              | FunctionType (dom, _) -> (
                                  match bind_static pat (Mono dom) with
                                  | Some bindings ->
                                      visit_expr (bindings @ env) type_env user_byte_lo lex_id offset
                                        full_tokens body
                                  | None ->
                                      visit_expr env type_env user_byte_lo lex_id offset full_tokens
                                        body)
                              | _ ->
                                  visit_expr env type_env user_byte_lo lex_id offset full_tokens body
                              )))
                  | Error _ ->
                      visit_expr env type_env user_byte_lo lex_id offset full_tokens body)))
      | ETernary (a, b, c) -> (
          match visit_expr env type_env user_byte_lo lex_id offset full_tokens a with
          | Some _ as r -> r
          | None -> (
              match visit_expr env type_env user_byte_lo lex_id offset full_tokens b with
              | Some _ as r -> r
              | None -> visit_expr env type_env user_byte_lo lex_id offset full_tokens c))
      | EBop (_, a, b) -> (
          match visit_expr env type_env user_byte_lo lex_id offset full_tokens a with
          | Some _ as r -> r
          | None -> visit_expr env type_env user_byte_lo lex_id offset full_tokens b)
      | ESwitch (scr, branches) -> (
          match visit_expr env type_env user_byte_lo lex_id offset full_tokens scr with
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
                          visit_expr env_in_branch type_env user_byte_lo lex_id offset full_tokens
                            be))
                None branches)
      | EVector es ->
          List.fold_left
            (fun acc e ->
              match acc with
              | Some _ as r -> r
              | None -> visit_expr env type_env user_byte_lo lex_id offset full_tokens e)
            None es
      | EBlock parts ->
          let rec walk_block (benv : static_env) = function
            | [] -> None
            | Expr ex :: rest -> (
                match visit_expr benv type_env user_byte_lo lex_id offset full_tokens ex with
                | Some _ as r -> r
                | None -> walk_block benv rest)
            | Defn d :: rest -> (
                match generate_defn benv type_env d with
                | Error _ -> None
                | Ok (nb, _, _) ->
                    let benv' = nb @ benv in
                    match
                      hover_in_defn benv' type_env user_byte_lo lex_id offset full_tokens d
                    with
                    | Some _ as r -> r
                    | None -> walk_block benv' rest)
          in
          walk_block env parts
      | ERecordLit fs ->
          List.fold_left
            (fun acc (_, ex) ->
              match acc with
              | Some _ as r -> r
              | None -> visit_expr env type_env user_byte_lo lex_id offset full_tokens ex)
            None fs
      | ERecordUpdate (base, upd) -> (
          match visit_expr env type_env user_byte_lo lex_id offset full_tokens base with
          | Some _ as r -> r
          | None ->
              List.fold_left
                (fun acc (_, ex) ->
                  match acc with
                  | Some _ as r -> r
                  | None -> visit_expr env type_env user_byte_lo lex_id offset full_tokens ex)
                None upd)
      | EFieldAccess (e0, _) -> visit_expr env type_env user_byte_lo lex_id offset full_tokens e0
      | EListEnumeration (a, b) -> (
          match visit_expr env type_env user_byte_lo lex_id offset full_tokens a with
          | Some _ as r -> r
          | None -> visit_expr env type_env user_byte_lo lex_id offset full_tokens b)
      | EListComprehension (e0, gens) -> (
          match visit_expr env type_env user_byte_lo lex_id offset full_tokens e0 with
          | Some _ as r -> r
          | None ->
              let rec walk_gens (env_acc : static_env) = function
                | [] -> None
                | (pat, ge) :: rest -> (
                    match try_list_comp_gen_pat_hover env_acc type_env lex_id offset pat ge with
                    | Some _ as r -> r
                    | None -> (
                        match visit_expr env_acc type_env user_byte_lo lex_id offset full_tokens ge
                        with
                        | Some _ as r -> r
                        | None ->
                            let _tp, pat_env, _ =
                              Typecheck.type_of_pat env_acc type_env pat
                            in
                            walk_gens (pat_env @ env_acc) rest))
              in
              walk_gens env gens)
      | EInt _ | EFloat _ | EBool _ | EString _ | EChar _ | EUnit | ENil -> None
      | _ -> None ) )

and hover_in_defn (env : static_env) (type_env : type_env) (user_byte_lo : int)
    (lex_id : string option) (offset : int) (full_tokens : Lex.token list)
    (d : c_defn) : string option =
  match d with
  | CDefn (pat, _, _, body, _, _) -> (
      match
        try_curried_params_using_defn_scheme env type_env ~user_byte_lo lex_id offset
          full_tokens pat body
      with
      | Some _ as r -> r
      | None -> (
          match try_defn_pat_hover env type_env lex_id offset pat body with
          | Some _ as r -> r
          | None -> visit_expr env type_env user_byte_lo lex_id offset full_tokens body))
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
      | None -> visit_expr rec_env type_env user_byte_lo lex_id offset full_tokens body )
  | CDefnMutRec defs ->
      (* Pattern hovers for every binding first. Do not interleave [visit_expr]
         on earlier bodies with [rec_env] (unresolved schemes): a cursor on
         [and is_odd] can wrongly match [is_odd] inside [is_even]'s body when
         id-queue spans align with the definition token, yielding e.g.
         [Int -> a] instead of [Int -> Bool]. *)
      ( match
          List.find_map
            (fun (pat, _, _, body, _, _) ->
              try_defn_pat_hover env type_env lex_id offset pat body)
            defs
        with
        | Some _ as r -> r
        | None -> (
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
                  List.map (fun (id, _) -> (id, Mono fresh_type)) pattern_env
                  @ acc_env)
                env patterns_and_fresh_types
            in
            List.fold_left
              (fun acc (_, _, _, body, _, _) ->
                match acc with
                | Some _ as r -> r
                | None ->
                    visit_expr rec_env type_env user_byte_lo lex_id offset full_tokens
                      body)
              None defs))
  | CClassDecl (_trait_name, _params, methods, default_lets) -> (
      match lex_id with
      | None -> None
      | Some name -> (
          let from_declared_val =
            match List.find_opt (fun (m, _, _) -> String.equal m name) methods with
            | Some (_, mt, _) ->
                (* Prelude traits are in bytes [< user_byte_lo]; do not report a
                   trait [val] type when the cursor is in the user's appended
                   buffer (e.g. user [let (>) a b = a] would match [Ord]'s [(>)]
                   otherwise). *)
                if user_byte_lo > 0 && offset >= user_byte_lo then None
                else Some (hover_string_of_mono mt)
            | None -> None
          in
          match from_declared_val with
          | Some _ as r -> r
          | None ->
              List.fold_left
                (fun acc (_, body) ->
                  match acc with
                  | Some _ as r -> r
                  | None ->
                      visit_expr env type_env user_byte_lo lex_id offset full_tokens
                        body)
                None default_lets))
  | CTypeAlias _ | CSumType _ | CSumTypeRec _ | CSumTypeRecMutRec _ -> None

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
    (lex_id : string option) (offset : int) (full_tokens : Lex.token list)
    ~(prelude_defn_cap_count : int) ~(idx : int) (defs : c_defn list) :
    string option =
  let safe_hover_in_defn env te d0 =
    try hover_in_defn env te user_byte_lo lex_id offset full_tokens d0
    with _ -> None
  in
  match defs with
  | [] -> None
  | d :: rest -> (
      let gen_res =
        try Some (generate_defn acc_env acc_te d) with _ -> None
      in
      match gen_res with
      | None | Some (Error _) -> (
          (* Keep hover working even when this definition fails to typecheck.
             Try hovering within the failing defn using the accumulated prefix
             environment, then continue scanning later defs. *)
          match safe_hover_in_defn acc_env acc_te d with
          | Some _ as r -> r
          | None ->
              walk_defns acc_env acc_te user_byte_lo lex_id offset full_tokens
                ~prelude_defn_cap_count ~idx:(idx + 1) rest)
      | Some (Ok (nb, _, _)) ->
          let env_for_hover = nb @ acc_env in
          let skip_hover_in_this_defn =
            user_byte_lo > 0 && offset >= user_byte_lo && idx < prelude_defn_cap_count
          in
          ( match
              if skip_hover_in_this_defn then None
              else safe_hover_in_defn env_for_hover acc_te d
            with
            | Some _ as r -> r
            | None ->
                walk_defns env_for_hover acc_te user_byte_lo lex_id offset
                  full_tokens ~prelude_defn_cap_count ~idx:(idx + 1) rest
            ))

type hover_kind =
  | HoverType
  | HoverDefinition

type hover_result = hover_kind * string

let find_type_definition_by_name (defs : c_defn list) (name : string) :
    c_defn option =
  let rec walk = function
    | [] -> None
    | d :: rest -> (
        match d with
        | CTypeAlias (n, _, _) when String.equal n name -> Some d
        | CSumType (n, _, _) when String.equal n name -> Some d
        | CSumTypeRec (n, _, _) when String.equal n name -> Some d
        | CSumTypeRecMutRec group ->
            if List.exists (fun (n, _, _) -> String.equal n name) group then
              Some (CSumTypeRecMutRec group)
            else walk rest
        | _ -> walk rest)
  in
  walk defs

let hover_string_of_type_params (args : string list) : string =
  match args with
  | [] -> ""
  | _ -> "<" ^ String.concat ", " args ^ ">"

let hover_string_of_sum_ctors (ctors : (string * c_type option) list) : string =
  String.concat "\n  "
    (List.map
       (fun (cons_name, payload_type_opt) ->
         match payload_type_opt with
         | None -> "| " ^ cons_name
         | Some payload_type ->
             "| " ^ cons_name ^ " of " ^ hover_string_of_c_type payload_type)
       ctors)

let hover_string_of_type_defn (d : c_defn) : string =
  match d with
  | CTypeAlias (name, args, body) ->
      "type " ^ name ^ hover_string_of_type_params args ^ " = "
      ^ hover_string_of_mono body
  | CSumType (name, args, ctors) ->
      "type " ^ name ^ hover_string_of_type_params args ^ " = "
      ^ hover_string_of_sum_ctors ctors
  | CSumTypeRec (name, args, ctors) ->
      "type rec " ^ name ^ hover_string_of_type_params args ^ " = "
      ^ hover_string_of_sum_ctors ctors
  | CSumTypeRecMutRec types ->
      String.concat "\n"
        (List.mapi
           (fun i (name, args, ctors) ->
             let prefix = if i = 0 then "type rec " else "and " in
             prefix ^ name ^ hover_string_of_type_params args ^ " = "
             ^ hover_string_of_sum_ctors ctors)
           types)
  | _ -> C_to_string.string_of_defn d

let hover_type_definition_at_offset (defs : c_defn list) (tokens : Lex.token list)
    (offset : int) : string option =
  match lexer_id_covering_offset tokens offset with
  | None -> None
  | Some (name, _, _) -> (
      match find_type_definition_by_name defs name with
      | Some d -> Some (hover_string_of_type_defn d)
      | None -> None)

let hover_for_position ~(prelude : bool) ~(src_path : string) ~(source : string)
    ~(line0 : int) ~(char0 : int) : (hover_result, string) result =
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
      let prelude_n =
        if delta <= 0 then 0
        else
          let frag = String.sub full_source 0 delta in
          let n = Prelude.defn_count_for_source_fragment frag in
          if n > 0 then n else Prelude.defn_count_when_parsed ()
      in
      let condensed =
        condense_program
          ?user_id_byte_min_after_prelude:
            (if delta = 0 then None else Some (prelude_n, delta))
          program
      in
      clear_id_queue ();
      let static_env = build_full_static_env () in
      let type_env : type_env = [] in
      match
        walk_defns static_env type_env user_byte_lo lex_id_at_offset offset
          full_tokens ~prelude_defn_cap_count:prelude_n ~idx:0 condensed
      with
      | Some s -> Ok (HoverType, s)
      | None -> (
          let op_or_id_type_fallback =
            match lex_id_at_offset with
            | Some name -> type_string_for_id static_env type_env name
            | None -> None
          in
          match op_or_id_type_fallback with
          | Some s -> Ok (HoverType, s)
          | None -> (
              match hover_type_definition_at_offset condensed full_tokens offset with
              | Some s -> Ok (HoverDefinition, s)
              | None -> Error "no typed identifier at this position" ) ) )

let hover_type_for_identifier ~(prelude : bool) ~(src_path : string)
    ~(source : string) ~(line0 : int) ~(char0 : int) : (string, string) result =
  match hover_for_position ~prelude ~src_path ~source ~line0 ~char0 with
  | Ok (_, s) -> Ok s
  | Error e -> Error e
