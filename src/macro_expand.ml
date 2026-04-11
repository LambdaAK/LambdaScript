open Expr

module StringSet = Set.Make (String)

type macro_def = { arms : macro_arm list }

let path_to_key (path : string list) : string = String.concat "." path
let scoped_key (path : string list) (name : string) : string = path_to_key path ^ "|" ^ name

let drop_last (path : string list) : string list =
  match List.rev path with [] -> [] | _ :: t -> List.rev t

let rec bound_ids_in_sub_pat : sub_pat -> string list = function
  | IdPat s | InfixPat s -> [ s ]
  | Pat p -> bound_ids_in_pat p
  | VectorPat ps -> List.concat (List.map bound_ids_in_pat ps)
  | RecordPat fs -> List.concat (List.map (fun (_, p) -> bound_ids_in_pat p) fs)
  | VariantPat (_, Some p) -> bound_ids_in_pat p
  | VariantPat (_, None) -> []
  | UnitPat | WildcardPat | IntPat _ | CharPat _ | StringPat _ | BoolPat _
  | NilPat ->
      []

and bound_ids_in_pat : pat -> string list = function
  | SubPat sp -> bound_ids_in_sub_pat sp
  | ConsPat (sp, p) -> bound_ids_in_sub_pat sp @ bound_ids_in_pat p

let add_pat_bound_ids (bound : StringSet.t) (p : pat) : StringSet.t =
  List.fold_left (fun acc id -> StringSet.add id acc) bound (bound_ids_in_pat p)

let local_defn_bound_ids (d : defn) : string list =
  match d with
  | Defn (p, _, _, _, _, _) | DefnRec (p, _, _, _, _, _) -> bound_ids_in_pat p
  | DefnMutRec defs ->
      List.concat (List.map (fun (p, _, _, _, _, _) -> bound_ids_in_pat p) defs)
  | _ -> []

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

let expr_as_atomic_factor (e : expr) : factor option =
  match e with
  | ConsExpr
      (DisjunctionUnderCons
         (ConjunctionUnderDisjunction
            (RelationUnderConjunction
               (ArithmeticUnderRelExpr (Term (Factor (FactorUnderApplication f))))))) ->
      Some f
  | _ -> None

let rec factor_as_path (f : factor) : string list option =
  match f with
  | Id s -> Some [ s ]
  | FieldAccess (base, fld) -> (
      match factor_as_path base with
      | Some segs -> Some (segs @ [ fld ])
      | None -> None)
  | _ -> None

let expr_as_id (e : expr) : string option =
  match expr_as_atomic_factor e with Some (Id s) -> Some s | _ -> None

let expr_as_path (e : expr) : string list option =
  match expr_as_atomic_factor e with Some f -> factor_as_path f | None -> None

let expr_as_string_literal (e : expr) : string option =
  match expr_as_atomic_factor e with
  | Some (String s) -> Some s
  | _ -> None

let rec substitute_expr (env : (string * expr) list) (bound : StringSet.t)
    (e : expr) : expr =
  match e with
  | Function (p, ann, body) ->
      let bound' = add_pat_bound_ids bound p in
      Function (p, ann, substitute_expr env bound' body)
  | Ternary (a, b, c) ->
      Ternary
        ( substitute_expr env bound a,
          substitute_expr env bound b,
          substitute_expr env bound c )
  | ConsExpr ce -> ConsExpr (substitute_cons_expr env bound ce)
  | Bind (p, ann, e1, e2, rt) ->
      let bound' = add_pat_bound_ids bound p in
      Bind
        ( p,
          ann,
          substitute_expr env bound e1,
          substitute_expr env bound' e2,
          rt )
  | BindRec (p, ann, e1, e2, rt) ->
      let bound' = add_pat_bound_ids bound p in
      BindRec
        ( p,
          ann,
          substitute_expr env bound' e1,
          substitute_expr env bound' e2,
          rt )
  | BindMutRec (bindings, body) ->
      let bound' =
        List.fold_left
          (fun acc (p, _, _, _, _) -> add_pat_bound_ids acc p)
          bound bindings
      in
      BindMutRec
        ( List.map
            (fun (p, ann, rhs, rt, n) ->
              (p, ann, substitute_expr env bound' rhs, rt, n))
            bindings,
          substitute_expr env bound' body )
  | Switch (scrut, branches) ->
      Switch
        ( substitute_expr env bound scrut,
          List.map
            (fun (p, rhs) ->
              let bound' = add_pat_bound_ids bound p in
              (p, substitute_expr env bound' rhs))
            branches )
  | Block parts ->
      let rec rewrite_parts bound_acc acc = function
        | [] -> List.rev acc
        | part :: rest -> (
            match part with
            | Expr e0 ->
                let e' = substitute_expr env bound_acc e0 in
                rewrite_parts bound_acc (Expr e' :: acc) rest
            | Definition d0 ->
                let d' = substitute_local_defn env bound_acc d0 in
                let bound_acc' =
                  List.fold_left
                    (fun b id -> StringSet.add id b)
                    bound_acc (local_defn_bound_ids d0)
                in
                rewrite_parts bound_acc' (Definition d' :: acc) rest)
      in
      Block (rewrite_parts bound [] parts)

and substitute_cons_expr (env : (string * expr) list) (bound : StringSet.t)
    (ce : cons_expr) : cons_expr =
  match ce with
  | Cons (d, ce') ->
      Cons
        ( substitute_disjunction env bound d,
          substitute_cons_expr env bound ce' )
  | DisjunctionUnderCons d -> DisjunctionUnderCons (substitute_disjunction env bound d)
  | Pipeline (a, b) ->
      Pipeline (substitute_cons_expr env bound a, substitute_cons_expr env bound b)

and substitute_disjunction (env : (string * expr) list) (bound : StringSet.t)
    (d : disjunction) : disjunction =
  match d with
  | Disjunction (a, b) ->
      Disjunction
        ( substitute_conjunction env bound a,
          substitute_disjunction env bound b )
  | ConjunctionUnderDisjunction a ->
      ConjunctionUnderDisjunction (substitute_conjunction env bound a)

and substitute_conjunction (env : (string * expr) list) (bound : StringSet.t)
    (c : conjunction) : conjunction =
  match c with
  | Conjunction (a, b) ->
      Conjunction (substitute_rel_expr env bound a, substitute_conjunction env bound b)
  | RelationUnderConjunction a -> RelationUnderConjunction (substitute_rel_expr env bound a)

and substitute_rel_expr (env : (string * expr) list) (bound : StringSet.t)
    (r : rel_expr) : rel_expr =
  match r with
  | Relation (op, a, b) ->
      Relation (op, substitute_rel_expr env bound a, substitute_arith_expr env bound b)
  | CustomRelExpr (op, a, b) ->
      CustomRelExpr
        (op, substitute_rel_expr env bound a, substitute_arith_expr env bound b)
  | ArithmeticUnderRelExpr a -> ArithmeticUnderRelExpr (substitute_arith_expr env bound a)

and substitute_arith_expr (env : (string * expr) list) (bound : StringSet.t)
    (a : arith_expr) : arith_expr =
  match a with
  | Plus (l, r) ->
      Plus (substitute_arith_expr env bound l, substitute_term env bound r)
  | Minus (l, r) ->
      Minus (substitute_arith_expr env bound l, substitute_term env bound r)
  | CustomArithExpr (op, l, r) ->
      CustomArithExpr
        (op, substitute_arith_expr env bound l, substitute_term env bound r)
  | Term t -> Term (substitute_term env bound t)

and substitute_term (env : (string * expr) list) (bound : StringSet.t) (t : term)
    : term =
  match t with
  | Mul (a, b) ->
      Mul (substitute_term env bound a, substitute_app_factor env bound b)
  | Div (a, b) ->
      Div (substitute_term env bound a, substitute_app_factor env bound b)
  | Mod (a, b) ->
      Mod (substitute_term env bound a, substitute_app_factor env bound b)
  | CustomTerm (op, a, b) ->
      CustomTerm
        (op, substitute_term env bound a, substitute_app_factor env bound b)
  | Factor af -> Factor (substitute_app_factor env bound af)

and substitute_app_factor (env : (string * expr) list) (bound : StringSet.t)
    (af : app_factor) : app_factor =
  match af with
  | Application (a, b) ->
      Application
        ( substitute_app_factor env bound a,
          substitute_factor env bound b )
  | FactorUnderApplication f -> FactorUnderApplication (substitute_factor env bound f)

and substitute_factor (env : (string * expr) list) (bound : StringSet.t)
    (f : factor) : factor =
  match f with
  | Id name -> (
      if StringSet.mem name bound then Id name
      else
        match List.assoc_opt name env with
        | Some replacement -> ParenFactor replacement
        | None -> Id name)
  | ParenFactor e -> ParenFactor (substitute_expr env bound e)
  | Opposite f0 -> Opposite (substitute_factor env bound f0)
  | Vector es -> Vector (List.map (substitute_expr env bound) es)
  | ListSugar es -> ListSugar (List.map (substitute_expr env bound) es)
  | ListEnumeration (a, b) ->
      ListEnumeration (substitute_expr env bound a, substitute_expr env bound b)
  | ListComprehension (body, generators) ->
      let rec rewrite_gens bound_acc acc = function
        | [] -> (List.rev acc, bound_acc)
        | (p, ge) :: rest ->
            let ge' = substitute_expr env bound_acc ge in
            let bound_acc' = add_pat_bound_ids bound_acc p in
            rewrite_gens bound_acc' ((p, ge') :: acc) rest
      in
      let gens', bound_for_body = rewrite_gens bound [] generators in
      ListComprehension (substitute_expr env bound_for_body body, gens')
  | RecordLit fields ->
      RecordLit
        (List.map (fun (n, e0) -> (n, substitute_expr env bound e0)) fields)
  | RecordUpdate (base, fields) ->
      RecordUpdate
        ( substitute_expr env bound base,
          List.map (fun (n, e0) -> (n, substitute_expr env bound e0)) fields )
  | FieldAccess (base, fld) -> FieldAccess (substitute_factor env bound base, fld)
  | MacroInvoke (name, args) ->
      MacroInvoke (name, List.map (substitute_expr env bound) args)
  | (Boolean _ | String _ | Unit | Integer _ | Char _ | FloatFactor _ | Nil) as x ->
      x

and substitute_local_defn (env : (string * expr) list) (bound : StringSet.t)
    (d : defn) : defn =
  match d with
  | Defn (p, cs, ann, body, rt, n) ->
      Defn (p, cs, ann, substitute_expr env bound body, rt, n)
  | DefnRec (p, cs, ann, body, rt, n) ->
      let bound' = add_pat_bound_ids bound p in
      DefnRec (p, cs, ann, substitute_expr env bound' body, rt, n)
  | DefnMutRec defs ->
      let bound' =
        List.fold_left
          (fun acc (p, _, _, _, _, _) -> add_pat_bound_ids acc p)
          bound defs
      in
      DefnMutRec
        (List.map
           (fun (p, cs, ann, body, rt, n) ->
             (p, cs, ann, substitute_expr env bound' body, rt, n))
           defs)
  | ClassDef (name, params, requires, items) ->
      let items' =
        List.map
          (function
            | TraitVal _ as i -> i
            | TraitLet (m, e) -> TraitLet (m, substitute_expr env bound e))
          items
      in
      ClassDef (name, params, requires, items')
  | InstanceDef (cls, head_ty, requires, impls) ->
      let impls' =
        List.map
          (fun (m, e) -> (m, substitute_expr env bound e))
          impls
      in
      InstanceDef (cls, head_ty, requires, impls')
  | (TypeDef _ | SumTypeDef _ | SumTypeDefRec _ | SumTypeDefMutRec _ | ModDef _
    | UseDef _ | ImportDef _ | MacroDef _) as d ->
      d

let matcher_params (m : macro_matcher) : macro_param list =
  match m with
  | MacroMatcherParams ps -> ps
  | MacroMatcherRepeat (p, _) -> [ p ]

let validate_macro_arm_params (name : string) ((m, _) : macro_arm) : unit =
  let params = List.map fst (matcher_params m) in
  let uniq = List.sort_uniq String.compare params in
  if List.length uniq <> List.length params then
    failwith ("forge: duplicate macro parameter in macro_rules! " ^ name)

let match_kind (kind : macro_fragment_kind) (arg : expr) : bool =
  match kind with
  | MacroExpr | MacroTT | MacroPat | MacroItem -> true
  | MacroIdent -> expr_as_id arg <> None
  | MacroType -> expr_as_path arg <> None

let collect_declared_macros (defns : defn list) : (string, macro_def) Hashtbl.t =
  let tbl = Hashtbl.create 128 in
  let rec go (path : string list) (defs : defn list) : unit =
    List.iter
      (function
        | MacroDef (name, arms) ->
            List.iter (validate_macro_arm_params name) arms;
            let key = scoped_key path name in
            if Hashtbl.mem tbl key then
              failwith
                ("forge: duplicate macro definition '"
                ^ (if path = [] then name else String.concat "." (path @ [ name ]))
                ^ "'")
            else Hashtbl.replace tbl key { arms }
        | ModDef (name, nested) -> go (path @ [ name ]) nested
        | _ -> ())
      defs
  in
  go [] defns;
  tbl

let select_macro_arm (_name : string) (arms : macro_arm list) (args : expr list) :
    (expr * (string * expr) list) option =
  let rec try_arms = function
    | [] -> None
    | (matcher, body) :: rest -> (
        match matcher with
        | MacroMatcherParams params ->
            if List.length params <> List.length args then try_arms rest
            else
              let all_match =
                List.for_all2 (fun (_, kind) arg -> match_kind kind arg) params args
              in
              if not all_match then try_arms rest
              else
                Some
                  ( body,
                    List.map2 (fun (pname, _) arg -> (pname, arg)) params args )
        | MacroMatcherRepeat ((pname, kind), one_or_more) ->
            if (one_or_more && args = [])
               || not (List.for_all (fun arg -> match_kind kind arg) args)
            then try_arms rest
            else
              (* Rust repetition is token-tree level; in this AST MVP we bind
                 repetition captures as a list expression. *)
              Some (body, [ (pname, list_expr args) ]))
  in
  try_arms arms

let expand_proc_macro (name : string) (args : expr list) : expr option =
  match name with
  | "count_args" -> Some (int_expr (List.length args))
  | "vec" -> Some (list_expr args)
  | "stringify" ->
      Some
        (string_expr (String.concat ", " (List.map Tostring.string_of_expr args)))
  | "concat_str" | "concat" ->
      let parts = List.map expr_as_string_literal args in
      if List.for_all (function Some _ -> true | None -> false) parts then
        Some
          (string_expr
             (parts |> List.filter_map (fun x -> x) |> String.concat ""))
      else
        failwith "forge: concat_str! expects only string literal arguments"
  | _ -> None

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
      Ternary (expand_expr tbl path depth a, expand_expr tbl path depth b, expand_expr tbl path depth c)
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

and expand_conjunction (tbl : (string, macro_def) Hashtbl.t) (path : string list)
    (depth : int) (c : conjunction) : conjunction =
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

and expand_arith_expr (tbl : (string, macro_def) Hashtbl.t) (path : string list)
    (depth : int) (a : arith_expr) : arith_expr =
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
      let args' = List.map (expand_expr tbl path depth) args in
      ParenFactor (expand_macro_call tbl path depth name args')
  | (Boolean _ | String _ | Unit | Integer _ | Char _ | FloatFactor _ | Id _ | Nil) as x ->
      x

and expand_macro_call (tbl : (string, macro_def) Hashtbl.t) (path : string list)
    (depth : int) (name : string) (args : expr list) : expr =
  if depth >= max_macro_expansion_depth then
    failwith
      (Printf.sprintf "forge: macro expansion exceeded max depth (%d) at %s!"
         max_macro_expansion_depth name)
  else
    match resolve_macro_from_scope tbl path name with
    | None -> (
        match expand_proc_macro name args with
        | Some e -> expand_expr tbl path (depth + 1) e
        | None -> failwith ("forge: unknown macro " ^ name ^ "!"))
    | Some { arms } -> (
        match select_macro_arm name arms args with
        | None ->
            failwith
              (Printf.sprintf
                 "forge: no matching arm for macro %s! with %d argument(s)" name
                 (List.length args))
        | Some (body, env) ->
            let substituted = substitute_expr env StringSet.empty body in
            expand_expr tbl path (depth + 1) substituted)

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
