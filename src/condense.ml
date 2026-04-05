open Expr
open Cexpr
open Forge_class_util

let is_plain_type_var_name (s : string) : bool =
  s <> "" && String.for_all (fun c -> c >= 'a' && c <= 'z') s

let rec condense_pat : pat -> c_pat = function
  | SubPat sub_pat -> condense_sub_pat sub_pat
  | ConsPat (sub_pat, pat) ->
      CConsPat (condense_sub_pat sub_pat, condense_pat pat)

and condense_sub_pat : sub_pat -> c_pat = function
  | IntPat i -> CIntPat i
  | BoolPat b -> CBoolPat b
  | StringPat s -> CStringPat s
  | UnitPat -> CUnitPat
  | IdPat s -> CIdPat s
  | NilPat -> CNilPat
  | VectorPat pats -> CVectorPat (List.map condense_pat pats)
  | RecordPat fields ->
      CRecordPat (List.map (fun (name, p) -> (name, condense_pat p)) fields)
  | WildcardPat -> CWildcardPat
  | Pat pat -> condense_pat pat
  | InfixPat s -> CIdPat s
  | CharPat c -> CCharPat c
  | VariantPat (name, payload_pat_opt) ->
      let payload_c_pat_opt =
        match payload_pat_opt with
        | None -> None
        | Some p -> Some (condense_pat p)
      in
      CVariantPat (name, payload_c_pat_opt)

let rec condense_defn : defn -> c_defn = function
  | Defn
      ( pattern,
        class_constraints,
        cto,
        body_expression,
        return_type,
        num_explicit_params ) ->
      let a : c_pat = condense_pat pattern in
      let constraints : (string * mono_type) list =
        List.map
          (fun (cls, ct) -> (cls, condense_compound_type ct))
          class_constraints
      in
      let b : c_type option =
        match cto with
        | None -> None
        | Some t -> Some (condense_type t)
      in
      let c : c_expr = condense_expr body_expression in
      let d : c_type option =
        match return_type with
        | None -> None
        | Some t -> Some (condense_type t)
      in
      CDefn (a, constraints, b, c, d, num_explicit_params)
  | DefnRec
      ( pattern,
        class_constraints,
        cto,
        body_expression,
        return_type,
        num_explicit_params ) ->
      let a : c_pat = condense_pat pattern in
      let constraints : (string * mono_type) list =
        List.map
          (fun (cls, ct) -> (cls, condense_compound_type ct))
          class_constraints
      in
      let b : c_type option =
        match cto with
        | None -> None
        | Some t -> Some (condense_type t)
      in
      let c : c_expr = condense_expr body_expression in
      let d : c_type option =
        match return_type with
        | None -> None
        | Some t -> Some (condense_type t)
      in
      CDefnRec (a, constraints, b, c, d, num_explicit_params)
  | DefnMutRec defns ->
      let condensed_defns =
        List.map
          (fun ( pattern,
                 class_constraints,
                 cto,
                 body_expression,
                 return_type,
                 num_explicit_params ) ->
            ( condense_pat pattern,
              List.map
                (fun (cls, ct) -> (cls, condense_compound_type ct))
                class_constraints,
              (match cto with
              | None -> None
              | Some t -> Some (condense_type t)),
              condense_expr body_expression,
              (match return_type with
              | None -> None
              | Some t -> Some (condense_type t)),
              num_explicit_params ))
          defns
      in
      CDefnMutRec condensed_defns
  | TypeDef (name, type_params, ct) ->
      CTypeAlias (name, type_params, condense_compound_type ct)
  | SumTypeDef (name, type_params, constructors) ->
      let condensed_constructors =
        List.map
          (fun (cons_name, payload_type_opt) ->
            ( cons_name,
              match payload_type_opt with
              | None -> None
              | Some ct ->
                  let mono_t = condense_compound_type ct in
                  Some (Mono mono_t) ))
          constructors
      in
      CSumType (name, type_params, condensed_constructors)
  | SumTypeDefRec (name, type_params, constructors) ->
      let condensed_constructors =
        List.map
          (fun (cons_name, payload_type_opt) ->
            ( cons_name,
              match payload_type_opt with
              | None -> None
              | Some ct ->
                  let mono_t = condense_compound_type ct in
                  Some (Mono mono_t) ))
          constructors
      in
      CSumTypeRec (name, type_params, condensed_constructors)
  | SumTypeDefMutRec types ->
      let condensed_types =
        List.map
          (fun (name, type_params, constructors) ->
            let condensed_constructors =
              List.map
                (fun (cons_name, payload_type_opt) ->
                  ( cons_name,
                    match payload_type_opt with
                    | None -> None
                    | Some ct ->
                        let mono_t = condense_compound_type ct in
                        Some (Mono mono_t) ))
                constructors
            in
            (name, type_params, condensed_constructors))
          types
      in
      CSumTypeRecMutRec condensed_types
  | ClassDef _ | InstanceDef _ ->
      failwith
        "internal: inter/impl definitions must be condensed with \
         Condense.condense_program"

and condense_expr : expr -> c_expr = function
  | Function (pat, ct_opt, expr) ->
      EFunction
        ( condense_pat pat,
          (match ct_opt with
          | None -> None
          | Some ct -> Some (condense_type ct)),
          condense_expr expr )
  | Ternary (e1, e2, e3) ->
      ETernary (condense_expr e1, condense_expr e2, condense_expr e3)
  | ConsExpr ce -> condense_cons_expr ce
  | Bind (pat, cto, e1, e2, return_type) ->
      (* TODO: fix this later *)
      EBind
        ( condense_pat pat,
          (match cto with
          | None -> None
          | Some ct -> Some (condense_type ct)),
          condense_expr e1,
          condense_expr e2,
          match return_type with
          | None -> None
          | Some ct -> Some (condense_type ct) )
  | BindRec (pat, cto, e1, e2, return_type) ->
      EBindRec
        ( condense_pat pat,
          (match cto with
          | None -> None
          | Some ct -> Some (condense_type ct)),
          condense_expr e1,
          condense_expr e2,
          match return_type with
          | None -> None
          | Some ct -> Some (condense_type ct) )
  | BindMutRec (bindings, body) ->
      let condensed_bindings =
        List.map
          (fun (pat, cto, e1, return_type, num_explicit_params) ->
            ( condense_pat pat,
              (match cto with
              | None -> None
              | Some ct -> Some (condense_type ct)),
              condense_expr e1,
              (match return_type with
              | None -> None
              | Some ct -> Some (condense_type ct)),
              num_explicit_params ))
          bindings
      in
      EBindMutRec (condensed_bindings, condense_expr body)
  | Switch (e, branches) ->
      ESwitch
        ( condense_expr e,
          List.map
            (fun (pat, expr) -> (condense_pat pat, condense_expr expr))
            branches )
  | Block parts ->
      EBlock
        (List.map
           (function
             | Definition d -> Defn (condense_defn d)
             | Expr e -> Expr (condense_expr e))
           parts)

and condense_cons_expr : cons_expr -> c_expr = function
  | Cons (e1, e2) -> EBop (CCons, condense_disjunction e1, condense_cons_expr e2)
  | DisjunctionUnderCons d -> condense_disjunction d

and condense_disjunction : disjunction -> c_expr = function
  | Disjunction (conj, disj) ->
      EBop (COr, condense_conjunction conj, condense_disjunction disj)
  | ConjunctionUnderDisjunction conj -> condense_conjunction conj

and condense_conjunction : conjunction -> c_expr = function
  | Conjunction (rel_expr, conj) ->
      EBop (CAnd, condense_rel_expr rel_expr, condense_conjunction conj)
  | RelationUnderConjunction rel_expr -> condense_rel_expr rel_expr

and condense_rel_expr : rel_expr -> c_expr = function
  | Relation (rel_op, rel_expr, arith_expr) -> begin
      match rel_op with
      | EQ ->
          EBop (CEQ, condense_rel_expr rel_expr, condense_arith_expr arith_expr)
      | NE ->
          EBop (CNE, condense_rel_expr rel_expr, condense_arith_expr arith_expr)
      | LT ->
          EBop (CLT, condense_rel_expr rel_expr, condense_arith_expr arith_expr)
      | GT ->
          EBop (CGT, condense_rel_expr rel_expr, condense_arith_expr arith_expr)
      | LE ->
          EBop (CLE, condense_rel_expr rel_expr, condense_arith_expr arith_expr)
      | GE ->
          EBop (CGE, condense_rel_expr rel_expr, condense_arith_expr arith_expr)
    end
  | ArithmeticUnderRelExpr arith_expr -> condense_arith_expr arith_expr
  | CustomRelExpr (op_string, rel_expr, arith_expr) ->
      EApp
        ( EApp (EId op_string, condense_rel_expr rel_expr),
          condense_arith_expr arith_expr )

and condense_arith_expr : arith_expr -> c_expr = function
  | Plus (arith_expr, term) ->
      EBop (CPlus, condense_arith_expr arith_expr, condense_term term)
  | Minus (arith_expr, term) ->
      EBop (CMinus, condense_arith_expr arith_expr, condense_term term)
  | Term term -> condense_term term
  | CustomArithExpr (op_string, ae, t) ->
      if op_string = "^" then
        EBop (CConcat, condense_arith_expr ae, condense_term t)
      else EApp (EApp (EId op_string, condense_arith_expr ae), condense_term t)

and condense_term : term -> c_expr = function
  | Mul (t, af) -> EBop (CMul, condense_term t, condense_app_factor af)
  | Div (t, af) -> EBop (CDiv, condense_term t, condense_app_factor af)
  | Mod (t, af) -> EBop (CMod, condense_term t, condense_app_factor af)
  | Factor app_factor -> condense_app_factor app_factor
  | CustomTerm (op_string, t, af) ->
      EApp (EApp (EId op_string, condense_term t), condense_app_factor af)

and condense_app_factor : app_factor -> c_expr = function
  | Application (app_factor, factor) ->
      EApp (condense_app_factor app_factor, condense_factor factor)
  | FactorUnderApplication factor -> condense_factor factor

and condense_factor : factor -> c_expr = function
  | Boolean b -> EBool b
  | String s -> EString s
  | Unit -> EUnit
  | Integer i -> EInt i
  | Char c -> EChar c
  | FloatFactor f -> EFloat f
  | Id s -> EId s
  | ParenFactor expr -> condense_expr expr
  | Opposite factor -> EBop (CMinus, EInt 0, condense_factor factor)
  | Vector expressions -> EVector (List.map condense_expr expressions)
  | Nil -> ENil
  | ListSugar e_list ->
      let c_e_list : c_expr list = List.map condense_expr e_list in
      cons_from_list c_e_list
  | ListEnumeration (e1, e2) ->
      EListEnumeration (condense_expr e1, condense_expr e2)
  | ListComprehension (e, generators) ->
      EListComprehension
        (condense_expr e, List.map condense_generator generators)
  | RecordLit fields ->
      ERecordLit
        (List.map (fun (name, expr) -> (name, condense_expr expr)) fields)
  | RecordUpdate (record_expr, updates) ->
      ERecordUpdate
        ( condense_expr record_expr,
          List.map (fun (name, expr) -> (name, condense_expr expr)) updates )
  | FieldAccess (factor, field_name) ->
      EFieldAccess (condense_factor factor, field_name)

(* Condense types *)

and condense_factor_type : factor_type -> mono_type = function
  | IntegerType -> IntType
  | StringType -> StringType
  | BooleanType -> BoolType
  | CharType -> CharType
  | UnitType -> UnitType
  | FloatType -> FloatType
  | TypeVarWritten i ->
      (* When converting a type var, we add the prefix `$written$_` so that the
         name will not conflict with any internal type variables that we use. *)
      TypeVar ("$written(" ^ i ^ ")")
  | TypeName t -> TypeName t
  | ParenFactorType expr -> condense_compound_type expr
  | VectorType types -> VectorType (List.map condense_compound_type types)
  | ListType et -> CListType (condense_compound_type et)
  | TypeApp (name, args) -> (
      (* [t] and list<t> both denote list types; list<t> avoids a CTypeApp that
         would hit "Type not found: list" during simplification. *)
      match (name, args) with
      | ("list" | "List"), [ elem ] -> CListType (condense_compound_type elem)
      | _ when is_plain_type_var_name name ->
          TCtorApp
            ( Type_arity.written_param_var name,
              List.map condense_compound_type args )
      | _ -> CTypeApp (name, List.map condense_compound_type args))
  | RecordTypeWritten fields ->
      RecordType
        (List.map (fun (name, ct) -> (name, condense_compound_type ct)) fields)

and condense_compound_type : compound_type -> mono_type = function
  | BasicType bt -> condense_factor_type bt
  | FunctionType (i, o) ->
      FunctionType (condense_factor_type i, condense_compound_type o)

(* Given a type, condense it into a mono_type, then just wrap it as a
   monomorphic c_type *)
and condense_type : compound_type -> c_type = function
  | ct ->
      let mono_t = condense_compound_type ct in
      Mono mono_t

and condense_generator ((pat, expr) : generator) : c_pat * c_expr =
  (condense_pat pat, condense_expr expr)

and cons_from_list : c_expr list -> c_expr = function
  | [] -> ENil
  | e :: es -> EBop (CCons, e, cons_from_list es)

(** Replace free [EId] keys in [sub] (mapping to arbitrary expressions). *)
let rec subst_c_expr (sub : (string * c_expr) list) (e : c_expr) : c_expr =
  let s = subst_c_expr sub in
  match e with
  | EId x -> (
      match List.assoc_opt x sub with
      | Some e' -> e'
      | None -> e)
  | EApp (a, b) -> EApp (s a, s b)
  | EFunction (p, t, b) -> EFunction (p, t, s b)
  | EBind (p, t, e1, e2, r) -> EBind (p, t, s e1, s e2, r)
  | EBindRec (p, t, e1, e2, r) -> EBindRec (p, t, s e1, s e2, r)
  | EBindMutRec (bs, body) ->
      EBindMutRec
        (List.map (fun (p, t, e1, r, n) -> (p, t, s e1, r, n)) bs, s body)
  | EBlock parts ->
      EBlock
        (List.map
           (function
             | Expr ex -> Expr (s ex)
             | Defn d -> Defn d)
           parts)
  | ETernary (a, b, c) -> ETernary (s a, s b, s c)
  | ESwitch (scr, brs) ->
      ESwitch (s scr, List.map (fun (p, e') -> (p, s e')) brs)
  | EBop (op, a, b) -> EBop (op, s a, s b)
  | EVector es -> EVector (List.map s es)
  | EListComprehension (e, gens) ->
      EListComprehension (s e, List.map (fun (p, ge) -> (p, s ge)) gens)
  | ERecordLit fs -> ERecordLit (List.map (fun (n, e') -> (n, s e')) fs)
  | ERecordUpdate (e', fs) ->
      ERecordUpdate (s e', List.map (fun (n, e'') -> (n, s e'')) fs)
  | EFieldAccess (e', f) -> EFieldAccess (s e', f)
  | EListEnumeration (a, b) -> EListEnumeration (s a, s b)
  | (EBool _ | EString _ | EUnit | EInt _ | EChar _ | EFloat _ | ENil) as leaf
    -> leaf

let sanitize_method_internal (s : string) : string =
  String.map
    (function
      | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9') as c -> c
      | _ -> '_')
    s

let internal_tc_id (dispatch_cls : string) (meth : string) : string =
  "__forge_tc_" ^ dispatch_cls ^ "_" ^ sanitize_method_internal meth

(** [let rec f … = e in f] parses as [EBindRec (f, e, Id f)]. Dictionary code
    re-binds under [__forge_tc_*]; strip the outer wrapper so native lowering
    sees [EFunction …] for [peel_efun]. *)
let dict_bindrec_payload ~(meth : string) ~(intid : string) (e : c_expr) :
    (c_type option * c_expr * c_type option) option =
  match e with
  | EBindRec (CIdPat nm, ta, e1, EId tail, rt)
    when String.equal nm meth && String.equal tail intid -> Some (ta, e1, rt)
  | _ -> None

(** Whether [e] mentions [id] as [EId] (trait dict internals are unique). *)
let rec expr_refs_c_id (id : string) (e : c_expr) : bool =
  match e with
  | EId s -> String.equal s id
  | EApp (a, b) -> expr_refs_c_id id a || expr_refs_c_id id b
  | EFunction (_, _, b) -> expr_refs_c_id id b
  | EBind (_, _, e1, e2, _) -> expr_refs_c_id id e1 || expr_refs_c_id id e2
  | EBindRec (_, _, e1, e2, _) -> expr_refs_c_id id e1 || expr_refs_c_id id e2
  | EBindMutRec (bs, body) ->
      List.exists (fun (_, _, e1, _, _) -> expr_refs_c_id id e1) bs
      || expr_refs_c_id id body
  | EBlock parts ->
      List.exists
        (function
          | Expr ex -> expr_refs_c_id id ex
          | Defn _ -> false)
        parts
  | ETernary (a, b, c) ->
      expr_refs_c_id id a || expr_refs_c_id id b || expr_refs_c_id id c
  | ESwitch (scr, brs) ->
      expr_refs_c_id id scr
      || List.exists (fun (_, e') -> expr_refs_c_id id e') brs
  | EBop (_, a, b) -> expr_refs_c_id id a || expr_refs_c_id id b
  | EVector es -> List.exists (expr_refs_c_id id) es
  | EListComprehension (e, gens) ->
      expr_refs_c_id id e
      || List.exists (fun (_, ge) -> expr_refs_c_id id ge) gens
  | ERecordLit fs -> List.exists (fun (_, e') -> expr_refs_c_id id e') fs
  | ERecordUpdate (e', fs) ->
      expr_refs_c_id id e'
      || List.exists (fun (_, e'') -> expr_refs_c_id id e'') fs
  | EFieldAccess (e', _) -> expr_refs_c_id id e'
  | EListEnumeration (a, b) -> expr_refs_c_id id a || expr_refs_c_id id b
  | EBool _ | EString _ | EUnit | EInt _ | EChar _ | EFloat _ | ENil -> false

(** Order methods for dict binding: dependencies (other methods' internal ids in
    the rhs) come earlier. Self-reference is excluded (handled via [EBindRec]).
    On cycle, return [None] (caller falls back to [EBindMutRec]). *)
let topo_dict_methods ~(dispatch_d : string) (names : string list)
    (condensed : (string * c_expr) list) : (string * c_expr) list option =
  let prereqs m =
    let e = List.assoc m condensed in
    List.filter
      (fun m' ->
        (not (String.equal m m'))
        && expr_refs_c_id (internal_tc_id dispatch_d m') e)
      names
  in
  let deg = ref (List.map (fun n -> (n, List.length (prereqs n))) names) in
  let get n = List.assoc n !deg in
  let set n d =
    deg := List.map (fun (x, d0) -> if x = n then (x, d) else (x, d0)) !deg
  in
  let rec go emitted =
    if List.length emitted = List.length names then
      Some (List.map (fun m -> (m, List.assoc m condensed)) (List.rev emitted))
    else
      match
        List.find_opt (fun n -> (not (List.mem n emitted)) && get n = 0) names
      with
      | None -> None
      | Some u ->
          List.iter
            (fun v ->
              if (not (List.mem v emitted)) && List.mem u (prereqs v) then
                set v (get v - 1))
            names;
          go (u :: emitted)
  in
  go []

(** Stable emission order for multi-dict [impl] (unknown classes sort last). *)
let preferred_dict_order = [ "Functor"; "Applicative"; "Monad" ]

let dict_class_rank (c : string) : int =
  let rec go i = function
    | [] -> if c = "" then 2000 else 1000 + Char.code (String.get c 0)
    | h :: t -> if String.equal h c then i else go (i + 1) t
  in
  go 0 preferred_dict_order

type class_entry = {
  params : string list;
  param_arities : (string * int) list;
      (** Declared on this trait only — [CClassDecl]. *)
  declared_triples : (string * mono_type * string) list;
      (** Full method set for [impl Class] (inheritance + defaults). *)
  impl_spec : (string * mono_type * string * expr option) list;
}

let verify_requires_head (our_param : string)
    ((_super, head_ct) : string * compound_type) : unit =
  match head_ct with
  | BasicType (TypeVarWritten v) when v = our_param -> ()
  | BasicType (TypeName v) when v = our_param -> ()
  | _ ->
      failwith
        "forge: requires must use the trait type parameter (e.g. requires \
         Functor<f> when the trait is <f>)"

let merge_inherited_specs
    (acc : (string * mono_type * string * expr option) list)
    (more : (string * mono_type * string * expr option) list) :
    (string * mono_type * string * expr option) list =
  List.fold_left
    (fun a ((n, _, _, _) as item) ->
      if List.exists (fun (n', _, _, _) -> String.equal n' n) a then a
      else item :: a)
    acc more

(** Expand [inter] / [impl] into [CClassDecl] plus dictionary [let]s.
    Definitions must appear in order: each [impl] references an [inter] defined
    earlier in the same file. *)
let condense_program (defns : defn list) : c_defn list =
  let record_ctor_arities (seen : (string * int) list) (d : defn) :
      (string * int) list =
    match d with
    | TypeDef (name, params, _) -> (name, List.length params) :: seen
    | SumTypeDef (name, params, _) -> (name, List.length params) :: seen
    | SumTypeDefRec (name, params, _) -> (name, List.length params) :: seen
    | SumTypeDefMutRec group ->
        List.fold_left
          (fun acc (name, params, _) -> (name, List.length params) :: acc)
          seen group
    | _ -> seen
  in
  let rec walk (classes : (string * class_entry) list)
      (seen_ctors : (string * int) list) (seen_instances : string list)
      (acc : c_defn list) = function
    | [] -> List.rev acc
    | ClassDef (name, param_specs, requires, items) :: rest ->
        let params = List.map fst param_specs in
        if params = [] then
          failwith "forge: inter/trait needs a type parameter list <a> (MVP)";
        let params_uniq = List.sort_uniq String.compare params in
        if List.length params_uniq <> List.length params then
          failwith "forge: duplicate type parameter in inter/trait";
        if List.exists (fun (n, _) -> String.equal n name) classes then
          failwith ("forge: duplicate inter/trait: " ^ name);
        if List.length params <> 1 then
          failwith
            "forge: exactly one inter/trait type parameter is supported in \
             this MVP";
        let our_param = List.hd params in
        List.iter (verify_requires_head our_param) requires;
        let own_vals =
          List.filter_map
            (function
              | TraitVal (m, ct) -> Some (m, ct)
              | TraitLet _ -> None)
            items
        in
        let own_lets =
          List.filter_map
            (function
              | TraitLet (m, e) -> Some (m, e)
              | TraitVal _ -> None)
            items
        in
        let lets_map = own_lets in
        let inherited =
          List.fold_left
            (fun acc_req (super, _head) ->
              match List.assoc_opt super classes with
              | None ->
                  failwith
                    ("forge: trait " ^ name ^ " requires unknown trait '"
                   ^ super ^ "'")
              | Some super_entry ->
                  merge_inherited_specs acc_req super_entry.impl_spec)
            [] requires
        in
        let own_names = List.map fst own_vals in
        let own_names_uniq = List.sort_uniq String.compare own_names in
        if List.length own_names_uniq <> List.length own_names then
          failwith ("forge: duplicate method name in trait " ^ name);
        List.iter
          (fun (ln, _) ->
            if not (List.mem ln own_names) then
              failwith
                ("forge: trait " ^ name ^ ": default let for '" ^ ln
               ^ "' has no matching val in this trait"))
          lets_map;
        let methods_mono_raw_own =
          List.map (fun (m, ct) -> (m, condense_compound_type ct)) own_vals
        in
        let inherited_types = List.map (fun (_, mt, _, _) -> mt) inherited in
        let param_arities_inferred =
          Type_arity.infer_inter_param_arities params
            (inherited_types @ List.map snd methods_mono_raw_own)
        in
        let param_arities =
          List.map
            (fun (p, explicit) ->
              let inferred = List.assoc p param_arities_inferred in
              if explicit >= 0 then (
                if inferred > 0 && inferred <> explicit then
                  failwith
                    ("forge: trait " ^ name ^ ": explicit arity for " ^ p
                   ^ " does not match use in method signatures (expected "
                   ^ string_of_int explicit ^ ", inferred "
                   ^ string_of_int inferred ^ ")");
                (p, explicit))
              else (p, inferred))
            param_specs
        in
        let methods_mono_own =
          List.map
            (fun (m, mt) ->
              (m, Type_arity.replace_inter_heads_with_tctor ~params mt))
            methods_mono_raw_own
        in
        let declared_triples =
          List.map (fun (m, mt) -> (m, mt, name)) methods_mono_own
        in
        let own_meth_names = List.map fst own_vals in
        let impl_spec =
          List.filter
            (fun (n, _, _, _) -> not (List.mem n own_meth_names))
            inherited
          @ List.map
              (fun (m, mt) -> (m, mt, name, List.assoc_opt m lets_map))
              methods_mono_own
        in
        let decl = CClassDecl (name, params, declared_triples) in
        let entry = { params; param_arities; declared_triples; impl_spec } in
        walk ((name, entry) :: classes) seen_ctors seen_instances (decl :: acc)
          rest
    | InstanceDef (cls, inst_ct, impls) :: rest -> (
        match List.assoc_opt cls classes with
        | None ->
            failwith
              ("forge: impl for unknown inter '" ^ cls
             ^ "' — declare [inter] above this [impl]")
        | Some entry -> (
            match entry.params with
            | [ p ] ->
                let inst_mono = condense_compound_type inst_ct in
                let arity = List.assoc p entry.param_arities in
                Type_arity.validate_impl_head ~class_name:cls
                  ~required_arity:arity ~seen_ctors inst_mono;
                let w = Type_arity.written_param_var p in
                let subst mt =
                  Type_arity.substitute_instance_in_mono ~written_var:w
                    ~inst:inst_mono arity mt
                in
                let slug = mono_type_slug inst_mono in
                let impl_names = List.map fst impls in
                let impl_uniq = List.sort_uniq String.compare impl_names in
                if List.length impl_uniq <> List.length impl_names then
                  failwith "forge: duplicate method in impl";
                List.iter
                  (fun (m, _) ->
                    match
                      List.find_opt
                        (fun (n, _, _, _) -> String.equal n m)
                        entry.impl_spec
                    with
                    | None ->
                        failwith ("forge: impl has unknown method: " ^ m)
                    | Some (_, _, d, _) when not (String.equal d cls) ->
                        failwith
                          ("forge: method '" ^ m ^ "' belongs to trait '" ^ d
                         ^ "'; provide it in impl " ^ d ^ " instead")
                    | _ -> ())
                  impls;
                List.iter
                  (fun (m, _, d, def) ->
                    if String.equal d cls then
                      let has_impl = List.mem m impl_names in
                      let has_def =
                        match def with
                        | None -> false
                        | Some _ -> true
                      in
                      if (not has_impl) && not has_def then
                        failwith ("forge: impl missing method: " ^ m))
                  entry.impl_spec;
                let dispatch_classes = [ cls ]
                in
                let expr_for_method (meth : string) : c_expr =
                  match List.assoc_opt meth impls with
                  | Some e -> condense_expr e
                  | None -> (
                      match
                        List.find_opt
                          (fun (n, _, _, _) -> String.equal n meth)
                          entry.impl_spec
                      with
                      | Some (_, _, _, Some def) -> condense_expr def
                      | _ ->
                          failwith ("forge: internal: missing body for " ^ meth)
                      )
                in
                let build_dict_expr (dispatch_d : string)
                    (fields : (string * mono_type) list) : c_expr =
                  let names = List.map fst fields in
                  let internals =
                    List.map (fun m -> (m, internal_tc_id dispatch_d m)) names
                  in
                  let sub =
                    List.map (fun (m, intid) -> (m, EId intid)) internals
                  in
                  let condensed =
                    List.map
                      (fun m -> (m, subst_c_expr sub (expr_for_method m)))
                      names
                  in
                  let record =
                    ERecordLit
                      (List.map (fun (m, intid) -> (m, EId intid)) internals)
                  in
                  let bind_chain ordered =
                    List.fold_right
                      (fun (m, e) acc ->
                        let intid = internal_tc_id dispatch_d m in
                        match dict_bindrec_payload ~meth:m ~intid e with
                        | Some (ta, e1, rt) ->
                            EBindRec (CIdPat intid, ta, e1, acc, rt)
                        | None ->
                            if expr_refs_c_id intid e then
                              EBindRec (CIdPat intid, None, e, acc, None)
                            else EBind (CIdPat intid, None, e, acc, None))
                      ordered record
                  in
                  match topo_dict_methods ~dispatch_d names condensed with
                  | Some ordered -> bind_chain ordered
                  | None ->
                      EBindMutRec
                        ( List.map
                            (fun (m, intid) ->
                              let e0 = List.assoc m condensed in
                              let ta, rhs, rt =
                                match
                                  dict_bindrec_payload ~meth:m ~intid e0
                                with
                                | Some (ta, e1, rt) -> (ta, e1, rt)
                                | None -> (None, e0, None)
                              in
                              (CIdPat intid, ta, rhs, rt, 0))
                            internals,
                          record )
                in
                let new_seen, new_cdefns =
                  List.fold_left
                    (fun (seen_acc, cdefns_acc) dispatch_d ->
                      let fields_here =
                        List.filter_map
                          (fun (m, mt, d, _) ->
                            if String.equal d dispatch_d then Some (m, mt)
                            else None)
                          entry.impl_spec
                      in
                      let field_types =
                        List.map (fun (m, mt) -> (m, subst mt)) fields_here
                      in
                      let expected = RecordType field_types in
                      let key = dispatch_d ^ "#" ^ slug in
                      if List.mem key seen_acc then
                        failwith
                          ("forge: duplicate impl dict for " ^ dispatch_d
                         ^ " at type " ^ slug);
                      let dict_name =
                        dict_for_instance ~class_name:dispatch_d inst_mono
                      in
                      let body = build_dict_expr dispatch_d fields_here in
                      let cdefn =
                        CDefn
                          ( CIdPat dict_name,
                            [],
                            Some (Mono expected),
                            body,
                            None,
                            0 )
                      in
                      (key :: seen_acc, cdefn :: cdefns_acc))
                    (seen_instances, []) dispatch_classes
                in
                walk classes seen_ctors new_seen
                  (List.rev_append new_cdefns acc)
                  rest
            | _ -> failwith "forge: internal inter arity"))
    | SumTypeDefRec ("List", _, ctors) :: rest
      when List.exists (fun (n, _) -> n = "[]") ctors
           && List.exists (fun (n, _) -> n = "(::)") ctors ->
        walk classes seen_ctors seen_instances acc rest
    | d :: rest ->
        let seen_ctors' = record_ctor_arities seen_ctors d in
        walk classes seen_ctors' seen_instances (condense_defn d :: acc) rest
  in
  walk [] [] [] [] defns

let rec all_type_vars_in_type : mono_type -> string list = function
  | IntType | FloatType | BoolType | StringType | CharType | UnitType -> []
  | TypeVar v -> [ v ]
  | FunctionType (i, o) ->
      all_type_vars_in_type i @ all_type_vars_in_type o
      |> List.sort_uniq compare
  | VectorType types ->
      List.concat (List.map all_type_vars_in_type types)
      |> List.sort_uniq compare
  | CListType et -> all_type_vars_in_type et
  | TypeName _ -> []
  | CTypeApp (_, args) ->
      List.concat (List.map all_type_vars_in_type args)
      |> List.sort_uniq compare
  | TCtorApp (_, args) ->
      List.concat (List.map all_type_vars_in_type args)
      |> List.sort_uniq compare
  | FixedPoint (_, body) -> all_type_vars_in_type body
  | RecordType fields ->
      List.concat (List.map (fun (_, t) -> all_type_vars_in_type t) fields)
      |> List.sort_uniq compare
