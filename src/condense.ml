open Expr
open Cexpr
open Forge_class_util

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
  | Defn (pattern, cto, body_expression, return_type, num_explicit_params) ->
      let a : c_pat = condense_pat pattern in
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
      CDefn (a, b, c, d, num_explicit_params)
  | DefnRec (pattern, cto, body_expression, return_type, num_explicit_params) ->
      let a : c_pat = condense_pat pattern in
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
      CDefnRec (a, b, c, d, num_explicit_params)
  | DefnMutRec defns ->
      let condensed_defns =
        List.map
          (fun (pattern, cto, body_expression, return_type, num_explicit_params) ->
            ( condense_pat pattern,
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
          (match return_type with
          | None -> None
          | Some ct -> Some (condense_type ct)) )
  | BindRec (pat, cto, e1, e2, return_type) ->
      EBindRec
        ( condense_pat pat,
          (match cto with
          | None -> None
          | Some ct -> Some (condense_type ct)),
          condense_expr e1,
          condense_expr e2,
          (match return_type with
          | None -> None
          | Some ct -> Some (condense_type ct)) )
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
      else
        EApp (EApp (EId op_string, condense_arith_expr ae), condense_term t)

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
      ERecordLit (List.map (fun (name, expr) -> (name, condense_expr expr)) fields)
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
      match name, args with
      | "list", [ elem ] ->
          CListType (condense_compound_type elem)
      | _ -> CTypeApp (name, List.map condense_compound_type args))
  | RecordTypeWritten fields ->
      RecordType (List.map (fun (name, ct) -> (name, condense_compound_type ct)) fields)

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

(** Expand [inter] / [impl] into [CClassDecl] plus dictionary [let]s. Definitions
    must appear in order: each [impl] references an [inter] defined earlier in
    the same file. *)
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
  let rec walk
      (classes :
        (string * (string list * (string * mono_type) list * (string * int) list))
        list)
      (seen_ctors : (string * int) list) (seen_instances : string list)
      (acc : c_defn list) = function
    | [] -> List.rev acc
    | ClassDef (name, params, methods) :: rest ->
        if params = [] then
          failwith "forge: inter needs a type parameter list <a> (MVP)";
        let params_uniq = List.sort_uniq String.compare params in
        if List.length params_uniq <> List.length params then
          failwith "forge: duplicate type parameter in inter";
        if List.exists (fun (n, _) -> n = name) classes then
          failwith ("forge: duplicate inter: " ^ name);
        if List.length params <> 1 then
          failwith
            "forge: exactly one inter type parameter is supported in this MVP";
        let methods_mono_raw =
          List.map (fun (m, ct) -> (m, condense_compound_type ct)) methods
        in
        let param_arities =
          Type_arity.infer_inter_param_arities params
            (List.map snd methods_mono_raw)
        in
        let methods_mono =
          List.map
            (fun (m, mt) ->
              ( m,
                Type_arity.replace_inter_heads_with_tctor ~params mt ))
            methods_mono_raw
        in
        let method_names = List.map fst methods_mono in
        let names_uniq = List.sort_uniq String.compare method_names in
        if List.length names_uniq <> List.length method_names then
          failwith "forge: duplicate method name in inter";
        let decl = CClassDecl (name, params, methods_mono) in
        walk
          ((name, (params, methods_mono, param_arities)) :: classes)
          seen_ctors seen_instances (decl :: acc) rest
    | InstanceDef (cls, inst_ct, impls) :: rest -> (
        match List.assoc_opt cls classes with
        | None ->
            failwith
              ("forge: impl for unknown inter '" ^ cls
             ^ "' — declare [inter] above this [impl]")
        | Some (params, meth_specs, param_arities) -> (
            match params with
            | [ p ] ->
                let inst_mono = condense_compound_type inst_ct in
                let arity = List.assoc p param_arities in
                Type_arity.validate_impl_head ~class_name:cls ~required_arity:arity
                  ~seen_ctors inst_mono;
                let w = Type_arity.written_param_var p in
                let subst mt =
                  Type_arity.substitute_instance_in_mono ~written_var:w
                    ~inst:inst_mono arity mt
                in
                let field_types =
                  List.map (fun (m, mt) -> (m, subst mt)) meth_specs
                in
                let expected = RecordType field_types in
                let impl_names = List.map fst impls in
                let impl_uniq = List.sort_uniq String.compare impl_names in
                if List.length impl_uniq <> List.length impl_names then
                  failwith "forge: duplicate method in impl";
                List.iter
                  (fun (m, _) ->
                    if not (List.mem_assoc m meth_specs) then
                      failwith ("forge: impl has unknown method: " ^ m))
                  impls;
                List.iter
                  (fun (m, _) ->
                    if not (List.mem m impl_names) then
                      failwith ("forge: impl missing method: " ^ m))
                  meth_specs;
                let dict_fields =
                  List.map
                    (fun (m, _) -> (m, condense_expr (List.assoc m impls)))
                    meth_specs
                in
                let slug = mono_type_slug inst_mono in
                let key = cls ^ "#" ^ slug in
                if List.mem key seen_instances then
                  failwith
                    ("forge: duplicate impl for inter " ^ cls ^ " at type "
                   ^ slug);
                let dict_name = dict_for_instance ~class_name:cls inst_mono in
                let cdefn =
                  CDefn
                    ( CIdPat dict_name,
                      Some (Mono expected),
                      ERecordLit dict_fields,
                      None,
                      0 )
                in
                walk classes seen_ctors (key :: seen_instances) (cdefn :: acc)
                  rest
            | _ -> failwith "forge: internal inter arity"))
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
