open Expr
open Cexpr

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
  | InterfaceDef (name, type_params, methods) ->
      let condensed_methods = List.map (fun (method_name, method_type) ->
        (method_name, condense_type method_type)
      ) methods in
      CInterfaceDef (name, type_params, condensed_methods)
  | InterfaceImpl (iface_name, impl_type, method_impls) ->
      let condensed_type = condense_compound_type impl_type in
      let condensed_methods = List.map (fun (method_name, method_expr) ->
        (method_name, condense_expr method_expr)
      ) method_impls in
      CInterfaceImpl (iface_name, condensed_type, condensed_methods)

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
  | TypeApp (name, args) -> CTypeApp (name, List.map condense_compound_type args)
  | RecordTypeWritten fields ->
      RecordType (List.map (fun (name, ct) -> (name, condense_compound_type ct)) fields)
  | ConstrainedType (_, base_type) ->
      (* For now, extract the base type and ignore constraints *)
      (* Constraints will be handled during type checking *)
      condense_compound_type base_type

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

and all_type_vars_in_type : mono_type -> string list = function
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
  | FixedPoint (_, body) -> all_type_vars_in_type body
  | RecordType fields ->
      List.concat (List.map (fun (_, t) -> all_type_vars_in_type t) fields)
      |> List.sort_uniq compare
