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
  | NothingPat -> CNothingPat
  | IdPat s -> CIdPat s
  | NilPat -> CNilPat
  | VectorPat pats -> CVectorPat (List.map condense_pat pats)
  | WildcardPat -> CWildcardPat
  | Pat pat -> condense_pat pat

let rec condense_defn : defn -> c_defn = function
  | Defn (pattern, cto, body_expression) ->
      let a : c_pat = condense_pat pattern in
      let b : c_type option =
        match cto with
        | None -> None
        | Some t -> Some (condense_type t)
      in

      let c : c_expr = condense_expr body_expression in
      CDefn (a, b, c)

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
  | BindRec (pat, cto, e1, e2) ->
      EBindRec
        ( condense_pat pat,
          (match cto with
          | None -> None
          | Some ct -> Some (condense_type ct)),
          condense_expr e1,
          condense_expr e2 )
  | Switch (e, branches) ->
      ESwitch
        ( condense_expr e,
          List.map
            (fun (pat, expr) -> (condense_pat pat, condense_expr expr))
            branches )

and condense_cons_expr : cons_expr -> c_expr = function
  | Cons (e1, e2) -> EBop (CCons, condense_disjunction e1, condense_cons_expr e2)
  | DisjunctionUnderCons d -> condense_disjunction d

and condense_disjunction : disjunction -> c_expr = function
  | Disjunction (conj, disj) ->
      EBop (COr, condense_conjunction conj, condense_disjunction disj)
  | ConjunctionUnderDisjunction conj -> condense_conjunction conj

and condense_conjunction : conjunction -> c_expr = function
  | Conjunction (eq_expr, conj) ->
      EBop (CAnd, condense_eq_expr eq_expr, condense_conjunction conj)
  | EqualityUnderConjunction eq_expr -> condense_eq_expr eq_expr

and condense_eq_expr : eq_expr -> c_expr = function
  | Equality (eq_op, rel_expr, eq_expr) -> begin
      match eq_op with
      | EQ -> EBop (CEQ, condense_rel_expr rel_expr, condense_eq_expr eq_expr)
      | NE -> EBop (CNE, condense_rel_expr rel_expr, condense_eq_expr eq_expr)
    end
  | RelationUnderEqExpr rel_expr -> condense_rel_expr rel_expr

and condense_rel_expr : rel_expr -> c_expr = function
  | Relation (rel_op, arith_expr, rel_expr) -> begin
      match rel_op with
      | LT ->
          EBop (CLT, condense_arith_expr arith_expr, condense_rel_expr rel_expr)
      | GT ->
          EBop (CGT, condense_arith_expr arith_expr, condense_rel_expr rel_expr)
      | LE ->
          EBop (CLE, condense_arith_expr arith_expr, condense_rel_expr rel_expr)
      | GE ->
          EBop (CGE, condense_arith_expr arith_expr, condense_rel_expr rel_expr)
    end
  | ArithmeticUnderRelExpr arith_expr -> condense_arith_expr arith_expr

and condense_arith_expr : arith_expr -> c_expr = function
  | Plus (term, arith_expr) ->
      EBop (CPlus, condense_term term, condense_arith_expr arith_expr)
  | Minus (term, arith_expr) ->
      EBop (CMinus, condense_term term, condense_arith_expr arith_expr)
  | Term term -> condense_term term

and condense_term : term -> c_expr = function
  | Mul (t, f) -> EBop (CMul, condense_term t, condense_factor f)
  | Div (t, f) -> EBop (CDiv, condense_term t, condense_factor f)
  | Mod (t, f) -> EBop (CMod, condense_term t, condense_factor f)
  | Factor factor -> condense_factor factor

and condense_factor : factor -> c_expr = function
  | Boolean b -> EBool b
  | String s -> EString s
  | Nothing -> ENothing
  | Integer i -> EInt i
  | Id s -> EId s
  | ParenFactor expr -> condense_expr expr
  | App (factor1, factor2) ->
      EApp (condense_factor factor1, condense_factor factor2)
  | Opposite factor -> EBop (CMinus, EInt 0, condense_factor factor)
  | Vector expressions -> EVector (List.map condense_expr expressions)
  | Nil -> ENil

and factor_type_to_t : factor_type -> c_type = function
  | BooleanType -> BoolType
  | StringType -> StringType
  | NothingType -> NothingType
  | IntegerType -> IntType
  | TypeVarWritten i -> TypeVarWritten i
  | ParenFactorType expr -> condense_type expr
  | VectorType types -> VectorType (List.map condense_type types)
  | ListType et -> CListType (condense_type et)

and condense_type : compound_type -> c_type = function
  | BasicType bt -> factor_type_to_t bt
  | FunctionType (i, o) -> FunctionType (factor_type_to_t i, condense_type o)
