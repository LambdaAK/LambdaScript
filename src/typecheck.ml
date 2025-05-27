open Cexpr
open C_to_string
open Typefixer

type type_equation = mono_type * mono_type
type type_equations = type_equation list

type type_error =
  | UnboundVariable of string
  | TypeMismatch of mono_type * mono_type
  | PatternMismatch of c_pat * mono_type
  | OtherError of string

type 'a type_check_result =
  | Ok of 'a
  | Error of type_error

let return (x : 'a) : 'a type_check_result = Ok x

let ( >>= ) (x : 'a type_check_result) (f : 'a -> 'b type_check_result) :
    'b type_check_result =
  match x with
  | Ok x -> f x
  | Error e -> Error e

let ( let* ) = ( >>= )

exception TypeFailure

(* Algorithm for performing type inference:

   Given an expression e, we want to infer the most general type of e.

   We start with an empty static environment. We generate constraints for e.

   Then, we solve the constraints to get a most general monomorphic type for e.

   Finally, we generalize the monomorphic type to a polymorphic type. *)

(** [split3 lst] splits a list of triples into three lists.

    For example, [split3 [(1,2,3); (4,5,6)]] returns ([1;4], [2;5], [3;6])

    @param lst The list of triples to split
    @return
      A triple containing three lists - one for each component of the input
      triples *)
let split3 (lst : ('a * 'b * 'c) list) : 'a list * 'b list * 'c list =
  let rec split3_helper (lst : ('a * 'b * 'c) list) (a : 'a list) (b : 'b list)
      (c : 'c list) : 'a list * 'b list * 'c list =
    match lst with
    | [] -> (a, b, c)
    | (a', b', c') :: tail -> split3_helper tail (a' :: a) (b' :: b) (c' :: c)
  in
  let a, b, c = split3_helper lst [] [] [] in
  (List.rev a, List.rev b, List.rev c)

(** [string_of_type_equation eq] converts a type equation to a string
    representation.

    For example, [string_of_type_equation (IntType, BoolType)] returns "int =
    bool"

    @param eq A type equation consisting of two monomorphic types
    @return
      A string representation of the equation with the types separated by " = "
*)
let string_of_type_equation ((t1, t2) : type_equation) : string =
  string_of_mono_type t1 ^ " = " ^ string_of_mono_type t2

(** [string_of_type_equations equations] converts a list of type equations to a
    string representation.

    Each equation is printed on a new line using [string_of_type_equation].

    For example,
    [string_of_type_equations [(IntType, BoolType); (StringType, IntType)]]
    returns: "int = bool string = int"

    @param equations A list of type equations
    @return
      A string representation of the equations with each equation on a new line
*)
let rec string_of_type_equations (c : type_equations) : string =
  match c with
  | [] -> ""
  | (t1, t2) :: c' ->
      string_of_type_equation (t1, t2) ^ "\n" ^ string_of_type_equations c'

(** [string_of_static_env env] converts a static environment to a string
    representation.

    Each binding in the environment is printed on a new line in the format "id :
    type".

    For example, [string_of_static_env [("x", IntType); ("y", BoolType)]]
    returns: "x : int y : bool"

    @param env A static environment mapping identifiers to types
    @return
      A string representation of the environment with each binding on a new line
*)
let rec string_of_static_env (env : static_env) : string =
  match env with
  | [] -> ""
  | (id, t) :: env' ->
      id ^ " : " ^ string_of_c_type t ^ "\n" ^ string_of_static_env env'

and string_of_mono_type (t : mono_type) : string =
  match t with
  | IntType -> "int"
  | FloatType -> "float"
  | BoolType -> "bool"
  | StringType -> "string"
  | UnitType -> "unit"
  | TypeVar v -> v
  | FunctionType (t1, t2) ->
      let t1_str = string_of_mono_type t1 in
      let t2_str = string_of_mono_type t2 in
      "(" ^ t1_str ^ " -> " ^ t2_str ^ ")"
  | VectorType types ->
      let types_str = List.map string_of_mono_type types in
      "(" ^ String.concat ", " types_str ^ ")"
  | CListType et -> "[" ^ string_of_mono_type et ^ "]"

(** [generate env e] performs type inference on the expression [e] in the static
    environment [env].

    It returns a pair [(t, constraints)], where [t] is the inferred monomorphic
    type of [e], and [constraints] is a list of type equations (constraints)
    that must be satisfied for the typing to be valid.

    The function recursively traverses the structure of [e], generating fresh
    type variables as needed, and accumulates constraints based on the typing
    rules for each kind of expression.

    @param env The static environment mapping variable names to their types
    @param e The expression to typecheck
    @return
      A pair [(t, constraints)] where [t] is the inferred type and [constraints]
      is the list of type equations *)
let rec generate (env : static_env) (e : c_expr) :
    (mono_type * type_equations) type_check_result =
  match e with
  | EInt _ -> generate_e_int
  | EFloat _ -> generate_e_float
  | EBool _ -> generate_e_bool
  | EString _ -> generate_e_string
  | EUnit -> generate_e_unit
  | ENil -> generate_e_nil ()
  | EId x -> generate_e_id env x
  | EBop (op, e1, e2) -> generate_e_bop env op e1 e2
  | EFunction (pat, cto, body) -> generate_e_function env pat cto body
  | EApp (e1, e2) -> generate_e_app env e1 e2
  | EBind (pat, cto, e1, e2) -> generate_e_bind env pat cto e1 e2
  | EBindRec (pat, _, e1, e2) -> generate_e_bind_rec env pat e1 e2
  | ETernary (e1, e2, e3) -> generate_e_ternary env e1 e2 e3
  | EVector expressions -> generate_e_vector env expressions
  | EListEnumeration (e1, e2) -> generate_e_list_enumeration env e1 e2
  | EListComprehension (e, generators) ->
      generate_e_list_comprehension env e generators
  | ESwitch (e1, branches) -> generate_e_switch env e1 branches

(** [generate_e_int] generates type constraints for integer literals.
    @return A pair containing IntType and an empty list of constraints *)
and generate_e_int = return (IntType, [])

(** [generate_e_float] generates type constraints for float literals.
    @return A pair containing FloatType and an empty list of constraints *)
and generate_e_float = return (FloatType, [])

(** [generate_e_bool] generates type constraints for boolean literals.
    @return A pair containing BoolType and an empty list of constraints *)
and generate_e_bool = return (BoolType, [])

(** [generate_e_string] generates type constraints for string literals.
    @return A pair containing StringType and an empty list of constraints *)
and generate_e_string = return (StringType, [])

(** [generate_e_unit] generates type constraints for unit literals.
    @return A pair containing UnitType and an empty list of constraints *)
and generate_e_unit = return (UnitType, [])

(** [generate_e_nil] generates type constraints for nil literals.
    @return
      A pair containing a list type with a fresh type variable and an empty list
      of constraints *)
and generate_e_nil () = return (CListType (fresh_type_var ()), [])

(** [generate_e_id env x] generates type constraints for identifier expressions.
    @param env The static environment mapping identifiers to their types
    @param x The identifier to look up
    @return
      A pair containing the instantiated type of the identifier and an empty
      list of constraints *)
and generate_e_id (env : static_env) (x : string) :
    (mono_type * type_equations) type_check_result =
  let uninstantiated : c_type = List.assoc x env in
  let t = instantiate uninstantiated in
  return (t, [])

(** [generate_e_bop env op e1 e2] generates type constraints for binary
    operations.
    @param env The static environment
    @param op The binary operator
    @param e1 The left operand expression
    @param e2 The right operand expression
    @return A pair containing the result type and constraints for the operation
*)
and generate_e_bop (env : static_env) (op : c_bop) (e1 : c_expr) (e2 : c_expr) :
    (mono_type * type_equations) type_check_result =
  let* t1, c1 = generate env e1 in
  let* t2, c2 = generate env e2 in
  match op with
  | CCons ->
      (* e1 :: e2, e2 must be a list of the type of e1 *)
      return (t2, ((CListType t1, t2) :: c1) @ c2)
  | CPlus | CMinus | CMul | CDiv | CMod ->
      (* arithmetic: both operands must be int, result is int *)
      return (IntType, ((t1, IntType) :: (t2, IntType) :: c1) @ c2)
  | CGE | CGT | CLE | CLT ->
      (* comparisons: both operands must be int, result is bool *)
      return (BoolType, ((t1, IntType) :: (t2, IntType) :: c1) @ c2)
  | CEQ | CNE ->
      (* equality/inequality: operands must be same type, result is bool *)
      return (BoolType, ((t1, t2) :: c1) @ c2)
  | CAnd | COr ->
      (* logical: both operands must be bool, result is bool *)
      return (BoolType, ((t1, BoolType) :: (t2, BoolType) :: c1) @ c2)

(** [generate_e_function env pat cto body] generates type constraints for
    function expressions.
    @param env The static environment
    @param pat The function parameter pattern
    @param cto Optional type annotation for the function
    @param body The function body expression
    @return A pair containing the function type and constraints for the function
*)
and generate_e_function (env : static_env) (pat : c_pat) (cto : c_type option)
    (body : c_expr) : (mono_type * type_equations) type_check_result =
  let input_type, new_env_bindings, constraints_from_pattern =
    type_of_pat pat
  in
  let constraints_from_type_annotation : type_equations =
    match cto with
    | Some t -> [ (input_type, instantiate t) ]
    | None -> []
  in
  let* output_type, c_output = generate (new_env_bindings @ env) body in
  return
    ( input_type => output_type,
      constraints_from_pattern @ constraints_from_type_annotation @ c_output )

(** [generate_e_app env e1 e2] generates type constraints for function
    application.
    @param env The static environment
    @param e1 The function expression
    @param e2 The argument expression
    @return
      A pair containing the result type and constraints for the application *)
and generate_e_app (env : static_env) (e1 : c_expr) (e2 : c_expr) :
    (mono_type * type_equations) type_check_result =
  let* t1, c1 = generate env e1 in
  let* t2, c2 = generate env e2 in
  let result_type = fresh_type_var () in
  let app_constraint = (t1, FunctionType (t2, result_type)) in
  return (result_type, (app_constraint :: c1) @ c2)

(** [generate_e_bind env pat cto e1 e2] generates type constraints for let
    bindings.
    @param env The static environment
    @param pat The pattern to bind to
    @param cto Optional type annotation for the binding
    @param e1 The expression to bind
    @param e2 The expression in the scope of the binding
    @return A pair containing the type of e2 and constraints for the binding *)
and generate_e_bind (env : static_env) (pat : c_pat) (cto : c_type option)
    (e1 : c_expr) (e2 : c_expr) : (mono_type * type_equations) type_check_result
    =
  let t_pat, pat_env, pat_constraints = type_of_pat pat in
  let* t1, c1 = generate env e1 in
  let annotation_constraints =
    match cto with
    | Some t -> [ (t_pat, instantiate t) ]
    | None -> []
  in
  let new_constraint = (t_pat, t1) in
  (* Generalize the type of e1 before using it in e2 *)
  let* generalized_type = generalize (new_constraint :: c1) env t1 in
  let* t2, c2 =
    generate ((fst (List.hd pat_env), generalized_type) :: env) e2
  in
  return
    (t2, pat_constraints @ annotation_constraints @ (new_constraint :: c1) @ c2)

(** [generate_e_bind_rec env pat e1 e2] generates type constraints for recursive
    let bindings.
    @param env The static environment
    @param pat The pattern to bind to (must be an identifier)
    @param e1 The expression to bind
    @param e2 The expression in the scope of the binding
    @return
      A pair containing the type of e2 and constraints for the recursive binding
*)
and generate_e_bind_rec (env : static_env) (pat : c_pat) (e1 : c_expr)
    (e2 : c_expr) : (mono_type * type_equations) type_check_result =
  let function_id =
    match pat with
    | CIdPat id -> id
    | _ -> failwith "not a valid pattern in new_typecheck.ml"
  in
  let function_type = fresh_type_var () in
  let new_env = (function_id, Mono function_type) :: env in
  let* t1, c1 = generate new_env e1 in
  (* Add constraint that function_type must equal t1 *)
  let new_constraint = (function_type, t1) in
  (* Generalize the function type to make it polymorphic *)
  let* generalized_type = generalize (new_constraint :: c1) new_env t1 in
  let* t2, c2 = generate ((function_id, generalized_type) :: env) e2 in
  return (t2, (new_constraint :: c1) @ c2)

(** [generate_e_ternary env e1 e2 e3] generates type constraints for ternary
    expressions.
    @param env The static environment
    @param e1 The condition expression
    @param e2 The then expression
    @param e3 The else expression
    @return A pair containing the result type and constraints for the ternary *)
and generate_e_ternary (env : static_env) (e1 : c_expr) (e2 : c_expr)
    (e3 : c_expr) : (mono_type * type_equations) type_check_result =
  let* t1, c1 = generate env e1 in
  let* t2, c2 = generate env e2 in
  let* t3, c3 = generate env e3 in
  let type_of_expression = fresh_type_var () in
  return
    ( type_of_expression,
      (t1, BoolType) :: (t2, type_of_expression) :: (t3, type_of_expression)
      :: c1
      @ c2 @ c3 )

(** [generate_e_vector env expressions] generates type constraints for vector
    expressions.
    @param env The static environment
    @param expressions The list of expressions in the vector
    @return A pair containing the vector type and constraints for the vector *)
and generate_e_vector (env : static_env) (expressions : c_expr list) :
    (mono_type * type_equations) type_check_result =
  let* results =
    let rec aux acc_types acc_constraints = function
      | [] -> return (List.rev acc_types, List.rev acc_constraints)
      | e :: es ->
          let* t, c = generate env e in
          aux (t :: acc_types) (c :: acc_constraints) es
    in
    aux [] [] expressions
  in
  let list_of_types, list_of_lists_of_constraints = results in
  return (VectorType list_of_types, List.flatten list_of_lists_of_constraints)

(** [generate_e_list_enumeration env e1 e2] generates type constraints for list
    enumeration.
    @param env The static environment
    @param e1 The start expression
    @param e2 The end expression
    @return A pair containing the list type and constraints for the enumeration
*)
and generate_e_list_enumeration (env : static_env) (e1 : c_expr) (e2 : c_expr) :
    (mono_type * type_equations) type_check_result =
  let* t1, c1 = generate env e1 in
  let* t2, c2 = generate env e2 in
  (* Enumerations can only be done with integers *)
  return (CListType IntType, ((t1, IntType) :: (t2, IntType) :: c1) @ c2)

(** [generate_e_list_comprehension env e generators] generates type constraints
    for list comprehensions.
    @param env The static environment
    @param e The expression to generate list elements from
    @param generators The list of pattern-expression pairs for generators
    @return
      A pair containing the list type and constraints for the comprehension *)
and generate_e_list_comprehension (env : static_env) (e : c_expr)
    (generators : (c_pat * c_expr) list) :
    (mono_type * type_equations) type_check_result =
  let* env, generator_constraints =
    let rec aux acc_env acc_constraints = function
      | [] -> return (acc_env, acc_constraints)
      | (p, e) :: rest ->
          let type_of_pattern, pattern_env, const = type_of_pat p in
          let* type_of_expression, expression_constraints =
            generate (pattern_env @ acc_env) e
          in
          let new_constraint =
            (type_of_expression, CListType type_of_pattern)
          in
          let* new_env, new_constraints =
            aux (pattern_env @ acc_env)
              ((new_constraint :: const) @ expression_constraints
             @ acc_constraints)
              rest
          in
          return (new_env, new_constraints)
    in
    aux env [] generators
  in

  let* type_of_expression, expression_constraints = generate env e in

  return
    ( CListType type_of_expression,
      expression_constraints @ generator_constraints )

(** [generate_e_switch env e1 branches] generates type constraints for switch
    expressions.
    @param env The static environment
    @param e1 The expression to switch on
    @param branches The list of pattern-expression pairs for each branch
    @return A pair containing the result type and constraints for the switch *)
and generate_e_switch (env : static_env) (e1 : c_expr)
    (branches : (c_pat * c_expr) list) :
    (mono_type * type_equations) type_check_result =
  let* t1, c1 = generate env e1 in
  let type_that_all_branch_expressions_must_be = fresh_type_var () in
  let* branch_constraints =
    let rec aux acc_constraints = function
      | [] -> return acc_constraints
      | (pat, expr) :: rest ->
          let type_of_pattern, pattern_env, const = type_of_pat pat in
          let* type_of_branch_expression, branch_expression_constraints =
            generate (pattern_env @ env) expr
          in
          let new_constraint =
            (type_of_pattern, t1)
            :: ( type_of_branch_expression,
                 type_that_all_branch_expressions_must_be )
            :: const
            @ branch_expression_constraints
          in
          let* new_constraints = aux (new_constraint :: acc_constraints) rest in
          return new_constraints
    in
    aux [] branches
  in
  return
    ( type_that_all_branch_expressions_must_be,
      c1 @ List.flatten branch_constraints )

and type_of_pat (pat : c_pat) : mono_type * static_env * type_equations =
  match pat with
  | CIdPat id ->
      let new_var = fresh_type_var () in
      (new_var, [ (id, Mono new_var) ], [])
  | CUnitPat -> (UnitType, [], [])
  | CWildcardPat -> (fresh_type_var (), [], [])
  | CVectorPat patterns ->
      let types, envs, eqs = split3 (List.map type_of_pat patterns) in
      (VectorType types, List.flatten envs, List.flatten eqs)
  | CIntPat _ -> (IntType, [], [])
  | CBoolPat _ -> (BoolType, [], [])
  | CStringPat _ -> (StringType, [], [])
  | CNilPat -> (CListType (fresh_type_var ()), [], [])
  | CConsPat (p1, p2) ->
      let t1, env1, c1 = type_of_pat p1 in
      let t2, env2, c2 = type_of_pat p2 in
      (* [t1] = t2 *)
      (CListType t1, env1 @ env2, (CListType t1, t2) :: (c1 @ c2))

and reduce_eq (c : type_equations) : type_equations =
  match c with
  | [] -> []
  | (t1, t2) :: c' -> (
      if t1 = t2 then reduce_eq c'
      else
        match (t1, t2) with
        | TypeVar id, _ when not (inside t1 t2) ->
            (t1, t2) :: reduce_eq (substitute id t2 c')
        | _, TypeVar _ -> reduce_eq ((t2, t1) :: c')
        | FunctionType (i1, o1), FunctionType (i2, o2) ->
            reduce_eq ((i1, i2) :: (o1, o2) :: c')
        | CListType et1, CListType et2 -> reduce_eq ((et1, et2) :: c')
        | VectorType types1, VectorType types2 -> (
            match (types1, types2) with
            | type1 :: tail1, type2 :: tail2 ->
                reduce_eq
                  ((type1, type2) :: (VectorType tail1, VectorType tail2) :: c')
            | _ -> raise TypeFailure)
        | _ -> raise TypeFailure)

(** [get_type var subs] applies a substitution to a type variable.

    Given a type variable and a list of type equations (substitutions), returns
    the type that the variable should be substituted with. If no substitution
    exists, returns the original variable.

    @param var The type variable to look up
    @param subs The list of type equations representing substitutions
    @return The type that the variable should be substituted with *)
and get_type (var : mono_type) (subs : type_equations) : mono_type =
  match var with
  | TypeVar _ -> (
      let looked_up_type = get_type_of_type_var_if_possible var subs in
      match looked_up_type with
      | FunctionType (i, o) -> FunctionType (get_type i subs, get_type o subs)
      | CListType et -> CListType (get_type et subs)
      | VectorType types ->
          VectorType (List.map (fun t -> get_type t subs) types)
      | _ -> looked_up_type)
  | FunctionType (i, o) -> FunctionType (get_type i subs, get_type o subs)
  | VectorType types -> VectorType (List.map (fun t -> get_type t subs) types)
  | IntType -> IntType
  | FloatType -> FloatType
  | BoolType -> BoolType
  | StringType -> StringType
  | UnitType -> UnitType
  | CListType et -> CListType (get_type et subs)

and get_type_of_type_var_if_possible (var : mono_type) (subs : type_equations) :
    mono_type =
  match var with
  | TypeVar _ -> (
      try
        let looked_up = List.assoc var subs in
        match looked_up with
        | TypeVar _ -> get_type_of_type_var_if_possible looked_up subs
        | _ -> looked_up
      with Not_found -> var)
  | _ -> failwith "not a type var"

(** [inside inside_type outside_type] checks if a type appears inside another
    type.

    For example, [inside (TypeVar "a") (FunctionType (TypeVar "a", IntType))]
    returns true.

    @param inside_type The type to look for
    @param outside_type The type to search in
    @return
      true if inside_type appears anywhere in outside_type, false otherwise *)
and inside (inside_type : mono_type) (outside_type : mono_type) : bool =
  match outside_type with
  | _ when inside_type = outside_type -> true
  | FunctionType (i, o) -> inside inside_type i || inside inside_type o
  | VectorType ts -> List.exists (inside inside_type) ts
  | CListType t -> inside inside_type t
  | _ -> false

(** [is_basic_type t] checks if a type is a basic type (int, bool, string,
    unit).

    @param t The type to check
    @return true if t is a basic type, false otherwise *)
and is_basic_type (t : mono_type) : bool =
  match t with
  | IntType | FloatType | BoolType | StringType | UnitType -> true
  | TypeVar _ -> false
  | FunctionType (i, o) -> is_basic_type i && is_basic_type o
  | VectorType types -> List.for_all is_basic_type types
  | CListType et -> is_basic_type et

(** [substitute var_id t equations] substitutes a type variable with a type
    throughout a list of type equations.

    For each equation (t1, t2) in the input list: 1. Substitute var_id with t in
    t1 2. Substitute var_id with t in t2 3. Create new equation with substituted
    types 4. Recursively substitute through remaining equations

    @param var_id The ID of the type variable to substitute
    @param t The type to substitute in place of the type variable
    @param equations The list of type equations to perform substitution on
    @return
      A new list of type equations with all occurrences of TypeVar(var_id)
      replaced with type t *)
and substitute (var_id : string) (t : mono_type) (equations : type_equations) :
    type_equations =
  let rec substitute_in_type (type_subbing_in : mono_type) : mono_type =
    match type_subbing_in with
    | IntType -> IntType
    | FloatType -> FloatType
    | BoolType -> BoolType
    | StringType -> StringType
    | UnitType -> UnitType
    | TypeVar id -> if id = var_id then t else TypeVar id
    | FunctionType (t1, t2) ->
        FunctionType (substitute_in_type t1, substitute_in_type t2)
    | VectorType types -> VectorType (List.map substitute_in_type types)
    | CListType et -> CListType (substitute_in_type et)
  in
  match equations with
  | [] -> []
  | (t1, t2) :: equations' ->
      (substitute_in_type t1, substitute_in_type t2)
      :: substitute var_id t equations'

(** [instantiate t] converts a polymorphic type to a monomorphic type.

    Given a polymorphic type (c_type), returns a monomorphic type by replacing
    all type variables with fresh type variables.

    For example, instantiating (∀x.x -> x) gives (t1 -> t1) where t1 is fresh.

    @param t The polymorphic type to instantiate
    @return A monomorphic type with fresh type variables *)
and instantiate (t : c_type) : mono_type =
  match t with
  | Mono t -> t (* if we have a monomorphic type, we can just return it*)
  | PolyType _ ->
      (* if we have a polymorphic type, reduce by one layer, then call
         instantiate again *)
      let fresh_var = fresh_type_var () in
      let applied_once = apply_type t fresh_var in
      (* call instantiate again on applied_once *)
      instantiate applied_once

(** [generalize constraints env t] converts a monomorphic type to a polymorphic
    type.

    Given a monomorphic type, its constraints, and the current environment,
    returns a polymorphic type by quantifying over all type variables that are
    not free in the environment.

    For example, if t is (t1 -> t2) and t1 is free in the environment but t2 is
    not, the result would be (∀t2.t1 -> t2).

    @param constraints The type equations that must be satisfied
    @param env The current static environment
    @param t The monomorphic type to generalize
    @return A polymorphic type with appropriate universal quantifiers *)
and generalize (constraints : type_equations) (env : static_env) (t : mono_type)
    : c_type type_check_result =
  (* First reduce the constraints to get a solution *)
  let solution = reduce_eq constraints in

  (* Apply the solution to the type *)
  let u1 = get_type t solution in

  (* Get all type variables in the type *)
  let type_vars = get_type_vars u1 in

  (* Get all types from the environment *)
  let env_types = List.map snd env in
  (* TODO: this code is kind of sus, but I think it works *)
  let env_types = List.map instantiate env_types in
  let env_types = flatten_env_types env_types in

  (* Filter out type variables that appear in the environment *)
  let free_vars =
    List.filter (fun t -> not (List.mem t env_types)) type_vars
    |> List.map (function
         | TypeVar v -> v
         | _ -> failwith "not a type var")
    |> List.sort_uniq compare
  in

  (* Create a polymorphic type by quantifying over free variables *)
  let res =
    List.fold_right (fun var acc -> PolyType (var, acc)) free_vars (Mono u1)
  in
  return res

and flatten_env_types (types : mono_type list) : mono_type list =
  match types with
  | [] -> []
  | t :: tail -> (
      match t with
      | FunctionType (i, o) -> flatten_env_types (i :: o :: tail)
      | VectorType types -> flatten_env_types (types @ tail)
      | CListType et -> flatten_env_types (et :: tail)
      | _ -> t :: flatten_env_types tail)

(** [get_type_vars t] extracts all type variables from a type.

    @param t The type to extract variables from
    @return A list of all type variables appearing in t *)
and get_type_vars (t : mono_type) : mono_type list =
  match t with
  | TypeVar _ -> [ t ]
  | FunctionType (i, o) -> get_type_vars i @ get_type_vars o
  | VectorType types -> List.flatten (List.map get_type_vars types)
  | CListType et -> get_type_vars et
  | _ -> []

and type_of_c_expr (env : static_env) (e : c_expr) : c_type type_check_result =
  let* t, constraints = generate env e in
  let solution = reduce_eq constraints in
  let the_mono_type = get_type t solution in
  let the_mono_type = fix_type the_mono_type in
  let* the_c_type = generalize constraints env the_mono_type in
  return the_c_type

(* swap all variables for new variables *)
and swap_all_variables_in_type (t : mono_type) : mono_type type_check_result =
  (* First generalize the type to quantify over all variables *)
  let* generalized = generalize [] [] t in
  (* Then instantiate it to get fresh variables *)
  let instantiated = instantiate generalized in
  return instantiated

let rec get_mono_type (t : c_type) : mono_type =
  match t with
  | Mono t -> t
  | PolyType (_, t) -> get_mono_type t
