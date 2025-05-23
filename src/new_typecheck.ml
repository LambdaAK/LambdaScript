open New_cexpr
open New_c_to_string

type type_equation = mono_type * mono_type
type type_equations = type_equation list

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

and string_of_c_type (t : c_type) : string =
  match t with
  | Mono t -> string_of_mono_type t
  | PolyType (id, t) -> "∀" ^ id ^ "." ^ string_of_c_type t

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
      "[" ^ String.concat ", " types_str ^ "]"
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
let rec generate (env : static_env) (e : c_expr) : mono_type * type_equations =
  (* pattern match on e *)
  match e with
  | EInt _ -> (IntType, [])
  | EFloat _ -> (FloatType, [])
  | EBool _ -> (BoolType, [])
  | EString _ -> (StringType, [])
  | EUnit -> (UnitType, [])
  | ENil -> (CListType (fresh_type_var ()), [])
  | EId x ->
      let uninstantiated : c_type = List.assoc x env in
      let t = instantiate uninstantiated in
      (t, [])
  | EBop (op, e1, e2) ->
      let t1, c1 = generate env e1 in
      let t2, c2 = generate env e2 in
      begin
        match op with
        | CCons ->
            (* e1 :: e2, e2 must be a list of the type of e1 *)
            (t2, ((CListType t1, t2) :: c1) @ c2)
        | CPlus | CMinus | CMul | CDiv | CMod ->
            (* arithmetic: both operands must be int, result is int *)
            (IntType, ((t1, IntType) :: (t2, IntType) :: c1) @ c2)
        | CGE | CGT | CLE | CLT ->
            (* comparisons: both operands must be int, result is bool *)
            (BoolType, ((t1, IntType) :: (t2, IntType) :: c1) @ c2)
        | CEQ | CNE ->
            (* equality/inequality: operands must be same type, result is
               bool *)
            (BoolType, ((t1, t2) :: c1) @ c2)
        | CAnd | COr ->
            (* logical: both operands must be bool, result is bool *)
            (BoolType, ((t1, BoolType) :: (t2, BoolType) :: c1) @ c2)
      end
  | EFunction _ -> failwith "not implemented: generate (EFunction)"
  | EApp _ -> failwith "not implemented: generate (EApp)"
  | EBindRec (pat, _, e1, e2) ->
      (* EBindRec (pat, _, e1, e2): let rec pat = e1 in e2 *)
      let function_id =
        match pat with
        | CIdPat id -> id
        | _ -> failwith "not a valid pattern in new_typecheck.ml"
      in
      let function_type = fresh_type_var () in
      let new_env = (function_id, Mono function_type) :: env in
      let t1, c1 = generate new_env e1 in
      let t2, c2 = generate new_env e2 in
      let new_constraint = (function_type, t1) in
      (t2, (new_constraint :: c1) @ c2)
  | ETernary (e1, e2, e3) ->
      let t1, c1 = generate env e1 in
      let t2, c2 = generate env e2 in
      let t3, c3 = generate env e3 in

      let type_of_expression = fresh_type_var () in

      ( type_of_expression,
        (t1, BoolType) :: (t2, type_of_expression) :: (t3, type_of_expression)
        :: c1
        @ c2 @ c3 )
  | EVector expressions ->
      let list_of_types, list_of_lists_of_constraints =
        List.split (List.map (generate env) expressions)
      in

      (VectorType list_of_types, List.flatten list_of_lists_of_constraints)
  | EListEnumeration (e1, e2) ->
      let t1, c1 = generate env e1 in
      let t2, c2 = generate env e2 in
      (CListType t1, ((CListType t1, t2) :: c1) @ c2)
  | EListComprehension (e, generators) ->
      let env, generator_constraints =
        List.fold_left
          (fun (env, constraints) (p, e) ->
            let type_of_pattern, pattern_env, const = type_of_pat p in
            let type_of_expression, expression_constraints =
              generate (pattern_env @ env) e
            in
            let new_constraint =
              (type_of_expression, CListType type_of_pattern)
            in
            ( pattern_env @ env,
              (new_constraint :: const) @ expression_constraints @ constraints
            ))
          (env, []) generators
      in

      (* generate the type and constraints of the expression *)
      let type_of_expression, expression_constraints = generate env e in

      ( CListType type_of_expression,
        expression_constraints @ generator_constraints )
  | ESwitch (e1, branches) ->
      let t1, c1 = generate env e1 in
      let type_that_all_branch_expressions_must_be = fresh_type_var () in
      let branch_constraints =
        List.map
          (fun (pat, expr) ->
            let type_of_pattern, pattern_env, const = type_of_pat pat in
            let type_of_branch_expression, branch_expression_constraints =
              generate (pattern_env @ env) expr
            in
            (* the type of the pattern must match the type of the switch expr *)
            (* the type of the branch expr must match the result type *)
            (type_of_pattern, t1)
            :: ( type_of_branch_expression,
                 type_that_all_branch_expressions_must_be )
            :: const
            @ branch_expression_constraints)
          branches
        |> List.flatten
      in
      (type_that_all_branch_expressions_must_be, c1 @ branch_constraints)

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
    : c_type =
  ignore constraints;
  ignore env;
  ignore t;
  failwith "not implemented: generalize"

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
  ignore t;
  failwith "not implemented: get_type_vars"

(** [type_of_value v] determines the type of a runtime value.

    @param v The value to determine the type of
    @return The monomorphic type of the value *)
and type_of_value (v : value) : mono_type =
  ignore v;
  failwith "not implemented: type_of_value"

and type_of_c_expr (e : c_expr) : c_type =
  let t, constraints = generate [] e in

  (* print the type and the constraints *)
  print_endline "Type:";
  print_endline (string_of_mono_type t);
  print_endline "Constraints:";
  print_endline (string_of_type_equations constraints);

  let solution = reduce_eq constraints in

  (* print the solution *)
  print_endline "Solution:";
  print_endline (string_of_type_equations solution);

  let the_type = get_type t solution in

  print_endline "The type:";
  print_endline (string_of_mono_type the_type);

  Mono the_type
