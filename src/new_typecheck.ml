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
  ignore env;
  ignore e;
  failwith "not implemented: generate"

(** [get_type var subs] applies a substitution to a type variable.

    Given a type variable and a list of type equations (substitutions), returns
    the type that the variable should be substituted with. If no substitution
    exists, returns the original variable.

    @param var The type variable to look up
    @param subs The list of type equations representing substitutions
    @return The type that the variable should be substituted with *)
and get_type (var : mono_type) (subs : type_equations) : mono_type =
  ignore var;
  ignore subs;
  failwith "not implemented: get_type"

(** [inside inside_type outside_type] checks if a type appears inside another
    type.

    For example, [inside (TypeVar "a") (FunctionType (TypeVar "a", IntType))]
    returns true.

    @param inside_type The type to look for
    @param outside_type The type to search in
    @return
      true if inside_type appears anywhere in outside_type, false otherwise *)
and inside (inside_type : mono_type) (outside_type : mono_type) : bool =
  ignore inside_type;
  ignore outside_type;
  failwith "not implemented: inside"

(** [is_basic_type t] checks if a type is a basic type (int, bool, string,
    unit).

    @param t The type to check
    @return true if t is a basic type, false otherwise *)
and is_basic_type (t : mono_type) : bool =
  ignore t;
  failwith "not implemented: is_basic_type"

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
and substitute (var_id : int) (t : mono_type) (equations : type_equations) :
    type_equations =
  ignore var_id;
  ignore t;
  ignore equations;
  failwith "not implemented: substitute"

(** [instantiate t] converts a polymorphic type to a monomorphic type.

    Given a polymorphic type (c_type), returns a monomorphic type by replacing
    all type variables with fresh type variables.

    @param t The polymorphic type to instantiate
    @return A monomorphic type with fresh type variables *)
and instantiate (t : c_type) : mono_type =
  ignore t;
  failwith "not implemented: instantiate"

(** [generalize constraints env t] converts a monomorphic type to a polymorphic
    type.

    Given a monomorphic type, its constraints, and the current environment,
    returns a polymorphic type by quantifying over all type variables that are
    not free in the environment.

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
