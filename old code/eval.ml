open Parse
open Expr
open Types
open Lex

type value =
  | IntegerValue of int
  | StringValue of string
  | BooleanValue of bool
  | NothingValue
  | FunctionClosure of env * pat * compound_type option * expr

and env = (string * value) list

type runtime_type =
  | RT_Int
  | RT_String
  | RT_Boolean
  | RT_Nothing
  | RT_Function of runtime_type * runtime_type

let rec string_of_runtime_type = function
  | RT_Int -> "int"
  | RT_String -> "string"
  | RT_Boolean -> "boolean"
  | RT_Nothing -> "()"
  | RT_Function (rt1, rt2) ->
      "(" ^ string_of_runtime_type rt1 ^ " -> " ^ string_of_runtime_type rt2
      ^ ")"

exception InvalidArgument of compound_type * compound_type
exception UnboundIdentifier of string
exception TernaryExpectedBoolean
exception PlusInvalidArguments
exception MinusInvalidArguments
exception MulInvalidArguments
exception DivInvalidArguments
exception ModInvalidArguments
exception OppositeExpectedInteger

let get_value (name : string) (env : env) : value option =
  try Some (List.assoc name env) with Not_found -> None

let string_of_value : value -> string = function
  | IntegerValue n -> string_of_int n
  | StringValue s -> "\"" ^ s ^ "\""
  | BooleanValue b -> string_of_bool b
  | NothingValue -> "()"
  | FunctionClosure _ -> "<function closure>"

let rec string_of_env : env -> string = function
  | [] -> ""
  | (k, v) :: t -> "(" ^ k ^ ", " ^ string_of_value v ^ ")" ^ string_of_env t

let bind_pat (p : pat) (v : value) : env option =
  match (p, v) with
  | NothingPat, NothingValue -> Some []
  | IdPat s, _ -> Some [ (s, v) ]
  | _ -> None (* no pattern matched *)

let rec eval_expr (e : expr) (env : env) =
  match e with
  | DisjunctionExpr d -> eval_disjunction d env
  | Ternary (e1, e2, e3) -> (
      let v1 : value = eval_expr e1 env in
      match v1 with
      | BooleanValue true -> eval_expr e2 env
      | BooleanValue false -> eval_expr e3 env
      | _ -> raise TernaryExpectedBoolean)
  | Function (p, cto, e) -> FunctionClosure (env, p, cto, e)

and eval_disjunction (d : disjunction) (env : env) =
  match d with
  | Disjunction (c, d) -> (
      let v1 : value = eval_conjunction c env in
      match v1 with
      | BooleanValue true -> BooleanValue true
      | _ -> eval_disjunction d env)
  | ConjunctionUnderDisjunction c -> eval_conjunction c env

and eval_conjunction (c : conjunction) (env : env) =
  match c with
  | Conjunction (ee, c) -> (
      let v1 : value = eval_eq_expr ee env in
      match v1 with
      | BooleanValue false -> BooleanValue false
      | _ -> eval_conjunction c env)
  | EqualityUnderConjunction ee -> eval_eq_expr ee env

and eval_eq_expr (ee : eq_expr) (env : env) =
  match ee with
  | Equality (op, re, ee) -> (
      let v1 : value = eval_rel_expr re env in
      let v2 : value = eval_eq_expr ee env in
      match (op, v1, v2) with
      | EQ, IntegerValue a, IntegerValue b -> BooleanValue (a = b)
      | NE, IntegerValue a, IntegerValue b -> BooleanValue (a <> b)
      | _ -> failwith "unimplemented eval_eq_expr" [@coverage off])
  | RelationUnderEqExpr r -> eval_rel_expr r env

and eval_rel_expr (re : rel_expr) (env : env) =
  match re with
  | Relation (op, ae, re) -> (
      let v1 : value = eval_arith_expr ae env in
      let v2 : value = eval_rel_expr re env in
      match (op, v1, v2) with
      | LT, IntegerValue a, IntegerValue b -> BooleanValue (a < b)
      | GT, IntegerValue a, IntegerValue b -> BooleanValue (a > b)
      | LE, IntegerValue a, IntegerValue b -> BooleanValue (a <= b)
      | GE, IntegerValue a, IntegerValue b -> BooleanValue (a >= b)
      | _ -> failwith "unimplemented eval_rel_expr" [@coverage off])
  | ArithmeticUnderRelExpr ae -> eval_arith_expr ae env

and eval_arith_expr (ae : arith_expr) (env : env) =
  match ae with
  | Term t -> eval_term t env
  | Plus (t, ae) -> (
      let v1 : value = eval_term t env in
      let v2 : value = eval_arith_expr ae env in
      match (v1, v2) with
      | IntegerValue a, IntegerValue b -> IntegerValue (a + b)
      | _ -> raise PlusInvalidArguments)
  | Minus (t, ae) -> (
      let v1 : value = eval_term t env in
      let v2 : value = eval_arith_expr ae env in
      match (v1, v2) with
      | IntegerValue a, IntegerValue b -> IntegerValue (a - b)
      | _ -> raise MinusInvalidArguments)

and eval_term (t : term) (env : env) =
  match t with
  | Factor f -> eval_factor f env
  | Mul (f, t) -> (
      let v1 : value = eval_factor f env in
      let v2 : value = eval_term t env in
      match (v1, v2) with
      | IntegerValue a, IntegerValue b -> IntegerValue (a * b)
      | _ -> raise MulInvalidArguments)
  | Div (f, t) -> (
      let v1 : value = eval_factor f env in
      let v2 : value = eval_term t env in
      match (v1, v2) with
      | IntegerValue a, IntegerValue b -> IntegerValue (a / b)
      | _ -> raise DivInvalidArguments)
  | Mod (f, t) -> (
      let v1 : value = eval_factor f env in
      let v2 : value = eval_term t env in
      match (v1, v2) with
      | IntegerValue a, IntegerValue b -> IntegerValue (a mod b)
      | _ -> raise ModInvalidArguments)

and eval_factor (f : factor) (env : env) =
  match f with
  | Boolean b -> BooleanValue b
  | String s -> StringValue s
  | Id s -> (
      match get_value s env with
      | None -> raise (UnboundIdentifier s)
      | Some v -> v)
  | Nothing -> NothingValue
  | Integer n -> IntegerValue n
  | ParenFactor e -> eval_expr e env
  | Opposite f -> (
      match eval_factor f env with
      | IntegerValue n -> IntegerValue (0 - n)
      | _ -> raise OppositeExpectedInteger)
  | App (f1, f2) -> (
      let v1 : value = eval_factor f1 env in
      (* v1 needs to be a function closure *)
      match v1 with
      | FunctionClosure (env_closure, p, _, body) -> (
          (* evaluate the argument to a value *)
          let v2 : value = eval_factor f2 env in
          (* create the binding *)
          let new_bindings_option : env option = bind_pat p v2 in
          match new_bindings_option with
          | None ->
              failwith "no bindings produced in function closure eval_factor"
              [@coverage off]
          | Some b_lst ->
              (* add the new bindings to the environment *)
              let new_env : env = b_lst @ env_closure in
              (* then, evaluate the body with the new binding *)
              eval_expr body new_env)
      | _ -> failwith "function closure expected in eval_factor" [@coverage off]
      )

let eval_empty_env (s : string) : value =
  eval_expr (s |> list_of_string |> lex |> parse_expr |> fst) []

let not_function : value =
  eval_empty_env {|
lam a [boolean] -> 
  if a then false else true
|}

let initial_env : env = [ ("not", not_function) ]

let eval (s : string) : string =
  eval_expr (s |> list_of_string |> lex |> parse_expr |> fst) initial_env
  |> string_of_value
