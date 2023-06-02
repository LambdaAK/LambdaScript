open Cexpr
open Expr
open Lex
open Parse
open Condense

type value =
  | IntegerValue of int
  | StringValue of string
  | BooleanValue of bool
  | NothingValue
  | FunctionClosure of env * pat * c_type option * c_expr

and env = (string * value) list


let string_of_value = function
  | IntegerValue i -> string_of_int i
  | StringValue s -> "\"" ^ s ^ "\""
  | BooleanValue b -> string_of_bool b
  | NothingValue -> "()"
  | FunctionClosure _ -> "function"


let bind_pat (p: pat) (v: value): env option =
  match p, v with
  | NothingPat, NothingValue -> Some []
  | IdPat s, _ -> Some [(s, v)]
  | _ -> None (* no pattern matched *)


let rec eval_c_expr (ce: c_expr) (env: env) =
  match ce with
  | EInt i -> IntegerValue i
  | EString s -> StringValue s
  | EBool b -> BooleanValue b
  | ENothing -> NothingValue
  | EId s -> List.assoc s env
  | EBop (op, e1, e2) -> eval_bop op e1 e2 env
  | EFunction (p, _, e) -> FunctionClosure (env, p, None, e)
  | ETernary (e1, e2, e3) ->
    let v1: value = eval_c_expr e1 env in
    (
      match v1 with
      | BooleanValue true -> eval_c_expr e2 env
      | BooleanValue false -> eval_c_expr e3 env
      | _ -> failwith "eval_c_expr: ETernary"
    )
  | EApp (e1, e2) ->
    let v1: value = eval_c_expr e1 env in
    let v2: value = eval_c_expr e2 env in
    (
      match v1 with
      | FunctionClosure (env', p, _, e) ->
        (
          match bind_pat p v2 with
          | Some env'' -> eval_c_expr e (env'' @ env')
          | None -> failwith "eval_c_expr: EApp"
        )
      | _ -> failwith "eval_c_expr: EApp"
    )
  


and eval_bop (op: c_bop) (e1: c_expr) (e2: c_expr) (env: env) =

  match op with
  | CAnd ->
    let v1: value = eval_c_expr e1 env in
    (
      match v1 with
      | BooleanValue false -> BooleanValue false
      | BooleanValue true -> eval_c_expr e2 env
      | _ -> failwith "eval_bop: CAnd"
    )
  | COr ->
    let v1: value = eval_c_expr e1 env in
    (
      match v1 with
      | BooleanValue true -> BooleanValue true
      | BooleanValue false -> eval_c_expr e2 env
      | _ -> failwith "eval_bop: COr"
    )
  | _ ->


  let v1: value = eval_c_expr e1 env in
  let v2: value = eval_c_expr e2 env in
  match op, v1, v2 with
  | CPlus, IntegerValue a, IntegerValue b -> IntegerValue (a + b)
  | CMinus, IntegerValue a, IntegerValue b -> IntegerValue (a - b)
  | CMul, IntegerValue a, IntegerValue b -> IntegerValue (a * b)
  | CDiv, IntegerValue a, IntegerValue b -> IntegerValue (a / b)
  | CMod, IntegerValue a, IntegerValue b -> IntegerValue (a mod b)
  | CEQ, IntegerValue a, IntegerValue b -> BooleanValue (a = b)
  | CNE, IntegerValue a, IntegerValue b -> BooleanValue (a <> b)
  | CLT, IntegerValue a, IntegerValue b -> BooleanValue (a < b)
  | CLE, IntegerValue a, IntegerValue b -> BooleanValue (a <= b)
  | CGT, IntegerValue a, IntegerValue b -> BooleanValue (a > b)
  | CGE, IntegerValue a, IntegerValue b -> BooleanValue (a >= b)
  
  | _ -> failwith "eval_bop unimplemented"


  let eval_c_empty_env (s: string): value =
    eval_c_expr (s |> list_of_string |> lex |> parse_expr |> fst |> condense_expr) []
  
  let not_function: value = eval_c_empty_env {|
  lam a [boolean] -> 
    if a then false else true
  |}
  
  let initial_env: env = [("not", not_function)]
  
  let c_eval (s: string): string = 
    eval_c_expr (s |> list_of_string |> lex |> parse_expr |> fst |> condense_expr) initial_env |> string_of_value