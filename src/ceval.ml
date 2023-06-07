open Cexpr
open Expr
open Lex
open Parse
open Condense
open Typecheck

type value =
  | IntegerValue of int
  | StringValue of string
  | BooleanValue of bool
  | NothingValue
  | FunctionClosure of env * pat * c_type option * c_expr
  | RecursiveFunctionClosure of env ref * pat * c_type option * c_expr

and env = (string * value) list


let rec string_of_env (env: env) =
  match env with
  | [] -> ""
  | (id, v) :: t ->
    "(" ^ id ^ ", " ^ (string_of_value v) ^ ")" ^ (string_of_env t)


and string_of_value = function
  | IntegerValue i -> string_of_int i
  | StringValue s -> "\"" ^ s ^ "\""
  | BooleanValue b -> string_of_bool b
  | NothingValue -> "()"
  | FunctionClosure _ -> "function"
  | RecursiveFunctionClosure _ -> "recursive function"


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
      (* recursive function *)
      | RecursiveFunctionClosure (env'_ref, p, _, e) ->
        let env': env = !env'_ref in
        (
          match bind_pat p v2 with
          | Some env'' -> eval_c_expr e (env'' @ env')
          | None -> failwith "eval_c_expr: EApp"
        )
      
      | _ -> failwith "eval_c_expr: EApp"
    )
  | EBindRec (pattern, _, e1, e2) ->
    let v1: value = eval_c_expr e1 env in
    let v1_rec = (
      match v1 with
      | FunctionClosure (closure_env, closure_pat, _, closure_body) ->
        RecursiveFunctionClosure (ref closure_env, closure_pat, None, closure_body)

      | _ -> v1
        (* not a function, so the rec doesn't really mean anything *)
        
    ) in

    (* backpatch *)
    (
      match v1_rec with
      | RecursiveFunctionClosure (env_ref, _, _, _) ->
        let recursive_bindings_option: env option = bind_pat pattern v1_rec in
        (
          match recursive_bindings_option with
          | None -> failwith "no pattern matched"
          | Some recursive_bindings ->
            env_ref := recursive_bindings @ env;
            eval_c_expr e2 (recursive_bindings @ env)
        )
      | _ ->
        (* evaluate a regular let expression *)
        let new_bindings_option: env option = bind_pat pattern v1_rec in
        (
          match new_bindings_option with
          | None -> failwith "no pattern matched"
          | Some new_bindings -> eval_c_expr e2 (new_bindings @ env)
        )
    );

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
  

let c_eval_ce (ce: c_expr): string =
  eval_c_expr ce initial_env |> string_of_value


let c_eval (s: string): string =
  eval_c_expr (s |> list_of_string |> lex |> parse_expr |> fst |> condense_expr) initial_env |> string_of_value

(**
returns a tuple (a, b, c, d)

a is the new dynamic environment
b is the new static environment
c is the type of the expression
d is the value of the expression

*)
let eval_defn (d: c_defn) (env: env) (static_env: static_env): env * static_env * c_type * value =
  match d with
  | CDefn (pattern, _, body_expression) ->
    let v: value = eval_c_expr body_expression env in
    let new_bindings_option: env option = bind_pat pattern v in

    match new_bindings_option with
    | None ->
      failwith "no pattern matched"
    | Some new_bindings ->
      match new_bindings with
      | [] -> failwith "unimplemented eval_defn"
      | _ ->
        (
          match pattern with
          | IdPat id ->
            let new_type: c_type = type_of_c_expr body_expression static_env in
            let new_env: env = new_bindings @ env in
            let new_static_env: static_env = (id, new_type) :: static_env in
            new_env, new_static_env, type_of_c_expr body_expression static_env, v
          | _ -> failwith "impossible"
        )
        
      