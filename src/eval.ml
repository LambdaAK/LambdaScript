open Expr

let () = ignore NothingPat

type value =
  | IntegerValue of int
  | StringValue of string
  | BooleanValue of bool
  | NothingValue
  | FunctionClosure of env * pat * compound_type * expr

and env = (string * value) list


let string_of_value: value -> string =
  function
  | IntegerValue n -> string_of_int n
  | StringValue s -> s
  | BooleanValue b -> string_of_bool b
  | NothingValue -> "()"
  | FunctionClosure _ -> "<function closure>"

let rec eval_expr: expr -> value =
  function
  | DisjunctionExpr d -> eval_disjunction d
  | Ternary (e1, e2, e3) ->
    let v1: value = eval_expr e1 in
    (
      match v1 with
      | BooleanValue true -> eval_expr e2
      | BooleanValue false -> eval_expr e3
      | _ -> failwith "unimplemented ternary eval_expr"
    )

  | _ -> failwith "unimplemented eval_expr"


and eval_disjunction: disjunction -> value =
  function
  | Disjunction (c, d) ->
    let v1: value = eval_conjunction c in
    (
    match v1 with
    | BooleanValue true -> BooleanValue true
    | _ -> eval_disjunction d
    )
  | ConjunctionUnderDisjunction c -> eval_conjunction c
      


and eval_conjunction: conjunction -> value =
  function
  | Conjunction (ee, c) ->
    let v1: value = eval_eq_expr ee in
    (
    match v1 with
    | BooleanValue false -> BooleanValue false
    | _ -> eval_conjunction c
    )
  | EqualityUnderConjunction ee ->
    eval_eq_expr ee

and eval_eq_expr: eq_expr -> value =
  function
  | Equality (op, re, ee) ->
    let v1: value = eval_rel_expr re in
    let v2: value = eval_eq_expr ee in
    (
    match op, v1, v2 with
    | EQ, IntegerValue a, IntegerValue b -> BooleanValue (a = b)
    | NE, IntegerValue a, IntegerValue b -> BooleanValue (a <> b)
    | _ -> failwith "unimplemented eval_eq_expr"
    )
  | RelationUnderEqExpr r -> eval_rel_expr r

and eval_rel_expr: rel_expr -> value =
  function
  | Relation (op, ae, re) ->
    let v1: value = eval_arith_expr ae in
    let v2: value = eval_rel_expr re in
    (
    match op, v1, v2 with
    | LT, IntegerValue a, IntegerValue b -> BooleanValue (a < b)
    | GT, IntegerValue a, IntegerValue b -> BooleanValue (a > b)
    | LE, IntegerValue a, IntegerValue b -> BooleanValue (a <= b)
    | GE, IntegerValue a, IntegerValue b -> BooleanValue (a >= b)
    | _ -> failwith "unimplemented eval_rel_expr"
    )
  | ArithmeticUnderRelExpr ae -> eval_arith_expr ae

and eval_arith_expr: arith_expr -> value =
  function
  | Term t -> eval_term t
  | Plus (t, ae) ->
    let v1: value = eval_term t in
    let v2: value = eval_arith_expr ae in
    (
      match v1, v2 with
      | IntegerValue a, IntegerValue b -> IntegerValue (a + b)
      | _ -> failwith "plus implemented in eval_arith_expr"
    )
  | Minus (t, ae) ->
    let v1: value = eval_term t in
    let v2: value = eval_arith_expr ae in
    (
      match v1, v2 with
      | IntegerValue a, IntegerValue b -> IntegerValue (a - b)
      | _ -> failwith "minus unimplemented in eval_arith_expr"
    )

and eval_term: term -> value =
  function
  | Factor f -> eval_factor f
  | Mul (f, t) ->
    let v1: value = eval_factor f in
    let v2: value = eval_term t in
    (
    match v1, v2 with
    | IntegerValue a, IntegerValue b -> IntegerValue (a * b)
    | _ -> failwith "mul unimplemented in eval_term"
    )
  | Div (f, t) ->
    let v1: value = eval_factor f in
    let v2: value = eval_term t in
    (
    match v1, v2 with
    | IntegerValue a, IntegerValue b -> IntegerValue (a / b)
    | _ -> failwith "mul unimplemented in eval_term"
    )

  | Mod (f, t) ->
    let v1: value = eval_factor f in
    let v2: value = eval_term t in
    (
    match v1, v2 with
    | IntegerValue a, IntegerValue b -> IntegerValue (a mod b)
    | _ -> failwith "mul unimplemented in eval_term"
    )


and eval_factor: factor -> value =
  function
  | Boolean b -> BooleanValue b
  | String s -> StringValue s
  | Nothing -> NothingValue
  | Integer n -> IntegerValue n
  | ParenFactor e -> eval_expr e
  | Opposite f ->
    (
    match eval_factor f with
    | IntegerValue n -> IntegerValue (0 - n)
    | _ -> failwith "opposite unimplemented in eval_factor"
    )
  | _ -> failwith "unimplemented eval_factor"
