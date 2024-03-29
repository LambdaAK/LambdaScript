open Lex
open Parse
open Condense
open Cexpr
open Env

(** Converts an env to a string *)
let rec string_of_env (env : env) =
  List.fold_left
    (fun acc (id, v) -> acc ^ "(" ^ id ^ ", " ^ string_of_value v ^ ") ")
    "" env

(** Converts a value to a string *)
and string_of_value = function
  | IntegerValue i -> string_of_int i
  | FloatValue f -> string_of_float f
  | StringValue s -> "\"" ^ s ^ "\""
  | BooleanValue b -> string_of_bool b
  | UnitValue -> "()"
  | FunctionClosure _ | RecursiveFunctionClosure _ | BuiltInFunction _ ->
      "function"
  | VectorValue values ->
      let values_string : string =
        values |> List.map string_of_value |> String.concat ", "
      in
      "(" ^ values_string ^ ")"
  | ListValue values ->
      let values_string : string =
        values |> List.map string_of_value |> String.concat ", "
      in
      "[" ^ values_string ^ "]"
  | ConstructorValue (id, None) -> id
  | ConstructorValue (id, Some v) -> id ^ " (" ^ string_of_value v ^ ")"

(** [bind_pat p v] returns [Some env] if [p] matches [v] and [env] is the
    bindings produced by binding [p] to [v], or [None] if [p] does not match
    [v]. *)
let rec bind_pat (p : c_pat) (v : value) : env option =
  match (p, v) with
  | CUnitPat, UnitValue -> Some []
  | CWildcardPat, _ -> Some []
  | CIdPat s, _ -> Some [ (s, v) ]
  | CIntPat i, IntegerValue j -> if i = j then Some [] else None
  | CStringPat s, StringValue t -> if s = t then Some [] else None
  | CBoolPat b, BooleanValue c -> if b = c then Some [] else None
  (* nullary constructor pattern matched with nullary constructor value*)
  | CConstructorPat pat_constructor_id, ConstructorValue (value_id, None) ->
      if pat_constructor_id = value_id then Some [] else None
  (* app pat (unary constructor application pattern) matched with a value *)
  | CAppPat (p1, p2), ConstructorValue (value_id, Some v) -> (
      (* use option monad *)
      match bind_pat p1 (ConstructorValue (value_id, None)) with
      | None -> None
      | Some bindings -> (
          match bind_pat p2 v with
          | None -> None
          | Some bindings' -> Some (bindings @ bindings')))
  | CNilPat, ListValue [] -> Some []
  | CConsPat (p1, p2), ListValue (v1 :: v2) -> (
      (* v1 is matched against p1 and v2 is matched against p2 if both match,
         then the bindings from both are returned *)
      match bind_pat p1 v1 with
      | None -> None
      | Some bindings -> (
          match bind_pat p2 (ListValue v2) with
          | None -> None
          | Some bindings' -> Some (bindings @ bindings')))
  | CVectorPat patterns, VectorValue values -> (
      match (patterns, values) with
      | [], [] -> Some []
      | p :: pt, v :: vt -> (
          match bind_pat p v with
          | None -> None
          | Some bindings -> (
              match bind_pat (CVectorPat pt) (VectorValue vt) with
              | None -> None
              | Some bindings' -> Some (bindings @ bindings')))
      | _ -> None)
  | _ -> None

(** [bind_static p t] returns [Some env] if [p] matches [t] and [env] is the
    static bindings produced by binding [p] to [t], or [None] if [p] does not
    match [t]. *)
let rec bind_static (p : c_pat) (t : c_type) : (string * c_type) list option =
  match (p, t) with
  | CUnitPat, UnitType -> Some []
  | CWildcardPat, _ -> Some []
  | CIdPat s, _ -> Some [ (s, t) ]
  | CVectorPat patterns, VectorType types -> (
      match (patterns, types) with
      | [], [] -> Some []
      | p :: pt, t :: tt -> (
          match bind_static p t with
          | None -> None
          | Some bindings -> (
              match bind_static (CVectorPat pt) (VectorType tt) with
              | None -> None
              | Some bindings' -> Some (bindings @ bindings')))
      | _ -> None)
  | _ -> None

(** [eval_c_expr ce env] is the value that [ce] evaluates to in dyanamic
    environment [env] *)
let rec eval_c_expr (ce : c_expr) (env : env) =
  match ce with
  | EConstructor name -> ConstructorValue (name, None)
  | EInt i -> IntegerValue i
  | EFloat f -> FloatValue f
  | EString s -> StringValue s
  | EBool b -> BooleanValue b
  | ENil -> ListValue []
  | EUnit -> UnitValue
  | EId s -> List.assoc s env
  | EBop (op, e1, e2) -> eval_bop op e1 e2 env
  | EFunction (p, _, e) -> FunctionClosure (env, p, None, e)
  | EListEnumeration (e1, e2) -> eval_list_enumeration e1 e2 env
  | EListComprehension (e, generators) ->
      let envs : env list = generate_envs_from_generators generators env in
      let values = List.map (fun en -> eval_c_expr e en) envs in
      ListValue values
  | EVector expressions ->
      (* evalute each sub expression to a value *)
      let transformer e = eval_c_expr e env in
      let values : value list = List.map transformer expressions in
      VectorValue values
  | ESwitch (e, branches) -> (
      let v : value = eval_c_expr e env in
      (* see if v matches any pattern in branches *)
      let rec find_bindings_and_body_if_possible
          (branches : (c_pat * c_expr) list) (v : value) : (env * c_expr) option
          =
        match branches with
        | [] -> None
        | (p, e) :: t -> (
            match bind_pat p v with
            | None -> find_bindings_and_body_if_possible t v
            | Some bindings -> Some (bindings, e))
      in

      match find_bindings_and_body_if_possible branches v with
      | None -> failwith "no pattern matched in switch"
      | Some (bindings, e) -> eval_c_expr e (bindings @ env))
  | ETernary (e1, e2, e3) -> (
      let v1 : value = eval_c_expr e1 env in
      match v1 with
      | BooleanValue true -> eval_c_expr e2 env
      | BooleanValue false -> eval_c_expr e3 env
      | _ -> failwith "eval_c_expr: ETernary")
  | EApp (e1, e2) -> (
      let v1 : value = eval_c_expr e1 env in
      let v2 : value = eval_c_expr e2 env in
      match v1 with
      | BuiltInFunction f -> eval_builtin f v2
      | FunctionClosure (env', p, _, e) -> (
          match bind_pat p v2 with
          | Some env'' -> eval_c_expr e (env'' @ env')
          | None -> failwith "eval_c_expr: EApp")
      (* recursive function *)
      | RecursiveFunctionClosure (env'_ref, p, _, e) -> (
          let env' : env = !env'_ref in
          match bind_pat p v2 with
          | Some env'' -> eval_c_expr e (env'' @ env')
          | None -> failwith "eval_c_expr: EApp")
      | ConstructorValue (id, None) -> ConstructorValue (id, Some v2)
      | _ -> failwith "eval_c_expr: EApp")
  | EBindRec (pattern, _, e1, e2) -> (
      let v1 : value = eval_c_expr e1 env in
      let v1_rec =
        match v1 with
        | FunctionClosure (closure_env, closure_pat, _, closure_body) ->
            RecursiveFunctionClosure
              (ref closure_env, closure_pat, None, closure_body)
        | _ -> v1 (* not a function, so the rec doesn't really mean anything *)
      in

      (* backpatch *)
      match v1_rec with
      | RecursiveFunctionClosure (env_ref, _, _, _) -> (
          let recursive_bindings_option : env option =
            bind_pat pattern v1_rec
          in
          match recursive_bindings_option with
          | None -> failwith "no pattern matched1"
          | Some recursive_bindings ->
              env_ref := recursive_bindings @ env;
              eval_c_expr e2 (recursive_bindings @ env))
      | _ -> (
          (* evaluate a regular let expression *)
          let new_bindings_option : env option = bind_pat pattern v1_rec in
          match new_bindings_option with
          | None -> failwith "no pattern matched2"
          | Some new_bindings -> eval_c_expr e2 (new_bindings @ env)))

(** [eval_builtin f v] is the value that "f v" evaluates to where [f] is a build
    in function and [v] is a value *)
and eval_builtin (f : builtin_function) (v : value) : value =
  match (f, v) with
  | Println, StringValue s ->
      print_endline s;
      UnitValue
  | Print, StringValue s ->
      print_string s;
      UnitValue
  | IntToString, IntegerValue i -> StringValue (string_of_int i)
  | IntToFloat, IntegerValue i -> FloatValue (float_of_int i)
  | FloatToInt, FloatValue f -> IntegerValue (int_of_float f)
  | _ -> failwith "eval_builtin: unimplemented"

(** In order to evaluate generators, we need to generate the list of
    environments under which to evaluate the general term.
    [generate_envs_from_generators generators env] is the list of environments
    generated by evaluating [generators] under [env]. *)
and generate_envs_from_generators generators env =
  match generators with
  | [] -> [ env ]
  | (p, e) :: t -> (
      let v = eval_c_expr e env in
      match v with
      | ListValue values ->
          let bindings =
            List.map
              (fun value ->
                match bind_pat p value with
                | None ->
                    failwith "generate envs from generators: no pattern matched"
                | Some bindings -> bindings)
              values
          in
          let envs = List.map (fun bindings -> bindings @ env) bindings in
          let envs' =
            List.map (fun env -> generate_envs_from_generators t env) envs
          in
          List.flatten envs'
      | _ -> failwith "generate envs from generators: expected a list value")

(** [eval_bop op e1 e2 env] is the value that [e1 op e2] evaluates to in dynamic
    environment [env], where [op] is a binary operator *)
and eval_bop (op : c_bop) (e1 : c_expr) (e2 : c_expr) (env : env) =
  (* these are seperate because they require short circuit evaluation *)
  match op with
  | CAnd -> (
      let v1 : value = eval_c_expr e1 env in
      match v1 with
      | BooleanValue false -> BooleanValue false
      | BooleanValue true -> eval_c_expr e2 env
      | _ -> failwith "eval_bop: CAnd")
  | COr -> (
      let v1 : value = eval_c_expr e1 env in
      match v1 with
      | BooleanValue true -> BooleanValue true
      | BooleanValue false -> eval_c_expr e2 env
      | _ -> failwith "eval_bop: COr")
  | _ -> (
      let v1 : value = eval_c_expr e1 env in
      let v2 : value = eval_c_expr e2 env in
      match (op, v1, v2) with
      | CPlus, IntegerValue a, IntegerValue b -> IntegerValue (a + b)
      | CMinus, IntegerValue a, IntegerValue b -> IntegerValue (a - b)
      | CMul, IntegerValue a, IntegerValue b -> IntegerValue (a * b)
      | CDiv, IntegerValue a, IntegerValue b -> IntegerValue (a / b)
      | CMod, IntegerValue a, IntegerValue b -> IntegerValue (a mod b)
      | CEQ, a, b -> BooleanValue (a = b)
      | CNE, a, b -> BooleanValue (a <> b)
      | CLT, IntegerValue a, IntegerValue b -> BooleanValue (a < b)
      | CLE, IntegerValue a, IntegerValue b -> BooleanValue (a <= b)
      | CGT, IntegerValue a, IntegerValue b -> BooleanValue (a > b)
      | CGE, IntegerValue a, IntegerValue b -> BooleanValue (a >= b)
      | CCons, v, ListValue vs -> ListValue (v :: vs)
      | _ -> failwith "eval_bop unimplemented")

(** [eval_list_enumeration e1 e2 env] is the value that [[e1 ... e2]] evaluates
    to under dynamic environment [env] *)
and eval_list_enumeration e1 e2 env =
  let v1 = eval_c_expr e1 env in
  let v2 = eval_c_expr e2 env in
  match (v1, v2) with
  | IntegerValue a, IntegerValue b ->
      let rec make_list_tr a b acc =
        if a > b then List.rev acc
        else make_list_tr (a + 1) b (IntegerValue a :: acc)
      in
      ListValue (make_list_tr a b [])
  | _ -> failwith "eval_list_enumeration failed"

let eval_c_empty_env (s : string) : value =
  eval_c_expr
    (s |> list_of_string |> lex |> parse_expr |> fst |> condense_expr)
    []

let initial_env : (string * value) list =
  List.map
    (fun (id, code) ->
      let v : value = eval_c_empty_env code in
      (id, v))
    code_mapping
  @ built_ins_values

(* env code *)

let c_eval_ce (ce : c_expr) : string =
  eval_c_expr ce initial_env |> string_of_value

let c_eval (s : string) : string =
  eval_c_expr
    (s |> list_of_string |> lex |> parse_expr |> fst |> condense_expr)
    initial_env
  |> string_of_value

let rec create_generic_type : c_pat -> c_type = function
  | CUnitPat -> UnitType
  | CWildcardPat -> fresh_type_var ()
  | CIdPat _ -> fresh_type_var ()
  | CIntPat _ -> IntType
  | CStringPat _ -> StringType
  | CBoolPat _ -> BoolType
  | CNilPat -> CListType (fresh_type_var ())
  | CConsPat _ -> CListType (fresh_type_var ())
  | CVectorPat patterns ->
      VectorType (List.map (fun p -> create_generic_type p) patterns)
  | _ -> failwith "create_generic_type: unimplemented"

let rec expr_of_pat : c_pat -> c_expr = function
  | CUnitPat -> EUnit
  | CWildcardPat -> failwith "expr_of_pat: wildcard pattern not allowed"
  | CIdPat s -> EId s
  | CIntPat i -> EInt i
  | CStringPat s -> EString s
  | CBoolPat b -> EBool b
  | CNilPat -> ENil
  | CConsPat (p1, p2) -> EBop (CCons, expr_of_pat p1, expr_of_pat p2)
  | CVectorPat patterns -> EVector (List.map expr_of_pat patterns)
  | _ -> failwith "expr_of_pat: unimplemented"
