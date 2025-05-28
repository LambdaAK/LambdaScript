open Lex
open Condense
open Cexpr
open Env
open Parser.ExprParser

let rec string_of_env (env : env) =
  List.fold_left
    (fun acc (id, v) -> acc ^ "(" ^ id ^ ", " ^ string_of_value v ^ ") ")
    "" env

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

and bind_pat (p : c_pat) (v : value) : env option =
  match (p, v) with
  | CUnitPat, UnitValue -> Some []
  | CWildcardPat, _ -> Some []
  | CIdPat s, _ -> Some [ (s, v) ]
  | CIntPat i, IntegerValue j -> if i = j then Some [] else None
  | CStringPat s, StringValue t -> if s = t then Some [] else None
  | CBoolPat b, BooleanValue c -> if b = c then Some [] else None
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

(** [bind_static p t] attempts to match the pattern [p] against the type [t] in
    a static (type-level) context. If the pattern matches the type, it returns
    [Some bindings], where [bindings] is a list of (variable name, type) pairs
    for each identifier bound in the pattern. If the pattern does not match the
    type, it returns [None].

    - [CUnitPat, UnitType]: matches the unit pattern to the unit type, returns
      empty bindings.
    - [CWildcardPat, _]: wildcard pattern matches any type, returns empty
      bindings.
    - [CIdPat s, _]: identifier pattern matches any type, binds the identifier
      to the type.
    - [CVectorPat patterns, VectorType types]: recursively matches each pattern
      in the vector to the corresponding type in the vector type. Returns the
      combined bindings if all match, otherwise [None].
    - [_]: any other pattern-type combination does not match, returns [None]. *)

and bind_static (p : c_pat) (t : c_type) : (string * c_type) list option =
  (* Helper to extract the monomorphic type from a c_type, if possible *)
  let rec get_mono_type (t : c_type) : mono_type option =
    match t with
    | Mono m -> Some m
    | PolyType (_, t') -> get_mono_type t'
  in
  match p with
  | CUnitPat -> (
      match get_mono_type t with
      | Some UnitType -> Some []
      | _ -> None)
  | CWildcardPat -> Some []
  | CIdPat s -> Some [ (s, t) ]
  | CVectorPat patterns -> (
      match get_mono_type t with
      | Some (VectorType types) -> (
          match (patterns, types) with
          | [], [] -> Some []
          | p :: pt, t :: tt -> (
              match bind_static p (Mono t) with
              | None -> None
              | Some bindings -> (
                  match bind_static (CVectorPat pt) (Mono (VectorType tt)) with
                  | None -> None
                  | Some bindings' -> Some (bindings @ bindings')))
          | _ -> None)
      | _ -> None)
  | _ -> None

and eval_c_expr (ce : c_expr) (env : env) =
  match ce with
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
  | EBlock parts ->
      let rec eval_block_parts parts env =
        match parts with
        | [] -> UnitValue (* when there are no parts, evaluate to unit *)
        | [ Expr e ] -> eval_c_expr e env
        | Expr e :: t ->
            (* if the last part is an expression, we evaluate to it *)
            let _ = eval_c_expr e env in
            eval_block_parts t env
        | Defn d :: t ->
            (* when the next part is a definition, we run the definition and add
               to the env, then continue evaluating the rest of the block*)
            let new_bindings = eval_defn d env in
            eval_block_parts t (new_bindings @ env)
      in

      eval_block_parts parts env
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
      | _ -> failwith "eval_c_expr: EApp")
  | EBind (pattern, _, e1, e2) ->
      (* We have let p = e1 in e2. We can convert this to (fun p -> e2) e1 and
         evaluate that instead. As far as dynamic semantics go, they are the
         same thing! *)

      (* construct the modified expression *)
      let modified_expr = EApp (EFunction (pattern, None, e2), e1) in
      (* evaluate the modified expression *)
      eval_c_expr modified_expr env
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
          | None -> failwith "no pattern matched"
          | Some recursive_bindings ->
              env_ref := recursive_bindings @ env;
              eval_c_expr e2 (recursive_bindings @ env))
      | _ -> (
          (* evaluate a regular let expression *)
          let new_bindings_option : env option = bind_pat pattern v1_rec in
          match new_bindings_option with
          | None -> failwith "no pattern matched"
          | Some new_bindings -> eval_c_expr e2 (new_bindings @ env)))

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
      | _ ->
          (* print the operator *)
          let op_string : string =
            match op with
            | CPlus -> "+"
            | CMinus -> "-"
            | CMul -> "*"
            | CDiv -> "/"
            | CMod -> "%"
            | CEQ -> "=="
            | CNE -> "!="
            | CLT -> "<"
            | CLE -> "<="
            | CGT -> ">"
            | CGE -> ">="
            | CAnd -> "&&"
            | COr -> "||"
            | CCons -> "::"
          in

          (* print the values *)
          let v1_string : string = string_of_value v1 in
          let v2_string : string = string_of_value v2 in

          print_endline v1_string;
          print_endline v2_string;
          print_endline op_string;

          failwith "eval_bop unimplemented")

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

(* let eval_c_empty_env (s : string) : value = eval_c_expr (s |> list_of_string
   |> lex |> parse_expr |> fst |> condense_expr) [] *)

and eval_c_empty_env (s : string) : value =
  let tokens = s |> list_of_string |> lex in
  let token_types = List.map (fun t -> t.token_type) tokens in
  let parse_result = expr_parser token_types in
  match parse_result with
  | None -> failwith "parsing failed"
  | Some (e, _) ->
      let c_e = condense_expr e in
      eval_c_expr c_e []

and initial_env () : (string * value) list =
  List.map
    (fun (id, code) ->
      let v : value = eval_c_empty_env code in
      (id, v))
    code_mapping
  @ built_ins_values

(* env code *)

and c_eval_ce (ce : c_expr) : string =
  eval_c_expr ce (initial_env ()) |> string_of_value

(* let c_eval (s : string) : string = eval_c_expr (s |> list_of_string |> lex |>
   parse_expr |> fst |> condense_expr) initial_env |> string_of_value *)

and c_eval (s : string) : string =
  let tokens = s |> list_of_string |> lex in
  let token_types = List.map (fun t -> t.token_type) tokens in
  let parse_result = expr_parser token_types in
  match parse_result with
  | None -> failwith "parsing failed"
  | Some (e, _) ->
      let c_e = condense_expr e in
      let result = eval_c_expr c_e (initial_env ()) in
      string_of_value result

and create_generic_type : c_pat -> c_type = function
  | CUnitPat -> Mono UnitType
  | CWildcardPat -> Mono (fresh_type_var ())
  | CIdPat _ -> Mono (fresh_type_var ())
  | CIntPat _ -> Mono IntType
  | CStringPat _ -> Mono StringType
  | CBoolPat _ -> Mono BoolType
  | CNilPat -> Mono (CListType (fresh_type_var ()))
  | CConsPat _ -> Mono (CListType (fresh_type_var ()))
  | CVectorPat patterns ->
      Mono
        (VectorType
           (List.map
              (fun p ->
                match create_generic_type p with
                | Mono t -> t
                | PolyType (_, _) ->
                    failwith
                      "Polymorphic types not supported in create_generic_type \
                       for vectors")
              patterns))

and expr_of_pat : c_pat -> c_expr = function
  | CUnitPat -> EUnit
  | CWildcardPat -> failwith "expr_of_pat: wildcard pattern not allowed"
  | CIdPat s -> EId s
  | CIntPat i -> EInt i
  | CStringPat s -> EString s
  | CBoolPat b -> EBool b
  | CNilPat -> ENil
  | CConsPat (p1, p2) -> EBop (CCons, expr_of_pat p1, expr_of_pat p2)
  | CVectorPat patterns -> EVector (List.map expr_of_pat patterns)

(** [eval_defn d env] takes a definition [d] and an environment [env], executes
    the definition, and returns the new bindings introduced by the definition.
    The caller is responsible for updating the environment with these bindings.
*)
and eval_defn (d : c_defn) (env : env) : env =
  match d with
  | CDefn (pat, _, body) -> (
      (* Evaluate the body in the current environment *)
      let value = eval_c_expr body env in
      (* Try to bind the pattern to the value *)
      match bind_pat pat value with
      | None -> failwith "eval_defn: pattern match failed"
      | Some new_bindings -> new_bindings)
  | CDefnRec (pat, _, body) -> (
      (* For recursive definitions, we need to create a recursive closure *)
      let value = eval_c_expr body env in
      let value_rec =
        match value with
        | FunctionClosure (closure_env, closure_pat, _, closure_body) ->
            RecursiveFunctionClosure
              (ref closure_env, closure_pat, None, closure_body)
        | _ ->
            value (* not a function, so the rec doesn't really mean anything *)
      in
      (* Try to bind the pattern to the recursive value *)
      match bind_pat pat value_rec with
      | None -> failwith "eval_defn: pattern match failed"
      | Some new_bindings ->
          (* If it's a recursive function, backpatch the environment *)
          (match value_rec with
          | RecursiveFunctionClosure (env_ref, _, _, _) ->
              env_ref := new_bindings @ env
          | _ -> ());
          new_bindings)
