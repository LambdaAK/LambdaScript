open Lex
open Condense
open Cexpr
open Env
open Parser.ExprParser

(** [eval_error] represents different kinds of errors that can occur during
    expression evaluation.
    - [PatternMatchError (p, v)] occurs when a pattern [p] fails to match a
      value [v]
    - [BinaryOpError (op, v1, v2)] occurs when a binary operation [op] cannot be
      applied to values [v1] and [v2]
    - [UnboundVariable s] occurs when trying to access a variable [s] that is
      not defined in the environment
    - [TypeError s] occurs when there is a type-related error, with description
      [s]
    - [OtherError s] represents any other evaluation error, with description [s]
*)
type eval_error =
  | PatternMatchError of c_pat * value
  | BinaryOpError of c_bop * value * value
  | UnboundVariable of string
  | TypeError of string
  | OtherError of string

(** [eval_result] is a monad that represents the result of an evaluation.
    - [Ok x] represents a successful evaluation with result [x]
    - [Error e] represents a failed evaluation with error [e] *)
type 'a eval_result =
  | Ok of 'a
  | Error of eval_error

(** [return x] lifts a value [x] into the eval_result monad.
    @param x The value to lift
    @return An eval_result containing the value *)
let return (x : 'a) : 'a eval_result = Ok x

(** [>>=] is the bind operation for the eval_result monad. It chains together
    computations that may fail. If the first computation fails, the error is
    propagated. Otherwise, the result is passed to the next computation.

    @param x The first computation
    @param f The function to apply to the result of the first computation
    @return The result of applying f to x's value, or the error if x failed *)
let ( >>= ) (x : 'a eval_result) (f : 'a -> 'b eval_result) : 'b eval_result =
  match x with
  | Ok x -> f x
  | Error e -> Error e

(** [let*] is a syntactic sugar for the bind operation. It allows writing
    monadic code in a more readable style. For example:
    {[
      let* x = expr1 in
      let* y = expr2 in
      return (x + y)
    ]} *)
let ( let* ) = ( >>= )

(** [unwrap_eval_result result] unwraps an eval_result, returning the value if
    successful or failing with an error message if the result is an error.
    @param result The eval_result to unwrap
    @return The unwrapped value
    @raise Failure if the result is an error *)
let unwrap_eval_result (result : 'a eval_result) : 'a =
  match result with
  | Ok x -> x
  | Error _ -> failwith "unwrap_eval_result: error"

(** [string_of_env env] converts an environment to a string representation.
    @param env The environment to convert
    @return A string representation of the environment *)
let rec string_of_env (env : env) =
  List.fold_left
    (fun acc (id, v) -> acc ^ "(" ^ id ^ ", " ^ string_of_value v ^ ") ")
    "" env

(** [string_of_value v] converts a value to its string representation.
    @param v The value to convert
    @return A string representation of the value *)
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

(** [bind_pat p v] attempts to match a pattern [p] against a value [v]. If
    successful, returns Some bindings where bindings is a list of (id, value)
    pairs. If unsuccessful, returns None.
    @param p The pattern to match
    @param v The value to match against
    @return Some bindings if match successful, None otherwise *)
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

(** [eval_c_expr ce env] evaluates a condensed expression [ce] in the context of
    environment [env].
    @param ce The condensed expression to evaluate
    @param env The environment to evaluate in
    @return The result of evaluation *)
let rec eval_c_expr (ce : c_expr) (env : env) : value eval_result =
  match ce with
  | EInt i -> IntegerValue i |> return
  | EFloat f -> FloatValue f |> return
  | EString s -> StringValue s |> return
  | EBool b -> BooleanValue b |> return
  | ENil -> ListValue [] |> return
  | EUnit -> UnitValue |> return
  | EId s -> List.assoc s env |> return
  | EBop (op, e1, e2) -> eval_bop op e1 e2 env
  | EFunction (p, _, e) -> FunctionClosure (env, p, None, e) |> return
  | EListEnumeration (e1, e2) -> eval_list_enumeration e1 e2 env
  | EBlock parts ->
      let rec eval_block_parts parts env : value eval_result =
        match parts with
        | [] ->
            UnitValue |> return (* when there are no parts, evaluate to unit *)
        | [ Expr e ] -> eval_c_expr e env
        | Expr e :: t ->
            (* if the last part is an expression, we evaluate to it *)
            let _ = eval_c_expr e env in
            eval_block_parts t env
        | Defn d :: t ->
            (* when the next part is a definition, we run the definition and add
               to the env, then continue evaluating the rest of the block*)
            let* new_bindings = eval_defn d env in
            eval_block_parts t (new_bindings @ env)
      in

      eval_block_parts parts env
  | EListComprehension (e, generators) ->
      let* envs = generate_envs_from_generators generators env in
      let rec eval_envs acc = function
        | [] -> return (List.rev acc)
        | en :: rest ->
            let* v = eval_c_expr e en in
            eval_envs (v :: acc) rest
      in
      let* values = eval_envs [] envs in
      ListValue values |> return
  | EVector expressions ->
      (* evaluate each sub expression to a value, collecting results *)
      let rec eval_vector acc = function
        | [] -> return (VectorValue (List.rev acc))
        | e :: rest ->
            let* v = eval_c_expr e env in
            eval_vector (v :: acc) rest
      in
      eval_vector [] expressions
  | ESwitch (e, branches) -> (
      let* v : value = eval_c_expr e env in
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
      | None -> Error (OtherError "no pattern matched in switch")
      | Some (bindings, e) -> eval_c_expr e (bindings @ env))
  | ETernary (e1, e2, e3) -> (
      let* v1 : value = eval_c_expr e1 env in
      match v1 with
      | BooleanValue true -> eval_c_expr e2 env
      | BooleanValue false -> eval_c_expr e3 env
      | _ -> Error (OtherError "eval_c_expr: ETernary"))
  | EApp (e1, e2) -> (
      let* v1 : value = eval_c_expr e1 env in
      let* v2 : value = eval_c_expr e2 env in
      match v1 with
      | BuiltInFunction f -> eval_builtin f v2
      | FunctionClosure (env', p, _, e) -> (
          match bind_pat p v2 with
          | Some env'' -> eval_c_expr e (env'' @ env')
          | None -> Error (OtherError "eval_c_expr: EApp"))
      (* recursive function *)
      | RecursiveFunctionClosure (env'_ref, p, _, e) -> (
          let env' : env = !env'_ref in
          match bind_pat p v2 with
          | Some env'' -> eval_c_expr e (env'' @ env')
          | None -> Error (OtherError "eval_c_expr: EApp"))
      | _ -> Error (OtherError "eval_c_expr: EApp"))
  | EBind (pattern, _, e1, e2) ->
      (* We have let p = e1 in e2. We can convert this to (fun p -> e2) e1 and
         evaluate that instead. As far as dynamic semantics go, they are the
         same thing! *)

      (* construct the modified expression *)
      let modified_expr = EApp (EFunction (pattern, None, e2), e1) in
      (* evaluate the modified expression *)
      eval_c_expr modified_expr env
  | EBindRec (pattern, _, e1, e2) -> (
      let* v1 : value = eval_c_expr e1 env in
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
          | None -> Error (OtherError "no pattern matched in let rec")
          | Some recursive_bindings ->
              env_ref := recursive_bindings @ env;
              eval_c_expr e2 (recursive_bindings @ env))
      | _ -> (
          (* evaluate a regular let expression *)
          let new_bindings_option : env option = bind_pat pattern v1_rec in
          match new_bindings_option with
          | None -> Error (OtherError "no pattern matched in let rec")
          | Some new_bindings -> eval_c_expr e2 (new_bindings @ env)))

and eval_builtin (f : builtin_function) (v : value) : value eval_result =
  match (f, v) with
  | Println, StringValue s ->
      print_endline s;
      return UnitValue
  | Print, StringValue s ->
      print_string s;
      return UnitValue
  | IntToString, IntegerValue i -> StringValue (string_of_int i) |> return
  | IntToFloat, IntegerValue i -> FloatValue (float_of_int i) |> return
  | FloatToInt, FloatValue f -> IntegerValue (int_of_float f) |> return
  | _ -> Error (OtherError "eval_builtin: unimplemented")

and generate_envs_from_generators generators env : env list eval_result =
  match generators with
  | [] -> return [ env ]
  | (p, e) :: t -> (
      let* v = eval_c_expr e env in
      match v with
      | ListValue values ->
          let rec collect_envs acc = function
            | [] -> return (List.flatten (List.rev acc))
            | value :: rest -> (
                match bind_pat p value with
                | None -> Error (PatternMatchError (p, value))
                | Some bindings ->
                    let new_env = bindings @ env in
                    let* envs = generate_envs_from_generators t new_env in
                    collect_envs (envs :: acc) rest)
          in
          collect_envs [] values
      | _ ->
          Error
            (OtherError "generate_envs_from_generators: expected a list value"))

and eval_bop (op : c_bop) (e1 : c_expr) (e2 : c_expr) (env : env) :
    value eval_result =
  match op with
  | CAnd -> (
      let* v1 : value = eval_c_expr e1 env in
      match v1 with
      | BooleanValue false -> BooleanValue false |> return
      | BooleanValue true -> eval_c_expr e2 env
      | _ -> Error (OtherError "eval_bop: CAnd expects boolean operands"))
  | COr -> (
      let* v1 : value = eval_c_expr e1 env in
      match v1 with
      | BooleanValue true -> BooleanValue true |> return
      | BooleanValue false -> eval_c_expr e2 env
      | _ -> Error (OtherError "eval_bop: COr expects boolean operands"))
  | _ -> (
      let* v1 : value = eval_c_expr e1 env in
      let* v2 : value = eval_c_expr e2 env in
      match (op, v1, v2) with
      | CPlus, IntegerValue a, IntegerValue b -> IntegerValue (a + b) |> return
      | CMinus, IntegerValue a, IntegerValue b -> IntegerValue (a - b) |> return
      | CMul, IntegerValue a, IntegerValue b -> IntegerValue (a * b) |> return
      | CDiv, IntegerValue a, IntegerValue b -> IntegerValue (a / b) |> return
      | CMod, IntegerValue a, IntegerValue b -> IntegerValue (a mod b) |> return
      | CEQ, a, b -> BooleanValue (a = b) |> return
      | CNE, a, b -> BooleanValue (a <> b) |> return
      | CLT, IntegerValue a, IntegerValue b -> BooleanValue (a < b) |> return
      | CLE, IntegerValue a, IntegerValue b -> BooleanValue (a <= b) |> return
      | CGT, IntegerValue a, IntegerValue b -> BooleanValue (a > b) |> return
      | CGE, IntegerValue a, IntegerValue b -> BooleanValue (a >= b) |> return
      | CCons, v, ListValue vs -> ListValue (v :: vs) |> return
      | _ -> Error (OtherError "eval_bop: unimplemented"))

and eval_list_enumeration e1 e2 env : value eval_result =
  let* v1 = eval_c_expr e1 env in
  let* v2 = eval_c_expr e2 env in
  match (v1, v2) with
  | IntegerValue a, IntegerValue b ->
      let rec make_list_tr a b acc =
        if a > b then List.rev acc
        else make_list_tr (a + 1) b (IntegerValue a :: acc)
      in
      ListValue (make_list_tr a b []) |> return
  | _ -> Error (OtherError "eval_list_enumeration: expected two integers")

(* let eval_c_empty_env (s : string) : value = eval_c_expr (s |> list_of_string
   |> lex |> parse_expr |> fst |> condense_expr) [] *)

and eval_c_empty_env (s : string) : value eval_result =
  let tokens = s |> list_of_string |> lex in
  let token_types = List.map (fun t -> t.token_type) tokens in
  let parse_result = expr_parser token_types in
  match parse_result with
  | None -> Error (OtherError "eval_c_empty_env: parsing failed")
  | Some (e, _) ->
      let c_e = condense_expr e in
      eval_c_expr c_e []

and initial_env () : (string * value) list eval_result =
  let rec map_env acc = function
    | [] -> Ok (List.rev acc @ built_ins_values)
    | (id, code) :: rest -> (
        match eval_c_empty_env code with
        | Ok v -> map_env ((id, v) :: acc) rest
        | Error e ->
            Error
              (OtherError
                 ("initial_env: failed to evaluate code for " ^ id ^ ": "
                 ^
                 match e with
                 | OtherError msg -> msg
                 | _ -> "")))
  in
  map_env [] code_mapping

(* env code *)

and c_eval_ce (ce : c_expr) : string eval_result =
  let* initial_env = initial_env () in
  match eval_c_expr ce initial_env with
  | Ok v -> string_of_value v |> return
  | Error _ -> Error (OtherError "c_eval_ce: evaluation failed")

and c_eval (s : string) : string eval_result =
  let tokens = s |> list_of_string |> lex in
  let token_types = List.map (fun t -> t.token_type) tokens in
  let parse_result = expr_parser token_types in
  match parse_result with
  | None -> Error (OtherError "c_eval: parsing failed")
  | Some (e, _) -> (
      let c_e = condense_expr e in
      let* initial_env = initial_env () in
      match eval_c_expr c_e initial_env with
      | Ok v -> string_of_value v |> return
      | Error _ -> Error (OtherError "c_eval: evaluation failed"))

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

(** [expr_of_pat p] converts a pattern [p] of type [c_pat] into a corresponding
    expression of type [c_expr]. This is useful for cases where a pattern needs
    to be treated as an expression, such as in pattern matching code generation
    or evaluation. Note that wildcard patterns are not allowed and will raise an
    exception.

    @param p The pattern to convert
    @return The corresponding expression
    @raise Failure if the pattern is a wildcard (CWildcardPat) *)
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

(** [eval_defn d env] evaluates a definition [d] in the context of environment
    [env].
    @param d The definition to evaluate
    @param env The environment to evaluate in
    @return The new bindings introduced by the definition *)
and eval_defn (d : c_defn) (env : env) : env eval_result =
  match d with
  | CDefn (pat, _, body) -> (
      (* Evaluate the body in the current environment *)
      let* value = eval_c_expr body env in
      (* Try to bind the pattern to the value *)
      match bind_pat pat value with
      | None -> Error (OtherError "eval_defn: pattern match failed")
      | Some new_bindings -> new_bindings |> return)
  | CDefnRec (pat, _, body) -> (
      (* For recursive definitions, we need to create a recursive closure *)
      let* value = eval_c_expr body env in
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
          new_bindings |> return)
  | CTypeAlias _ ->
      (* doesn't do anything *)
      return []

and string_of_bop = function
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

and string_of_pat = function
  | CUnitPat -> "()"
  | CWildcardPat -> "_"
  | CIdPat s -> s
  | CIntPat i -> string_of_int i
  | CStringPat s -> "\"" ^ s ^ "\""
  | CBoolPat b -> string_of_bool b
  | CNilPat -> "[]"
  | CConsPat (p1, p2) -> string_of_pat p1 ^ " :: " ^ string_of_pat p2
  | CVectorPat ps -> "(" ^ String.concat ", " (List.map string_of_pat ps) ^ ")"

and string_of_eval_error = function
  | PatternMatchError (p, v) ->
      "Pattern match error: " ^ string_of_pat p ^ " != " ^ string_of_value v
  | BinaryOpError (op, v1, v2) ->
      "Binary operation error: " ^ string_of_bop op ^ " " ^ string_of_value v1
      ^ " " ^ string_of_value v2
  | UnboundVariable s -> "Unbound variable: " ^ s
  | TypeError s -> "Type error: " ^ s
  | OtherError s -> "Error: " ^ s
