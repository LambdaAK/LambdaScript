open Language.Parse
open Language.Lex
open Language.Cexpr
open Language.Ceval
open Language.Condense
open Language.Typecheck
open Language.Ctostringtree.CToStringTree
open Language.Typefixer
open Language.Ceval_defn
open Language.Env

type action =
  | EvalDefn
  | EvalExpr

let attempt_lex (input_string : string) : token list =
  input_string |> list_of_string |> lex

let attempt_parse (tokens : token list) : c_expr =
  tokens |> parse_expr |> fst |> condense_expr

let attempt_parse_defn (tokens : token list) : c_defn =
  tokens |> parse_defn |> fst |> condense_defn

let attempt_type_check (ce : c_expr) (env : static_env) type_env : c_type =
  type_of_c_expr ce env type_env

let attempt_eval (ce : c_expr) (env : env) : string =
  eval_c_expr ce env |> string_of_value

let () =
  ignore attempt_lex;
  ignore attempt_parse;
  ignore attempt_type_check;
  ignore attempt_eval

let rec repl_loop (env : env) (static_env : static_env) (type_env : type_env) :
    unit =
  (* print the type environment *)
  let type_env_string =
    List.map
      (fun (id, t) ->
        let t_string = string_of_c_type t in
        "type " ^ id ^ " : " ^ t_string)
      type_env
    |> String.concat "\n"
  in
  if type_env_string <> "" then print_endline type_env_string else ();

  (* print the environment *)
  print_string "> ";
  let input_string : string = read_line () in
  let tokens : token list = attempt_lex input_string in

  let action =
    match tokens with
    | { token_type = Type; _ } :: _ | { token_type = Union; _ } :: _ -> EvalDefn
    | { token_type = Let; line = _ } :: _ -> (
        (* This is one of a few things 1. Let definition 2. Let rec definition
           3. Expression *)
        let _, tokens_after_defn = parse_defn tokens in
        (* check the tokens after the definition if the token is In, then it's
           an expression else, it's a definition *)
        match tokens_after_defn with
        | { token_type = In; line = _ } :: _ ->
            (* we are evaluating an expression *)
            repl_expr env static_env type_env input_string;
            repl_loop env static_env type_env;
            EvalExpr
        | _ -> EvalDefn)
    | _ ->
        (* we are evaluating an expression *)
        EvalExpr
  in

  match action with
  | EvalExpr ->
      repl_expr env static_env type_env input_string;
      repl_loop env static_env type_env
  | EvalDefn ->
      (* we are evaluating a definition *)
      let d : c_defn = attempt_parse_defn tokens in
      let ( new_env,
            new_static_env,
            new_type_env,
            new_value_bindings,
            new_type_bindings ) =
        eval_defn d env static_env type_env
      in

      (* for each new binding, make a string id : type = value *)
      let new_value_bindings_string =
        List.map
          (fun id ->
            let value = List.assoc id new_env in
            let value_string = string_of_value value in
            let t : c_type =
              List.assoc id new_static_env |> instantiate |> fix
            in
            let t_string = string_of_c_type t in
            id ^ " : " ^ t_string ^ " = " ^ value_string)
          new_value_bindings
        |> String.concat "\n"
      in

      let new_type_bindings_string =
        List.map
          (fun id ->
            let t : c_type = List.assoc id new_type_env in
            let t_string = string_of_c_type t in
            "type " ^ id ^ " : " ^ t_string)
          new_type_bindings
        |> String.concat "\n"
      in

      (* print the new bindings *)
      if new_value_bindings_string <> "" then
        print_endline new_value_bindings_string
      else ();
      if new_type_bindings_string <> "" then
        print_endline new_type_bindings_string
      else ();

      repl_loop new_env new_static_env new_type_env

and repl_expr (env : env) (static_env : static_env)
    (type_env : (string * c_type) list) (e : string) =
  ignore type_env;
  try
    (* let input_string: string = e in let tokens: token list = attempt_lex
       input_string in let ce: c_expr = attempt_parse tokens in let t: c_type =
       attempt_type_check ce static_env in let t_string: string =
       string_of_c_type t in let result: string = attempt_eval ce env in
       print_endline ("\n" ^ t_string ^ ": " ^ result ^ "\n") *)
    let input_string : string = e in
    let tokens : token list = attempt_lex input_string in
    (* print the tokens *)
    (* end print *)
    let ce : c_expr = attempt_parse tokens in

    let t : c_type = attempt_type_check ce static_env type_env in
    let t_string : string = string_of_c_type t in

    (* evaluate ce *)
    (* don't typecheck *)
    let result : string = attempt_eval ce env in
    print_string (t_string ^ ": ");
    print_endline (result ^ "\n")
  with
  | LexFailure -> print_endline "Lex Failure\n"
  | ParseFailure -> print_endline "Parse Failure\n"
  | TypeFailure -> print_endline "Type Failure\n"
  | UnexpectedToken (expected, Some got, line) ->
      (* print the error *)
      "Unexpected Token on line " ^ string_of_int line ^ ": expected "
      ^ string_of_token { token_type = expected; line }
      ^ " but got "
      ^ string_of_token { token_type = got; line }
      |> print_endline
  | UnexpectedToken (expected, None, line) ->
      (* print the error *)
      "Unexpected Toke: " ^ string_of_int line ^ ": expected "
      ^ string_of_token { token_type = expected; line }
      ^ " but got none"
      |> print_endline

let run_repl () : unit =
  print_endline "[LambdaScript REPL]\n";
  let env =
    List.map
      (fun (id, code) ->
        let v = eval_c_empty_env code in
        (id, v))
      code_mapping
    @ built_ins_values
  in
  let static_env =
    List.map
      (fun (id, code) ->
        (* get the type of the value *)
        let tokens : token list = code |> list_of_string |> lex in
        let e, _ = parse_expr tokens in
        let c_e = condense_expr e in
        let t = type_of_c_expr c_e [] [] in
        (id, t))
      code_mapping
    @ built_ins_types
  in
  repl_loop env static_env []

let () = print_endline "Welcome to LambdaScript!"
let () = run_repl ()
