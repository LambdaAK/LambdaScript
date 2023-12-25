open Language.Parse
open Language.Lex
open Language.Cexpr
open Language.Ceval
open Language.Condense
open Language.Typecheck
open Language.Ctostringtree.CToStringTree
open Language.Typefixer

let attempt_lex (input_string : string) : token list =
  input_string |> list_of_string |> lex

let attempt_parse (tokens : token list) : c_expr =
  tokens |> parse_expr |> fst |> condense_expr

let attempt_parse_defn (tokens : token list) : c_defn =
  tokens |> parse_defn |> fst |> condense_defn

let () = ignore attempt_parse_defn

let attempt_type_check (ce : c_expr) (env : static_env) : c_type =
  type_of_c_expr ce env

let attempt_eval (ce : c_expr) (env : env) : string =
  eval_c_expr ce env |> string_of_value

let () =
  ignore attempt_lex;
  ignore attempt_parse;
  ignore attempt_type_check;
  ignore attempt_eval

let rec repl_loop (env : env) (static_env : static_env) : unit =
  print_string "> ";
  let input_string : string = read_line () in
  let tokens : token list = attempt_lex input_string in

  match tokens with
  | { token_type = Let; line = _ } :: _ -> (
      (* This is one of a few things 1. Let definition 2. Let rec definition 3.
         Expression *)
      let _, tokens_after_defn = parse_defn tokens in
      (* check the tokens after the definition if the token is In, then it's an
         expression else, it's a definition *)
      match tokens_after_defn with
      | { token_type = In; line = _ } :: _ ->
          (* we are evaluating an expression *)
          repl_expr env static_env input_string;
          repl_loop env static_env
      | _ ->
          (* we are evaluating a definition *)
          let d : c_defn = attempt_parse_defn tokens in
          let new_env, new_static_env, new_bindings =
            eval_defn d env static_env
          in

          (* for each new binding, make a string id : type = value *)
          let new_bindings_string =
            List.map
              (fun id ->
                let value = List.assoc id new_env in
                let value_string = string_of_value value in
                let t : c_type =
                  List.assoc id new_static_env |> instantiate |> fix
                in
                let t_string = string_of_c_type t in
                id ^ " : " ^ t_string ^ " = " ^ value_string)
              new_bindings
            |> String.concat "\n"
          in

          (* print the new bindings *)
          print_endline new_bindings_string;
          repl_loop new_env new_static_env)
  | _ ->
      (* we are evaluating an expression *)
      repl_expr env static_env input_string;
      repl_loop env static_env

and repl_expr (env : env) (static_env : static_env) (e : string) =
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

    (* print ce *)

    (* end print *)
    let t : c_type = attempt_type_check ce static_env in
    let t_string : string = string_of_c_type t in
    print_string ("\n" ^ t_string ^ ": ");
    (* evaluate ce *)
    (* don't typecheck *)
    let result : string = attempt_eval ce env in
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
  repl_loop Language.Ceval.initial_env Language.Typecheck.initial_env

let () = run_repl ()
