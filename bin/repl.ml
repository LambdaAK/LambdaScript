open Language.Parse
open Language.Lex
open Language.Cexpr
open Language.Ceval
open Language.Condense
open Language.Typecheck
open Language.Ctostring


let attempt_lex (input_string: string): token list = input_string |> list_of_string |> lex

let attempt_parse (tokens: token list): c_expr = tokens |> parse_expr |> fst |> condense_expr

let attempt_parse_defn (tokens: token list): c_defn = tokens |> parse_defn |> fst |> condense_defn

let attempt_type_check (ce: c_expr) (env: static_env): c_type = type_of_c_expr ce env

let attempt_eval (ce: c_expr) (env: env): string = eval_c_expr ce env |> string_of_value


let () = ignore attempt_lex; ignore attempt_parse; ignore attempt_type_check; ignore attempt_eval


let rec repl_loop (env: env) (static_env: static_env): unit =
  print_string "> ";
  let input_string: string = read_line () in
  let tokens: token list = attempt_lex input_string in
  match tokens with
  | {token_type = Let; line = _} :: _ ->
    let d: c_defn = attempt_parse_defn tokens in
    let new_env, new_static_env, t, v = eval_defn d env static_env in
    let t_string: string = string_of_c_type t in
    let v_string: string = string_of_value v in
    print_endline ("\n" ^ t_string ^ ": " ^ v_string ^ "\n");

    repl_loop new_env new_static_env
  | _ ->
    repl_expr env static_env input_string;
    repl_loop env static_env


and repl_expr (env: env) (static_env: static_env) (e: string) =
  (
    try
    (*
    let input_string: string = e in
    let tokens: token list = attempt_lex input_string in
    let ce: c_expr = attempt_parse tokens in
    let t: c_type = attempt_type_check ce static_env in
    let t_string: string = string_of_c_type t in
    let result: string = attempt_eval ce env in
    print_endline ("\n" ^ t_string ^ ": " ^ result ^ "\n")
    *)
    
    let input_string: string = e in
    let tokens: token list = attempt_lex input_string in
    (* print the tokens *)
    tokens |> List.map string_of_token |> String.concat " " |> print_endline;
    (* end print *)
    let ce: c_expr = attempt_parse tokens in
    (* print ce *)
    let ce_string: string = string_of_c_expr ce in
    print_endline ("\n" ^ ce_string ^ "\n");
    (* end print *)
    let t: c_type = attempt_type_check ce static_env in
    let t_string: string = string_of_c_type t in
    print_endline ("\n" ^ t_string ^ ": ");
    (* evaluate ce *)
    (* don't typecheck *)
    let result: string = attempt_eval ce env in
    print_endline ("\n" ^ result ^ "\n")
    
    with
    | LexFailure -> print_endline "Lex Failure\n"
    | ParseFailure -> print_endline "Parse Failure\n"
    | TypeFailure -> print_endline "Type Failure\n"
    | UnexpectedToken (expected, Some got, line) ->
      (* print the error *)
      "Unexpected Token on line " 
      ^ (string_of_int line) 
      ^ ": expected "  
      ^ (string_of_token {token_type = expected; line = line}) 
      ^ " but got " ^ (string_of_token {token_type = got; line = line})
      |> print_endline;
  
    
    | UnexpectedToken (expected, None, line) ->
      (* print the error *)
      "Unexpected Toke: " 
      ^ (string_of_int line) 
      ^ ": expected "  
      ^ (string_of_token {token_type = expected; line = line}) 
      ^ " but got none"
      |> print_endline
  
    )


let run_repl (): unit = 
  

  print_endline "[LambdaScript REPL]\n"; repl_loop Language.Ceval.initial_env Language.Typecheck.initial_env

let () = run_repl ()