open Language.Parse
open Language.Lex
open Language.Cexpr
open Language.Ceval
open Language.Condense
open Language.Typecheck
open Language.Ctostring


let attempt_lex (input_string: string): token list = input_string |> list_of_string |> lex

let attempt_parse (tokens: token list): c_expr = tokens |> parse_expr |> fst |> condense_expr

let attempt_type_check (ce: c_expr): c_type = type_of_c_expr ce

let attempt_eval (ce: c_expr): string = c_eval_ce ce

let rec repl_loop (): unit =
  (
  try
  counter := 0;
  print_string "> ";
  let input_string: string = read_line () in
  let tokens: token list = attempt_lex input_string in
  let ce: c_expr = attempt_parse tokens in
  let () = ce |> string_of_c_expr |> print_endline in
  let () = tokens |> parse_expr |> fst |> Language.Tostring.string_of_expr |> print_endline in
  let t: c_type = attempt_type_check ce in
  let t_string: string = string_of_c_type t in
  let result: string = attempt_eval ce in
  print_endline ("\n" ^ t_string ^ ": " ^ result ^ "\n")
  
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

  );
  

  repl_loop ()
 

let run_repl (): unit = 

  print_endline "[LambdaScript REPL]\n"; repl_loop ()

let () = run_repl ()