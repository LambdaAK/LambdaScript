open Language.Parse
open Language.Lex
open Language.Cexpr
open Language.Ceval
open Language.Condense
open Language.Typecheck
open Language.Ctostring





let attempt_lex (input_string: string): token list =
  try 
    input_string |> list_of_string |> lex
  with
  | LexFailure -> print_endline "Lexing error"; raise (Failure "Lexing error")


let attempt_parse (tokens: token list): c_expr =
  try
    tokens |> parse_expr |> fst |> condense_expr
  with
  | _ -> print_endline "Parsing error"; raise (Failure "Parsing error")



let attempt_type_check (ce: c_expr): c_type =
  try
    type_of_c_expr ce
  with
  | _ -> print_endline "Type error"; raise (Failure "Type error")



let attempt_eval (ce: c_expr): string =
  try
    c_eval_ce ce

  with
  | _ -> print_endline "Evaluation error"; raise (Failure "Evaluation error")


let rec repl_loop (): unit =
  counter := 0;
  print_string "> ";
  let input_string: string = read_line () in
  let tokens: token list = attempt_lex input_string in
  let ce: c_expr = attempt_parse tokens in
  let t: c_type = attempt_type_check ce in
  let t_string: string = string_of_c_type t in
  let result: string = attempt_eval ce in
  print_endline ("\n" ^ t_string ^ ": " ^ result ^ "\n");


 
  

  repl_loop ()
 

let run_repl (): unit = 

  print_endline "[LambdaScript REPL]\n"; repl_loop ()

let () = run_repl ()