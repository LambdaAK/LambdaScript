open Language.Eval
open Language.Parse
open Language.Lex
open Language.Tostring
open Language.Expr

let rec repl_loop (): unit =
  let input_string: string = read_line () in
  (*input_string |> list_of_string |> lex |> print_tokens_list;*)
  let e: expr = input_string |> list_of_string |> lex |> parse_expr |> fst in
  
  
  print_endline "[AST]\n";
  string_of_expr e |> print_endline; 
 
  print_endline "\n[Value]\n";
  eval input_string |> print_endline;
  print_endline "\n";

  repl_loop ()


let run_repl (): unit = 

  print_endline "[LambdaScript REPL]\n"; repl_loop ()

let () = run_repl ()