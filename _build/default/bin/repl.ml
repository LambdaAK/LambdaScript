open Language.Eval
open Language.Parse
open Language.Lex
open Language.Tostring
open Language.Expr


let rec repl_loop (): unit =
  let input_string: string = read_line () in
  (*input_string |> list_of_string |> lex |> print_tokens_list;*)
  let e: expr = input_string |> list_of_string |> lex |> parse_expr |> fst in
  
  try
  (
  string_of_expr e |> print_endline;

 
  print_endline "\n[Value]\n";
  eval input_string |> print_endline;
  print_endline "\n";
  (* get_type e [] |> fst |> string_of_type |> print_endline; *)

  repl_loop ()
  )
  with
  | _ -> print_endline "Error"; repl_loop ()

let run_repl (): unit = 

  print_endline "[LambdaScript REPL]\n"; repl_loop ()

let () = run_repl ()