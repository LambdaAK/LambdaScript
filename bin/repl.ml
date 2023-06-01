open Language.Parse
open Language.Lex
open Language.Tostring
open Language.Expr
open Language.Cexpr
open Language.Ctostring
open Language.Ceval



let rec repl_loop (): unit =
  let input_string: string = read_line () in
  if input_string = "#exit" then exit 0
  else
  (*input_string |> list_of_string |> lex |> print_tokens_list;*)
  let e: expr = input_string |> list_of_string |> lex |> parse_expr |> fst in
  let ce: c_expr = condense_expr e in
  let () = print_endline "\n[Condensed]\n" in
  let () = string_of_c_expr ce 0 |> print_endline in
  let () = print_endline "\n[Condensed]\n" in
  

  string_of_expr e |> print_endline;

 
  print_endline "\n[Value]\n";
  eval_c_expr ce [] |> string_of_value |> print_endline;
  print_endline "\n";
  (* get_type e [] |> fst |> string_of_type |> print_endline; *)

  repl_loop ()
 

let run_repl (): unit = 

  print_endline "[LambdaScript REPL]\n"; repl_loop ()

let () = run_repl ()