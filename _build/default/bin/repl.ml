open Language.Parse
open Language.Lex
open Language.Expr
open Language.Cexpr
open Language.Ctostring
open Language.Ceval
open Language.Condense
open Language.Typecheck



let rec repl_loop (): unit =
  counter := 0;
  let input_string: string = read_line () in
  if input_string = "#exit" then exit 0
  else
  (*input_string |> list_of_string |> lex |> print_tokens_list;*)
  let e: expr = input_string |> list_of_string |> lex |> parse_expr |> fst in
  let ce: c_expr = condense_expr e in

  let () = print_endline "\n[AST]\n" in
  let () = ce |> string_of_c_expr |> print_endline in
  let () = print_endline "\n[AST]\n" in
  let t, cn = generate initial_env ce in
  print_string "type: ";
  let () = t |> string_of_c_type |> print_endline in
  print_string "\nconstraints:\n";
  let () = cn |> string_of_constraints |> print_endline in
  print_string "\nunify:\n";
  let () = cn |> reduce_eq |> string_of_constraints |> print_endline in

  print_string "\nthe expression is of type:\n";
  let () = get_type t (reduce_eq cn) |> string_of_c_type |> print_endline in
 
  print_endline "\n[Value]\n";
  input_string |> c_eval |> print_endline;
  print_endline "\n";
  (* get_type e [] |> fst |> string_of_type |> print_endline; *)

  repl_loop ()
 

let run_repl (): unit = 

  print_endline "[LambdaScript REPL]\n"; repl_loop ()

let () = run_repl ()