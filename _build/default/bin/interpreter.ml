open Language.Eval
open Language.Expr
open Language.Lex
open Language.Reader

let get_dir (): string =
  if Array.length Sys.argv = 1 then
    let () = print_endline "A command line argument must be provided" in
    exit 1
  else
    Sys.argv.(1)




let run_run (dir: string): unit = 
  let contents: string = read dir in
  let tokens: token list = contents |> list_of_string |> lex in
  let ast, _ = parse_expr tokens in
  print_endline "[AST]\n";
  (string_of_expr ast 0) |> print_endline;
  print_endline "\n[Value]\n";
  (eval_expr ast []) |> string_of_value |> print_endline; print_newline ();
  print_endline "\n"

let () = run_run (get_dir ())