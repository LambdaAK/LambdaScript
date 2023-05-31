open Language.Eval
open Language.Parse
open Language.Lex
open Language.Reader
open Language.Tostring

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
  (string_of_expr ast) |> print_endline;
  print_endline "\n[Value]\n";
  (eval contents) |> print_endline; print_newline ();
  print_endline "\n"

let () = 

try run_run (get_dir ())
with
| _ -> print_endline "Error"; exit 1