open Language.Ceval
open Language.Parse
open Language.Lex
open Language.Reader
open Language.Typecheck
open Language.Ctostring
open Language.Condense

let get_dir (): string =
  if Array.length Sys.argv = 1 then
    let () = print_endline "A command line argument must be provided" in
    exit 1
  else
    Sys.argv.(1)


let run_run (dir: string): unit = 
  let contents: string = read dir in
  (*
  print_endline "\n[AST]\n";
  ast |> condense_expr |> string_of_c_expr |> print_endline;
  
  print_endline "\n[Value]\n";
  *)print_newline ();
  contents |> c_eval |> print_endline; print_newline ();
  type_of_c_expr (contents |> list_of_string |> lex |> parse_expr |> fst |> condense_expr) Language.Typecheck.initial_env |> string_of_c_type |> print_endline;
  print_endline "\n"

let () = 
(*
try run_run (get_dir ())
with
| _ -> print_endline "Error"; exit 1
*)

run_run (get_dir ())