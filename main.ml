open Lex
open Expr



let () = ignore print_tokens_list

type action = Run of string | REPL

let get_action (): action =
  if Array.length Sys.argv = 1 then
    let () = print_endline "A command line argument must be provided" in
    exit 1
  else
    match Sys.argv.(1) with
    | "run" ->
      (* get the second argument *)
      if Array.length Sys.argv = 2 then
        let () = print_endline "A directory must be provided" in
        exit 1
      else
        let dir: string = Sys.argv.(2) in
        Run dir
    | "repl" -> REPL
    | _ -> print_endline "Invalid arguments"; exit 1



let rec repl_loop (): unit =
  let input_string: string = read_line () in
  let e: expr = input_string |> list_of_string |> lex |> parse_expr |> fst in
  
  print_endline "[Printing AST]\n"; e |> string_of_expr |> print_endline; print_endline "\n[End AST]\n";

  repl_loop ()


let run_repl (): unit = 

  print_endline "[LambdaScript REPL]\n"; repl_loop ()
  


let run_run (dir: string): unit = 
  let contents: string = Reader.read dir in
  let tokens: token list = contents |> Lex.list_of_string |> Lex.lex in
  print_endline "[TOKENS]";
  print_tokens_list tokens;
  print_endline "[END TOKENS]";
  let ast, _ = parse_expr tokens in
  ast |> string_of_expr |> print_endline
  ;print_newline ()

let main (): unit =
  print_newline ();
  
  match get_action () with
  | REPL -> run_repl ()
  | Run dir -> run_run dir

let () = main ()