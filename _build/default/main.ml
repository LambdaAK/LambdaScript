open Lex
open Expr
let rec print_tokens_list: Lex.token list -> unit = function
| [] -> ()
| token :: tail ->
  token |> Lex.string_of_token |> print_endline; print_tokens_list tail


let () = ignore print_tokens_list

let main (): unit =
  print_newline ();
  if Array.length Sys.argv = 1 then
    print_endline "a directory must be provided"
  else
  let dir: string = Sys.argv.(1) in


  let contents: string = Reader.read dir in
  let tokens: token list = contents |> Lex.list_of_string |> Lex.lex in

  let ast, _ = parse_expr tokens in
  ast |> string_of_expr |> print_endline


  ;print_newline ()


let () = main ()