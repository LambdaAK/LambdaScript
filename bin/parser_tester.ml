open Language.Lex
open Language.Parser.ExprOrDefnParser
open Language.Tostring
open Language.Expr

(* ANSI color codes *)
let color_reset = "\027[0m"
let color_red = "\027[31m"
let color_green = "\027[32m"
let color_yellow = "\027[33m"
let color_blue = "\027[34m"
let color_magenta = "\027[35m"
let color_cyan = "\027[36m"
let color_bold = "\027[1m"
let color_dim = "\027[2m"
let () = ignore (color_cyan, color_magenta, color_yellow)

(* Color printing functions *)
let print_colored color text = print_string (color ^ text ^ color_reset)
let print_colored_line color text = print_endline (color ^ text ^ color_reset)

let print_separator () =
  print_colored_line color_dim "-----------------------------"

let print_error msg = print_colored_line color_red ("Error: " ^ msg)

let print_ast ast =
  print_colored color_blue "AST: ";
  print_colored_line color_cyan
    (match ast with
    | Expr e -> string_of_expr e
    | Definition d -> string_of_defn d 0)

(** [read_multiline ()] reads input from the user until a line containing only
    ";;" is encountered. The ;; can be on the first line or any subsequent line.
    @return The concatenated input lines *)
let rec read_multiline () =
  let line = read_line () in
  if String.trim line = ";;" then ""
  else if String.ends_with ~suffix:";;" (String.trim line) then
    String.sub line 0 (String.length line - 2)
  else line ^ "\n" ^ read_multiline ()

let rec parser_tester () =
  (* Print prompt *)
  print_colored (color_bold ^ color_green) "λ> ";
  flush_all ();

  (* get multiline input from the user *)
  let input = read_multiline () |> String.to_seq |> List.of_seq in
  (* lex tokens *)
  let tokens = lex input |> List.map (fun t -> t.token_type) in

  let expr_or_defn = expr_or_defn_parser tokens in

  match expr_or_defn with
  | None ->
      print_error "Parsing failed";
      parser_tester ()
  | Some (ast, remaining_tokens) ->
      if List.length remaining_tokens > 0 then
        print_error
          ("Parsing succeeded but there were leftover tokens: "
          ^ String.concat " " (List.map string_of_token_type remaining_tokens))
      else
        (* parsing succeeded *)
        print_separator ();
      print_ast ast;
      print_separator ();
      parser_tester ()

let run_parser_tester () =
  (* Print welcome message *)
  print_colored_line (color_bold ^ color_cyan) "🔍 Forge Parser Tester";
  print_colored_line color_dim "Enter expressions or definitions to parse";
  print_colored_line color_dim "End input with ;;";
  print_newline ();

  parser_tester ()

let () = run_parser_tester ()
