open Language.Lex
open Language.Parser.ExprParser
open Language.Condense
open Language.C_to_string
open Language.Typecheck
open Language.Ceval

let rec repeat_forever (f : unit -> unit) =
  f ();
  repeat_forever f

let repl () =
  (* get input from the user . convert to list of chars*)
  let input = read_line () |> String.to_seq |> List.of_seq in
  (* lex tokens *)

  let tokens = lex input |> List.map (fun t -> t.token_type) in

  match expr_parser tokens with
  | Some (expr, _) -> (
      (* parsing succeeded *)
      (* condense the expression *)
      let c_expr = condense_expr expr in
      (* type check the expression *)
      match type_of_c_expr [] c_expr with
      | Ok t ->
          (* it typechecked properly *)
          (* evaluate the expression *)
          let value = eval_c_expr c_expr [] in
          (* pretty print the type and value *)
          print_endline "-----------------------------";
          print_endline ("Type : " ^ string_of_c_type t);
          print_endline ("Value: " ^ string_of_value value);
          print_endline "-----------------------------"
      | Error e -> print_endline (string_of_type_check_error e))
  | None ->
      (* parsing failed *)
      print_endline "Parsing failed"

let run_repl () = repeat_forever repl
let () = run_repl ()
