open Language.Lex
open Language.New_parser.ExprParser
open Language.New_condense
open Language.New_c_to_string
open Language.New_typecheck

let s = "1 :: []"
let chars = s |> String.to_seq |> List.of_seq
let lexed = lex chars |> List.map (fun t -> t.token_type)
let e = expr_parser lexed |> Option.get |> fst

(* Condense e into a c_expr object *)

let ce = condense_expr e

let () =
  print_endline "Parsed expression";
  print_endline (string_of_expr ce)

let t = type_of_c_expr ce
let () = t |> string_of_c_type |> print_endline
