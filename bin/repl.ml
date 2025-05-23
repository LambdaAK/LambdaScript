open Language.Lex
open Language.New_parser.ExprParser

let s = "1 + 1 + 2"
let chars = s |> String.to_seq |> List.of_seq
let lexed = lex chars |> List.map (fun t -> t.token_type)
let _ = expr_parser lexed

(* Condense e into a c_expr object *)
