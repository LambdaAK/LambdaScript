open Language.Lex
open Language.Parse
open Language.Condense
open Language.Ctostringtree.CToStringTree
open Language.Typecheck

let () = print_endline "other stuff"
let s = "let rec f x = x in f f"
let chars = s |> String.to_seq |> List.of_seq
let lexed = lex chars

(* parse *)

let e = parse_expr lexed |> fst |> condense_expr

(* get the type of e *)

let t = type_of_c_expr e []

let () =
  print_endline "type of e:";
  t |> string_of_c_type |> print_endline
