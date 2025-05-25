open Language.Lex
open Language.New_parser.ExprParser
open Language.New_condense
open Language.New_c_to_string
open Language.New_typecheck
open Language.New_cexpr

let t : mono_type = fresh_type_var ()

let () =
  print_endline "t: ";
  print_endline (string_of_mono_type t)

let new_t = swap_all_variables_in_type t

let () =
  print_endline "new_t: ";
  print_endline (string_of_mono_type new_t)

(* other stuff *)

let () = print_endline "other stuff"

let map_string =
  {|
  let rec map f lst = switch lst =>
    | [] -> []
    | h :: t -> f h :: map f t
  end
in map
|}

let filter_string =
  {|
  let rec filter p lst = switch lst =>
    | [] -> []
    | h :: t -> if p h then h :: filter p t else filter p t
  end
in filter
  |}

let fold_left_string =
  {|
  let rec fold_left f acc lst = switch lst =>
    | [] -> acc
    | h :: t -> fold_left f (f acc h) t
  end
in fold_left
  |}

let () = ignore (map_string, filter_string, fold_left_string)
let chars = filter_string |> String.to_seq |> List.of_seq
let lexed = lex chars |> List.map (fun t -> t.token_type)
let e = expr_parser lexed |> Option.get |> fst

(* Condense e into a c_expr object *)

let ce = condense_expr e

let () =
  print_endline "Parsed expression";
  print_endline (string_of_expr ce)

let t = type_of_c_expr ce
let () = t |> string_of_c_type |> print_endline
