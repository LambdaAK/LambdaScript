open Language.Lex
open Language.Parser.ExprParser
open Language.Condense
open Language.C_to_string
open Language.Typecheck
open Language.Ceval
open Language.Env

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

let fold_right_string =
  {|
  let rec fold_right f lst acc = switch lst =>
    | [] -> acc
    | h :: t -> f h (fold_right f t acc)
  end
in fold_right
  |}

let flip_string = {|
  let rec flip f x y = f y x in flip
|}

let trivial_string = {|
  let rec f x = f x in f
|}

let test_one =
  {|
  let rec map f lst = switch lst =>
  | [] -> []
  | h :: t -> f h :: map f t
  end
in map (\x -> if x then false else true) [true, false, true]
|}

let test_two = {|
    \ f [a -> b] -> \ x [a] -> f
|}

let () =
  ignore
    ( map_string,
      filter_string,
      fold_left_string,
      fold_right_string,
      flip_string,
      trivial_string,
      test_one,
      test_two )

let chars = test_two |> String.to_seq |> List.of_seq
let lexed = lex chars |> List.map (fun t -> t.token_type)
let e = expr_parser lexed |> Option.get |> fst

(* Condense e into a c_expr object *)

let ce = condense_expr e

let () =
  print_endline "Parsed expression";
  print_endline (string_of_expr ce)

let t = type_of_c_expr built_ins_types ce
let () = t |> string_of_c_type |> print_endline

(* then, evaluate the expression and print the result *)

let value = eval_c_expr ce []

let () =
  print_endline "Value: ";
  print_endline (string_of_value value)
