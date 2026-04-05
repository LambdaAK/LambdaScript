Expected:
A
B 1

Source:
type T =
  | A
  | B of Int

let str_of_t t =
  case t do
    | A -> "A"
    | B n -> "B " ^ int_to_str n

let () = print_string (str_of_t A)
let () = print_string (str_of_t (B 1))
