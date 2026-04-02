Expected:
0

Source:
type rec Even = | Zero | SuccE of Odd
and Odd = | SuccO of Even

let rec even_val e =
  case e do
    | Zero -> 0
    | SuccE o -> 1 + odd_val o
and odd_val o =
  case o do
    | SuccO e -> 1 + even_val e

let () = println (int_to_str (even_val Zero))
