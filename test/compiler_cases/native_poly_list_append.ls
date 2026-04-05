Expected:
3

Source:
let rec append a b =
  case a do
    | [] -> b
    | h :: t -> h :: append t b

let rec len lst =
  case lst do
    | [] -> 0
    | _ :: t -> 1 + len t

let u = 1 :: []
let v = 2 :: 3 :: []
let w = append u v

let () = print_string (int_to_str (len w))
