Expected:
4

Source:
let rec append a b =
  case a do
    | [] -> b
    | h :: t -> h :: append t b

let rec len lst =
  case lst do
    | [] -> 0
    | _ :: t -> 1 + len t

let xs = 1 :: 2 :: []
let () = println (int_to_str (len (append xs xs)))
