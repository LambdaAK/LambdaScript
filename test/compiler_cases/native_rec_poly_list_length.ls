Expected:
3
0

Source:
let rec length lst =
  case lst do
    | [] -> 0
    | _ :: t -> 1 + length t

let () = println (int_to_str (length (1 :: 2 :: 3 :: [])))
let () = println (int_to_str (length []))
