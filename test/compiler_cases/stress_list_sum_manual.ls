Expected:
78

Source:
let rec sum lst =
  case lst do
    | [] -> 0
    | h :: t -> h + sum t

let xs = [3, 10, 5, 20, 40]

let () = println (int_to_str (sum xs))
