Expected:
1
10

Source:
let rec contains_zero lst =
  case lst do
    | [] -> false
    | h :: t -> if h == 0 then true else contains_zero t

let xs = [3, 4, 0, 9]

let ys = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

let () = println (int_to_str (if contains_zero xs then 1 else 0))
let () = println (int_to_str (if contains_zero ys then 0 else 10))
