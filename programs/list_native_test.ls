let rec sum (lst : [Int]) : Int = case lst do
  | [] -> 0
  | h :: t -> h + sum t

let () = println (int_to_str (sum [0 ... 4]))
let () = println (int_to_str (sum (0 :: 1 :: 2 :: 3 :: 4 :: [])))
