Expected:
1
2
3
2
4
6
3
6
9

Source:
let rec print_list lst =
  case lst do
  | [] -> ()
  | h :: t -> let () = println (int_to_str h) in print_list t

let ns = [1, 2, 3]
let table = [x * y | x <- ns, y <- ns]
let () = print_list table
