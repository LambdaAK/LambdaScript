Expected:
2
4
6
8
10

Source:
let rec print_list lst =
  case lst do
  | [] -> ()
  | h :: t -> let () = println (int_to_str h) in print_list t

let xs = [1, 2, 3, 4, 5]
let doubled = [x * 2 | x <- xs]
let () = print_list doubled
