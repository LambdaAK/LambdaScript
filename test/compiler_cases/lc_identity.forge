Expected:
1
2
3

Source:
let rec print_list lst =
  case lst do
  | [] -> ()
  | h :: t -> let () = print_string (int_to_str h) in print_list t

let xs = [1, 2, 3]
let ys = [x | x <- xs]
let () = print_list ys
