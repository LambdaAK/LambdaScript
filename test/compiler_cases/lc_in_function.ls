Expected:
0
1
4
9
16

Source:
let rec print_list lst =
  case lst do
  | [] -> ()
  | h :: t -> let () = println (int_to_str h) in print_list t

let squares lst = [x * x | x <- lst]

let () = print_list (squares [0, 1, 2, 3, 4])
