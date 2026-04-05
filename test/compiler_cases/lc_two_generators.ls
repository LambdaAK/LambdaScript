Expected:
11
21
12
22
13
23

Source:
let rec print_list lst =
  case lst do
  | [] -> ()
  | h :: t -> let () = print_string (int_to_str h) in print_list t

let xs = [1, 2, 3]
let ys = [10, 20]
let pairs = [x + y | x <- xs, y <- ys]
let () = print_list pairs
