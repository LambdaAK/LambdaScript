Expected:
111
211
121
221
112
212
122
222

Source:
let rec print_list lst =
  case lst do
  | [] -> ()
  | h :: t -> let () = println (int_to_str h) in print_list t

let xs = [1, 2]
let ys = [10, 20]
let zs = [100, 200]
let result = [x + y + z | x <- xs, y <- ys, z <- zs]
let () = print_list result
