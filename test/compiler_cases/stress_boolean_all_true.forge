Expected:
1

Source:
let rec all_pos lst =
  case lst do
    | [] -> true
    | h :: t -> if h > 0 then all_pos t else false

let xs = [1, 2, 3, 4, 9]

let () = print_string (int_to_str (if all_pos xs then 1 else 0))
