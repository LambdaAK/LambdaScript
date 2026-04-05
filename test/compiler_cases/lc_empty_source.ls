Expected:
3

Source:
let rec len lst = case lst do | [] -> 0 | _ :: t -> 1 + len t

let all_items = [1, 2, 3]
let none_filtered = [x | x <- all_items]
let () = print_string (int_to_str (len none_filtered))
