Expected:
99

Source:
let rec max_list lst =
  case lst do
    | [] -> 0 - 1
    | h :: t ->
      case t do
        | [] -> h
        | _ -> if h > max_list t then h else max_list t

let xs = [3, 99, 7, 12, 45, 8]

let () = println (int_to_str (max_list xs))
