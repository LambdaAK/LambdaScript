let rec map f lst =
  case lst do
    | [] -> []
    | h :: t -> f h :: map f t

let rec print_list lst =
  case lst do
    | [] -> ()
    | h :: t ->
      let () = println (int_to_str h) in
        print_list t

let rec extend_list lst =
  case lst do
    | [] -> []
    | h :: t -> h :: h :: extend_list t

let id x = x

let my_lst = [1, 2, 3, 4, 5]

let result = extend_list (map id my_lst)

let () = print_list result