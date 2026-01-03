let rec map f lst =
  case lst do
    | [] -> []
    | h :: t -> f h :: map f t

let rec filter p lst =
  case lst do
    | [] -> []
    | h :: t -> if p h then h :: filter p t else filter p t

let rec fold f acc lst =
  case lst do
    | [] -> acc
    | h :: t -> fold f (f acc h) t

let my_list = [1, 2, 3, 4, 5]

let doubled_list = map (fn x -> x * 2) my_list

let sum_of_doubled_list = fold (fn acc -> fn x -> acc + x) 0 doubled_list

let () = println (int_to_str sum_of_doubled_list)