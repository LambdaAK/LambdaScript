let my_list = [1, 2, 3, 4, 5]

let rec length_of_list lst =
  case lst do
    | [] -> 0
    | _ :: t -> 1 + length_of_list t

let rec double_each lst =
  case lst do
    | [] -> []
    | h :: t -> (2 * h) :: double_each t



