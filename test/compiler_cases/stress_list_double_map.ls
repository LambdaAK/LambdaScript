Expected:
3

Source:
let rec map_add1 lst =
  case lst do
    | [] -> []
    | h :: t -> (h + 1) :: map_add1 t

let xs = [0, 1, 2]

let ys = map_add1 (map_add1 (map_add1 xs))

let rec len lst =
  case lst do
    | [] -> 0
    | _ :: r -> 1 + len r

let () = println (int_to_str (len ys))
