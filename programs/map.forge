let rec map f arr =
switch arr =>
  | [] -> []
  | h :: t -> f h :: map f t

let () = println (int_to_str (fold_left (\acc -> \x -> acc + x) 0 (map (\x -> x * 2) [1 ... 100])))