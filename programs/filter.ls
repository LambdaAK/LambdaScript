let rec filter f arr =
  switch arr =>
  | [] -> []
  | h :: t -> if f h then h :: filter f t else filter f t

let () = println (int_to_str (fold_left (\acc -> \x -> acc + x) 0 (filter (\x -> x % 2 == 0) [1 ... 100])))