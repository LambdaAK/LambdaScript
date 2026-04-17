let rec fold_left f acc lst =
switch lst =>
  | [] -> acc
  | h :: t -> fold_left f (f acc h) t

let () = println (int_to_str (fold_left (\acc -> \x -> acc + x) 0 [1 ... 100]))
