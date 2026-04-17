let rec fold_right f lst acc =
switch lst =>
  | [] -> acc
  | h :: t -> f h (fold_right f t acc)

let () = println (fold_right (\s -> \acc -> s ^ " " ^ acc) ["hello", "world", "lambda", "script"] "")

