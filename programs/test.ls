let rec fold_left f acc lst =
  case lst do
  | [] -> acc
  | h :: t -> fold_left f (f acc h) t

let rec fold_right f acc lst =
  case lst do
  | [] -> acc
  | h :: t -> fold_right f (f h acc) t

let rec map f lst =
  case lst do
  | [] -> []
  | h :: t -> f h :: map f t

let rec filter f lst =
  case lst do
  | [] -> []
  | h :: t -> if f h then h :: filter f t else filter f t

let rec fold f acc lst =
  case lst do
  | [] -> acc
  | h :: t -> fold f (f acc h) t

