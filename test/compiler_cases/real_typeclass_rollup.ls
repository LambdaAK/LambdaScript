Expected:
10
5
15

Source:
inter Accumulate <a> {
  val combine : a -> a -> a
  val zero : a
}

impl Accumulate for Int where
  let combine x y = x + y
  let zero = 0
end

impl Accumulate for [Int] where
  let rec combine xs ys =
    case xs do
    | [] -> ys
    | h :: t -> h :: combine t ys
  let zero = []
end

let rec fold_values<Accumulate a> xs =
  case xs do
  | [] -> zero
  | h :: t -> combine h (fold_values t)

let rec len xs =
  case xs do
  | [] -> 0
  | _ :: t -> 1 + len t

let rec list_append xs ys =
  case xs do
  | [] -> ys
  | h :: t -> h :: list_append t ys

let total = fold_values [1,2,3,4]
let joined = fold_values [[1,2], [3], [4,5]]
let chained = combine total (len joined)

let () = print_string (int_to_str total)
let () = print_string (int_to_str (len joined))
let () = print_string (int_to_str chained)
