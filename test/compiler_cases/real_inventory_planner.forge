Expected:
16
1990
2
1791

Source:
type Item = | Item of (String, Int, Int)

let qty item =
  case item do
  | Item (_, q, _) -> q

let price item =
  case item do
  | Item (_, _, p) -> p

let rec total_qty items =
  case items do
  | [] -> 0
  | h :: t -> qty h + total_qty t

let rec total_value items =
  case items do
  | [] -> 0
  | h :: t -> qty h * price h + total_value t

let rec low_stock_count threshold items =
  case items do
  | [] -> 0
  | h :: t ->
      if qty h <= threshold then 1 + low_stock_count threshold t
      else low_stock_count threshold t

let rec apply_discount percent items =
  case items do
  | [] -> []
  | Item (name, q, p) :: t ->
      Item (name, q, (p * (100 - percent)) / 100) :: apply_discount percent t

let items =
  [
    Item ("apple", 5, 120),
    Item ("cable", 2, 350),
    Item ("pen", 8, 30),
    Item ("notebook", 1, 450),
  ]

let () = print_string (int_to_str (total_qty items))
let () = print_string (int_to_str (total_value items))
let () = print_string (int_to_str (low_stock_count 2 items))
let () = print_string (int_to_str (total_value (apply_discount 10 items)))
