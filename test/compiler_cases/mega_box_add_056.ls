Expected:
30

Source:
inter Additive <a> {
  val add : a -> a -> a
}

impl Additive for Int where
  add x y = x + y
end

type Box<a> =
  | Box of a

impl Additive for Box<a> requires Additive<a> where
  add l r =
    case (l, r) do
    | (Box x, Box y) -> Box (add x y)
end

let rec fold_boxes xs =
  case xs do
  | [] -> Box 0
  | h :: t -> add h (fold_boxes t)

let () =
  case fold_boxes [Box 5, Box 14, Box 7, Box 4] do
  | Box n -> print_string (int_to_str n)
