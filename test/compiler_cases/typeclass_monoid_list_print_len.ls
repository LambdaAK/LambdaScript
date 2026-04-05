Expected:
3

Source:
inter Monoid <a> {
  val mappend : a -> a -> a
  val mempty : a
}

impl Monoid for [a] where
  let rec mappend x y =
    case x do
    | [] -> y
    | h :: t -> h :: mappend t y

  let mempty = []
end

let rec len xs =
  case xs do
  | [] -> 0
  | _ :: t -> 1 + len t

let () = print_string (int_to_str (len (mappend (1 :: []) (2 :: 3 :: []))))
