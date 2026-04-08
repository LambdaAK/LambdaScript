Expected:
5

Source:
inter Monoid <a> {
  val mappend : a -> a -> a
  val mempty : a
}

impl Monoid for [Int] where
  let rec mappend xs ys =
    let rec step zs =
      case zs do
      | [] -> ys
      | h :: t -> h :: mappend t ys
    in
    step xs

  let mempty = []
end

let rec len xs =
  case xs do
  | [] -> 0
  | _ :: t -> 1 + len t

let () = print_string (int_to_str (len (mappend [1,2] [3,4,5])))
