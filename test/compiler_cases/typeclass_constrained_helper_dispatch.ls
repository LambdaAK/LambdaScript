Expected:
3

Source:
inter Monoid <a> {
  val mappend : a -> a -> a
  val mempty : a
}

impl Monoid for [Int] where
  let rec mappend xs ys =
    case xs do
    | [] -> ys
    | h :: t -> h :: mappend t ys
  let mempty = []
end

let combine (x : [Int]) (y : [Int]) = mappend x y

let rec len xs =
  case xs do
  | [] -> 0
  | _ :: t -> 1 + len t

let merged = combine [1,2] [3]
let () = print_string (int_to_str (len merged))
