Expected:
4
10

Source:
inter Monoid <a> {
  val mappend : a -> a -> a
  val mempty : a
}

impl Monoid for [Int] where
  let rec mappend xs ys =
    let rec append_rest rest =
      case rest do
      | [] -> ys
      | h :: t -> h :: append_rest t
    in
    append_rest xs

  let mempty = []
end

let rec len xs =
  case xs do
  | [] -> 0
  | _ :: t -> 1 + len t

let rec sum xs =
  case xs do
  | [] -> 0
  | h :: t -> h + sum t

let merged = mappend [1,2] [3,4]
let () = print_string (int_to_str (len merged))
let () = print_string (int_to_str (sum merged))
