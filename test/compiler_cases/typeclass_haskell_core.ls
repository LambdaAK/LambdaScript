Expected:
42
true
[?]
1
0
42
3
42

Source:
inter Show <a> {
  val show : a -> string
}

impl Show for Int where
  show = int_to_str
end

impl Show for Bool where
  show = fn b -> if b then "true" else "false"
end

impl Show for [a] where
  show xs =
    case xs do
    | [] -> "[]"
    | _ :: _ -> "[?]"
end

inter Eq <a> {
  val eq : a -> a -> bool
}

impl Eq for Int where
  eq x y = x == y
end

impl Eq for Bool where
  eq x y = if x then y else if y then false else true
end

inter Semigroup <a> {
  val sappend : a -> a -> a
}

impl Semigroup for Int where
  sappend x y = x + y
end

impl Semigroup for [a] where
  sappend xs ys =
    case xs do
    | [] -> ys
    | h :: t -> h :: sappend t ys
end

inter Monoid <a> {
  val mappend : a -> a -> a
  val mempty : a
}

impl Monoid for [a] where
  mappend x y =
    case x do
    | [] -> y
    | h :: t -> h :: mappend t y

  ,
  mempty = []
end

type Option<a> =
  | None
  | Some of a

inter Functor <f> {
  val fmap : (a -> b) -> f<a> -> f<b>
}

impl Functor for Option where
  fmap g x =
    case x do
    | None -> None
    | Some v -> Some (g v)
end

let rec len xs =
  case xs do
  | [] -> 0
  | _ :: t -> 1 + len t

let () = print_string (show 42)
let () = print_string (show true)
let () = print_string (show (1 :: 2 :: []))
let () = print_string (int_to_str (if eq 5 5 then 1 else 0))
let () = print_string (int_to_str (if eq true false then 1 else 0))
let () = print_string (int_to_str (sappend 8 34))
let () = print_string (int_to_str (len (mappend (1 :: []) (2 :: 3 :: []))))
let () =
  print_string
    (int_to_str
       (case fmap (fn x -> x * 2) (Some 21) do
        | Some n -> n
        | None -> 0))
