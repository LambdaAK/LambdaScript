// Haskell-style typeclasses: Show, Eq, Semigroup, Monoid, Functor (Option).
// Compiles with compile_forge and runs in the interpreter / REPL.
// Native-list Functor [u] is demonstrated in programs/typeclass_functor_native_list.ls
// (CEval + typechecker support it; the LLVM monomorph pass still rejects that combo).

inter Show <a> {
  val show : a -> string
}

impl Show for Int {
  let show = int_to_str
}

impl Show for Bool {
  let show = fn b -> if b then "true" else "false"
}

impl Show for [a] {
  let show xs =
    case xs do
    | [] -> "[]"
    | _ :: _ -> "[?]"
}

inter Eq <a> {
  val eq : a -> a -> bool
}

impl Eq for Int {
  let eq x y = x == y
}

impl Eq for Bool {
  let eq x y = if x then y else if y then false else true
}

inter Semigroup <a> {
  val sappend : a -> a -> a
}

impl Semigroup for Int {
  let sappend x y = x + y
}

impl Semigroup for [a] {
  let rec sappend xs ys =
    case xs do
    | [] -> ys
    | h :: t -> h :: sappend t ys
}

inter Monoid <a> {
  val mappend : a -> a -> a
  val mempty : a
}

impl Monoid for [a] {
  let rec mappend x y =
    case x do
    | [] -> y
    | h :: t -> h :: mappend t y

  let mempty = []
}

type Option<a> =
  | None
  | Some of a

inter Functor <f> {
  val fmap : (a -> b) -> f<a> -> f<b>
}

impl Functor for Option {
  let fmap g x =
    case x do
    | None -> None
    | Some v -> Some (g v)
}

let rec len xs =
  case xs do
  | [] -> 0
  | _ :: t -> 1 + len t

let () = println (show 42)
let () = println (show true)
let () = println (show (1 :: 2 :: []))
let () = println (int_to_str (if eq 5 5 then 1 else 0))
let () = println (int_to_str (if eq true false then 1 else 0))
let () = println (int_to_str (sappend 8 34))
let () = println (int_to_str (len (mappend (1 :: []) (2 :: 3 :: []))))
let () =
  println
    (int_to_str
       (case fmap (fn x -> x * 2) (Some 21) do
        | Some n -> n
        | None -> 0))
