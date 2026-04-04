// Functor instance for native lists [u] — run with interpreter or REPL.
// (LLVM compilation can fail on this path; see comment in haskell_style_typeclasses.ls.)

inter Functor <f> {
  val fmap : (a -> b) -> f<a> -> f<b>
}

impl Functor for [u] {
  let rec fmap g xs =
    case xs do
    | [] -> []
    | h :: t -> g h :: fmap g t
}

let () =
  println
    (int_to_str
       (case fmap (fn x -> x + 1) (1 :: 2 :: 3 :: []) do
        | h :: _ -> h
        | [] -> 0))
