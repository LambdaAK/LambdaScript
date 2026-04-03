inter Monoid<'a> {
  val mappend : 'a -> 'a -> 'a
  val mempty : 'a
}

impl Monoid for [int] {
  let rec mappend x y =
    case x do
      | [] -> y
      | h :: t -> h :: mappend t y

  let mempty = []
}
