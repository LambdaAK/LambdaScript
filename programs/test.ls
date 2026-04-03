inter Show<'a> {
  val show : 'a -> string
}

inter Monoid<'a> {
  val mappend : 'a -> 'a -> 'a,
  val mempty : 'a
}

impl Monoid for int {
  let mappend x y = x + y
  let mempty = 0
}

impl Monoid for bool {
  let mappend x y = x && y
  let mempty = false
}
