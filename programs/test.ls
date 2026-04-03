inter Show<'a> {
  show : 'a -> string
}

inter Monoid<'a> {
  mappend : 'a -> 'a -> 'a,
  mempty : 'a
}

impl Monoid = int {
  mappend = fn x -> fn y -> x + y,
  mempty = 0
}

impl Monoid = bool {
  mappend = fn x -> fn y -> x && y,
  mempty = false
}
