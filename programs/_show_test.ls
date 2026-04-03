inter Show<'a> {
  val show : 'a -> string
}

impl Show for int {
  let show = int_to_str
}

impl Show for bool {
  let show = fn b -> if b then "true" else "false"
}

impl Show for ['a] {
  let show = fn lst -> ""
}
