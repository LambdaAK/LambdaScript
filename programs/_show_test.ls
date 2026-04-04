inter Show<a> {
  val show : a -> string
}

impl Show for Int {
  let show = int_to_str
}

impl Show for Bool {
  let show = fn b -> if b then "true" else "false"
}

impl Show for [a] {
  let show = fn lst -> ""
}
