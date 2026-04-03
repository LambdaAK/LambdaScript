inter Show<'a> {
  show : 'a -> string
}

impl Show = int {
  show = int_to_str
}

impl Show = bool {
  show = fn b -> if b then "true" else "false"
}

impl Show = ['a] {
  show = fn lst -> ""
}
