inter Show<a> {
  val show : a -> string
}

impl Show for Int where
  show = int_to_str
end

impl Show for Bool where
  show = fn b -> if b then "true" else "false"
end

impl Show for [a] where
  show = fn lst -> ""
end
