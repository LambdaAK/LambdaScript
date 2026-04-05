inter Show<a> {
  val show : a -> string
}

impl Show for Int where
  let show = int_to_str
end

impl Show for Bool where
  let show = fn b -> if b then "true" else "false"
end

impl Show for [a] where
  let show = fn lst -> ""
end
