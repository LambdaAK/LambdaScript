Expected:
50

Source:
inter Semigroup <a> {
  val sappend : a -> a -> a
}

impl Semigroup for Int where
  let sappend x y = x + y
end

let () = print_string (int_to_str (sappend 20 30))
