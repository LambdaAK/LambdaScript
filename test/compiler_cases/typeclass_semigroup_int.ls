Expected:
50

Source:
inter Semigroup <a> {
  val sappend : a -> a -> a
}

impl Semigroup for Int {
  let sappend x y = x + y
}

let () = println (int_to_str (sappend 20 30))
