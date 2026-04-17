Expected:
15
8

Source:
inter Semigroup <a> {
  val sappend : a -> a -> a
}

impl Semigroup for Int where
  sappend x y = x + y
end

let add7 x = sappend 7 x

let () = print_string (int_to_str (add7 8))
let () = print_string (int_to_str (add7 1))
