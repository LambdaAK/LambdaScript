Expected:
42

Source:
mod Op where
  trait Semigroup<a> where
    val (++) : a -> a -> a
  end

  impl Semigroup for Int where
    (++) x y = x + y
  end
end

use Op
let join <Semigroup Int> x y = (++) x y
let () = print_string (int_to_str (join 19 23))
