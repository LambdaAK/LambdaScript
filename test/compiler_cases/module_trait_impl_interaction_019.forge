Expected:
42

Source:
let render = fn x -> x + 100

mod M where
  trait Render<a> where
    val render_tc : a -> Int
  end

  impl Render for Int where
    render_tc x = x + 1
  end
end

use M
let out <Render Int> x = render_tc x
let () = print_string (int_to_str (out 41))
