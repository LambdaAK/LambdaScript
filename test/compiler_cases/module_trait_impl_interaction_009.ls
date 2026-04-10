Expected:
1

Source:
mod EqM where
  trait EqLike<a> where
    val (==) : a -> a -> Bool
    val (!=) : a -> a -> Bool
    let (!=) x y = if (==) x y then false else true
  end

  impl EqLike for Int where
    (==) x y = x == y
  end
end

use EqM
let out = if (!=) 2 3 then 1 else 0
let () = print_string (int_to_str out)
