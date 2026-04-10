Expected:
eq

Source:
mod M where
  trait EqLike<a> where
    val (==) : a -> a -> Bool
  end

  impl EqLike for Int where
    (==) x y = x == y
  end

  trait ShowEq<a> where
    val showeq : a -> a -> String
  end

  impl ShowEq for Int requires EqLike<Int> where
    showeq x y = if (==) x y then "eq" else "neq"
  end

  let out = showeq 4 4
end

let () = print_string M.out
