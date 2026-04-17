Expected:
42

Source:
mod Logic where
  trait EqLike<a> where
    val (==) : a -> a -> Bool
    val (!=) : a -> a -> Bool
    let (!=) x y = if (==) x y then false else true
  end

  impl EqLike for Bool where
    (==) x y = if x then y else if y then false else true
  end

  let out = if (!=) true false then 42 else 0
end

let () = print_string (int_to_str Logic.out)
