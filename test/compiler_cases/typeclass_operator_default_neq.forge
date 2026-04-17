Expected:
1
0

Source:
inter EqLike <a> {
  val (==) : a -> a -> Bool
  val (!=) : a -> a -> Bool
  let (!=) x y = if (==) x y then false else true
}

impl EqLike for Int where
  (==) x y = x == y
end

let () = print_string (int_to_str (if (!=) 2 3 then 1 else 0))
let () = print_string (int_to_str (if (!=) 4 4 then 1 else 0))
