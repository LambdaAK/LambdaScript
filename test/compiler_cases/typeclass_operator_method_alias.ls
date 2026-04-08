Expected:
1
0

Source:
inter EqLike <a> {
  val (==) : a -> a -> Bool
  val (!=) : a -> a -> Bool
}

impl EqLike for Int where
  let (==) x y = x == y
  let (!=) x y = if x == y then false else true
end

let () = print_string (int_to_str (if (!=) 1 2 then 1 else 0))
let () = print_string (int_to_str (if (!=) 2 2 then 1 else 0))
