// poly let rec + HO (monomorph sees local f)
let rec deep n f x = if n == 0 then x else deep (n - 1) f (f x)
let inc t = t + 1
// poly let rec … and …
let rec wrap x = x
and unwrap y = wrap y
let () = println (int_to_str (deep 2 inc 0 + unwrap 1))
