Expected:
5
18
6
7
50
7
10
21
48
17
120
720
1
1
81
81
30
15

Source:
let rec unfold n acc f =
  if n == 0 then acc else unfold (n - 1) (f acc) f

let twice f x = f (f x)

let thrice f x = f (f (f x))

let compose f g x = f (g x)

let flip f x y = f y x

let sub a b = a - b

let rec tri n = if n <= 0 then 0 else n + tri (n - 1)

let rec iter n f x = if n == 0 then x else iter (n - 1) f (f x)

let id2 x y = x

let rec fact n = if n <= 1 then 1 else n * fact (n - 1)

let rec pow_acc b e acc =
  if e == 0 then acc else pow_acc b (e - 1) (acc * b)

let rec digital_sum n =
  if n < 10 then n else (n % 10) + digital_sum (n / 10)

let () = println (int_to_str (unfold 5 0 (fn x -> x + 1)))
let () = println (int_to_str (unfold 4 10 (fn x -> x + 2)))
let () = println (int_to_str (twice (fn x -> x + 3) 0))
let () = println (int_to_str (thrice (fn x -> x + 2) 1))
let () = println (int_to_str (compose (fn x -> x + 10) (fn x -> x * 2) 20))
let () = println (int_to_str (flip sub 30 37))
let () = println (int_to_str (tri 4))
let () = println (int_to_str (tri 6))
let () = println (int_to_str (iter 4 (fn x -> x * 2) 3))
let () = println (int_to_str (iter 3 (fn x -> x + 5) 2))
let () = println (int_to_str (fact 5))
let () = println (int_to_str (fact 6))
let () = println (int_to_str ((id2 1 2) + (id2 0 0)))
let () = println (int_to_str (if true then 1 else 0))
let () = println (int_to_str (pow_acc 3 4 1))
let () = println (int_to_str (pow_acc 9 2 1))
let () = println (int_to_str (digital_sum 6789))
let () = println (int_to_str (digital_sum 12345))
