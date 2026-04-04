Expected:
7
10
1
7
1
42
41
13
6
12
30
25
43
7
100
99
7
10
42
12
42
5
12
6
101
7
1
3
99
5
edge
z

Source:
// Polymorphism, currying, eta, and anonymous fn edge cases (native compiler).

let id x = x

let bump x = x + 1

let add1 x = x + 1

let add a b = a + b

let run_bin g = g 3 + g 4

let use_unary h = h 1

let fst2 x y = x

let snd2 x y = y

let const_ x _ = x

let apply_ f x = f x

let twice_n x = x * 2

let square x = x * x

let compose_ f g x = f (g x)

let flip_ f x y = f y x

let sub a b = a - b

let both f x = f x + f x

// Typed monomorphic identity (compose spine)
let cmono f g x = f (g x)
let id_int (z : Int) : Int = z

let succ1 x = x + 1

let wrap_mul x = fn y -> x * y + x

let add2 u v = u + v

let lift_dup f = fn x -> f x x

let pipe x g = g x

let indirect f x = let u = f in u x

// fff for repeated poly application f(f(f(12)))
let fff x = x

let () = println (int_to_str (id 7))
let () = println (int_to_str ((id bump) ((id bump) 8)))

let mono_f x = x
let () = println (int_to_str ((id mono_f) 1))

let () = println (int_to_str (run_bin id))
let () = println (int_to_str (use_unary id))

let () = println (int_to_str ((fn p -> fn q -> p - q) 50 8))
let () = println (int_to_str ((fn t -> t + 1) ((fn u -> u * 2) 20)))
let () = println (int_to_str ((fn k -> k 10) (fn n -> n + 3)))
let () = println (int_to_str ((fn a -> fn b -> fn c -> a + b + c) 1 2 3))

let () = println (int_to_str ((wrap_mul 2) 5))
let () = println (int_to_str (lift_dup add2 15))
let () = println (int_to_str (compose_ square add1 4))
let () = println (int_to_str (cmono id_int id_int 43))
let () = println (int_to_str (const_ (const_ 7 99) 3))
let () = println (int_to_str (fst2 (fst2 (fst2 100 1) 2) 3))
let () = println (int_to_str (snd2 0 99))
let () = println (int_to_str (flip_ sub 3 10))
let () = println (int_to_str ((id succ1) 9))
let () = println (int_to_str (apply_ twice_n 21))
let () = println (int_to_str (both (fn z -> z + 1) 5))
let () = println (int_to_str (pipe 6 (fn n -> n * 7)))
let () = println (int_to_str (indirect add1 4))
let () = println (int_to_str (fff (fff (fff 12))))
let () = println (int_to_str ((id (id add1)) 5))

let hi = id (fn x -> x + 1)
let () = println (int_to_str (hi 100))

let () = println (int_to_str (fst2 (snd2 100 7) 0))

let () = println (int_to_str (if const_ true false then 1 else 0))

let peel2 f = f 1 2
let () = println (int_to_str (peel2 (flip_ add)))
let () = println (int_to_str ((id id) 99))
let () =
  println (int_to_str ((fn x -> fn f -> f (f x)) 3 (fn u -> u + 1)))

let () = println (id "edge")
let () =
  let g = id println in
  g "z"
