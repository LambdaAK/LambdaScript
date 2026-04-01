Expected:
3
10
8

Source:
let r : {a: int, b: int, k: int} = {k: 3, a: 1, b: 2}
let s : {a: int, b: int, k: int} = { r with k = 10 }
let sum_ab (p : {a: int, b: int}) : int = p.a + p.b

let bump (q : {a: int, b: int}) : int =
  case q do
  | {a: xa, b: yb} -> xa + yb + 1

let () = println (int_to_str (sum_ab {a: r.a, b: r.b}))
let () = println (int_to_str s.k)
let () = println (int_to_str (bump ({b: 3, a: 4})))
