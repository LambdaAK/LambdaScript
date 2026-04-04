Expected:
3
10
8

Source:
let r : {a: Int, b: Int, k: Int} = {k: 3, a: 1, b: 2}
let s : {a: Int, b: Int, k: Int} = { r with k = 10 }
let sum_ab (p : {a: Int, b: Int}) : Int = p.a + p.b

let bump (q : {a: Int, b: Int}) : Int =
  case q do
  | {a: xa, b: yb} -> xa + yb + 1

let () = println (int_to_str (sum_ab {a: r.a, b: r.b}))
let () = println (int_to_str s.k)
let () = println (int_to_str (bump ({b: 3, a: 4})))
