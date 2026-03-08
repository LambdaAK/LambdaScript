type Point = {
  x: int,
  y: int
}

let p1 : Point = {x: 10, y: 20}
let p2 = { p1 with x = 100 }
let p3 = { p2 with y = 50 }
let p4 = { p1 with x = 5, y = 15 }

let () = println (int_to_str p2.x)
let () = println (int_to_str p2.y)
let () = println (int_to_str p3.x)
let () = println (int_to_str p3.y)
let () = println (int_to_str p4.x)
let () = println (int_to_str p4.y)
