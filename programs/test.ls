let re = {
  x : 1,
  y : 2,
  z : 3,
  f : fn x -> x + 1,
  g : fn x -> x + 2
}

let x = re.x
let y = re.y
let z = re.z
let f = re.f
let g = re.g

let () = println (int_to_str x)
let () = println (int_to_str y)
let () = println (int_to_str z)
let () = println (int_to_str (f (f 1)))
let () = println (int_to_str (g (g 1)))