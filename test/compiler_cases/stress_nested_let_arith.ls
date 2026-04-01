Expected:
36

Source:
let a =
  let b = 10 in
  let c = b * 2 in
  let d = c + 4 in
    d + d / 2

let () = println (int_to_str a)
