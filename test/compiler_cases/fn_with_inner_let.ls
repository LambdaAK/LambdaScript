Expected:
25

Source:
let compute n =
  let doubled = n * 2 in
  let tripled = n * 3 in
  doubled + tripled
let () = println (int_to_str (compute 5))
