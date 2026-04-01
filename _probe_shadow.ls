let id x = x
let () = println (int_to_str (
  let id y = y + 1 in
  id 4
))
