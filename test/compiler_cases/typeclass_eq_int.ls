Expected:
1
0

Source:
inter Eq <a> {
  val eq : a -> a -> bool
}

impl Eq for Int {
  let eq x y = x == y
}

let () = println (int_to_str (if eq 7 7 then 1 else 0))
let () = println (int_to_str (if eq 1 2 then 1 else 0))
