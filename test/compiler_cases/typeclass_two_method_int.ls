Expected:
22

Source:
inter Numish <a> {
  val add1 : a -> a
  val double : a -> a
}

impl Numish for Int {
  let add1 x = x + 1
  let double x = x * 2
}

let () = println (int_to_str (double (add1 10)))
