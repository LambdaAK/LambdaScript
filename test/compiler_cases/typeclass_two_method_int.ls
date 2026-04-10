Expected:
22

Source:
inter Numish <a> {
  val add1 : a -> a
  val double : a -> a
}

impl Numish for Int where
  add1 x = x + 1
  ,
  double x = x * 2
end

let () = print_string (int_to_str (double (add1 10)))
