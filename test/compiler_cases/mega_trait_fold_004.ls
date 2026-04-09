Expected:
52

Source:
inter Accumulate <a> {
  val combine : a -> a -> a
}

impl Accumulate for Int where
  let combine x y = x + y
end

let rec fold_range n =
  if n <= 0 then 0
  else combine n (fold_range (n - 1))

let rec repeat_add n step =
  if n <= 0 then 0
  else combine step (repeat_add (n - 1) step)

let () = print_string (int_to_str (combine (fold_range 7) (repeat_add 6 4)))
