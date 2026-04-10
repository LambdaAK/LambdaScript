Expected:
1
0

Source:
inter Eq <a> {
  val eq : a -> a -> bool
}

impl Eq for Int where
  eq x y = x == y
end

let () = print_string (int_to_str (if eq 7 7 then 1 else 0))
let () = print_string (int_to_str (if eq 1 2 then 1 else 0))
