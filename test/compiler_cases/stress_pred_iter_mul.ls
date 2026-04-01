Expected:
405

Source:
let rec repeat_apply n x f =
  if n == 0 then x else repeat_apply (n - 1) (f x) f

let () = println (int_to_str (repeat_apply 4 5 (fn x -> x * 3)))
