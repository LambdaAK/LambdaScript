Expected:
42

Source:
let const (x : int) (_ : int) : int = x
let () = println (int_to_str (const 42 0))
