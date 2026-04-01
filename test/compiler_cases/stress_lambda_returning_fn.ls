Expected:
72

Source:
let mk_add n = fn x -> x + n

let f = mk_add 40

let () = println (int_to_str (f 32))
