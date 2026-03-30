let foo f g x y = f (g x y)

let add x y = x + y

let inc x = x + 1

let partially_applied = foo inc add

let result = partially_applied 1 2

let () = println (int_to_str result)