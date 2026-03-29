let foo () () = true

let add x y bb =
    if bb then x + y
    else x - y

let add_two = add 1
let add_three = add_two 2
let res = add_three (foo () ())

let () = println (int_to_str res)