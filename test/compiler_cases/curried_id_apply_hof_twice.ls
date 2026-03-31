Expected:
1

Source:
let id x = x

let f x = x

let () = println (int_to_str ((id f) 1))
