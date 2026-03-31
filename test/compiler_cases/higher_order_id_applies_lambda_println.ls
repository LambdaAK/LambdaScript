Expected:
ok

Source:
let id x = x

let g = id (fn s -> println s)

let () = g "ok"
