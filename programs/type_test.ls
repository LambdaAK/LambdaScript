let () = println "Testing simple type"

type MyInt = int

let () = println "Simple type works!"

let x [MyInt] = 42
let () = println (int_to_str x)
