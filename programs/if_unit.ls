let () = if false then println "yes" else (
  if true then println "yes1" else println "no"
)

let x =
    let a = 1 in
    let b = 2 in
    let c = 3 in
      let d = 4 in
        a + b + c + d

let () = println (int_to_str x)