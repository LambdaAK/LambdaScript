Expected:
yes

Source:
let re = { x: 1, y: true }
let v = case re do
  | { x: 1 } -> true
  | _ -> false
let () =
  if v then println "yes" else println "no"
