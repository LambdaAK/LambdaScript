let re = {
  x : 1,
  y: true
}

let v = case re do
  | {x : 1} -> 1
  | {x : 1, y: true} -> 2
  | _ -> 3


let () = println (int_to_str v)

