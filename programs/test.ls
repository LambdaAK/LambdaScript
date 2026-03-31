let a = case (1, 2) do
  | (2, 1) -> "two"
  | (1, 2) -> "one"
  | _ -> "other"

let () = println a