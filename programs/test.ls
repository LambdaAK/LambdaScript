let a = case (1, 2) do
  | (0, 0) -> "zero"
  | (n, m) -> if n > 0 then "positive" else "negative"

let () = println a
