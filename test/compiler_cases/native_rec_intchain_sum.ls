Expected:
30

Source:
type rec IntChain = | End | Link of (int, IntChain)

let rec sum c =
  case c do
    | End -> 0
    | Link (n, rest) -> n + sum rest

let () = println (int_to_str (sum (Link (10, Link (20, End)))))
