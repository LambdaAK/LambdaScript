Expected:
11:3

Source:
type Task =
  | Task of (Int, Bool)

let hours task =
  case task do
  | Task (h, _) -> h

let is_done task =
  case task do
  | Task (_, d) -> d

let rec remaining tasks =
  case tasks do
  | [] -> 0
  | h :: t ->
      if is_done h then remaining t
      else hours h + remaining t

let rec finished tasks =
  case tasks do
  | [] -> 0
  | h :: t -> (if is_done h then 1 else 0) + finished t

let tasks =
  [
    Task (4, false),
    Task (7, false),
    Task (10, true),
    Task (6, true),
    Task (5, true),
  ]

let () =
  print_string
    (int_to_str (remaining tasks) ^ ":" ^ int_to_str (finished tasks))
