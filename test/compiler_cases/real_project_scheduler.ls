Expected:
18
2
26

Source:
type Task = | Task of (String, Int, Bool)

let hours task =
  case task do
  | Task (_, h, _) -> h

let is_done task =
  case task do
  | Task (_, _, d) -> d

let rec remaining_hours tasks =
  case tasks do
  | [] -> 0
  | h :: t ->
      if is_done h then remaining_hours t
      else hours h + remaining_hours t

let rec done_count tasks =
  case tasks do
  | [] -> 0
  | h :: t -> (if is_done h then 1 else 0) + done_count t

let rec total_hours tasks =
  case tasks do
  | [] -> 0
  | h :: t -> hours h + total_hours t

let sprint =
  [
    Task ("design", 5, true),
    Task ("build", 12, false),
    Task ("test", 6, false),
    Task ("docs", 3, true),
  ]

let () = print_string (int_to_str (remaining_hours sprint))
let () = print_string (int_to_str (done_count sprint))
let () = print_string (int_to_str (total_hours sprint))
