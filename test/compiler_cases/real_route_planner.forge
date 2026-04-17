Expected:
47
20
2
3

Source:
type Stop = | Stop of (String, Int)

let leg stop =
  case stop do
  | Stop (_, d) -> d

let rec route_distance stops =
  case stops do
  | [] -> 0
  | h :: t -> leg h + route_distance t

let rec max_leg stops =
  case stops do
  | [] -> 0
  | h :: t ->
      let d = leg h in
      let m = max_leg t in
      if d > m then d else m

let rec count_over limit stops =
  case stops do
  | [] -> 0
  | h :: t ->
      if leg h > limit then 1 + count_over limit t
      else count_over limit t

let route =
  [
    Stop ("A", 5),
    Stop ("B", 12),
    Stop ("C", 7),
    Stop ("D", 20),
    Stop ("E", 3),
  ]

let () = print_string (int_to_str (route_distance route))
let () = print_string (int_to_str (max_leg route))
let () = print_string (int_to_str (count_over 10 route))
let () = print_string (int_to_str (count_over 5 route))
