Expected:
30
2
18
1

Source:
type Reading = | Reading of (String, Int)

let value reading =
  case reading do
  | Reading (_, v) -> v

let rec max_reading readings =
  case readings do
  | [] -> 0
  | h :: t ->
      let m = max_reading t in
      if value h > m then value h else m

let rec count_alerts threshold readings =
  case readings do
  | [] -> 0
  | h :: t ->
      if value h > threshold then 1 + count_alerts threshold t
      else count_alerts threshold t

let rec sum_values readings =
  case readings do
  | [] -> 0
  | h :: t -> value h + sum_values t

let rec len readings =
  case readings do
  | [] -> 0
  | _ :: t -> 1 + len t

let average readings = sum_values readings / len readings

let readings =
  [
    Reading ("north", 12),
    Reading ("east", 9),
    Reading ("south", 30),
    Reading ("west", 18),
    Reading ("core", 25),
  ]

let () = print_string (int_to_str (max_reading readings))
let () = print_string (int_to_str (count_alerts 20 readings))
let () = print_string (int_to_str (average readings))
let () = print_string (int_to_str (if count_alerts 25 readings == 1 then 1 else 0))
