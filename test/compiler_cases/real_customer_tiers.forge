Expected:
2
2590
5

Source:
type Customer = | Customer of (String, Int)

let points customer =
  case customer do
  | Customer (_, p) -> p

let with_bonus bonus customer =
  case customer do
  | Customer (name, p) -> Customer (name, p + bonus)

let rec map_bonus bonus customers =
  case customers do
  | [] -> []
  | h :: t -> with_bonus bonus h :: map_bonus bonus t

let rec total_points customers =
  case customers do
  | [] -> 0
  | h :: t -> points h + total_points t

let rec gold_count customers =
  case customers do
  | [] -> 0
  | h :: t ->
      if points h >= 700 then 1 + gold_count t
      else gold_count t

let customers =
  [
    Customer ("ana", 120),
    Customer ("ben", 450),
    Customer ("cy", 820),
    Customer ("dia", 650),
    Customer ("eli", 300),
  ]

let updated = map_bonus 50 customers

let () = print_string (int_to_str (gold_count updated))
let () = print_string (int_to_str (total_points updated))
let () = print_string (int_to_str (if gold_count updated == 2 then 5 else 0))
