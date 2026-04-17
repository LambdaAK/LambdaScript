Expected:
2
700
29

Source:
type Lead =
  | Cold
  | Warm
  | Qualified
  | Won of Int

let rec won_count leads =
  case leads do
  | [] -> 0
  | h :: t ->
      case h do
      | Won _ -> 1 + won_count t
      | _ -> won_count t

let rec won_revenue leads =
  case leads do
  | [] -> 0
  | h :: t ->
      case h do
      | Won amount -> amount + won_revenue t
      | _ -> won_revenue t

let lead_score lead =
  case lead do
  | Cold -> 0
  | Warm -> 2
  | Qualified -> 5
  | Won _ -> 10

let rec pipeline_score leads =
  case leads do
  | [] -> 0
  | h :: t -> lead_score h + pipeline_score t

let leads = [Cold, Warm, Won 500, Qualified, Won 200, Warm]

let () = print_string (int_to_str (won_count leads))
let () = print_string (int_to_str (won_revenue leads))
let () = print_string (int_to_str (pipeline_score leads))
