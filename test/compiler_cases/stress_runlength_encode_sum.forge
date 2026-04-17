Expected:
15

Source:
let rec run_sum xs prev acc =
  case xs do
    | [] -> acc
    | h :: t ->
      if h == prev then run_sum t prev acc else run_sum t h (acc + h)

let xs = [1, 1, 2, 2, 2, 3, 4, 4, 5]

let () = print_string (int_to_str (run_sum xs 999 0))
