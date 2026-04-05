Expected:
5

Source:
let rec head_def d lst =
  case lst do
    | [] -> d
    | h :: _ -> h

let () = println (int_to_str (head_def 0 (5 :: [])))
