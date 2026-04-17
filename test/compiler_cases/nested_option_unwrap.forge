Expected:
7

Source:
type Option<a> = | None | Some of a

let unwrap_or d o =
  case o do
    | None -> d
    | Some v -> v

let inner = Some 7
let outer = Some inner

let () =
  print_string (int_to_str (unwrap_or 0 (unwrap_or inner outer)))
