Expected:
0

Source:
type Option<a> = | None | Some of a

let none = None

let extract_default default option =
  case option do
    | None -> default
    | Some v -> v

let () = print_string (int_to_str (extract_default 0 none))
