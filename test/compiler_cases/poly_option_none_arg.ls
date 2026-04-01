Expected:
0

Source:
type Option<'a> = | None | Some of 'a

let extract_default default option =
  case option do
    | None -> default
    | Some v -> v

let () = println (int_to_str (extract_default 0 None))
