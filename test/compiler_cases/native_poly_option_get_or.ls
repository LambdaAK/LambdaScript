Expected:
110

Source:
type Option<'a> = | None | Some of 'a

let get_or d o =
  case o do
    | None -> d
    | Some v -> v

let a = Some 10
let b = None
let () = println (int_to_str (get_or 0 a + get_or 100 b))
