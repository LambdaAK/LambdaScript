Expected:
42
7

Source:
mod Opt where
  type Option<a> =
    | None
    | Some of a

  let default opt d =
    case opt do
    | None -> d
    | Some x -> x
end

use Opt
let a = default None 42
let b = default (Some 7) 0

let () = print_string (int_to_str a)
let () = print_string (int_to_str b)
