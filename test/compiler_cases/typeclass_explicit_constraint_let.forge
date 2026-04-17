Expected:
3

Source:
inter Monad<m> {
  val return : a -> m<a>
}

type Option<a> = | None | Some of a

impl Monad for Option where
  return x = Some x
end

let lift<Monad m> (x : m<a>) = x
let v = lift (Some 3)
let () =
  case v do
  | Some n -> print_string (int_to_str n)
  | None -> print_string "none"
