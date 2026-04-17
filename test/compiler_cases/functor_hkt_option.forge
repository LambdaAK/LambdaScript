Expected:
4

Source:
type Option<a> = | None | Some of a

inter Functor<f> {
  val fmap : (a -> b) -> f<a> -> f<b>
}

impl Functor for Option where
  fmap g x =
    case x do
    | None -> None
    | Some v -> Some(g v)
end

let () =
  case fmap (fn x -> x + 1) (Some 3) do
  | Some n -> print_string (int_to_str n)
  | None -> print_string "no"
