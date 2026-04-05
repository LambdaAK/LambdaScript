inter Functor<f> {
  val fmap : (a -> b) -> f<a> -> f<b>
}

impl Functor for [u] where
  let rec fmap g xs =
    case xs do
    | [] -> []
    | h :: t -> g h :: fmap g t
end

let () = println (int_to_str (case fmap (fn x -> x + 1) [1; 2; 3] do | h :: _ -> h | [] -> 0))
