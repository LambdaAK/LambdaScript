Expected:
Link 2 Link 12 Link 14 End

Source:
trait Render<a> where
  val render : a -> String
end

impl Render for Int where
  let render x = int_to_str x
end

type rec Chain<a> =
  | End
  | Link of (a, Chain<a>)

impl Render for Chain<a> requires Render<a> where
  let render l =
    case l do
    | End -> "End"
    | Link (h, t) -> "Link " ^ (render h) ^ " " ^ (render t)
end

let xs = Link (2, Link (12, Link (14, End)))
let () = print_string (render xs)
