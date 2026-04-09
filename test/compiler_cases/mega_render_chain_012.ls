Expected:
Link 8 Link 4 Link 12 End

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

let xs = Link (8, Link (4, Link (12, End)))
let () = print_string (render xs)
