Expected:
Link 11 Link 17 Link 11 End

Source:
trait Render<a> where
  val render : a -> String
end

impl Render for Int where
  render x = int_to_str x
end

type rec Chain<a> =
  | End
  | Link of (a, Chain<a>)

impl Render for Chain<a> requires Render<a> where
  render l =
    case l do
    | End -> "End"
    | Link (h, t) -> "Link " ^ (render h) ^ " " ^ (render t)
end

let xs = Link (11, Link (17, Link (11, End)))
let () = print_string (render xs)
