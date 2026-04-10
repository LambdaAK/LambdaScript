Expected:
empty
nonempty

Source:
inter Show <a> {
  val show : a -> String
}

impl Show for [a] where
  show xs =
    case xs do
    | [] -> "empty"
    | _ :: _ -> "nonempty"
end

let () = print_string (show [])
let () = print_string (show (1 :: []))
