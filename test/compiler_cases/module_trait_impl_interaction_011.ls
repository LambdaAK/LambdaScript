Expected:
42

Source:
mod Boxed where
  type Box =
    | Box of Int

  trait Render<a> where
    val render : a -> String
  end

  impl Render for Box where
    render b =
      case b do
      | Box n -> int_to_str n
  end

  let out = render (Box 42)
end

let () = print_string Boxed.out
