Expected:
42

Source:
mod M where
  trait Render<a> where
    val render : a -> String
  end

  type Box =
    | Box of Int

  impl Render for Int where
    render x = int_to_str x
  end

  impl Render for Box requires Render<Int> where
    render b =
      case b do
      | Box n -> render n
  end

  let out = render (Box 42)
end

let () = print_string M.out
