Expected:
42
T

Source:
mod A where
  trait RenderA<a> where
    val renderA : a -> String
  end

  impl RenderA for Int where
    renderA x = int_to_str x
  end
end

mod B where
  trait RenderB<a> where
    val renderB : a -> String
  end

  impl RenderB for Bool where
    renderB x = if x then "T" else "F"
  end
end

let show_int <A.RenderA Int> x = renderA x
let show_bool <B.RenderB Bool> x = renderB x

let () = print_string (show_int 42)
let () = print_string (show_bool true)
