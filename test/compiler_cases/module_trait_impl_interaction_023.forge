Expected:
L1
R1

Source:
mod Left where
  trait RenderLeft<a> where
    val render_left : a -> String
  end

  impl RenderLeft for Int where
    render_left x = "L" ^ int_to_str x
  end
end

mod Right where
  trait RenderRight<a> where
    val render_right : a -> String
  end

  impl RenderRight for Int where
    render_right x = "R" ^ int_to_str x
  end
end

let l <Left.RenderLeft Int> x = render_left x
let r <Right.RenderRight Int> x = render_right x

let () = print_string (l 1)
let () = print_string (r 1)
