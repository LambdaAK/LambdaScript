Expected:
42

Source:
mod M where
  trait Render<a> where
    val render : a -> String
  end

  impl Render for Int where
    render x = int_to_str x
  end
end

let show_num <M.Render Int> x = render x
let () = print_string (show_num 42)
