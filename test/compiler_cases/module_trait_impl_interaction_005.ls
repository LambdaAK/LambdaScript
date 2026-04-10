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

  let show_num <Render Int> x = render x
end

let () = print_string (M.show_num 42)
