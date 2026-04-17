Expected:
42

Source:
mod A where
  mod B where
    trait Render<a> where
      val render : a -> String
    end

    impl Render for Int where
      render x = int_to_str x
    end

    let out <Render Int> x = render x
  end
end

let () = print_string (A.B.out 42)
