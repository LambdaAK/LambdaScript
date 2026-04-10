Expected:
42

Source:
mod A where
  mod B where
    mod C where
      let x = 21
      let twice n = n + n
    end
  end
end

use A.B.C
let () = print_string (int_to_str (twice x))
