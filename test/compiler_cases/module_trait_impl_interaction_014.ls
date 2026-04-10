Expected:
42

Source:
mod A where
  mod B where
    let x = 42
  end
end

use A.B
let () = print_string (int_to_str x)
