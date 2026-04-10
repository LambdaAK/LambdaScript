Expected:
42

Source:
mod A where
  let x = 40
end

mod B where
  let x = 2
end

use A
use B
let y = A.x + B.x
let () = print_string (int_to_str y)
