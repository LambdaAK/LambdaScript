Expected:
42

Source:
mod A where
  mod B where
    let x = 39
  end
end

let () = print_string (int_to_str (A.B.x + 3))
