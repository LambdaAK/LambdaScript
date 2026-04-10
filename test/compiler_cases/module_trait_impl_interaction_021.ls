Expected:
42

Source:
mod C where
  type T = Int

  trait Inc<a> where
    val inc : a -> a
  end

  impl Inc for Int where
    inc x = x + 1
  end

  let bump <Inc Int> x = inc x
  let out : T = bump 41
end

let () = print_string (int_to_str C.out)
