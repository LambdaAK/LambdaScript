Expected:
42

Source:
mod Root where
  mod Shared where
    let base = 40
  end

  mod Worker where
    use Shared
    let out = base + 2
  end
end

let () = print_string (int_to_str Root.Worker.out)
