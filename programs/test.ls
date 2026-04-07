impl Semigroup for Bool where
  let mappend x y = x
  let (++) x y = mappend x y
end

let _ = println (true ++ false)
