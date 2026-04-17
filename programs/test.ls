macro_rules! collect_and_count where
  ($($x:expr),*) => count_args!($($x),*)
end

let n0 = collect_and_count!()
let n4 = collect_and_count!(1, 2, 3, 4)
