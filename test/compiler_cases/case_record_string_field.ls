Expected:
2

Source:
let person = { name: "John", age: 30, city: "New York" }
let v = case person do
  | { name: "Alex" } -> 1
  | { name: "John" } -> 2
  | _ -> 3
let () = print_string (int_to_str v)
