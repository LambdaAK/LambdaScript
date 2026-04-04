type Pair<a> = (a, a)

let () = println "Testing Pair type..."

let int_pair [Pair<Int>] = (5, 10)
let () = println "Created Int pair"

let get_first p = switch p =>
  | (x, y) -> x

let get_second p = switch p =>
  | (x, y) -> y

let first = get_first int_pair
let second = get_second int_pair

let () = println (int_to_str first)
let () = println (int_to_str second)

let () = println "Pair type works!"
