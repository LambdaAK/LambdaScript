type rec Tree<a> =
  | Leaf
  | Node of (a, Tree<a>, Tree<a>)

let leaf [Tree<int>] = Leaf

let single_node = Node 5 Leaf Leaf

let tree = Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf)

let rec size t =
  switch t =>
    | Leaf -> 0
    | Node _ left right -> 1 + size left + size right

let () = println (int_to_str (size tree))

let rec height t =
  switch t =>
    | Leaf -> 0
    | Node _ left right ->
        let left_h = height left in
        let right_h = height right in
        1 + (if left_h > right_h then left_h else right_h)

let () = println (int_to_str (height tree))
