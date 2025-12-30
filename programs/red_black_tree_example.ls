type Color = | Red | Black

type rec RBTree<a> =
  | Leaf
  | Node of (Color, a, RBTree<a>, RBTree<a>)

let rec contains x tree =
  switch tree =>
  | Leaf -> false
  | Node (_, y, left, right) ->
      if x == y then true
      else if x < y then contains x left
      else contains x right

let rec size tree =
  switch tree =>
  | Leaf -> 0
  | Node (_, _, left, right) -> 1 + size left + size right

let rec height tree =
  switch tree =>
  | Leaf -> 0
  | Node (_, _, left, right) ->
      let left_h = height left in
      let right_h = height right in
      1 + (if left_h > right_h then left_h else right_h)

let rec minimum tree =
  switch tree =>
  | Leaf -> 0
  | Node (_, x, Leaf, _) -> x
  | Node (_, _, left, _) -> minimum left

let rec maximum tree =
  switch tree =>
  | Leaf -> 0
  | Node (_, x, _, Leaf) -> x
  | Node (_, _, _, right) -> maximum right

let balance tree =
  switch tree =>
  | Node (Black, z, Node (Red, y, Node (Red, x, a, b), c), d) ->
      Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
  | Node (Black, z, Node (Red, x, a, Node (Red, y, b, c)), d) ->
      Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
  | Node (Black, x, a, Node (Red, z, Node (Red, y, b, c), d)) ->
      Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
  | Node (Black, x, a, Node (Red, y, b, Node (Red, z, c, d))) ->
      Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
  | _ -> tree

let rec insert_aux x tree =
  switch tree =>
  | Leaf -> Node (Red, x, Leaf, Leaf)
  | Node (color, y, left, right) ->
      if x < y then
        balance (Node (color, y, insert_aux x left, right))
      else if x > y then
        balance (Node (color, y, left, insert_aux x right))
      else
        tree

let make_black tree =
  switch tree =>
  | Leaf -> Leaf
  | Node (_, x, left, right) -> Node (Black, x, left, right)

let insert x tree =
  make_black (insert_aux x tree)

let example_tree = Node (Black, 5,
                     Node (Red, 3,
                       Node (Black, 1, Leaf, Leaf),
                       Node (Black, 4, Leaf, Leaf)),
                     Node (Red, 7,
                       Node (Black, 6, Leaf, Leaf),
                       Node (Black, 9, Leaf, Leaf)))

let () = println (int_to_str (size example_tree))
let () = println (int_to_str (height example_tree))
let () = println (int_to_str (minimum example_tree))
let () = println (int_to_str (maximum example_tree))

let found = contains 6 example_tree
let () = if found then println "Found 6" else println "Not found"

let not_found = contains 10 example_tree
let () = if not_found then println "Found 10" else println "Not found 10"

let () = println ""
let () = println "Testing insert function:"

let empty = Leaf
let tree1 = insert 5 empty
let () = println (int_to_str (size tree1))

let tree2 = insert 3 tree1
let tree3 = insert 7 tree2
let tree4 = insert 1 tree3
let tree5 = insert 9 tree4

let () = println (int_to_str (size tree5))
let () = println (int_to_str (minimum tree5))
let () = println (int_to_str (maximum tree5))

let contains_5 = contains 5 tree5
let () = if contains_5 then println "Contains 5" else println "Does not contain 5"

let contains_3 = contains 3 tree5
let () = if contains_3 then println "Contains 3" else println "Does not contain 3"

let contains_10 = contains 10 tree5
let () = if contains_10 then println "Contains 10" else println "Does not contain 10"
