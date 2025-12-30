type Color = | Red | Black

type rec RBTree<a> =
  | Leaf
  | Node of (Color, a, RBTree<a>, RBTree<a>)

let make_node color value left right =
  Node (color, value, left, right)

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
  | Leaf -> make_node Red x Leaf Leaf
  | Node (color, y, left, right) ->
      if x < y then
        balance (make_node color y (insert_aux x left) right)
      else if x > y then
        balance (make_node color y left (insert_aux x right))
      else
        tree

let make_black tree =
  switch tree =>
  | Leaf -> Leaf
  | Node (_, x, left, right) -> make_node Black x left right

let insert x tree =
  make_black (insert_aux x tree)

let () = println "Testing with workaround helper function:"
let tree1 = insert 5 Leaf
let tree2 = insert 3 tree1
let tree3 = insert 7 tree2

let () = println (int_to_str (size tree3))
let () = if contains 5 tree3 then println "Found 5" else println "Not found"

let () = println ""
let () = println "Direct Node constructor still works but accepts wrong types:"
let bad_node = Node (5, 10, Leaf, Leaf)
let () = println (int_to_str (size bad_node))
