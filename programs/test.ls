type rec Tree<a> =
  | Leaf
  | Node of (a, Tree<a>, Tree<a>)

impl Functor for Tree where
  let fmap f tree =
    case tree do
      | Leaf -> Leaf
      | Node (v, left, right) -> Node (f v, fmap f left, fmap f right)
end

let rec flatten_tree =
  fn tree ->
    case tree do
      | Leaf -> []
      | Node (v, left, right) ->
        let first_list = [v] in
          let second_list = flatten_tree left in
          let third_list = flatten_tree right in
          first_list ++ second_list ++ third_list

let my_tree = Node (1, Node (2, Leaf, Leaf), Node (5, Leaf, Leaf))

let flattened = flatten_tree my_tree