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

let x = 1 

let ss = 2

let f x = x

let g x y = x

let map func l =
  case l do
    | [] -> []
    | h :: t -> func h :: map func t

let rec is_even n =
  if n == 0 then true
  else is_odd (n - 1)

and is_odd n =
  if n == 0 then false
  else is_even (n - 1)
