type rec Tree<a> =
  | Leaf
  | Branch of (a, Tree<a>, Tree<a>)
