type rec List<a> =
  | Nil
  | Cons of (a, List<a>)
