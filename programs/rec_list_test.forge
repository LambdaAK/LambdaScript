type rec List<a> =
  | Nil
  | Cons of (a, List<a>)

bind nil <- Nil in
bind singleton <- Cons (1, Nil) in
bind list123 <- Cons (1, (Cons (2, (Cons (3, Nil))))) in
bind rec length lst <-
  switch lst =>
    | Nil -> 0
    | Cons (_, t) -> 1 + length t
  end
in
bind _ <- println (int_to_str (length list123)) in
bind rec sum lst <-
  switch lst =>
    | Nil -> 0
    | Cons (h, t) -> h + sum t
  end
in
println (int_to_str (sum list123))
