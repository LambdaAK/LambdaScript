// Standard prelude: canonical typeclass headers (implement in user code).

trait Monad<m<_>> where
  val bind : m<a> -> (a -> m<b>) -> m<b>
  val pure : a -> m<a>
end

trait Foldable<f<_>> where
  val fold_left : (b -> a -> b) -> b -> f<a> -> b
  val fold_right : (a -> b -> b) -> b -> f<a> -> b
end

trait Bifunctor<f<_, _>> where
  val bimap : (a -> c) -> (b -> d) -> f<a, b> -> f<c, d>
end
