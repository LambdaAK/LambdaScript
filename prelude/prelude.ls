// Standard prelude: typeclass hierarchy (implement instances in user code).

trait Functor<f<_>> where
  val fmap : (a -> b) -> f<a> -> f<b>
end

trait Applicative<f<_>> requires Functor<f> where
  val ap : f<a -> b> -> f<a> -> f<b>
  val pure : a -> f<a>
end

trait Monad<f<_>> requires Applicative<f> where
  val bind : f<a> -> (a -> f<b>) -> f<b>
  val (>>=) : f<a> -> (a -> f<b>) -> f<b>
  let (>>=) x f = bind x f
end

trait Foldable<f<_>> where
  val fold_left : (b -> a -> b) -> b -> f<a> -> b
  val fold_right : (a -> b -> b) -> b -> f<a> -> b
end

trait Bifunctor<f<_, _>> where
  val bimap : (a -> c) -> (b -> d) -> f<a, b> -> f<c, d>
end
