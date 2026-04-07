# Traits (typeclasses)

Traits abstract over behavior shared by many types. They are declared with **`trait`** (or the alias **`inter`**) and implemented with **`impl … for …`**.

## Declaration

Prelude-style (excerpt):

```ocaml
trait Functor<f<_>> where
  val fmap : (a -> b) -> f<a> -> f<b>
end
```

Haskell-style keyword `inter` is equivalent; see the repository’s `programs/haskell_style_typeclasses.ls` for examples.

## Implementation

```ocaml
impl Functor for List where
  let fmap f xs =
    case xs do
    | [] -> []
    | h :: t -> f h :: fmap f t
end
```

## Prelude hierarchy

The standard prelude defines, among others:

- `Functor`, `Applicative`, `Monad`, `Alternative`
- `Foldable`, `Bifunctor`
- `Show`, `Eq`, `Ord`
- `Semigroup`, `Monoid`

Higher-kinded parameters use `f<_>` / `f<_, _>` notation. Some trait-heavy programs are smoother in the interpreter than on the native backend; see comments in the repo’s typeclass demo programs.
