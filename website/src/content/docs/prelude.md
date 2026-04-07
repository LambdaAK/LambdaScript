# Standard prelude

The file **`prelude/prelude.ls`** in the repository is loaded for normal compilation and interpretation, and once at startup in the REPL and web Playground. It is the canonical place for **shared data types** and the **trait hierarchy**.

## Algebraic types

| Type | Role |
|------|------|
| `List<a>` | Recursive list: `[]` and `::` |
| `Option<a>` | `None` or `Some of a` |
| `Ordering` | `LT`, `EQ`, `GT` for `Ord` |

## Traits (overview)

The prelude declares (among others):

- **Functor** — `fmap`
- **Applicative** — `ap`, `pure`
- **Monad** — `bind`, `(>>=)`
- **Alternative** — `aempty`, `(<|>)`
- **Foldable** — `fold_left`, `fold_right`
- **Bifunctor** — `bimap`
- **Show** — `show`
- **Eq** — `(==)`, `(!=)`
- **Ord** — `compare`, comparison operators
- **Semigroup** / **Monoid** — `mappend`, `mempty`, `(++)`

Default instances for built-in types and `List` / `Option` live in the same file.

## Working with this page

The prelude is maintained as real Forge source — open `prelude/prelude.ls` for full definitions, laws in comments, and instance bodies. This site summarizes structure; the repo file remains the source of truth (manual docs stay accurate without a codegen step for now).
