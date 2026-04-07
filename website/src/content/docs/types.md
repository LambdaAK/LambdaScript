# Types

## Primitive types

| Type | Examples |
|------|----------|
| `int` | `42`, `-1` |
| `float` | `3.14` |
| `bool` | `true`, `false` |
| `string` | `"hello"` |
| `char` | `'a'` |
| `unit` | `()` |

## Functions

Written `a -> b` (or polymorphic `'a -> 'b`). Functions are introduced with `fn x -> …` or `let f x = …`.

## Tuples

Fixed-width products: `(int, string)`, `(1, "ok", true)`. Pattern match with `(x, y)`.

## Lists

Concrete syntax `[1, 2, 3]` and `1 :: 2 :: []`. With the prelude loaded, lists are the `List<'a>` ADT (`[]` and `::`).

## Algebraic data types (ADTs)

Sum types with constructors:

```ocaml
type Option<a> =
  | None
  | Some of a
```

Recursive types use `type rec`:

```ocaml
type rec List<a> =
  | []
  | (::) of (a, List<a>)
```

## Records

Anonymous row types: `{ name: string, age: int }`. Field access `r.name`, update `{ r with age = 31 }`.

## Type aliases

Name shorthands for existing types.

## Polymorphism

Type parameters `'a`, `'b` on definitions and data types. Inference fills them in; you can add annotations where useful.
