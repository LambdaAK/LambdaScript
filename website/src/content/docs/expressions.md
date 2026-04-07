# Expressions

## Literals and variables

Integers, floats, booleans, strings, characters, and `()` are expressions. Names refer to bindings in scope.

## Let and recursion

```ocaml
let x = 41 in x + 1

let rec fact n =
  if n == 0 then 1 else n * fact (n - 1)
in
fact 5
```

## Functions

Lambdas: `fn x -> x + 1`. Application is juxtaposition: `f x y`.

## Conditionals

`if e0 then e1 else e2` — both branches must have the same type.

## Pattern matching

```ocaml
case e do
| pat1 -> e1
| pat2 -> e2
```

See **Pattern matching** in the sidebar.

## Lists and comprehensions

- Literals: `[1, 2, 3]`
- Cons: `x :: xs`
- Ranges: `[1...10]`
- Comprehensions: `[x * 2 | x => [1...5], x > 2]`

## Blocks

`{ e1; e2; e3 }` — sequence expressions; value is the last one.

## Operators as values

Wrap in parentheses: `(+)`, `(::)`, `(*)`, etc., for higher-order use.

## Type annotations

On patterns or whole expressions, e.g. `(x: int)` or `fn (x: int) : int -> x + 1`.
