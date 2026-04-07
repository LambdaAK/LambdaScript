# Pattern matching

`case expr do` introduces a sequence of branches `| pattern -> result`.

## What you can match

- **Literals** — integers, strings, booleans, characters, unit.
- **Variables** — bind a name to the matched value.
- **Wildcard** — `_` discards a value.
- **Cons** — `h :: t` for lists.
- **Tuples** — `(x, y, z)`.
- **Constructors** — `None`, `Some x`, `Left y`, etc.
- **Records** — `{ name: "Alice" }` (literal field), `{ x, y }` (bind fields `x` and `y`).
- **Nesting** — any combination of the above.

## Examples

List length:

```ocaml
let rec length lst =
  case lst do
  | [] -> 0
  | h :: t -> 1 + length t
in
length [1, 2, 3]
```

Record tags:

```ocaml
let person = { name: "John", age: 30 }
in
case person do
| { name: "Alex" } -> 1
| { name: "John" } -> 2
| _ -> 3
```

Patterns are checked for exhaustiveness where the implementation supports it; redundant branches may be reported.
