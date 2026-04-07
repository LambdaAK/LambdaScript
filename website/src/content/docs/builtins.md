# Built-in functions and operators

These are provided by the implementation (not defined in your source or the prelude source file). Operators can be used as first-class values when wrapped in parentheses, e.g. `(+)`, `(::)`.

## I/O

| | Type (conceptually) | Description |
|--|---------------------|-------------|
| `print` | `string -> unit` | Print without newline |
| `println` | `string -> unit` | Print with newline |

## Conversions

| | Description |
|--|-------------|
| `int_to_str` | `int` → `string` |
| `int_to_float` | `int` → `float` |
| `float_to_int` | `float` → `int` |
| `string_to_list` | `string` → list of `char` |

## Strings

| | Description |
|--|-------------|
| `str_length` | Length |
| `str_concat` | Concatenate two strings |
| `str_slice` | Substring by start and length |

## Lists

| | Description |
|--|-------------|
| `list_length` | Length |
| `list_head` | First element (fails if empty) |
| `list_tail` | Tail (fails if empty) |
| `list_nth` | Element at index |

## Tuples

| | Description |
|--|-------------|
| `tuple_fst` | First of a pair |
| `tuple_snd` | Second of a pair |

## Higher-order list helpers

| | Description |
|--|-------------|
| `map` | Map a function over a list |
| `filter` | Filter by predicate |
| `reduce_left` | Left fold |
| `reduce_right` | Right fold |

## Operators (selection)

- **Arithmetic:** `+`, `-`, `*`, `/`, `%`
- **String:** `^` (concatenation)
- **Compare:** `==`, `!=`, `<>`, `<`, `>`, `<=`, `>=`
- **Logic:** `&&`, `||`; prefix `not`
- **Lists:** `::` (cons)

Exact types are inferred; see the interpreter / compiler for the authoritative signatures.
