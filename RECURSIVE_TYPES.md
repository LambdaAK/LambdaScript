# Recursive Sum Types in LambdaScript

LambdaScript now supports **recursive sum types** using the `type rec` syntax. This allows you to define types that reference themselves, such as lists, trees, and other recursive data structures.

## Syntax

```
type rec TypeName<params> =
  | Constructor1
  | Constructor2 of type
  | ...
```

## Semantics

Recursive types in LambdaScript use **isorecursive** semantics:
- The type is represented internally as a fixed-point type: `μTypeName. body`
- Constructors implicitly perform the **fold** operation
- Pattern matching implicitly performs the **unfold** operation
- For polymorphic types, the universal quantification (`∀`) is on the outside: `∀a. μList. (Nil | Cons of (a, List<a>))`

## Examples

### Recursive List

```
type rec List<a> =
  | Nil
  | Cons of (a, List<a>)
```

This defines a polymorphic list type where:
- `Nil` is a nullary constructor representing an empty list
- `Cons` takes a pair of an element and a list

### Using Recursive Types

You can load a file with recursive type definitions into the REPL:

```bash
# Start REPL with preloaded definitions
dune exec ./bin/repl.exe programs/preload_example.txt

# Or use the Makefile
make repl-with-file FILE=programs/preload_example.txt
```

Then use the types interactively:

```
λ> Nil
-----------------------------
Nil : List<'a>
-----------------------------

λ> Cons (1, Nil)
-----------------------------
Cons (1, Nil) : List<int>
-----------------------------

λ> Cons (1, (Cons (2, (Cons (3, Nil)))))
-----------------------------
Cons (1, (Cons (2, (Cons (3, Nil))))) : List<int>
-----------------------------
```

### Pattern Matching

You can pattern match on recursive types:

```
bind rec length lst <-
  switch lst =>
    | Nil -> 0
    | Cons (_, t) -> 1 + length t
  end
in
length (Cons (1, (Cons (2, Nil))))
```

This will evaluate to `2`.

## Important Notes

1. **Recursive types must use `type rec`**: Non-recursive sum types should use just `type`
2. **No mutual recursion**: Currently, mutually recursive types are not supported
3. **Type aliases cannot be recursive**: Only sum types can be recursive
4. **Full type application**: Recursive references must use full type application (e.g., `List<a>`, not partial application)

## Implementation Details

- Recursive types are represented using the `FixedPoint` constructor in the type system
- Type checking properly handles the fixed-point semantics
- The evaluator treats recursive type constructors the same as regular constructors
- Pattern matching works seamlessly with recursive types
