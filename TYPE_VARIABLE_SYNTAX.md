# Type Variable Syntax Change

## Summary

Type variables in LambdaScript now **require** the OCaml-style single quote prefix (`'a`).

## New Syntax (Required)

```ocaml
// Type definitions with type parameters
type Option<'a> = | None | Some of 'a
type Pair<'a, 'b> = ('a, 'b)
type List<'a> = | Nil | Cons of ('a, List<'a>)

// Recursive types with type parameters
type rec Tree<'a> = | Leaf of 'a | Node of ('a, Tree<'a>, Tree<'a>)

// Multiple type parameters
type Result<'a, 'b> = | Ok of 'a | Err of 'b
type Triple<'a, 'b, 'c> = ('a, ('b, 'c))
```

## Old Syntax (No Longer Supported)

```ocaml
// These will now fail to parse:
type Option<a> = | None | Some of a        // ❌ Error
type Pair<a, b> = (a, b)                   // ❌ Error
```

## Why This Change?

1. **Clear Distinction**: The `'` prefix makes it immediately obvious what is a type variable vs a type constructor
   - Type variables: `'a`, `'b`, `'result`, `'t`
   - Type constructors: `int`, `bool`, `Option`, `List`, `MyType`

2. **Consistency**: Matches OCaml convention, which the language is already inspired by

3. **No Ambiguity**: Without the prefix, lowercase identifiers could be confused with type names

## Examples

### Before
```ocaml
type rec List<a> = | Nil | Cons of (a, List<a>)
and Option<a> = | None | Some of a
```

### After
```ocaml
type rec List<'a> = | Nil | Cons of ('a, List<'a>)
and Option<'a> = | None | Some of 'a
```

## Implementation Changes

**File**: `src/parser.ml`
- Modified `string_parser` to only accept `TypeVar` tokens (with `'` prefix)
- Removed acceptance of plain `Id` tokens as type parameters

**Tests**: All 641+ tests updated to use `'a` syntax

## Type Variable Naming

You can use any lowercase identifier after the quote:
- `'a`, `'b`, `'c` - single letters (most common)
- `'result`, `'error` - descriptive names
- `'t`, `'u`, `'v` - short names
- `'key`, `'value` - semantic names
