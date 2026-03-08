# Mutually Recursive Types Implementation

## Overview

Mutually recursive types allow defining multiple type definitions that reference each other. This is essential for representing complex data structures like trees with forests, ASTs with multiple node types, and more.

## Syntax

```
type rec TypeName1 = | Constructor1 of Type1 | Constructor2 of Type2
and TypeName2 = | Constructor3 of Type3 | Constructor4 of Type4
and TypeName3 = ...
```

## Examples

### Example 1: Even and Odd Natural Numbers

```ocaml
type rec Even = | Zero | SuccE of Odd
and Odd = | SuccO of Even

let rec even_to_int = fn e ->
  case e do
  | Zero -> 0
  | SuccE o -> 1 + odd_to_int o
and odd_to_int = fn o ->
  case o do
  | SuccO e -> 1 + even_to_int e

(* Usage *)
let four = SuccE (SuccO (SuccE (SuccO Zero)))
let result = even_to_int four  (* Evaluates to 4 *)
```

### Example 2: Tree and Forest

```ocaml
type rec Tree<a> = | Leaf of a | Node of (a, Forest<a>)
and Forest<a> = | Empty | Trees of (Tree<a>, Forest<a>)

let rec sum_tree = fn t ->
  case t do
  | Leaf x -> x
  | Node (x, f) -> x + sum_forest f
and sum_forest = fn f ->
  case f do
  | Empty -> 0
  | Trees (t, rest) -> sum_tree t + sum_forest rest

(* Usage *)
let tree = Node (1, Trees (Leaf 2, Trees (Leaf 3, Empty)))
let total = sum_tree tree  (* Evaluates to 6 *)
```

### Example 3: Circular Type References

```ocaml
type rec A = | AVal of int | ToB of B
and B = | BVal of int | ToC of C
and C = | CVal of int | ToA of A

let rec get_value = fn a ->
  case a do
  | AVal x -> x
  | ToB b -> get_b b
and get_b = fn b ->
  case b do
  | BVal y -> y
  | ToC c -> get_c c
and get_c = fn c ->
  case c do
  | CVal z -> z
  | ToA a2 -> get_value a2

(* Usage *)
let value = get_value (ToB (ToC (ToA (AVal 42))))  (* Evaluates to 42 *)
```

### Example 4: Rose Trees

```ocaml
type rec Rose<a> = | RNode of (a, RoseList<a>)
and RoseList<a> = | RNil | RCons of (Rose<a>, RoseList<a>)

let rec count_rose = fn r ->
  case r do
  | RNode (_, children) -> 1 + count_list children
and count_list = fn rl ->
  case rl do
  | RNil -> 0
  | RCons (r, rest) -> count_rose r + count_list rest

(* Usage *)
let rose = RNode (1, RCons (RNode (2, RNil), RCons (RNode (3, RNil), RNil)))
let count = count_rose rose  (* Evaluates to 3 *)
```

## Type System Representation

Mutually recursive types are represented using fixed-point types (μ notation):
- Each type in the mutual recursion gets its own FixedPoint
- The type environment includes all types before processing constructors
- Constructor payloads can reference any type in the mutual recursion group

For example:
```
type rec Even = | Zero | SuccE of Odd
and Odd = | SuccO of Even
```

Is represented internally as:
- Even: μEven. CTypeApp(Even, [])
- Odd: μOdd. CTypeApp(Odd, [])

With constructors:
- Zero: Even
- SuccE: Odd -> Even
- SuccO: Even -> Odd

## Implementation Details

### Files Modified

1. **src/Expr.ml** - Added `SumTypeDefMutRec` AST variant
2. **src/parser.ml** - Added parsing support for `type rec A = ... and B = ...`
3. **src/cexpr.ml** - Added `CSumTypeRecMutRec` condensed AST variant
4. **src/condense.ml** - Added condensing logic for mutually recursive types
5. **src/typecheck.ml** - Added type checking with proper environment extension
6. **src/c_to_string.ml** - Added string conversion for display
7. **src/ceval.ml** - Added evaluation support for constructors
8. **src/tostring.ml** - Added AST to string conversion

### Key Design Decisions

1. **Environment Extension**: All mutually recursive types are added to the type environment before processing any constructors, allowing them to reference each other.

2. **Type Variable Conversion**: References to type names in constructor payloads are converted to the appropriate type applications or type variables.

3. **Polymorphism Support**: Mutually recursive types can have type parameters that are properly threaded through all references.

## Test Coverage

**Total Tests: 50**

### Test Categories:

1. **Basic Tests (10 tests)**: Simple even/odd numbers, constructor types, pattern matching
2. **With Type Parameters (10 tests)**: Tree/Forest with generics, polymorphic constructors
3. **Complex Scenarios (10 tests)**: Three-way recursion, cycles, nested structures
4. **Evaluation Tests (10 tests)**: Conversions, mapping, searching, aggregation
5. **Advanced Tests (10 tests)**: Multiple type parameters, rose trees, graphs

All tests pass successfully!

## Limitations

1. Each type in the mutual recursion must have at least one constructor
2. Constructor payloads cannot be polymorphic types themselves
3. The `and` keyword must be used to separate mutually recursive types

## Future Enhancements

Potential improvements:
- Better error messages for cyclic type references
- Support for mutually recursive type aliases (currently only sum types)
- Optimization for common patterns like tree/forest
