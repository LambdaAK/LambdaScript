# LambdaScript

A statically-typed functional programming language with type inference, polymorphism, and powerful pattern matching.

This is an OCaml-based interpreter. I have also experimented with a TypeScript-based implementation: [LambdaScript 2](https://github.com/LambdaAK/LambdaScript-2).

## Table of Contents

1. [Overview](#overview)
2. [Language Features](#language-features)
3. [Examples](#examples)
4. [Installation](#installation)
5. [Usage](#usage)
6. [Testing](#testing)

## Overview

LambdaScript is a **statically-typed functional programming language** inspired by OCaml and Haskell. It features:

- **Static type system** with Hindley-Milner style type inference
- **Polymorphic types** with type parameters
- **Algebraic data types** with pattern matching
- **First-class functions** with closures
- **Recursive types** for defining lists, trees, and other recursive structures
- **Type annotations** for clarity and documentation
- **Comprehensive built-in operators** usable as first-class values

## Language Features

### Type System

- **Type Inference**: Automatic type deduction using constraint-based type inference
- **Polymorphism**: Generic types with type parameters (`'a`, `'b`)
- **Type Annotations**: Optional type annotations for clarity
- **Algebraic Data Types**: Sum types with constructors
- **Recursive Types**: Self-referential type definitions
- **Type Aliases**: Named type definitions

**Basic Types**: `int`, `float`, `bool`, `string`, `char`, `unit`

**Composite Types**: Functions (`a -> b`), Lists (`[a]`), Tuples (`(a, b, c)`)

### Expressions

- **Literals**: Integers, floats, booleans, strings, characters, unit `()`
- **Variables**: Named bindings
- **Functions**: Lambda expressions with `fn x -> expr`
- **Function Application**: `f x y`
- **Let Bindings**: `let x = expr in body`
- **Recursive Functions**: `let rec fact n = ...`
- **Conditionals**: `if condition then expr1 else expr2`
- **Pattern Matching**: `case expr do | pattern -> result`
- **Lists**: `[1, 2, 3]` or `1 :: 2 :: 3 :: []`
- **List Ranges**: `[1...10]`
- **List Comprehensions**: `[x * 2 | x => [1...5], x > 2]`
- **Tuples/Vectors**: `(1, "hello", true)`
- **Code Blocks**: `{ expr1; expr2; result }`

### Operators

All operators can be used as first-class values by wrapping in parentheses: `(+)`, `(*)`, `(::)`, etc.

**Arithmetic**: `+`, `-`, `*`, `/`, `%`

**Comparison**: `==`, `!=`, `<>`, `<`, `>`, `<=`, `>=`

**Logical**: `&&`, `||`

**List**: `::` (cons)

### Pattern Matching

Supports comprehensive pattern matching including:
- Literal patterns: `42`, `"hello"`, `true`
- Variable binding: `x`, `name`
- Wildcard: `_`
- Cons patterns: `h :: t`
- Tuple patterns: `(x, y, z)`
- Constructor patterns: `Just x`, `Left y`
- Nested patterns: `(x, y :: rest)`

### Built-in Functions

**I/O**:
- `print : string -> unit` - print without newline
- `println : string -> unit` - print with newline

**Type Conversions**:
- `int_to_str : int -> string`
- `int_to_float : int -> float`
- `float_to_int : float -> int`
- `string_to_list : string -> [char]`

**Higher-Order Functions**:
- `map : (a -> b) -> [a] -> [b]`
- `filter : (a -> bool) -> [a] -> [a]`
- `reduce_left : (a -> b -> a) -> a -> [b] -> a`
- `reduce_right : (a -> b -> b) -> [a] -> b -> b`
- `not : bool -> bool`

## Examples

### Basic Types and Expressions

```ocaml
(* Integers *)
42
-17

(* Floats *)
3.14
-2.5

(* Booleans *)
true
false

(* Strings *)
"Hello, world!"

(* Characters *)
'a'
'Z'

(* Unit *)
()
```

### Variables and Let Bindings

```ocaml
(* Simple binding *)
let x = 42 in x + 1
(* Result: 43 *)

(* Multiple bindings *)
let x = 5 in
let y = 10 in
x + y
(* Result: 15 *)

(* Type annotations *)
let (x: int) = 42 in x
```

### Functions

```ocaml
(* Lambda function *)
fn x -> x + 1

(* Named function *)
let add = fn x -> fn y -> x + y in
add 3 4
(* Result: 7 *)

(* Pattern matching in parameters *)
let first = fn (x, y) -> x in
first (5, 10)
(* Result: 5 *)

(* With type annotations *)
let increment = fn (x: int) : int -> x + 1 in
increment 41
(* Result: 42 *)

(* Recursive functions *)
let rec factorial = fn n ->
  if n == 0 then 1
  else n * factorial (n - 1)
in
factorial 5
(* Result: 120 *)
```

### Operators as First-Class Values

```ocaml
(* Use operators as functions *)
let add = (+) in
add 3 4
(* Result: 7 *)

(* Partial application *)
let add5 = (+) 5 in
add5 10
(* Result: 15 *)

(* Pass to higher-order functions *)
map (*) [1, 2, 3] [4, 5, 6]

(* Custom operator definitions *)
let (++) = fn a -> fn b -> a + b in
(++) 10 20
(* Result: 30 *)
```

### Lists

```ocaml
(* Empty list *)
[]

(* List literals *)
[1, 2, 3, 4, 5]

(* Cons operator *)
1 :: 2 :: 3 :: []
(* Result: [1, 2, 3] *)

(* List ranges *)
[1...10]
(* Result: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] *)

(* List comprehensions *)
[x * x | x => [1...5]]
(* Result: [1, 4, 9, 16, 25] *)

(* With filter *)
[x | x => [1...10], x % 2 == 0]
(* Result: [2, 4, 6, 8, 10] *)
```

### Tuples/Vectors

```ocaml
(* Pairs *)
(1, 2)

(* Triples *)
(1, "hello", true)

(* Pattern matching tuples *)
let (x, y) = (5, 10) in x + y
(* Result: 15 *)
```

### Pattern Matching

```ocaml
(* Match on lists *)
let length = fn lst ->
  case lst do
  | [] -> 0
  | h :: t -> 1 + length t
in
length [1, 2, 3]
(* Result: 3 *)

(* Match on integers *)
let sign = fn n ->
  case n do
  | 0 -> "zero"
  | n -> if n > 0 then "positive" else "negative"
in
sign (-5)
(* Result: "negative" *)

(* Complex patterns *)
let get_second = fn lst ->
  case lst do
  | [] -> 0
  | x :: [] -> 0
  | x :: y :: rest -> y
in
get_second [1, 2, 3]
(* Result: 2 *)
```

### Higher-Order Functions

```ocaml
(* Map *)
let rec map = fn f -> fn lst ->
  case lst do
  | [] -> []
  | h :: t -> f h :: map f t
in
map (fn x -> x * x) [1, 2, 3, 4, 5]
(* Result: [1, 4, 9, 16, 25] *)

(* Filter *)
let rec filter = fn pred -> fn lst ->
  case lst do
  | [] -> []
  | h :: t ->
    if pred h then h :: filter pred t
    else filter pred t
in
filter (fn x -> x % 2 == 0) [1, 2, 3, 4, 5, 6]
(* Result: [2, 4, 6] *)

(* Fold/Reduce *)
let rec fold_left = fn op -> fn acc -> fn lst ->
  case lst do
  | [] -> acc
  | h :: t -> fold_left op (op acc h) t
in
fold_left (+) 0 [1, 2, 3, 4, 5]
(* Result: 15 *)
```

### Algebraic Data Types

```ocaml
(* Type aliases *)
type IntPair = (int, int)

(* Sum types *)
type Option<a> =
  | None
  | Some of a

type Either<a, b> =
  | Left of a
  | Right of b

(* Recursive types *)
type rec List<a> =
  | Nil
  | Cons of a * List<a>

type rec Tree<a> =
  | Leaf
  | Node of a * Tree<a> * Tree<a>

(* Using custom types *)
let rec tree_size = fn t ->
  case t do
  | Leaf -> 0
  | Node (value, left, right) ->
    1 + tree_size left + tree_size right
in
tree_size (Node (5, Leaf, Node (3, Leaf, Leaf)))
(* Result: 2 *)
```

### Polymorphic Functions

```ocaml
(* Identity function *)
let id = fn x -> x
(* Type: 'a -> 'a *)

(* Composition *)
let compose = fn f -> fn g -> fn x -> f (g x)
(* Type: ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c *)

(* Map for any type *)
let rec map = fn f -> fn lst ->
  case lst do
  | [] -> []
  | h :: t -> f h :: map f t
(* Type: ('a -> 'b) -> ['a] -> ['b] *)
```

### Code Blocks

```ocaml
{
  let x = 5;
  let y = 10;
  let z = x + y;
  z * 2
}
(* Result: 30 *)
```

### Type Annotations

```ocaml
(* Variable annotations *)
let (x: int) = 42 in x

(* Function parameter annotations *)
let add = fn (x: int) -> fn (y: int) -> x + y in
add 3 4

(* Return type annotations *)
let increment = fn (x: int) : int -> x + 1 in
increment 5

(* Full function type annotation *)
let apply = fn (f: int -> int) -> fn (x: int) : int -> f x in
apply (fn x -> x * 2) 21
(* Result: 42 *)
```

## Installation

### Prerequisites

- OCaml 5.0.0 or higher
- Dune build system
- OPAM (OCaml package manager)

### Building from Source

1. Clone the repository:
```bash
git clone https://github.com/LambdaAK/LambdaScript
cd LambdaScript
```

2. Build the project:
```bash
make
```

This will compile both the interpreter and the REPL.

## Usage

### Running the REPL

Start an interactive session:
```bash
make repl
```

In the REPL, you can type expressions and see their types and evaluated results immediately.

### Running LambdaScript Files

Execute a `.ls` or `.txt` file containing LambdaScript code:
```bash
dune exec ./bin/interpreter.exe <filename>
```

Example:
```bash
dune exec ./bin/interpreter.exe programs/factorial.txt
```

### File Extension

LambdaScript files typically use `.ls` or `.txt` extensions.

## Testing

### Running Tests

The test suite contains over 700 unit tests covering all language features:

```bash
make test
```

Or using dune directly:
```bash
dune test
```

### Test Coverage

To run tests with coverage analysis:
```bash
make bisect
```

This will generate a coverage report showing which parts of the codebase are tested.

### Test Organization

Tests are organized in `test/test.ml` and cover:
- Type checking
- Type inference
- Expression evaluation
- Pattern matching
- Algebraic data types
- Higher-order functions
- Built-in operators
- Edge cases and error conditions

## Language Semantics

For a rigorous formal definition of LambdaScript's semantics, see:
https://github.com/LambdaAK/LambdaScript/blob/main/documentation/LambdaScript.pdf

**Note**: The formal semantics document may not reflect all recent language features and syntax changes.

## Documentation

### Generating Code Documentation

Generate OCaml documentation for the codebase:
```bash
make doc
```

### Viewing Documentation

Open the generated documentation in your browser:
```bash
make opendoc
```

## Project Structure

```
LambdaScript/
├── bin/           # Executable entry points (REPL, interpreter)
├── src/           # Source code
│   ├── lex.ml     # Lexer
│   ├── parser.ml  # Parser
│   ├── expr.ml    # Expression types
│   ├── typecheck.ml  # Type checker
│   ├── eval.ml    # Evaluator
│   └── env.ml     # Environment and built-ins
├── test/          # Test suite
├── programs/      # Example programs
└── documentation/ # Formal semantics
```

## License

See LICENSE file for details.