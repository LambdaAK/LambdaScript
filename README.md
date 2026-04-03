# Forge

A statically-typed functional programming language with type inference, polymorphism, and powerful pattern matching.

The reference implementation in this repository is written in **OCaml** and includes both:

- an **interpreter** (evaluate programs directly), and  
- an **ahead-of-time compiler** that lowers programs to **Min IR**, then **LLVM IR**, then **assembly**, and links a **native executable** with Clang using a small C runtime (`runtime/ls_runtime.c`).

An experimental TypeScript implementation also exists in a separate repository: [LambdaScript-2](https://github.com/LambdaAK/LambdaScript-2).

## Table of Contents

1. [Overview](#overview)
2. [Language Features](#language-features)
3. [Examples](#examples)
4. [Installation](#installation)
5. [Usage](#usage)
6. [Native compilation](#native-compilation)
7. [Testing](#testing)

## Overview

Forge is a **statically-typed functional programming language** inspired by OCaml and Haskell. It features:

- **Static type system** with Hindley-Milner style type inference
- **Polymorphic types** with type parameters
- **Algebraic data types** with pattern matching
- **First-class functions** with closures
- **Recursive types** for defining lists, trees, and other recursive structures
- **Record types** with field update syntax
- **Type annotations** for clarity and documentation
- **Comprehensive built-in operators** usable as first-class values
- **Native compiler** (LLVM IR + Clang) alongside the interpreter

## Language Features

### Type System

- **Type Inference**: Automatic type deduction using constraint-based type inference
- **Polymorphism**: Generic types with type parameters (`'a`, `'b`)
- **Type Annotations**: Optional type annotations for clarity
- **Algebraic Data Types**: Sum types with constructors
- **Recursive Types**: Self-referential type definitions
- **Type Aliases**: Named type definitions
- **Records**: Named rows with `{ field: ty, ... }`, field access `r.field`, update `{ base with field = expr }`

**Basic Types**: `int`, `float`, `bool`, `string`, `char`, `unit`

**Composite Types**: Functions (`'a -> 'b`), Lists (`['a]`), Tuples (`('a, 'b, 'c)`), Records

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

**Logical**: `&&`, `||`, and `not` (prefix)

**List**: `::` (cons)

### Pattern Matching

Supports comprehensive pattern matching including:
- Literal patterns: `42`, `"hello"`, `true`
- Variable binding: `x`, `name`
- Wildcard: `_`
- Cons patterns: `h :: t`
- Tuple patterns: `(x, y, z)`
- Constructor patterns: `Just x`, `Left y`
- Record patterns: `{ name: "Alice" }`, `{ x, y }`
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

**String Operations**:
- `str_length : string -> int` - length of string
- `str_concat : string -> string -> string` - concatenate two strings
- `str_slice : string -> int -> int -> string` - substring (start, length)

**List Operations**:
- `list_length : ['a] -> int` - length of list
- `list_head : ['a] -> 'a` - first element (fails on empty)
- `list_tail : ['a] -> ['a]` - all but first element
- `list_nth : ['a] -> int -> 'a` - nth element (0-indexed)

**Tuple Operations**:
- `tuple_fst : ('a, 'b) -> 'a` - first element of pair
- `tuple_snd : ('a, 'b) -> 'b` - second element of pair

**Higher-Order Functions**:
- `map : ('a -> 'b) -> ['a] -> ['b]`
- `filter : ('a -> bool) -> ['a] -> ['a]`
- `reduce_left : ('a -> 'b -> 'a) -> 'a -> ['b] -> 'a`
- `reduce_right : ('a -> 'b -> 'b) -> ['a] -> 'b -> 'b`
- `not : bool -> bool`

## Examples

The snippets below are illustrative; you can also open the `.ls` files under [`programs/`](programs/) for runnable examples (for example `programs/record_update.ls`, `programs/builtins_test.ls`, `programs/minimal.ls`).

### Basic Types and Expressions

```ocaml
(* Integers *)
42
-17

(* Floats *)
3.14
-2.5
int_to_float 2
float_to_int 3.9

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

### Conditionals and Boolean Operators

```ocaml
(* if / then / else (both branches must match in type) *)
if 3 < 5 then "yes" else "no"

(* Short-circuiting logical operators *)
true && false
true || false
not true

(* Combining comparisons *)
let n = 7 in
if n > 0 && n % 2 == 0 then "positive even" else "other"
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

(* Record patterns (fields can be literals or binders) *)
let person = { name: "John", age: 30, city: "New York" }
let tag = case person do
  | { name: "Alex" } -> 1
  | { name: "John" } -> 2
  | _ -> 3
(* tag == 2 *)
```

### Records

```ocaml
(* Record type and literals *)
type Point = { x: int, y: int }

let p1 : Point = { x: 10, y: 20 }

(* Field access *)
let sum_xy = p1.x + p1.y

(* Functional update (immutable: creates a new record) *)
let p2 = { p1 with x = 100 }
let p3 = { p2 with y = 50 }
let p4 = { p1 with x = 5, y = 15 }

(* Matching on record fields *)
type User = { login: string, active: bool }

let status = fn u ->
  case u do
  | { active: true } -> u.login ^ " is active"
  | { login: name } -> name ^ " is inactive"
  | _ -> "unknown"
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

### Built-in Library Functions

The standard environment includes the helpers listed under [Built-in Functions](#built-in-functions). You can call them directly instead of re-implementing `map`, folds, and string helpers:

```ocaml
let () = print "no newline"
let () = println "with newline"

let () = println (int_to_str (str_length "hello"))
let () = println (str_concat "hello" " world")
let () = println (str_slice "hello" 1 3)

let () = println (int_to_str 99)

let () = println (int_to_str (list_length [1, 2, 3]))
let () = println (int_to_str (list_head [1, 2, 3]))
let () = println (int_to_str (list_head (list_tail [1, 2, 3])))
let () = println (int_to_str (list_nth [10, 20, 30] 2))

let () = println (int_to_str (tuple_fst (5, 10)))
let () = println (int_to_str (tuple_snd (5, 10)))

let squares = map (fn x -> x * x) [1, 2, 3, 4]
let evens = filter (fn x -> x % 2 == 0) [1, 2, 3, 4, 5, 6]
let sum = reduce_left (+) 0 [1, 2, 3, 4, 5]
let product = reduce_right (*) [1, 2, 3, 4] 1

let chars = string_to_list "hey"
let m = float_to_int 3.7
let x = int_to_float 42
```

### Algebraic Data Types

```ocaml
(* Type aliases *)
type IntPair = (int, int)

(* Sum types *)
type Option<'a> =
  | None
  | Some of 'a

type Either<'a, 'b> =
  | Left of 'a
  | Right of 'b

(* Recursive types *)
type rec List<'a> =
  | Nil
  | Cons of 'a * List<'a>

type rec Tree<'a> =
  | Leaf
  | Node of 'a * Tree<'a> * Tree<'a>

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

(* Type variable in an annotation: `'a`, `'b`, ... *)
let poly_id = fn (x: 'a) -> x
(* Type: 'a -> 'a *)
```

## Installation

### Prerequisites

- OCaml 5.0.0 or higher
- Dune build system
- OPAM (OCaml package manager)
- **Clang** (for the native compiler: LLVM IR → object code and linking with `runtime/ls_runtime.c`)

### Building from Source

1. Clone the repository:
```bash
git clone https://github.com/LambdaAK/LambdaScript
cd LambdaScript
```

*(The remote repository and default clone folder may still be named **LambdaScript** until you rename them; the language itself is **Forge**.)*

2. Build the project:
```bash
make
```

This builds the **interpreter**, **REPL**, **compiler** (`compile_forge`), and other developer tools under `bin/`.

## Usage

### Running the REPL

Start an interactive session:
```bash
make repl
```

In the REPL, you can type expressions and see their types and evaluated results immediately.

### Running Forge programs

Execute a `.ls` or `.txt` file containing Forge code:
```bash
dune exec ./bin/interpreter.exe <filename>
```

Example:
```bash
dune exec ./bin/interpreter.exe programs/factorial.txt
```

### File Extension

Source files typically use `.ls` or `.txt` extensions.

## Native compilation

The compiler parses and typechecks a Forge source file, lowers it to **Min IR** (`.mir`), emits **LLVM IR** (`.ll`), runs **Clang** to produce assembly (`.s`) and a **linked executable**. The C runtime in `runtime/ls_runtime.c` provides memory and runtime glue for the generated code.

From the repository root:

```bash
make compile-ls FILE=programs/minimal.ls
./a.out
```

Optional output name:

```bash
make compile-ls FILE=programs/minimal.ls OUT=./my_program
./my_program
```

Equivalent direct invocation:

```bash
dune exec ./bin/compile_forge.exe programs/minimal.ls ./my_program
```

**Finding the runtime:** compilation searches upward from the current directory for `runtime/ls_runtime.c`. If you run the compiler from elsewhere, set `FORGE_ROOT` to the checkout path (the legacy variable `LAMBDASCRIPT_ROOT` is still accepted).

**Inspecting IR without linking:**

```bash
make dump-ir FILE=programs/minimal.ls
```

Additional compiler integration tests and fixtures live in `test/compiler_cases/`.

## Testing

### Running Tests

The project ships two main test entry points (the exact counts change as tests are added):

- **Interpreter / typechecker / evaluator coverage** — OUnit suite:
  ```bash
  make suite
  ```
- **Compiler end-to-end cases** (compile, run native executable, compare output):
  ```bash
  make compiler-suite
  ```

To run everything Dune knows about (when the tree builds cleanly):

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

For a rigorous formal definition of Forge's semantics, see:
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
Forge/                # clone may still be named LambdaScript/ until the repo is renamed
├── bin/              # interpreter, REPL, compile_forge, dump_min_ir, ...
├── src/              # Source code
│   ├── lex.ml, parser.ml
│   ├── typecheck.ml, eval.ml, env.ml
│   ├── compile_pipeline.ml  # native compile driver (IR emit + Clang)
│   ├── min_ir.ml, lower_min_ir.ml, llvm_emit.ml  # compiler middle/back end
│   └── ...
├── runtime/          # ls_runtime.c (linked into native executables)
├── test/             # OUnit tests + compiler_cases/ integration fixtures
├── programs/         # Example .ls programs
└── documentation/    # Formal semantics
```

