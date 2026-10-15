# Forge

**Forge** is a statically typed functional language with type inference, polymorphism, pattern matching, and (via the standard prelude) traits/typeclasses.

The reference implementation in this repository is written in **OCaml** and includes both:

- an **interpreter** (evaluate programs directly), and  
- an **ahead-of-time compiler** that lowers programs to **Min IR**, then **LLVM IR**, then **assembly**, and links a **native executable** with Clang using a small C runtime (`runtime/ls_runtime.c`).

An experimental TypeScript implementation is maintained in a separate repository.

## Docker (quick start)

Run Forge without a local OCaml/opam/clang toolchain.

**Build** the image from the repository root:

```bash
docker build -t forge:local .
```

**Rebuild** after changing sources (same command). For a clean rebuild that ignores the layer cache:

```bash
docker build --no-cache -t forge:local .
```

**Run** a program that was copied into the image at build time (`programs/` is available as `/opt/forge/programs/`):

```bash
docker run --rm forge:local run /opt/forge/programs/<file>.forge
```

**Run your working tree** (no rebuild needed; mount the repo and use `/work`):

```bash
docker run --rm -v "$PWD":/work -w /work forge:local run /work/programs/<file>.forge
```

Other entrypoints: `docker run --rm forge:local help`, `docker run --rm -it forge:local repl`, and compile/run native binaries as described under [Installation](#installation) (subsection **Docker Quick Start**).

## Table of Contents

1. [Docker (quick start)](#docker-quick-start)
2. [Overview](#overview)
3. [Standard prelude](#standard-prelude)
4. [Language Features](#language-features)
5. [Examples](#examples)
6. [Installation](#installation)
7. [Usage](#usage)
8. [Native compilation](#native-compilation)
9. [Editor support: LSP hover](#editor-support-lsp-hover)
10. [Testing](#testing)
11. [Language Semantics](#language-semantics)
12. [Documentation](#documentation)
13. [Project Structure](#project-structure)

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
- **Traits / typeclasses** (`trait` / `inter`, `impl … for …`, dictionary passing in the compiler)
- **`macro_rules!` token-tree macros** with fragment matching and repetition (`*`, `+`)

## Standard prelude

By default, user programs are combined with [`prelude/prelude.forge`](prelude/prelude.forge) (some tools, such as `forge_hover`, can opt out of prepending for a raw buffer):

- **Interpreter and `compile_forge`**: the prelude source is **prepended** to your file (skipping self-prepend when you are editing the prelude itself). Resolution searches `prelude/prelude.forge` from the current directory, the executable’s directory, and a few parent layouts—see [`src/prelude.ml`](src/prelude.ml).
- **REPL**: the prelude is **loaded once** at startup into the environment (not re-prepended per line).

The prelude defines the canonical `List`/`Option` types, standard traits (Functor, Applicative, Monad, Foldable, …), and default `Show` / `Eq` / `Ord` instances for built-in types. You can still define your own traits using either `trait … where` (prelude style) or `inter …` (see [`programs/haskell_style_typeclasses.forge`](programs/haskell_style_typeclasses.forge)).

## Language Features

### Type System

- **Type Inference**: Automatic type deduction using constraint-based type inference
- **Polymorphism**: Generic types with type parameters (`a`, `b`, … — syntax `<a>` on type constructors)
- **Type Annotations**: Optional type annotations for clarity
- **Algebraic Data Types**: Sum types with constructors
- **Recursive Types**: Self-referential type definitions
- **Type Aliases**: Named type definitions
- **Records**: Named rows with `{ field: ty, ... }`, field access `r.field`, update `{ base with field = expr }`

**Basic Types**: `int`, `float`, `bool`, `string`, `char`, `unit`

**Composite Types**: Functions (`a -> b`), lists (`[a]` or `List<a>` once the prelude is loaded), tuples (`(a, b, c)`), records

### Typeclasses (traits)

Forge’s traits are the surface syntax for **typeclasses**: named bundles of operations (methods) that types can implement. The compiler passes **dictionaries** at call sites so polymorphic code can use the right implementation for each type. The standard prelude defines the usual hierarchy (`Functor`, `Applicative`, `Monad`, `Show`, `Eq`, `Ord`, `Semigroup`, `Monoid`, …) and instances for built-ins; your own programs can add new traits and instances with **`trait`** (or **`inter`**) and **`impl`**.

#### Declaring a trait: `trait` and `inter`

A **trait declaration** introduces a class name, optional type parameters, optional **supertrait** constraints, and a body of method signatures (and optional default definitions):

- **Shape**: `trait Name<…> requires … where … end`
- **Type parameters** can be simple (`<a>`, `<a, b>`) or **higher-kinded** (`<f<_>>`) for type constructors, as in `trait Functor<f<_>> where …` in the prelude.
- **`requires`** lists traits that must already be implemented for the same type parameters (e.g. `trait Monoid<a> requires Semigroup<a> where …`). That corresponds to a superclass constraint in Haskell-style typeclasses.
- Inside **`where … end`**, each method is introduced with **`val`** and its type (`val show : a -> String`). You can add default implementations with **`let`** in the trait body so instances may omit them unless they override.

The keyword **`inter`** is an alternative, brace-oriented spelling for the same concept (see [`programs/haskell_style_typeclasses.forge`](programs/haskell_style_typeclasses.forge) and the small [`programs/typeclass_show.forge`](programs/typeclass_show.forge)). Prefer **`trait … where … end`** in new code if you want to match the prelude style.

**Source order:** every trait must be **declared above** any `impl` that uses it in the same compilation unit. The parser/condenser resolves `impl` against traits seen earlier in the file (and, for the REPL, against traits from the prelude that were loaded at startup).

#### Implementing a trait: `impl`

An **instance** is written **`impl Trait for Type where … end`**:

- **`Type`** is the implementing type: monomorphic (`Int`, `Bool`, `String`, …), a type constructor applied to parameters (`List<a>`, `Option<a>`), etc., as allowed by the typechecker.
- Inside **`where … end`**, you give one equation per method required by the trait (unless the trait gave a default `let` you are happy to inherit), written like a function definition without `let` — `show x = …`, `(++) x y = …` — and separated by commas.
- Instances may be **parameterized**: for example, `impl Semigroup for List<a>` in the prelude implements the trait for all element types `a` at once.

Illustrative fragments (same ideas as [`prelude/prelude.forge`](prelude/prelude.forge)):

```text
trait Semigroup<a> where
  val mappend : a -> a -> a
  val (++) : a -> a -> a
end

trait Monoid<a> requires Semigroup<a> where
  val empty : a
end

impl Semigroup for String where
  mappend x y = str_concat x y,
  (++) x y = mappend x y
end

impl Monoid for String where
  empty = ""
end
```

#### Further reading and limitations

- **Prelude**: [`prelude/prelude.forge`](prelude/prelude.forge) is the canonical reference for trait definitions and instances.
- **Smaller demos**: [`programs/typeclass_show.forge`](programs/typeclass_show.forge), [`programs/functor_list.forge`](programs/functor_list.forge).
- **Both back ends**: the interpreter passes dictionaries at run time; the native compiler resolves them statically (one specialised copy of a function per instance it is used with). [`programs/haskell_style_typeclasses.forge`](programs/haskell_style_typeclasses.forge) implements the prelude's traits for user types; [`programs/typeclass_functor_native_list.forge`](programs/typeclass_functor_native_list.forge) gives built-in lists a `Functor` instance.

### Macros (`macro_rules!`)

Forge supports Rust-style declarative macros that expand before typechecking and evaluation/compilation.

#### Defining and invoking macros

```text
macro_rules! add where
  ($a:expr, $b:expr) => $a + $b
end

let x = add!(1, 2)
let y = add![3, 4]
let z = add!{5, 6}
```

- Macros are declared with `macro_rules! name where ... end` (same delimiters as `mod` / `impl` bodies).
- A macro can have multiple arms; expansion uses the first arm whose matcher fits.
- Invocation delimiters `()`, `[]`, and `{}` are all supported.

#### Fragment kinds

Matcher metavariables can be typed with:

- `expr`
- `pat`
- `ty` / `type`
- `ident`
- `item`
- `tt`
- `literal` / `lit`
- `path`
- `block`

Example:

```text
macro_rules! id1 where
  ($x:ident) => $x
end
macro_rules! use_path where
  ($p:path) => $p
end
macro_rules! show_ty where
  ($t:ty) => "ok"
end
```

#### Repetition

Repetitions follow Rust-like syntax:

- `$( ... )*` for zero or more
- `$( ... )+` for one or more
- optional separator: `$( ... ),*`, `$( ... );+`, etc.

Example passthrough:

```text
macro_rules! passthrough where
  ($($x:expr),*) => vec!($($x),*)
end
```

Compatibility behavior currently implemented:

- If a repeated capture (for example `$x` from `($($x:expr),*)`) is used directly (not inside a transcriber repetition), it expands to a list literal.
  - Example: `($($x:expr),*) => $x` expands to `[ ... ]`.

#### Built-in macros

Forge currently includes these built-in macros:

- `count_args!(...)` -> integer count of comma-separated arguments
- `vec!(...)` -> list literal
- `stringify!(...)` -> string representation of argument expression ASTs
- `concat!(...)` / `concat_str!(...)` -> concatenates string literal arguments

Example:

```text
macro_rules! debug_expr where
  ($e:expr) => concat!("DBG(", stringify!($e), ")")
end
```

#### Scope and placement

- Macros can be declared at top level, including inside modules.
- `macro_rules!` definitions are not supported inside expression blocks.
- Current macro expansion target is expressions (not arbitrary item generation).

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
- **List Ranges**: `[1...10]` or `[1 ... 10]` (spaces optional)
- **List Comprehensions**: `[x * 2 | x <- [1...5], x > 2]` (generators `p <- list`, filters `cond`; elements a generator pattern does not match are skipped)
- **Tuples/Vectors**: `(1, "hello", true)`
- **Code Blocks**: `{ expr1; expr2; result }`
- **Macro Invocations**: `name!(...)`, `name![...]`, `name!{...}`

### Operators

All operators can be used as first-class values by wrapping in parentheses: `(+)`, `(*)`, `(::)`, etc.

**Arithmetic**: `+`, `-`, `*`, `/`, `%` on `Int`; `+.`, `-.`, `*.`, `/.` on `Float` (same precedence as `+` and `*`)

**Strings**: `^` (concatenation)

**Comparison**: `==`, `!=`, `<>` (same as `!=`), `<`, `>`, `<=`, `>=` — structural, on values of any type except functions (records field by field, constructors in declaration order)

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
- `print : String -> Unit` - print a string without a newline
- `print_string : String -> Unit` - print a string followed by a newline
- `println : a -> Unit` (prelude, for any `a` with a `Show` instance) - print `show x` followed by a newline
- `show : a -> String` (prelude `Show` trait) - instances for `Int`, `Float`, `Bool`, `Char`, `String`, `Unit`, `Ordering`, lists, `Option` and pairs/triples

**Type Conversions**:
- `int_to_str : int -> string`
- `int_to_float : int -> float`
- `float_to_int : float -> int` (truncates toward zero)
- `float_to_str : float -> string` (shortest form that reads back: `0.1`, `3.0`)
- `sqrt`, `exp`, `log : float -> float`
- `char_to_str : char -> string`
- `string_to_list : string -> [char]` (also `list_of_string`)

**String Operations**:
- `str_length : string -> int` - length of string
- `str_concat : string -> string -> string` - concatenate two strings
- `str_slice : string -> int -> int -> string` - substring (start, length)

**List Operations**:
- `list_length : [a] -> int` - length of list
- `list_head : [a] -> a` - first element (fails on empty)
- `list_tail : [a] -> [a]` - all but first element
- `list_nth : [a] -> int -> a` - nth element (0-indexed)

**Tuple Operations**:
- `tuple_fst : (a, b) -> a` - first element of pair
- `tuple_snd : (a, b) -> b` - second element of pair

**Higher-Order Functions**:
- `map : (a -> b) -> [a] -> [b]`
- `filter : (a -> bool) -> [a] -> [a]`
- `reduce_left : (a -> b -> a) -> a -> [b] -> a`
- `reduce_right : (a -> b -> b) -> [a] -> b -> b`
- `not : bool -> bool`

## Examples

The snippets below are illustrative; runnable examples live under [`programs/`](programs/), e.g. [`programs/minimal.forge`](programs/minimal.forge), [`programs/builtins_test.forge`](programs/builtins_test.forge), [`programs/record_update.forge`](programs/record_update.forge), [`programs/red_black_tree_example.forge`](programs/red_black_tree_example.forge), and the typeclass demos linked in [Typeclasses (traits)](#typeclasses-traits).

Additional macro-focused examples:

- [`programs/test.forge`](programs/test.forge)
- [`test/macro_system_tests.ml`](test/macro_system_tests.ml) (language-level macro coverage)

### Basic Types and Expressions

```ocaml
// Integers
42
-17

// Floats
3.14
-2.5
int_to_float 2
float_to_int 3.9

// Booleans
true
false

// Strings
"Hello, world!"

// Characters
'a'
'Z'

// Unit
()
```

### Variables and Let Bindings

```ocaml
// Simple binding
let x = 42 in x + 1
// Result: 43

// Multiple bindings
let x = 5 in
let y = 10 in
x + y
// Result: 15

// Type annotations
let (x: int) = 42 in x
```

### Functions

```ocaml
// Lambda function
fn x -> x + 1

// Named function
let add = fn x -> fn y -> x + y in
add 3 4
// Result: 7

// Pattern matching in parameters
let first = fn (x, y) -> x in
first (5, 10)
// Result: 5

// With type annotations
let increment = fn (x: int) : int -> x + 1 in
increment 41
// Result: 42

// Recursive functions
let rec factorial = fn n ->
  if n == 0 then 1
  else n * factorial (n - 1)
in
factorial 5
// Result: 120
```

### Operators as First-Class Values

```ocaml
// Use operators as functions
let add = (+) in
add 3 4
// Result: 7

// Partial application
let add5 = (+) 5 in
add5 10
// Result: 15

// Pass to higher-order functions
map ((*) 2) [1, 2, 3]

// Custom operator definitions
let (++) = fn a -> fn b -> a + b in
(++) 10 20
// Result: 30
```

### Macros

```text
macro_rules! choose where
  () => 0;
  ($x:expr) => $x
end

let a = choose!()
let b = choose!{42}
```

```text
macro_rules! collect where
  ($($x:expr),*) => $x
end

let xs = collect!(1, 2, 3, 4)
let n = list_length xs
// n == 4
```

```text
macro_rules! collect_and_count where
  ($($x:expr),*) => count_args!($($x),*)
end

let n0 = collect_and_count!()
let n3 = collect_and_count!(1, 2, 3)
```

### Lists

```ocaml
// Empty list
[]

// List literals
[1, 2, 3, 4, 5]

// Cons operator
1 :: 2 :: 3 :: []
// Result: [1, 2, 3]

// List ranges
[1...10]
// Result: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

// List comprehensions
[x * x | x <- [1...5]]
// Result: [1, 4, 9, 16, 25]

// With filter
[x | x <- [1...10], x % 2 == 0]
// Result: [2, 4, 6, 8, 10]
```

### Tuples/Vectors

```ocaml
// Pairs
(1, 2)

// Triples
(1, "hello", true)

// Pattern matching tuples
let (x, y) = (5, 10) in x + y
// Result: 15
```

### Conditionals and Boolean Operators

```ocaml
// if / then / else (both branches must match in type)
if 3 < 5 then "yes" else "no"

// Short-circuiting logical operators
true && false
true || false
not true

// Combining comparisons
let n = 7 in
if n > 0 && n % 2 == 0 then "positive even" else "other"
```

### Pattern Matching

```ocaml
// Match on lists
let rec length = fn lst ->
  case lst do
  | [] -> 0
  | h :: t -> 1 + length t
in
length [1, 2, 3]
// Result: 3

// Match on integers
let sign = fn n ->
  case n do
  | 0 -> "zero"
  | n -> if n > 0 then "positive" else "negative"
in
sign (-5)
// Result: "negative"

// Complex patterns
let get_second = fn lst ->
  case lst do
  | [] -> 0
  | x :: [] -> 0
  | x :: y :: rest -> y
in
get_second [1, 2, 3]
// Result: 2

// Record patterns (fields can be literals or binders)
let person = { name: "John", age: 30, city: "New York" }
let tag = case person do
  | { name: "Alex" } -> 1
  | { name: "John" } -> 2
  | _ -> 3
// tag == 2
```

### Records

```ocaml
// Record type and literals
type Point = { x: int, y: int }

let p1 : Point = { x: 10, y: 20 }

// Field access
let sum_xy = p1.x + p1.y

// Functional update (immutable: creates a new record)
let p2 = { p1 with x = 100 }
let p3 = { p2 with y = 50 }
let p4 = { p1 with x = 5, y = 15 }

// Matching on record fields
type User = { login: string, active: bool }

let status = fn u ->
  case u do
  | { active: true } -> u.login ^ " is active"
  | { login: name } -> name ^ " is inactive"
  | _ -> "unknown"
```

### Higher-Order Functions

```ocaml
// Map
let rec map = fn f -> fn lst ->
  case lst do
  | [] -> []
  | h :: t -> f h :: map f t
in
map (fn x -> x * x) [1, 2, 3, 4, 5]
// Result: [1, 4, 9, 16, 25]

// Filter
let rec filter = fn pred -> fn lst ->
  case lst do
  | [] -> []
  | h :: t ->
    if pred h then h :: filter pred t
    else filter pred t
in
filter (fn x -> x % 2 == 0) [1, 2, 3, 4, 5, 6]
// Result: [2, 4, 6]

// Fold/Reduce
let rec fold_left = fn op -> fn acc -> fn lst ->
  case lst do
  | [] -> acc
  | h :: t -> fold_left op (op acc h) t
in
fold_left (+) 0 [1, 2, 3, 4, 5]
// Result: 15
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
// Type aliases
type IntPair = (int, int)

// Sum types
type Option<a> =
  | None
  | Some of a

type Either<a, b> =
  | Left of a
  | Right of b

// Recursive types
type rec List<a> =
  | []
  | (::) of (a, List<a>)

type rec Tree<a> =
  | Leaf
  | Node of (a, Tree<a>, Tree<a>)

// Using custom types
let rec tree_size = fn t ->
  case t do
  | Leaf -> 0
  | Node (value, left, right) ->
    1 + tree_size left + tree_size right
in
tree_size (Node (5, Leaf, Node (3, Leaf, Leaf)))
// Result: 2
```

### Polymorphic Functions

```ocaml
// Identity function
let id = fn x -> x
// Type: a -> a

// Composition
let compose = fn f -> fn g -> fn x -> f (g x)
// Type: (b -> c) -> (a -> b) -> a -> c

// Map for any type
let rec map = fn f -> fn lst ->
  case lst do
  | [] -> []
  | h :: t -> f h :: map f t
// Type: (a -> b) -> [a] -> [b]
```

### Code Blocks

```ocaml
{
  let x = 5;
  let y = 10;
  let z = x + y;
  z * 2
}
// Result: 30
```

### Type Annotations

```ocaml
// Variable annotations
let (x: int) = 42 in x

// Function parameter annotations
let add = fn (x: int) -> fn (y: int) -> x + y in
add 3 4

// Return type annotations
let increment = fn (x: int) : int -> x + 1 in
increment 5

// Full function type annotation
let apply = fn (f: int -> int) -> fn (x: int) : int -> f x in
apply (fn x -> x * 2) 21
// Result: 42

// Type variable in an annotation: a, b, …
let poly_id = fn (x: a) -> x
// Type: a -> a
```

## Installation

### Docker Quick Start (no local toolchain)

If you want to try Forge without installing `opam`, `ocaml`, or `clang` locally, use Docker.

Build the image from this repository:

```bash
docker build -t forge:local .
```

Run the bundled minimal program:

```bash
docker run --rm forge:local run /opt/forge/programs/minimal.forge
```

Expected output:

```text
3
```

Start a REPL:

```bash
docker run --rm -it forge:local repl
```

Run a local source file (from your current host directory):

```bash
docker run --rm -it -v "$PWD":/work -w /work forge:local run /work/hello.forge
```

Native compile a local file:

```bash
docker run --rm -it -v "$PWD":/work -w /work forge:local compile /work/hello.forge
docker run --rm -v "$PWD":/work -w /work forge:local /work/a.out
```

`forge compile` in the container always writes the executable to `/work/a.out`
(on your host this is `./a.out` in the mounted directory).

If you publish this image (for example `ghcr.io/<owner>/forge:latest`), replace `forge:local` with that image tag in the commands above.

### Quick Start (10 minutes)

#### 1) Install toolchain

Forge needs **OCaml**, **Dune**, **OPAM**, **Clang**, and the **Boehm garbage collector** (`bdw-gc`, used by natively compiled programs).

macOS (Homebrew):
```bash
brew install opam
brew install llvm clang bdw-gc
opam init -a --disable-sandboxing
eval "$(opam env)"
opam switch create 5.1.1
eval "$(opam env)"
opam install dune ounit2
```

Ubuntu / Debian:
```bash
sudo apt update
sudo apt install -y opam m4 pkg-config libgmp-dev clang libgc-dev
opam init -a
eval "$(opam env)"
opam switch create 5.1.1
eval "$(opam env)"
opam install dune ounit2
```

(`ounit2` is the test framework; `make` builds the test suites too.)

Verify:
```bash
ocamlc -version
dune --version
clang --version
```

#### 2) Clone and build

```bash
git clone https://github.com/LambdaAK/Forge
cd Forge
make
```

This builds the **interpreter**, **REPL**, **compiler** (`compile_forge`), and other tools under `bin/`.

#### 3) Run your first program

```bash
dune exec ./bin/interpreter.exe programs/minimal.forge
```

Expected output:
```text
3
```

## Usage

### Running the REPL

Start an interactive session:
```bash
make repl
```

Optionally preload a file after the prelude:

```bash
make repl FILE=programs/simple_test.forge
```

In the REPL, you can type expressions and see their types and evaluated results immediately.

### Running Forge programs

Execute a `.forge` file containing Forge code (from the repository root, with the prelude available as usual):
```bash
dune exec ./bin/interpreter.exe <filename>
```

Example:
```bash
dune exec ./bin/interpreter.exe programs/minimal.forge
```

### File Extension

Source files use `.forge`.

## Native compilation

The compiler parses and typechecks a Forge source file, lowers it to **Min IR** (`.mir`), emits **LLVM IR** (`.ll`), runs **Clang** to produce assembly (`.s`) and a **linked executable**. The C runtime in `runtime/ls_runtime.c` provides memory management (the Boehm–Demers–Weiser conservative garbage collector, found with `pkg-config bdw-gc`; set `FORGE_GC_FLAGS` to override the flags) and runtime glue for the generated code.

From the repository root:

```bash
make compile-forge FILE=programs/minimal.forge
./a.out
```

Optional output name:

```bash
make compile-forge FILE=programs/minimal.forge OUT=./my_program
./my_program
```

Equivalent direct invocation:

```bash
dune exec ./bin/compile_forge.exe programs/minimal.forge ./my_program
```

**Finding the runtime:** compilation searches upward from the current directory for `runtime/ls_runtime.c`. If you run the compiler from elsewhere, set `FORGE_ROOT` to the checkout path.

**Inspecting IR without linking:**

```bash
make dump-ir FILE=programs/minimal.forge
```

Additional compiler integration tests and fixtures live in `test/compiler_cases/`.

## Editor support: LSP hover

[`bin/forge_hover.ml`](bin/forge_hover.ml) builds **`forge_hover`**, a small stdin/stdout tool meant to be driven by an LSP server for hover/type-at-point:

```bash
dune exec ./bin/forge_hover.exe path/to/file.forge 1 10 4 < path/to/file.forge
```

Arguments: `path`, `prelude` (`1`/`true` to prepend the standard prelude, `0`/`false` for raw buffer only), zero-based `line`, zero-based `character`. On success it prints the type string to stdout; on failure it prints `ERROR: …` and exits with a non-zero status. Query logic lives in [`src/hover_query.ml`](src/hover_query.ml).

## Testing

### Running Tests

The project ships two main test entry points (the exact counts change as tests are added):

- **Interpreter / typechecker / evaluator coverage** — OUnit suite:
  ```bash
  make suite
  ```
- **All test suites** (every suite under `test/`, including the compiler end-to-end cases, which compile, run the native executable and compare its output):
  ```bash
  make compiler-suite
  ```

`dune test` runs the same suites. Several of them run every program through **both** the interpreter and the native compiler and require identical output: `eval_tests`, `features_tests`, `prelude_tests`, `compiler_interpreter_parity_tests` (all of `programs/`, `examples/`, `test/compiler_cases/` and `test/diff_cases/`) and `fuzz_tests` (randomly generated well-typed programs; set `FORGE_FUZZ_SEED` / `FORGE_FUZZ_COUNT` to explore more).

### Test Coverage

To run tests with coverage analysis:
```bash
make bisect
```

This will generate a coverage report showing which parts of the codebase are tested.

### Test Organization

- **`test/test.ml`** — large OUnit suite for the interpreter pipeline: type checking, inference, evaluation, pattern matching, ADTs, higher-order functions, builtins, and edge cases.
- **`test/lexer_tests.ml`, `parser_tests.ml`, `typecheck_tests.ml`** — the front end, including error messages and their locations.
- **`test/eval_tests.ml`, `prelude_tests.ml`, `features_tests.ml`** — semantics, the prelude, traits / macros / modules / imports, in both back ends.
- **`test/exhaustiveness_tests.ml`** — warnings for non-exhaustive and redundant `case` arms.
- **`test/cli_tests.ml`** — exit codes and diagnostics of the executables.
- **`test/macro_system_tests.ml`** — `macro_rules!` matching/expansion and builtin macro behavior.
- **`test/compiler_tests.ml`** + **`test/compiler_cases/`** — compile with `compile_forge`, run the binary, compare stdout.
- **`test/compiler_interpreter_parity_tests.ml`**, **`test/fuzz_tests.ml`** — differential testing of the two back ends.
- **`test/hover_ident_tests.ml`** — hover / identifier typing via `Hover_query`.
- **`test/manual_tests.ml`** — every example of the reference manual (`documentation/examples/`), in both back ends.

## Language Semantics

The **Forge Reference Manual** ([`documentation/Forge.tex`](documentation/Forge.tex)) is the complete description of the language: a guided tour, the tools, a reference chapter for every part of the language, the EBNF grammar with typing and evaluation rules, the standard library (every function, with an example), how the implementation works, and appendices on operators, reserved words and error messages.

Build it (needs a LaTeX distribution with `latexmk`, `pdflatex` and `makeindex`) and open `documentation/Forge.pdf`:

```bash
make docs
```

Every program in the manual is a file under [`documentation/examples/`](documentation/examples), and `test/manual_tests.ml` runs each one through the interpreter and the native compiler and checks that it prints exactly what the manual shows.

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

## Website

The repository includes a static website/docs app under `website/`, built with **React + TypeScript + Vite** (no backend).

### Run locally

```bash
npm --prefix website install
npm --prefix website run dev
```

Then open the local URL printed by Vite (usually `http://localhost:5173`).

### Production build

```bash
npm --prefix website run build
npm --prefix website run preview
```

### Netlify deployment

`netlify.toml` is configured to:

- run `npm ci --prefix website && npm --prefix website run build`
- publish `website/dist`
- rewrite SPA routes to `index.html`
- use Node.js `20`

If you deploy from the Netlify control panel:

- **Build command**: `npm ci --prefix website && npm --prefix website run build`
- **Publish directory**: `website/dist`
- **Base directory**: leave empty (repo root)

## Project Structure

```
Forge/                # repository root (language: Forge)
├── bin/              # interpreter, repl, compile_forge, dump_min_ir, forge_hover, …
├── src/              # lexer, parser, typecheck, interpreter, compiler pipeline
│   ├── lex.ml, parser.ml, condense.ml   # front end (tokens, AST, core AST)
│   ├── typecheck.ml                     # inference and type-class elaboration
│   ├── exhaustiveness.ml                # missing / redundant case arms
│   ├── ceval.ml                         # the interpreter
│   ├── compile_pipeline.ml              # native driver (prelude, typecheck, IR, Clang)
│   ├── specialize.ml, mono.ml, tast.ml  # static dictionaries, monomorphisation
│   ├── lower_typed.ml, min_ir.ml, llvm_emit.ml  # typed lowering, Min IR, LLVM
│   ├── hover_query.ml                   # type-at-point for forge_hover / IDE integration
│   └── ...
├── prelude/          # prelude source (prepended or REPL-loaded)
├── runtime/          # ls_runtime.c (linked into native executables)
├── test/             # OUnit suites, compiler_cases/, diff_cases/
├── programs/         # example .forge programs
├── documentation/    # Forge.tex, the reference manual (make docs)
└── website/          # React + TypeScript + Vite website/docs app
```
