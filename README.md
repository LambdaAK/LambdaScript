# LambdaScript - a Functional Programming Language Inspired by OCaml and Haskell

## Table of Contents
1. [Overview](#overview)
2. [Example Usages](#examples)
3. [Installation](#installation)
4. [Usage](#usage)
5. [Testing](#testing)
6. [Code Documentation](#documentation)


# Overview

LambdaScript is a ***statically-typed*** ***functional programming language*** designed to make it easy to write elegant and expressive code. It features a powerful type system underpinned by ***kinds***, which allows for ***higher-order arithmetic on types***. 

It also features a type inference engine similar to the ***Hindley-Milner algorithm***, which can infer the types of most expressions, so you don't have to write them out. 

Currently, LambdaScript is an interpreted language with the lexer, parser, typechecker, and evaluator all written in OCaml. In the future, I plan to make a compiler to JavaScript, which would allow LambdaScript to have a wider range of applications.


# Semantics

- To view the rigerously-defined semantics for LambdaScript, please refer to
  https://github.com/LambdaAK/LambdaScript/blob/main/documentation/LambdaScript.pdf

- Note that they are currently outdated, and have not yet been updated to reflect the new syntax and features.

# Examples
## Basic Types

_input_
```ocaml
17
```
_output_
```ocaml
Int: 17
```

_input_
```ocaml
true
```
_output_
```ocaml
Bool: true
```

_input_
```ocaml
"hello world!"
```
_output_
```ocaml
String: hello world!
```

_input_
```ocaml
()
```
_output_
```ocaml
Unit: ()
```

## Compound Types

_input_
```ocaml
[]
```
_output_
```ocaml
[a]: []
```

_input_
```ocaml
1 :: 2 :: 3 :: 4 :: 5 :: []
```
_output_
```ocaml
[Int]: [1, 2, 3, 4, 5]
```

_input_
```ocaml
[1, 2, 3, 4, 5]
```
_output_
```ocaml
[Int]: [1, 2, 3, 4, 5]
```

_input_
```ocaml
(1, true)
```
_output_
```ocaml
(Int, Bool): (1, true)
```

_input_
```ocaml
\ x -> x
```
_output_
```ocaml
a -> a: function
```

_input_
```ocaml
\ x -> x + 1
```
_output_
```ocaml
Int -> Int: function
```

## Let Expressions

_input_
```ocaml
let x = 1 in
let y = 2 in
x + y
```
_output_
```ocaml
Int: 3
```

_input_
```ocaml
let f x y z = x (y + z) in f
```
_output_
```ocaml
f : (Int -> a) -> Int -> Int -> a = function
```

## Ternary Expressions

_input_
```ocaml
if true then 1 else 2
```
_output_
```ocaml
Int: 1
```

## Switch Expressions

_input_
```ocaml
switch [] =>
  | [] -> true
  | _ :: _ -> false
end
```
_output_
```ocaml
Bool: true
```

_input_
```ocaml
switch [1, 2] =>
  | [] -> true
  | _ :: _ -> false
end
```
_output_
```ocaml
Bool: false
```

_input_
```ocaml

type IntList =
  | Nil
  | Cons (Int, IntList)
 
let rec length l =
  switch l =>
  | Nil -> 0
  | Cons (_, t) -> 1 + length t
  end
 
```
_output_
```ocaml
length : IntList -> Int = function
```

## Higher Order Functions

_input_
```ocaml
let rec map f arr =
  switch arr =>
    | [] -> []
    | h :: t -> f h :: map f t
  end
```
_output_
```ocaml
(a -> b) -> [a] -> [b]: function
```

_input_
```ocaml
map (\ x -> x * x) [1, 2, 3, 4, 5]
```
_output_
```ocaml
[Int]: [1, 4, 9, 16, 25]
```

_input_
```ocaml
let rec filter f arr =
  switch arr =>
    | [] -> []
    | h :: t ->
      if f h then h :: filter t
      else filter t
    end
```
_output_
```ocaml
(a -> Bool) -> [a] -> [a]: function
```

_input_
```ocaml
let rec reduce_left op arr acc =
  switch arr =>
  | [] -> acc
  | h :: t -> reduce_left op t (op acc h)
  end
```
_output_
```ocaml
(a -> b -> a) -> [b] -> a -> a: function
```

_input_
```ocaml
let rec reduce_right op arr acc =
  switch arr =>
  | [] -> acc
  | h :: t -> op h (reduce_right op t acc)
  end
```
_output_
```ocaml
(a -> b -> b) -> [a] -> b -> b: function
```

## Algebraic Data Types

_input_
```ocaml
type IntPair = (Int, Int)
```
_output_
```ocaml
type IntPair : (Int, Int)
```

_input_
```ocaml

type IntList =
  | Nil
  | Cons (Int, IntList)

```
_output_
```ocaml
type IntList : Nil | Cons (Int, IntList)
```

_input_
```ocaml

type IntBT =
  | Leaf
  | Node (Int, IntBT, IntBT)


```
_output_
```ocaml
type IntBT : Leaf | Node (Int, IntBT, IntBT)
```

_input_
```ocaml

type IntOpt =
  | None
  | Some (Int

```
_output_
```ocaml
type IntOpt : None | Some (Int)
```

## Polymorphic Algebraic Data Types

_input_
```ocaml
type Option a = | None | Some (a)
```
_output_
```ocaml
Option : * -> *
```

_input_
```ocaml
type List a = | Nil | Cons (a, List a)
```
_output_
```ocaml
List : * -> *
```

_input_
```ocaml
type Either a b = | Left (a) | Right (b)
```
_output_
```ocaml
Either : * -> * -> *
```

_input_
```ocaml
type AssocList key val = [(key, val)]
```
_output_
```ocaml
AssocList : * -> * -> *
```

_input_
```ocaml

     type App a b = a b
     
```
_output_
```

     App : (* -> *) -> * -> *
     
```

_input_
```ocaml
type App2 a b c d e = e (b c a) d
```
_output_
```
* -> (* -> * -> *) -> * -> * -> (* -> * -> *) -> *
```

# Installation

## Prerequisites
- OCaml 5.0.0 or higher
- Dune

## Building

- To clone the project, run the following command:
```bash
git clone https://github.com/LambdaAK/LambdaScript
```

- Then, `cd` into the project directory and run the following command:
```bash
make
```

# Usage

- You can run the REPL by running the following command:
```bash
make repl
```
- You can execute a LambdaScript file by running the following command:
```bash
dune exec ./bin/interpreter.exe <filename>
```

# Testing

## Testing Overview

There are currently around 450 unit tests in the `test` directory. In order to achieve greater exhaustiveness, I implemented a mechanism taking pre-built unit tests and generating new ones by applying random transformatios to them. This allows for a much greater number of tests to be generated, and thus a greater degree of confidence in the correctness of the implementation. With this, the 450 unit tests can be expanded to around 13,000 unit tests.

## Running Tests

To run the unit tests, run the following command:
```bash
make test
```

- To use the random test generator, set the `modify_tests` variable in `test/test.ml` to `true` and follow the instructions above.

- To run tests with coverage, run the following command:
```bash
make bisect
```


# Documentation

- To generate the code documentation, run the following command:
```bash
make doc
```
- To view the code documentation, run the following command
```bash
make opendoc
```