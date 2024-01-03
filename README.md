# LambdaScript

## An functional programming language inspired by Haskell and OCaml

Lambdascript is a statically-typed functional programming language designed to
make it easy to write elegant and expressive code. It has key features that
allow users to write clean and expressive code.

# Features

- Basic and compound data types: int, bool, str, unit, list, vector, et cetera

- Functional constructs: ternary expressions, anonymous functions, let
  expressions, currying, structural pattern matching

- Type inference algorithm: uses a type constraint generator and a unification
  algorithm to infer types

- A REPL allowing a user to type expressions and receive their value and type

- Rigorous OUnit test suite that utilizes functors, taking pre-built unit tests
  and outputting modified ones

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
hello world!
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
  | Some (Int)

```
_output_
```ocaml
type IntOpt : None | Some (Int)
```


