# LambdaScript

## An functional programming language inspired by Haskell and OCaml

<br><br>

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
<br><br>
_input_
```ocaml
17
```
_output_
```ocaml
Int: 17
```
<br><br><br><br>
_input_
```ocaml
true
```
_output_
```ocaml
Bool: true
```
<br><br><br><br>
_input_
```ocaml
hello world!
```
_output_
```ocaml
String: hello world!
```
<br><br><br><br>
_input_
```ocaml
()
```
_output_
```ocaml
Unit: ()
```
<br><br>
## Compound Types
<br><br>
_input_
```ocaml
[]
```
_output_
```ocaml
[a]: []
```
<br><br><br><br>
_input_
```ocaml
1 :: 2 :: 3 :: 4 :: 5 :: []
```
_output_
```ocaml
[Int]: [1, 2, 3, 4, 5]
```
<br><br><br><br>
_input_
```ocaml
[1, 2, 3, 4, 5]
```
_output_
```ocaml
[Int]: [1, 2, 3, 4, 5]
```
<br><br><br><br>
_input_
```ocaml
(1, true)
```
_output_
```ocaml
(Int, Bool): (1, true)
```
<br><br><br><br>
_input_
```ocaml
\ x -> x
```
_output_
```ocaml
a -> a: function
```
<br><br><br><br>
_input_
```ocaml
\ x -> x + 1
```
_output_
```ocaml
Int -> Int: function
```
<br><br>
## Let Expressions
<br><br>
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
<br><br><br><br>
_input_
```ocaml
let f x y z = x (y + z) in f
```
_output_
```ocaml
f : (Int -> a) -> Int -> Int -> a = function
```
<br><br>
## Ternary Expressions
<br><br>
_input_
```ocaml
if true then 1 else 2
```
_output_
```ocaml
Int: 1
```
<br><br>
## Switch Expressions
<br><br>
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
<br><br><br><br>
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
<br><br><br><br>
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
<br><br>
## Higher Order Functions
<br><br>
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
<br><br><br><br>
_input_
```ocaml
map (\ x -> x * x) [1, 2, 3, 4, 5]
```
_output_
```ocaml
[Int]: [1, 4, 9, 16, 25]
```
<br><br><br><br>
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
<br><br><br><br>
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
<br><br><br><br>
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
<br><br>
## Algebraic Data Types
<br><br>
_input_
```ocaml
type IntPair = (Int, Int)
```
_output_
```ocaml
type IntPair : (Int, Int)
```
<br><br><br><br>
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
<br><br><br><br>
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
<br><br><br><br>
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
<br><br>