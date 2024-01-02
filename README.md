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

### int

_input_

```ocaml
1
```

_output_

```ocaml
Int: 1
```

<br><br>

_input_

```ocaml
17
```

_output_

```ocaml
Int: 17
```

<br><br>

### bool

_input_

```ocaml
true
```

_output_

```ocaml
Bool: true
```

<br><br>

_input_

```ocaml
false
```

_output_

```ocaml
Bool: false
```

<br><br>

### str

_input_

```ocaml
"hello"
```

_output_

```ocaml
Str: "hello"
```

<br><br>

_input_

```ocaml
"world"
```

_output_

```ocaml
Str: "world"
```

<br><br>

### Unit

_input_

```ocaml
()
```

_output_

```ocaml
Unit: ()
```

<br><br>

## compound types

### list

_input_

```ocaml
[]
```

_output_

```ocaml
[a]: []
```

<br><br>

_input_

```ocaml
1 :: 2 :: 3 :: 4 :: 5 :: []
```

_output_

```ocaml
[Int]: [1, 2, 3, 4, 5]
```

<br><br>

_input_

```ocaml
[1, 2, 3, 4, 5]
```

_output_

```ocaml
[Int]: [1, 2, 3, 4, 5]
```

<br><br>

_input_

```ocaml
[] :: []
```

_output_

```ocaml
[[a]]: [[]]
```

<br><br>

### vector

_input_

```ocaml
(1, true)
```

_output_

```ocaml
(Int, Bool): (1, true)
```

<br><br>

_input_

```ocaml
(1, (true, (), "a"))
```

_output_

```ocaml
(Int, (Bool, Unit, Str)): (1, (true, (), "a"))
```

<br><br>

## function types

_input_

```ocaml
\ x -> x
```

_output_

```ocaml
a -> a: function
```

<br><br>

_input_

```ocaml
\ x -> x + 1
```

_output_

```ocaml
Int -> Int: function
```

<br><br>

_input_

```ocaml
\ x -> \ y -> x + y
```

_output_

```ocaml
Int -> Int -> Int: function
```

<br><br>

## bind expressions

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

<br><br>

_input_

```ocaml
let f x y z = x (y + z)
```

_output_

```ocaml
f : (Int -> a) -> Int -> Int -> a = function
```

<br><br>

_input_

```ocaml
let f x y z = x (y + z) in f (\ x -> x > 0) 1 2
```

_output_

```ocaml
Bool: true
```

_input_

```ocaml
let (a, b) = (1, 2) in a + b
```

_output_

```ocaml
Int: 3
```

<br><br>

## ternary expressions

_input_

```ocaml
if true then 1 else 2
```

_output_

```ocaml
Int: 1
```

<br><br>

_input_

```ocaml
if false then 1 else 2
```

_output_

```ocaml
Int: 2
```

<br><br>

# 

## switch expresssions

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

<br><br>

_input_

```ocaml
switch 1 :: 2 :: [] =>
  | [] -> true
  | _ :: _ -> false
end
```

_output_

```ocaml
Bool: false
```

# some more advanced examples

## map

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

<br><br>

_input_

```ocaml
map (\ x -> ~- x) ([1,2,3,4,5])
```

_output_

```ocaml
[Int]: [-1, -2, -3, -4, -5]
```

<br><br>

## filter

_input_

```
let rec filter f arr =
  switch arr =>
    | [] -> []
    | h :: t ->
      if f h then h :: filter t
      else filter t
    end

```

_output_

```
(a -> Bool) -> [a] -> [a]: function
```

# fibonacci

_input_

```ocaml
let rec fib n =
  switch n =>
    | 0 -> 1
    | 1 -> 1
    | _ -> fib (n - 1) + fib (n - 2)
  end

```

<br><br>

_output_

```
Int -> Int: function
```

<br><br>

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

<br><br>

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