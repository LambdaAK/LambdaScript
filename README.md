# LambdaScript

## An interpreted, functional programming language written using OCaml

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

# Examples

## Basic Types

### int

_input_

```
1
```

_output_

```
int: 1
```

<br><br>

_input_

```
17
```

_output_

```
int: 17
```

<br><br>

### bool

_input_

```
true
```

_output_

```
bool: true
```

<br><br>

_input_

```
false
```

_output_

```
bool: false
```

<br><br>

### str

_input_

```
"hello"
```

_output_

```
str: "hello"
```

<br><br>

_input_

```
"world"
```

_output_

```
str: "world"
```

<br><br>

### ng ("nothing", also called "unit")

_input_

```
()
```

_output_

```
ng: ()
```

<br><br>

## compound types

### list

_input_

```
[]
```

_output_

```
[t1]: []
```

<br><br>

_input_

```
1 :: 2 :: 3 :: 4 :: 5 :: []
```

_output_

```
[int]: [1, 2, 3, 4, 5]
```

<br><br>

_input_

```
[] :: []
```

_output_

```
[[t1]]: [[]]
```

<br><br>

### vector

_input_

```
(1, true)
```

_output_

```
(int, bool): (1, true)
```

<br><br>

_input_

```
(1, (true, (), "a"))
```

_output_

```
(int, (bool, ng, str)): (1, (true, (), "a"))
```

<br><br>

## function types

_input_

```
fn x -> x
```

_output_

```
t1 -> t1: function
```

<br><br>

_input_

```
fn x -> x + 1
```

_output_

```
int -> int: function
```

<br><br>

_input_

```
fn x -> fn y -> x + y
```

_output_

```
int -> int -> int: function
```

<br><br>

## bind expressions

_input_

```
bind x <- 1 in
bind y <- 2 in
x + y
```

_output_

```
int: 3
```

<br><br>

_input_

```
bind f x y z <- x (y + z) in f
```

_output_

```
(int -> t1) -> int -> int -> t1: function
```

<br><br>

_input_

```
bind f x y z <- x (y + z) in
f (fn x -> x > 0) 1 2
```

_output_

```
bool: true
```

_input_

```
bind (a, b) <- (1, 2) in
a + b
```

_output_

```
int: 3
```

<br><br>

## ternary expressions

_input_

```
if true then 1 else 2
```

_output_

```
int: 1
```

<br><br>

_input_

```
if false then 1 else 2
```

_output_

```
int: 2
```

<br><br>

# 

## switch expresssions

_input_

```
switch [] =>
  | [] -> true
  | _ :: _ -> false
end
```

_output_

```
bool: true
```

<br><br>

_input_

```
switch 1 :: 2 :: [] =>
  | [] -> true
  | _ :: _ -> false
end
```

_output_

```
bool: false
```

# some more advanced examples

## map

_input_

```
bind rec map f arr <-
  switch arr =>
    | [] -> []
    | h :: t -> f h :: map f t
  end
in
map
```

_output_

```
(t1 -> t2) -> [t1] -> [t2]: function
```

<br><br>

_input_

```
bind rec map f arr <-
  switch arr =>
    | [] -> []
    | h :: t -> f h :: map f t
  end
in
map (fn x -> ~- x) (1 :: 2 :: 3 :: 4 :: 5 :: [])
```

_output_

```
[-1, -2, -3, -4, -5]
```

<br><br>

## filter

_input_

```
bind filter f arr <-
  switch arr =>
    | [] -> []
    | h :: t ->
      if f h then h :: filter t
      else filter t
    end
in
filter
```

_output_

```
(t1 -> bool) -> [t1] -> [t1]: function
```
