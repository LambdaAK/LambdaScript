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

```c
1
```

_output_

```c
int: 1
```

<br><br>

_input_

```c
17
```

_output_

```c
int: 17
```

<br><br>

### bool

_input_

```c
true
```

_output_

```c
bool: true
```

<br><br>

_input_

```c
false
```

_output_

```c
bool: false
```

<br><br>

### str

_input_

```c
"hello"
```

_output_

```c
str: "hello"
```

<br><br>

_input_

```c
"world"
```

_output_

```c
str: "world"
```

<br><br>

### ng ("nothing", also called "unit")

_input_

```c
()
```

_output_

```c
ng: ()
```

<br><br>

## compound types

### list

_input_

```c
[]
```

_output_

```c
[t1]: []
```

<br><br>

_input_

```c
1 :: 2 :: 3 :: 4 :: 5 :: []
```

_output_

```c
[int]: [1, 2, 3, 4, 5]
```

<br><br>

_input_

```c
[] :: []
```

_output_

```c
[[t1]]: [[]]
```

<br><br>

### vector

_input_

```c
(1, true)
```

_output_

```c
(int, bool): (1, true)
```

<br><br>

_input_

```c
(1, (true, (), "a"))
```

_output_

```c
(int, (bool, ng, str)): (1, (true, (), "a"))
```

<br><br>

## function types

_input_

```c
fn x -> x
```

_output_

```c
t1 -> t1: function
```

<br><br>

_input_

```c
fn x -> x + 1
```

_output_

```c
int -> int: function
```

<br><br>

_input_

```c
fn x -> fn y -> x + y
```

_output_

```c
int -> int -> int: function
```

<br><br>

## bind expressions

_input_

```c
bind x <- 1 in
bind y <- 2 in
x + y
```

_output_

```c
int: 3
```

<br><br>

_input_

```c
bind f x y z <- x (y + z) in f
```

_output_

```c
(int -> t1) -> int -> int -> t1: function
```

<br><br>

_input_

```c
bind f x y z <- x (y + z) in
f (fn x -> x > 0) 1 2
```

_output_

```c
bool: true
```

_input_

```c
bind (a, b) <- (1, 2) in
a + b
```

_output_

```c
int: 3
```

<br><br>

## ternary expressions

_input_

```c
if true then 1 else 2
```

_output_

```c
int: 1
```

<br><br>

_input_

```c
if false then 1 else 2
```

_output_

```c
int: 2
```

<br><br>

# 

## switch expresssions

_input_

```c
switch [] =>
  | [] -> true
  | _ :: _ -> false
end
```

_output_

```c
bool: true
```

<br><br>

_input_

```c
switch 1 :: 2 :: [] =>
  | [] -> true
  | _ :: _ -> false
end
```

_output_

```c
bool: false
```

# some more advanced examples

## map

_input_

```c
bind rec map f arr <-
  switch arr =>
    | [] -> []
    | h :: t -> f h :: map f t
  end
in
map
```

_output_

```c
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
