Expected:
42
3
7
10
0
-1
0
1
3628800
144
6765
14
2187
5050
5040
35
6
1275
1
0
111
55
40320
1
1
0
11
16
42
42
50
10
42
99
42
1
42
42
42
42
ok
done

Source:
// Arithmetic helpers
let abs_val x = if x < 0 then 0 - x else x
let min_of a b = if a < b then a else b
let max_of a b = if a > b then a else b
let clamp lo hi x = if x < lo then lo else if x > hi then hi else x
let sign x = if x > 0 then 1 else if x < 0 then 0 - 1 else 0

// Recursive algorithms
let rec factorial n = if n == 0 then 1 else n * factorial (n - 1)

let rec fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2)

let rec fib_iter a b n =
  if n == 0 then a else fib_iter b (a + b) (n - 1)

let rec gcd a b = if b == 0 then a else gcd b (a % b)

let rec power base exp =
  if exp == 0 then 1 else base * power base (exp - 1)

let rec sum_to n = if n == 0 then 0 else n + sum_to (n - 1)

let rec product_to n = if n == 0 then 1 else n * product_to (n - 1)

let rec digit_sum n =
  if n == 0 then 0 else n % 10 + digit_sum (n / 10)

let rec digit_count n =
  if n < 10 then 1 else 1 + digit_count (n / 10)

let rec range_sum lo hi =
  if lo > hi then 0 else lo + range_sum (lo + 1) hi

// Primality
let rec prime_helper n d =
  if d * d > n then 1
  else if n % d == 0 then 0
  else prime_helper n (d + 1)

let is_prime n = if n < 2 then 0 else prime_helper n 2

// Collatz
let rec collatz n =
  if n == 1 then 0
  else if n % 2 == 0 then 1 + collatz (n / 2)
  else 1 + collatz (3 * n + 1)

// Nested let rec
let triangle n =
  let rec go n acc =
    if n == 0 then acc else go (n - 1) (acc + n)
  in
  go n 0

let nested_product n =
  let rec go n acc =
    if n == 0 then acc else go (n - 1) (n * acc)
  in
  go n 1

// Mutual recursion
let rec even n = if n == 0 then 1 else odd (n - 1)
and odd n = if n == 0 then 0 else even (n - 1)

// Named helpers for higher-order functions
let add1 x = x + 1
let double x = x * 2
let square x = x * x
let add x y = x + y
let sub x y = x - y
let mul x y = x * y

// Higher-order functions
let compose (f : int -> int) (g : int -> int) (x : int) : int = f (g x)
let twice  (f : int -> int) (x : int) : int = f (f x)
let thrice (f : int -> int) (x : int) : int = f (f (f x))
let both   (f : int -> int) (x : int) : int = f x + f x
let flip   (f : int -> int -> int) (x : int) (y : int) : int = f y x
let call_with (f : int -> int) (x : int) : int = f x

// Polymorphic functions
let id x = x
let const x _ = x

// Partial application values
let add5   = add 5
let triple = mul 3

// Lambda values (no closures needed)
let get_inc    () : int -> int = fn x -> x + 1
let get_double () : int -> int = fn x -> x * 2

// String values
let ok_str   = "ok"
let done_str = "done"

// Output (42 lines)

// Arithmetic helpers
let () = println (int_to_str (abs_val (0 - 42)))    // 42
let () = println (int_to_str (min_of 7 3))           // 3
let () = println (int_to_str (max_of 7 3))           // 7
let () = println (int_to_str (clamp 0 10 15))        // 10
let () = println (int_to_str (clamp 0 10 (0 - 5)))  // 0
let () = println (int_to_str (sign (0 - 99)))        // -1
let () = println (int_to_str (sign 0))               // 0
let () = println (int_to_str (sign 42))              // 1

// Recursive algorithms
let () = println (int_to_str (factorial 10))         // 3628800
let () = println (int_to_str (fib 12))               // 144
let () = println (int_to_str (fib_iter 0 1 20))      // 6765
let () = println (int_to_str (gcd 252 98))           // 14
let () = println (int_to_str (power 3 7))            // 2187
let () = println (int_to_str (sum_to 100))           // 5050
let () = println (int_to_str (product_to 7))         // 5040
let () = println (int_to_str (digit_sum 98765))      // 35
let () = println (int_to_str (digit_count 100000))   // 6
let () = println (int_to_str (range_sum 1 50))       // 1275
let () = println (int_to_str (is_prime 97))          // 1
let () = println (int_to_str (is_prime 100))         // 0
let () = println (int_to_str (collatz 27))           // 111

// Nested let rec
let () = println (int_to_str (triangle 10))          // 55
let () = println (int_to_str (nested_product 8))     // 40320

// Mutual recursion
let () = println (int_to_str (even 12))              // 1
let () = println (int_to_str (odd 13))               // 1
let () = println (int_to_str (even 7))               // 0

// Higher-order functions
let () = println (int_to_str (compose add1 double 5)) // add1(double 5) = 11
let () = println (int_to_str (compose square add1 3)) // square(add1 3) = 16
let () = println (int_to_str (twice  add1 40))        // add1(add1 40)  = 42
let () = println (int_to_str (thrice add1 39))        // add1(add1(add1 39)) = 42
let () = println (int_to_str (both   square 5))       // 25 + 25 = 50
let () = println (int_to_str (flip   sub 3 13))       // sub 13 3 = 10
let () = println (int_to_str (call_with add1 41))     // 42

// Polymorphic functions
let () = println (int_to_str (id 99))                            // 99
let () = println (int_to_str (const 42 100))                     // 42
let () = println (int_to_str (if const true false then 1 else 0)) // 1

// Partial application
let () = println (int_to_str (add5  37))             // add 5 37 = 42
let () = println (int_to_str (triple 14))             // mul 3 14 = 42

// Lambda values
let () = println (int_to_str ((get_inc    ()) 41))   // 42
let () = println (int_to_str ((get_double ()) 21))   // 42

// String bindings
let () = println ok_str                               // ok
let () = println done_str                             // done
