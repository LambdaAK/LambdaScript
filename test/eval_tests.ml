(** Evaluation semantics, checked end to end. Every program runs through the
    interpreter AND the native compiler: both must print exactly the expected
    output and agree on the exit status. *)

open OUnit2
open Forge_test_util

let ok name src expected = name >:: fun _ -> assert_both ~expected name src

(** Only check that the two back ends agree (semantics not pinned down by the
    docs, e.g. ordering of constructors). *)
let agree name src = name >:: fun _ -> assert_both name src

let rt_error ?(stdout = "") name src =
  name >:: fun _ -> assert_both_runtime_error ~stdout name src

let p = Printf.sprintf

(* ---------------------------------------------------------------- ints --- *)

let int_expr name e expected = ok name (p "let () = println (%s)" e) (expected ^ "\n")

let arithmetic =
  [
    int_expr "add" "2 + 3" "5";
    int_expr "sub negative result" "2 - 3" "-1";
    int_expr "mul" "6 * 7" "42";
    int_expr "div truncates" "7 / 2" "3";
    int_expr "div negative dividend truncates toward zero" "~-7 / 2" "-3";
    int_expr "div negative divisor" "7 / ~-2" "-3";
    int_expr "div both negative" "~-7 / ~-2" "3";
    int_expr "mod positive" "7 % 3" "1";
    int_expr "mod negative dividend" "~-7 % 3" "-1";
    int_expr "mod negative divisor" "7 % ~-3" "1";
    int_expr "mod both negative" "~-7 % ~-3" "-1";
    int_expr "mod by one" "12345 % 1" "0";
    int_expr "div by one" "~-12345 / 1" "-12345";
    int_expr "zero divided" "0 / 5" "0";
    int_expr "opposite" "~-(3 - 10)" "7";
    int_expr "opposite of zero" "~-0" "0";
    int_expr "precedence mix" "2 + 3 * 4 - 10 / 2 % 3" "12";
    int_expr "int32 max" "2147483647" "2147483647";
    int_expr "beyond int32" "2147483647 + 1" "2147483648";
    int_expr "beyond int32 negative" "~-2147483648 - 1" "-2147483649";
    int_expr "large product" "100000 * 100000" "10000000000";
    int_expr "large literal" "9007199254740993" "9007199254740993";
    int_expr "large division" "10000000000 / 3" "3333333333";
    int_expr "large modulo" "10000000001 % 1000000007" "999999938";
    ok "factorial 20 (fits in 64 bits)"
      "let rec fact n = if n == 0 then 1 else n * fact (n - 1)\nlet () = println (fact 20)"
      "2432902008176640000\n";
    ok "fibonacci 50 iterative"
      "let rec fib a b n = if n == 0 then a else fib b (a + b) (n - 1)\nlet () = println (fib 0 1 50)"
      "12586269025\n";
    ok "sum to 100000" "let rec sum n acc = if n == 0 then acc else sum (n - 1) (acc + n)\nlet () = println (sum 100000 0)"
      "5000050000\n";
    ok "int_to_str negative" "let () = print_string (int_to_str (0 - 42))" "-42\n";
    ok "int_to_str large" "let () = print_string (int_to_str (3000000000 * 2))" "6000000000\n";
  ]

let runtime_errors =
  [
    rt_error "division by zero" "let () = println (1 / 0)";
    rt_error "modulo by zero" "let () = println (1 % 0)";
    rt_error "division by computed zero" "let z = 5 - 5\nlet () = println (10 / z)";
    rt_error ~stdout:"before\n" "output before division by zero is kept"
      "let () = print_string \"before\"\nlet () = println (1 / 0)\nlet () = print_string \"after\"";
    rt_error "list_head of empty list" "let () = println (list_head [])";
    rt_error "list_tail of empty list" "let () = println (list_length (list_tail []))";
    rt_error "list_nth out of range" "let () = println (list_nth [1, 2] 5)";
    rt_error "non-exhaustive case" "let f x = case x do | 1 -> \"one\"\nlet () = print_string (f 2)";
    rt_error "non-exhaustive constructor case"
      "type C = | R | G | B\nlet f c = case c do | R -> 1 | G -> 2\nlet () = println (f B)";
    rt_error "refutable let pattern" "let (1, x) = (2, 3)\nlet () = println x";
    rt_error "refutable function parameter" "let f (1, x) = x\nlet () = println (f (2, 3))";
    rt_error ~stdout:"1\n2\n" "error mid-program after output"
      "let () = println 1\nlet () = println 2\nlet () = println (case [] do | h :: _ -> h)";
  ]

(* --------------------------------------------------- equality/ordering --- *)

let b e = p "let () = println (%s)" e

let comparisons =
  [
    ok "int eq" (b "3 == 3") "true\n";
    ok "int ne" (b "3 != 4") "true\n";
    ok "int lt" (b "~-1 < 0") "true\n";
    ok "int le eq" (b "2 <= 2") "true\n";
    ok "int gt" (b "5 > 9") "false\n";
    ok "int ge" (b "9 >= 9") "true\n";
    ok "bool eq" (b "true == true") "true\n";
    ok "bool ne" (b "true != false") "true\n";
    ok "bool lt" (b "false < true") "true\n";
    ok "char eq" (b "'a' == 'a'") "true\n";
    ok "char lt" (b "'a' < 'b'") "true\n";
    ok "char ne" (b "'a' != 'A'") "true\n";
    ok "string eq" (b "\"abc\" == \"abc\"") "true\n";
    ok "string ne" (b "\"abc\" != \"abd\"") "true\n";
    ok "string lt" (b "\"abc\" < \"abd\"") "true\n";
    ok "string prefix lt" (b "\"ab\" < \"abc\"") "true\n";
    ok "string empty lt" (b "\"\" < \"a\"") "true\n";
    ok "string gt case" (b "\"a\" > \"B\"") "true\n";
    ok "string eq computed" (b "\"ab\" ^ \"c\" == \"abc\"") "true\n";
    ok "unit eq" (b "() == ()") "true\n";
    ok "float eq" (b "1.5 == 1.5") "true\n";
    ok "float lt" (b "1.5 < 2.25") "true\n";
    ok "float ne" (b "0.1 != 0.2") "true\n";
    ok "tuple eq" (b "(1, \"a\") == (1, \"a\")") "true\n";
    ok "tuple ne" (b "(1, \"a\") != (1, \"b\")") "true\n";
    ok "tuple lt lexicographic" (b "(1, 9) < (2, 0)") "true\n";
    ok "tuple lt second component" (b "(1, 2) < (1, 3)") "true\n";
    ok "nested tuple eq" (b "((1, 2), 3) == ((1, 2), 3)") "true\n";
    ok "list eq" (b "[1, 2, 3] == [1, 2, 3]") "true\n";
    ok "list ne length" (b "[1, 2] != [1, 2, 3]") "true\n";
    ok "list eq empty" (b "[] == [1] ") "false\n";
    ok "list lt lexicographic" (b "[1, 2] < [1, 3]") "true\n";
    ok "list lt prefix" (b "[1, 2] < [1, 2, 0]") "true\n";
    ok "list of strings eq" (b "[\"a\", \"b\"] == [\"a\", \"b\"]") "true\n";
    ok "record eq" (b "{x: 1, y: 2} == {x: 1, y: 2}") "true\n";
    ok "record eq field order independent" (b "{x: 1, y: 2} == {y: 2, x: 1}") "true\n";
    ok "record ne" (b "{x: 1, y: 2} == {x: 1, y: 3}") "false\n";
    ok "option eq" (b "Some 1 == Some 1") "true\n";
    ok "option ne" (b "Some 1 == None") "false\n";
    ok "option payload ne" (b "Some 1 != Some 2") "true\n";
    ok "adt eq" "type C = | R | G | B\nlet () = println (G == G)" "true\n";
    ok "adt ne" "type C = | R | G | B\nlet () = println (R == B)" "false\n";
    ok "adt with payload eq"
      "type S = | Circle of Int | Square of Int\nlet () = println (Circle 2 == Circle 2)" "true\n";
    ok "adt with payload ne ctor"
      "type S = | Circle of Int | Square of Int\nlet () = println (Circle 2 == Square 2)" "false\n";
    ok "recursive adt eq"
      "type rec T = | L | N of (T, Int, T)\nlet () = println (N (L, 1, N (L, 2, L)) == N (L, 1, N (L, 2, L)))"
      "true\n";
    agree "adt ordering" "type C = | Zed | Alpha\nlet () = println (Zed < Alpha)";
    agree "adt ordering with payload" "type S = | B of Int | A of Int\nlet () = println (B 5 < A 1)";
    ok "comparison result in if" "let () = print_string (if \"b\" > \"a\" then \"yes\" else \"no\")"
      "yes\n";
    ok "compare via prelude Ord Int" "let () = println (compare 1 2)" "LT\n";
    ok "compare via prelude Ord String" "let () = println (compare \"b\" \"a\")" "GT\n";
    ok "compare via prelude Ord Char" "let () = println (compare 'x' 'x')" "EQ\n";
    ok "compare via prelude Ord Bool" "let () = println (compare false true)" "LT\n";
    ok "compare via prelude Ord Float" "let () = println (compare 2.5 1.0)" "GT\n";
    ok "compare via prelude Ord Unit" "let () = println (compare () ())" "EQ\n";
  ]

(* --------------------------------------------- functions and closures --- *)

let functions =
  [
    ok "capturing lambda passed to map"
      "let n = 10\nlet () = println (map (fn x -> x + n) [1, 2])" "[11, 12]\n";
    ok "capturing lambda built from a parameter"
      "let scale k = map (fn x -> x * k) [1, 2, 3]\nlet () = println (scale 3)" "[3, 6, 9]\n";
    ok "list of one-argument functions"
      "let fs = [fn x -> x + 1, fn x -> x * 2]\nlet () = println (map (fn f -> f 10) fs)" "[11, 20]\n";
    ok "builtin passed as a function value"
      "let () = println (map int_to_str [1, 2])\nlet () = { map print_string [\"a\", \"b\"]; () }" "[1, 2]\na\nb\n";
    ok "immediately applied lambda" "let () = println ((fn (a, b) -> a + b) (1, 2))" "3\n";
    ok "closure captures" "let make_adder n = fn x -> x + n\nlet add5 = make_adder 5\nlet () = println (add5 10)"
      "15\n";
    ok "closure captures at creation time"
      "let x = 1\nlet f = fn y -> x + y\nlet x = 100\nlet () = println (f 1)" "2\n";
    ok "shadowing in let-in" "let () = println (let x = 1 in let x = x + 1 in let x = x * 10 in x)"
      "20\n";
    ok "shadowing parameter" "let f x = let x = x * 2 in x + 1\nlet () = println (f 5)" "11\n";
    ok "shadowing top-level" "let a = 1\nlet a = a + 1\nlet () = println a" "2\n";
    ok "shadowing in case branch"
      "let x = 5\nlet () = println (case 3 do | x -> x + 1)" "4\n";
    ok "inner function shadows outer name"
      "let f x = x + 1\nlet g x = let f y = y * 100 in f x\nlet () = println (f 1 + g 1)" "102\n";
    ok "curried application" "let add3 a b c = a + b + c\nlet () = println (add3 1 2 3)" "6\n";
    ok "partial application" "let add3 a b c = a + b + c\nlet f = add3 1\nlet g = f 2\nlet () = println (g 3)"
      "6\n";
    ok "higher order twice" "let twice f x = f (f x)\nlet () = println (twice (fn x -> x * 3) 2)" "18\n";
    ok "function returning function returning function"
      "let f a = fn b -> fn c -> a * 100 + b * 10 + c\nlet () = println (f 1 2 3)" "123\n";
    ok "composition" "let compose f g x = f (g x)\nlet inc x = x + 1\nlet dbl x = x * 2\nlet () = println (compose inc dbl 5)"
      "11\n";
    ok "functions in a list"
      "let fs = [fn x -> x + 1, fn x -> x * 2, fn x -> x - 3]\nlet rec apply_all fs x = case fs do | [] -> [] | f :: t -> f x :: apply_all t x\nlet () = println (apply_all fs 10)"
      "[11, 20, 7]\n";
    ok "functions in a tuple" "let p = (fn x -> x + 1, fn x -> x * 2)\nlet () = println ((tuple_fst p) ((tuple_snd p) 5))"
      "11\n";
    ok "function stored in record" "let r = {f: fn x -> x * x}\nlet () = println (r.f 9)" "81\n";
    ok "lambda with tuple param" "let swap = fn (a, b) -> (b, a)\nlet () = println (tuple_fst (swap (1, 2)))" "2\n";
    ok "operator as value" "let apply op a b = op a b\nlet () = println (apply (+) 2 3 + apply (*) 2 3)" "11\n";
    ok "user-defined operator" "let (+++) a b = a * 10 + b\nlet () = println (1 +++ 2 +++ 3)" "123\n";
    ok "user-defined operator section" "let (+++) a b = a * 10 + b\nlet () = println (reduce_left (+++) 0 [4, 5, 6])"
      "456\n";
    ok "unit function" "let f () = 42\nlet () = println (f ())" "42\n";
    ok "ignore argument" "let k _ = 7\nlet () = println (k \"anything\")" "7\n";
    ok "closure over closure"
      "let counter start = fn step -> fn n -> start + step * n\nlet c = counter 10\nlet c2 = c 3\nlet () = println (c2 4)"
      "22\n";
    ok "pipeline" "let () = println ([1, 2, 3, 4] |> map (fn x -> x * x) |> filter (fn x -> x > 4))"
      "[9, 16]\n";
    ok "let-polymorphic helper used at two types"
      "let id x = x\nlet () = print_string (id \"s\")\nlet () = println (id 3)" "s\n3\n";
    ok "block sequencing" "let () = { print \"a\"; print \"b\"; print_string \"c\" }" "abc\n";
    ok "block local definitions" "let v = { let a = 3; let b = a * a; a + b }\nlet () = println v" "12\n";
    ok "block function definition" "let v = { let sq x = x * x; sq 7 }\nlet () = println v" "49\n";
    ok "nested blocks" "let v = { let a = { let b = 2; b * 3 }; a + 1 }\nlet () = println v" "7\n";
    ok "if as expression" "let abs x = if x < 0 then ~-x else x\nlet () = println (abs ~-5 + abs 5)" "10\n";
    ok "nested if" "let sign x = if x < 0 then \"neg\" else if x == 0 then \"zero\" else \"pos\"\nlet () = { print_string (sign ~-1); print_string (sign 0); print_string (sign 9) }"
      "neg\nzero\npos\n";
    ok "short-circuit and" "let rec boom x = boom x\nlet () = println (false && boom 1)" "false\n";
    ok "short-circuit or" "let rec boom x = boom x\nlet () = println (true || boom 1)" "true\n";
    ok "evaluation order of effects in let" "let a = print_string \"first\"\nlet b = print_string \"second\"" "first\nsecond\n";
  ]

(* ------------------------------------------------------------ recursion --- *)

let recursion =
  [
    ok "tail recursion 1e6" "let rec loop n = if n == 0 then \"done\" else loop (n - 1)\nlet () = print_string (loop 1000000)"
      "done\n";
    ok "non-tail recursion depth 10000"
      "let rec sum n = if n == 0 then 0 else n + sum (n - 1)\nlet () = println (sum 10000)" "50005000\n";
    ok "non-tail recursion depth 100000"
      "let rec sum n = if n == 0 then 0 else n + sum (n - 1)\nlet () = println (sum 100000)" "5000050000\n";
    ok "build long list" "let rec range a b = if a > b then [] else a :: range (a + 1) b\nlet () = println (list_length (range 1 50000))"
      "50000\n";
    ok "map over long list" "let () = println (list_length (map (fn x -> x + 1) [1...100000]))" "100000\n";
    ok "mutual recursion" "let rec even n = if n == 0 then true else odd (n - 1)\nand odd n = if n == 0 then false else even (n - 1)\nlet () = println (even 1000)"
      "true\n";
    ok "mutual recursion deep" "let rec even n = if n == 0 then true else odd (n - 1)\nand odd n = if n == 0 then false else even (n - 1)\nlet () = println (odd 100001)"
      "true\n";
    ok "local let rec" "let f n = let rec go i acc = if i > n then acc else go (i + 1) (acc + i) in go 1 0\nlet () = println (f 100)"
      "5050\n";
    ok "local mutual recursion"
      "let v = let rec e n = if n == 0 then 1 else o (n - 1) and o n = if n == 0 then 0 else e (n - 1) in e 10\nlet () = println v"
      "1\n";
    ok "ackermann" "let rec ack m n = if m == 0 then n + 1 else if n == 0 then ack (m - 1) 1 else ack (m - 1) (ack m (n - 1))\nlet () = println (ack 2 3)"
      "9\n";
    ok "recursive closure captured" "let rec f n = if n == 0 then fn x -> x else fn x -> (f (n - 1)) (x + 1)\nlet () = println ((f 5) 0)"
      "5\n";
    ok "let rec value (non-function)" "let rec x = 5\nlet () = println x" "5\n";
  ]

(* ------------------------------------------------------- records & ADTs --- *)

let records =
  [
    ok "record literal and access" "let r = {name: \"ann\", age: 30}\nlet () = { print_string r.name; println r.age }"
      "ann\n30\n";
    ok "record update one field" "let r = {x: 1, y: 2}\nlet r2 = { r with x = 10 }\nlet () = println (r2.x + r2.y)" "12\n";
    ok "record update keeps original" "let r = {x: 1, y: 2}\nlet r2 = { r with x = 10 }\nlet () = println r.x" "1\n";
    ok "record update two fields" "let r = {x: 1, y: 2, z: 3}\nlet r2 = { r with x = 10, z = 30 }\nlet () = println (r2.x + r2.y + r2.z)"
      "42\n";
    ok "record update same field twice" "let r = {x: 1}\nlet r2 = { { r with x = 2 } with x = 3 }\nlet () = println r2.x" "3\n";
    ok "nested record access" "let r = {inner: {v: 7}}\nlet () = println r.inner.v" "7\n";
    ok "nested record update" "let r = {inner: {v: 7}, k: 1}\nlet r2 = { r with inner = { r.inner with v = 8 } }\nlet () = println (r2.inner.v * 10 + r2.k)"
      "81\n";
    ok "record type alias" "type Point = {x: Int, y: Int}\nlet p : Point = {x: 3, y: 4}\nlet () = println (p.x * p.x + p.y * p.y)"
      "25\n";
    ok "record field order irrelevant for access" "let r = {b: 2, a: 1}\nlet () = println (r.a * 10 + r.b)" "12\n";
    ok "record with function field" "let r = {op: fn a -> fn b -> a - b, v: 10}\nlet () = println (r.op r.v 3)" "7\n";
    ok "record pattern literal field"
      "let tag p = case p do | {name: \"Alex\"} -> 1 | {name: \"John\"} -> 2 | _ -> 3\nlet () = println (tag {name: \"John\", age: 30})"
      "2\n";
    ok "record pattern binding"
      "let f r = case r do | {x: a, y: b} -> a - b\nlet () = println (f {x: 10, y: 4})" "6\n";
    ok "record pattern partial"
      "let f r = case r do | {active: true} -> \"on\" | _ -> \"off\"\nlet () = print_string (f {active: false, id: 3})"
      "off\n";
    ok "record in list" "let rs = [{v: 1}, {v: 2}, {v: 3}]\nlet () = println (reduce_left (fn acc -> fn r -> acc + r.v) 0 rs)"
      "6\n";
    ok "record punning pattern" "let f r = case r do | {x, y} -> x + y\nlet () = println (f {x: 1, y: 2})" "3\n";
    ok "record field named like keyword prefix" "let r = {ending: 1, iffy: 2}\nlet () = println (r.ending + r.iffy)" "3\n";
  ]

let adts =
  [
    ok "nullary constructors"
      "type C = | Red | Green | Blue\nlet name c = case c do | Red -> \"r\" | Green -> \"g\" | Blue -> \"b\"\nlet () = print_string (name Green)"
      "g\n";
    ok "constructor with payload"
      "type S = | Circle of Int | Rect of (Int, Int)\nlet area s = case s do | Circle r -> 3 * r * r | Rect (w, h) -> w * h\nlet () = println (area (Circle 2) + area (Rect (3, 4)))"
      "24\n";
    ok "generic adt" "type Box<a> = | Box of a\nlet unbox b = case b do | Box x -> x\nlet () = { println (unbox (Box 3)); print_string (unbox (Box \"s\")) }"
      "3\ns\n";
    ok "recursive tree"
      "type rec Tree = | Leaf | Node of (Tree, Int, Tree)\nlet rec sum t = case t do | Leaf -> 0 | Node (l, v, r) -> sum l + v + sum r\nlet () = println (sum (Node (Node (Leaf, 1, Leaf), 2, Node (Leaf, 3, Leaf))))"
      "6\n";
    ok "generic recursive tree"
      "type rec Tree<a> = | Leaf | Node of (Tree<a>, a, Tree<a>)\nlet rec size t = case t do | Leaf -> 0 | Node (l, _, r) -> size l + 1 + size r\nlet () = println (size (Node (Leaf, \"x\", Node (Leaf, \"y\", Leaf))))"
      "2\n";
    ok "user list type"
      "type rec IntList = | Nil | Cons of (Int, IntList)\nlet rec len l = case l do | Nil -> 0 | Cons (_, t) -> 1 + len t\nlet () = println (len (Cons (1, Cons (2, Cons (3, Nil)))))"
      "3\n";
    ok "mutually recursive types"
      "type rec Expr = | Num of Int | Add of (Expr, Expr) | Let of Binding\nand Binding = | Bind of (Int, Expr)\nlet rec eval e = case e do | Num n -> n | Add (a, b) -> eval a + eval b | Let b -> eval_b b\nand eval_b b = case b do | Bind (k, e) -> k * eval e\nlet () = println (eval (Add (Num 1, Let (Bind (10, Num 4)))))"
      "41\n";
    ok "either type"
      "type Either<a, b> = | Left of a | Right of b\nlet f e = case e do | Left n -> n + 1 | Right s -> str_length s\nlet () = println (f (Left 1) + f (Right \"abc\"))"
      "5\n";
    ok "constructor as function" "type W = | W of Int\nlet ws = map W [1, 2]\nlet () = println (list_length ws)" "2\n";
    ok "option from prelude" "let safe_div a b = if b == 0 then None else Some (a / b)\nlet show_opt o = case o do | None -> \"none\" | Some v -> int_to_str v\nlet () = { print_string (show_opt (safe_div 7 2)); print_string (show_opt (safe_div 7 0)) }"
      "3\nnone\n";
    ok "ordering from prelude" "let f o = case o do | LT -> \"lt\" | EQ -> \"eq\" | GT -> \"gt\"\nlet () = print_string (f (compare 3 3))"
      "eq\n";
    ok "adt payload is a record" "type P = | P of {x: Int}\nlet f p = case p do | P r -> r.x\nlet () = println (f (P {x: 9}))" "9\n";
    ok "adt payload is a function" "type F = | F of (Int -> Int)\nlet run f x = case f do | F g -> g x\nlet () = println (run (F (fn x -> x * 3)) 4)"
      "12\n";
    ok "adt payload is a list" "type L = | L of [Int]\nlet f l = case l do | L xs -> list_length xs\nlet () = println (f (L [1, 2, 3]))" "3\n";
    ok "constructor names shared prefix" "type T = | Ab | Abc | A\nlet f t = case t do | A -> 1 | Ab -> 2 | Abc -> 3\nlet () = println (f Abc * 100 + f Ab * 10 + f A)"
      "321\n";
  ]

(* --------------------------------------------------------- patterns ---- *)

let patterns =
  [
    ok "int literal patterns" "let f n = case n do | 0 -> \"zero\" | 1 -> \"one\" | _ -> \"many\"\nlet () = { print_string (f 0); print_string (f 1); print_string (f 7) }"
      "zero\none\nmany\n";
    ok "negative int scrutinee" "let f n = case n do | 0 -> \"zero\" | _ -> \"other\"\nlet () = print_string (f ~-3)" "other\n";
    ok "string patterns" "let f s = case s do | \"a\" -> 1 | \"b\" -> 2 | _ -> 0\nlet () = println (f \"b\" + f \"z\")" "2\n";
    ok "char patterns" "let f c = case c do | 'x' -> 1 | _ -> 0\nlet () = println (f 'x' + f 'y')" "1\n";
    ok "bool patterns" "let f b = case b do | true -> \"t\" | false -> \"f\"\nlet () = print_string (f false)" "f\n";
    ok "unit pattern" "let f u = case u do | () -> 5\nlet () = println (f ())" "5\n";
    ok "first matching branch wins" "let f n = case n do | _ -> 1 | 0 -> 2\nlet () = println (f 0)" "1\n";
    ok "nested list patterns"
      "let f l = case l do | [] -> 0 | x :: [] -> 100 | x :: y :: _ -> x + y\nlet () = println (f [] + f [5] + f [1, 2, 3])"
      "103\n";
    ok "list literal pattern" "let f l = case l do | a :: b :: [] -> a * b | _ -> 0\nlet () = println (f [3, 4] + f [1, 2, 3])" "12\n";
    ok "cons with literal head" "let f l = case l do | 1 :: t -> list_length t | _ -> 99\nlet () = println (f [1, 2, 3] + f [2])" "101\n";
    ok "tuple of lists" "let f p = case p do | ([], []) -> 0 | (_ :: _, []) -> 1 | ([], _ :: _) -> 2 | _ -> 3\nlet () = println (f ([1], []) * 10 + f ([1], [2]))"
      "13\n";
    ok "nested constructor patterns"
      "let f o = case o do | Some (Some x) -> x | Some None -> 1 | None -> 0\nlet () = println (f (Some (Some 7)) + f (Some None) + f None)"
      "8\n";
    ok "constructor with tuple payload pattern"
      "type T = | P of (Int, String)\nlet f t = case t do | P (1, s) -> s | P (_, s) -> s ^ \"!\"\nlet () = { print_string (f (P (1, \"a\"))); print_string (f (P (2, \"b\"))) }"
      "a\nb!\n";
    ok "deeply nested pattern"
      "let f x = case x do | Some ((1, a :: b :: []), c) -> a + b + c | _ -> 0\nlet () = println (f (Some ((1, [2, 3]), 4)))"
      "9\n";
    ok "wildcard in tuple" "let f p = case p do | (_, 0) -> \"zero\" | (_, _) -> \"nonzero\"\nlet () = print_string (f (5, 0))"
      "zero\n";
    ok "pattern variables bind right values" "let f p = case p do | (a, (b, c)) -> a * 100 + b * 10 + c\nlet () = println (f (1, (2, 3)))"
      "123\n";
    ok "case on function result" "let () = print_string (case list_length [1, 2] do | 2 -> \"two\" | _ -> \"?\")" "two\n";
    ok "nested case needs parens"
      "let f a b = case a do | 0 -> (case b do | 0 -> \"00\" | _ -> \"0x\") | _ -> \"x\"\nlet () = { print_string (f 0 1); print_string (f 1 0) }"
      "0x\nx\n";
    ok "top-level tuple destructuring" "let (a, b) = (1, 2)\nlet () = println (a * 10 + b)" "12\n";
    ok "let-in tuple destructuring" "let () = println (let (a, b) = (3, 4) in a * b)" "12\n";
    ok "let-in nested destructuring" "let () = println (let (a, (b, c)) = (1, (2, 3)) in a + b + c)" "6\n";
    ok "let-in destructuring in function" "let f p = let (a, b) = p in a - b\nlet () = println (f (10, 3))" "7\n";
    ok "function tuple parameter" "let f (a, b) = a * b\nlet () = println (f (6, 7))" "42\n";
    ok "comprehension pattern filters" "let () = println [x | (x, true) <- [(1, true), (2, false), (3, true)]]" "[1, 3]\n";
    ok "comprehension cons pattern filters" "let () = println [h | h :: _ <- [[1, 2], [], [3]]]" "[1, 3]\n";
  ]

(* ------------------------------------------------------------ strings --- *)

let strings =
  [
    ok "concat" "let () = print_string (\"foo\" ^ \"bar\")" "foobar\n";
    ok "concat empty" "let () = print_string (\"\" ^ \"x\" ^ \"\")" "x\n";
    ok "str_length" "let () = println (str_length \"hello\")" "5\n";
    ok "str_length empty" "let () = println (str_length \"\")" "0\n";
    ok "str_concat" "let () = print_string (str_concat \"a\" \"b\")" "ab\n";
    ok "str_slice middle" "let () = print_string (str_slice \"hello\" 1 3)" "ell\n";
    ok "str_slice from start" "let () = print_string (str_slice \"hello\" 0 2)" "he\n";
    ok "str_slice past end clamps" "let () = print_string (str_slice \"hello\" 3 100)" "lo\n";
    ok "str_slice zero length" "let () = print_string (str_slice \"hello\" 2 0)" "\n";
    ok "str_slice start past end" "let () = print_string (str_slice \"hi\" 10 2)" "\n";
    ok "string_to_list" "let () = println (list_length (string_to_list \"abc\"))" "3\n";
    ok "string_to_list chars" "let () = println (case string_to_list \"xy\" do | a :: b :: [] -> a == 'x' && b == 'y' | _ -> false)"
      "true\n";
    ok "string_to_list empty" "let () = println (list_length (string_to_list \"\"))" "0\n";
    ok "print without newline" "let () = { print \"a\"; print \"b\"; print \"\\n\" }" "ab\n";
    ok "newline escape in string" "let () = print \"line1\\nline2\\n\"" "line1\nline2\n";
    ok "tab escape in string" "let () = print_string \"a\\tb\"" "a\tb\n";
    ok "quote escape in string" "let () = print_string \"say \\\"hi\\\"\"" "say \"hi\"\n";
    ok "backslash escape in string" "let () = print_string \"a\\\\b\"" "a\\b\n";
    ok "unicode passthrough" "let () = print_string \"h\xc3\xa9llo\"" "h\xc3\xa9llo\n";
    ok "str_length counts bytes" "let () = println (str_length \"\xc3\xa9\")" "2\n";
    ok "show string" "let () = print_string (show \"abc\")" "abc\n";
    ok "long string building" "let rec rep n = if n == 0 then \"\" else \"ab\" ^ rep (n - 1)\nlet () = println (str_length (rep 5000))"
      "10000\n";
  ]

(* ------------------------------------------------------- lists & misc --- *)

let lists =
  [
    ok "list literal" "let () = println [1, 2, 3]" "[1, 2, 3]\n";
    ok "empty list" "let () = println (list_length [])" "0\n";
    ok "nested lists" "let () = println [[1], [], [2, 3]]" "[[1], [], [2, 3]]\n";
    ok "cons" "let () = println (0 :: [1, 2])" "[0, 1, 2]\n";
    ok "range" "let () = println [1...5]" "[1, 2, 3, 4, 5]\n";
    ok "range single" "let () = println [3...3]" "[3]\n";
    ok "range empty" "let () = println (list_length [5...1])" "0\n";
    ok "range with variable bound" "let n = 4\nlet () = println [1...n]" "[1, 2, 3, 4]\n";
    ok "comprehension" "let () = println [x * x | x <- [1...5]]" "[1, 4, 9, 16, 25]\n";
    ok "comprehension two generators" "let () = println [x * 10 + y | x <- [1, 2], y <- [3, 4]]" "[13, 14, 23, 24]\n";
    ok "comprehension dependent generator" "let () = println [y | x <- [[1, 2], [3]], y <- x]" "[1, 2, 3]\n";
    ok "comprehension over strings" "let () = println (list_length [c | c <- string_to_list \"hello\"])" "5\n";
    ok "map" "let () = println (map (fn x -> x + 1) [1, 2, 3])" "[2, 3, 4]\n";
    ok "filter" "let () = println (filter (fn x -> x % 2 == 0) [1...10])" "[2, 4, 6, 8, 10]\n";
    ok "reduce_left order" "let () = println (reduce_left (fn acc -> fn x -> acc * 10 + x) 0 [1, 2, 3])" "123\n";
    ok "reduce_right order" "let () = println (reduce_right (fn x -> fn acc -> acc * 10 + x) [1, 2, 3] 0)" "321\n";
    ok "reduce_left on empty" "let () = println (reduce_left (+) 42 [])" "42\n";
    ok "list_length" "let () = println (list_length [1, 2, 3, 4])" "4\n";
    ok "list_head" "let () = println (list_head [7, 8])" "7\n";
    ok "list_tail" "let () = println (list_tail [7, 8, 9])" "[8, 9]\n";
    ok "list_nth" "let () = println (list_nth [10, 20, 30] 2)" "30\n";
    ok "list_nth zero" "let () = println (list_nth [10, 20, 30] 0)" "10\n";
    ok "tuple_fst" "let () = println (tuple_fst (5, \"x\"))" "5\n";
    ok "tuple_snd" "let () = print_string (tuple_snd (5, \"x\"))" "x\n";
    ok "list of tuples" "let () = println [(1, true), (2, false)]" "[(1, true), (2, false)]\n";
    ok "list of strings" "let () = println [\"a\", \"b\"]" "[a, b]\n";
    ok "list of options" "let () = println [Some 1, None]" "[Some 1, None]\n";
    ok "append via semigroup" "let () = println ([1, 2] ++ [3])" "[1, 2, 3]\n";
    ok "int_to_float and back" "let () = println (float_to_int (int_to_float 7))" "7\n";
    ok "float_to_int truncates" "let () = println (float_to_int 3.99)" "3\n";
    ok "float_to_int of literal" "let () = println (float_to_int 2.5)" "2\n";
    ok "not" "let () = println (not true)" "false\n";
    ok "not of comparison" "let () = println (not (1 > 2))" "true\n";
  ]

(* Sample programs from the README and website must run as documented. *)
let documented_examples =
  [
    ok "README: minimal" "let x = 1 + 2\nlet () = println (int_to_str x)" "3\n";
    ok "README: builtins block"
      {|let () = print "no newline"
let () = println "with newline"
let () = println (int_to_str (str_length "hello"))
let () = println (str_concat "hello" " world")
let () = println (str_slice "hello" 1 3)
let () = println (int_to_str (list_length [1, 2, 3]))
let () = println (int_to_str (list_head [1, 2, 3]))
let () = println (int_to_str (list_head (list_tail [1, 2, 3])))
let () = println (int_to_str (list_nth [10, 20, 30] 2))
let () = println (int_to_str (tuple_fst (5, 10)))
let () = println (int_to_str (tuple_snd (5, 10)))
|}
      "no newlinewith newline\n5\nhello world\nell\n3\n1\n2\n30\n5\n10\n";
    ok "README: records"
      {|type Point = { x: Int, y: Int }
let p1 : Point = { x: 10, y: 20 }
let sum_xy = p1.x + p1.y
let p2 = { p1 with x = 100 }
let p3 = { p2 with y = 50 }
let p4 = { p1 with x = 5, y = 15 }
let () = println (sum_xy + p3.x + p3.y + p4.x + p4.y)
|}
      "200\n";
    ok "README: record status"
      {|type User = { login: String, active: Bool }
let status = fn u ->
  case u do
  | { active: true } -> u.login ^ " is active"
  | { login: name } -> name ^ " is inactive"
  | _ -> "unknown"
let () = println (status { login: "ann", active: true })
let () = println (status { login: "bob", active: false })
|}
      "ann is active\nbob is inactive\n";
    ok "README: tree size"
      {|type rec Tree<a> =
  | Leaf
  | Node of (a, Tree<a>, Tree<a>)
let rec tree_size = fn t ->
  case t do
  | Leaf -> 0
  | Node (value, left, right) ->
    1 + tree_size left + tree_size right
let () = println (tree_size (Node (5, Leaf, Node (3, Leaf, Leaf))))
|}
      "2\n";
    ok "README: code block"
      {|let v = {
  let x = 5;
  let y = 10;
  let z = x + y;
  z * 2
}
let () = println v
|}
      "30\n";
    ok "README: operators as values"
      {|let add = (+)
let add5 = (+) 5
let (+++) = fn a -> fn b -> a + b
let () = println (add 3 4)
let () = println (add5 10)
let () = println (map ((*) 2) [1, 2, 3])
let () = println ((+++) 10 20)
|}
      "7\n15\n[2, 4, 6]\n30\n";
    ok "README: sign with negative literal"
      {|let sign = fn n ->
  case n do
  | 0 -> "zero"
  | n -> if n > 0 then "positive" else "negative"
let () = println (sign (-5))
|}
      "negative\n";
    ok "README: increment with annotations"
      {|let increment = fn (x: Int) : Int -> x + 1
let () = println (increment 41)
|}
      "42\n";
    ok "README: comprehension with filter"
      {|let () = println [x * 2 | x <- [1...5], x > 2]
|}
      "[6, 8, 10]\n";
    ok "README: macros"
      {|macro_rules! add where
  ($a:expr, $b:expr) => $a + $b
end
let x = add!(1, 2)
let y = add![3, 4]
let z = add!{5, 6}
let () = println (x + y + z)
|}
      "21\n";
    ok "website: modules"
      {|mod Math where
  let square x = x * x
  let cube x = x * x * x
end

use Math
let () = println (int_to_str (Math.square 12))
let () = println (int_to_str (Math.cube 3))
|}
      "144\n27\n";
    ok "website: trait"
      {|trait Render<a> where
  val render : a -> String
end

impl Render for Int where
  render x = int_to_str x
end

let () = println (render 42)
|}
      "42\n";
    ok "website: option describe"
      {|type Option<a> =
  | None
  | Some of a

let describe opt =
  case opt do
  | None -> "empty"
  | Some n -> (++) "value=" (int_to_str n)

let () = println (describe (Some 7))
|}
      "value=7\n";
    ok "website: values"
      {|let project = "Forge"
let major = 1
let release_ready = false
let banner = str_concat "Language: " project
let () = println banner
let () = println (int_to_str major)
|}
      "Language: Forge\n1\n";
  ]

let suite =
  "eval"
  >::: [
         "arithmetic" >::: arithmetic;
         "runtime_errors" >::: runtime_errors;
         "comparisons" >::: comparisons;
         "functions" >::: functions;
         "recursion" >::: recursion;
         "records" >::: records;
         "adts" >::: adts;
         "patterns" >::: patterns;
         "strings" >::: strings;
         "lists" >::: lists;
         "documented_examples" >::: documented_examples;
       ]

let () = run_test_tt_main suite
