(** Parser tests: every syntactic form must parse completely; operator
    precedence and associativity are checked by evaluating expressions;
    malformed programs must be rejected with a clean diagnostic (no crash, no
    hang) by the real interpreter executable. *)

open OUnit2
open Forge_test_util

exception Timeout

let with_timeout (secs : int) (f : unit -> 'a) : 'a =
  let old = Sys.signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise Timeout)) in
  ignore (Unix.alarm secs);
  Fun.protect
    ~finally:(fun () ->
      ignore (Unix.alarm 0);
      Sys.set_signal Sys.sigalrm old)
    f

let tokens_of (s : string) =
  Language.Lex.lex (s |> String.to_seq |> List.of_seq)
  |> List.map (fun t -> t.Language.Lex.token_type)

(** Whole program parses with no leftover tokens. *)
let t_parses name src =
  name >:: fun _ ->
  match with_timeout 10 (fun () -> Language.Parser.ProgramParser.program_parser (tokens_of src)) with
  | Some (_, []) -> ()
  | Some (_, rest) ->
      assert_failure
        (Printf.sprintf "%d tokens left unparsed (next: %s)%s" (List.length rest)
           (String.concat " "
              (List.map Language.Lex.string_of_token_type (List.filteri (fun i _ -> i < 6) rest)))
           (show_src src))
  | None -> assert_failure ("program parser returned None" ^ show_src src)
  | exception Timeout -> assert_failure ("parser hung" ^ show_src src)
  | exception e -> assert_failure ("parser raised " ^ Printexc.to_string e ^ show_src src)

let definition_forms =
  [
    t_parses "let value" "let x = 1";
    t_parses "let unit" "let () = print_string \"hi\"";
    t_parses "let wildcard" "let _ = 1";
    t_parses "let function sugar" "let add x y = x + y";
    t_parses "let with value annotation" "let x : Int = 1";
    t_parses "let with param annotation" "let f (x : Int) = x";
    t_parses "let with return annotation" "let f (x : Int) : Int = x";
    t_parses "let tuple pattern" "let (a, b) = (1, 2)";
    t_parses "let nested tuple pattern" "let (a, (b, c)) = (1, (2, 3))";
    t_parses "let record pattern" "let {x: a, y: b} = {x: 1, y: 2}";
    t_parses "let cons pattern" "let h :: t = [1, 2]";
    t_parses "let operator definition" "let (+++) a b = a + b";
    t_parses "let rec" "let rec f n = if n == 0 then 0 else f (n - 1)";
    t_parses "let rec and" "let rec even n = if n == 0 then true else odd (n - 1)\nand odd n = if n == 0 then false else even (n - 1)";
    t_parses "let with constraint" "let show_twice [Show a] (x : a) = show x ^ show x";
    t_parses "type alias" "type Pair = (Int, Int)";
    t_parses "type alias generic" "type Pair<a> = (a, a)";
    t_parses "type alias function" "type F = Int -> Int";
    t_parses "type alias record" "type P = {x: Int, y: Int}";
    t_parses "sum type" "type Color = | Red | Green | Blue";
    t_parses "sum type multiline" "type Shape =\n  | Circle of Int\n  | Rect of (Int, Int)";
    t_parses "generic sum type" "type Box<a> = | Box of a";
    t_parses "two params" "type Either<a, b> = | Left of a | Right of b";
    t_parses "recursive type" "type rec Tree<a> = | Leaf | Node of (Tree<a>, a, Tree<a>)";
    t_parses "mutually recursive types" "type rec A = | A0 | A1 of B\nand B = | B0 | B1 of A";
    t_parses "payload function type" "type F = | F of (Int -> Int)";
    t_parses "payload list type" "type L = | L of [Int]";
    t_parses "payload record" "type R = | R of {x: Int}";
    t_parses "trait" "trait Size<a> where\n  val size : a -> Int\nend";
    t_parses "trait with default" "trait Size<a> where\n  val size : a -> Int\n  val big : a -> Bool\n  let big x = size x > 10\nend";
    t_parses "trait requires" "trait Big<a> requires Eq<a> where\n  val big : a -> Bool\nend";
    t_parses "trait higher kinded" "trait Box<f<_>> where\n  val unbox : f<a> -> a\nend";
    t_parses "inter braces" "inter Size<a> {\n  val size : a -> Int\n}";
    t_parses "impl" "trait Size<a> where\n  val size : a -> Int\nend\nimpl Size for Int where\n  size x = x\nend";
    t_parses "impl operator method" "trait Add<a> where\n  val (+++) : a -> a -> a\nend\nimpl Add for Int where\n  (+++) x y = x + y\nend";
    t_parses "impl two methods comma" "trait T<a> where\n  val f : a -> Int\n  val g : a -> Int\nend\nimpl T for Int where\n  f x = x,\n  g x = x\nend";
    t_parses "impl requires" "impl Show for List<a> requires Show<a> where\n  show x = \"l\"\nend";
    t_parses "mod" "mod M where\n  let x = 1\nend";
    t_parses "nested mod" "mod A where\n  mod B where\n    let x = 1\n  end\nend";
    t_parses "use" "mod M where\n  let x = 1\nend\nuse M";
    t_parses "import" "import \"other.forge\"";
    t_parses "macro" "macro_rules! twice where\n  ($e:expr) => $e + $e\nend";
    t_parses "macro two arms" "macro_rules! m where\n  () => 0;\n  ($x:expr) => $x\nend";
    t_parses "macro repetition" "macro_rules! m where\n  ($($x:expr),*) => vec!($($x),*)\nend";
    t_parses "comments everywhere" "// a\nlet /* b */ x /* c */ = // d\n  1 // e\n/* f */";
    t_parses "empty program" "";
    t_parses "only comments" "// nothing\n/* still nothing */";
  ]

let expression_forms =
  List.mapi
    (fun i e -> t_parses (Printf.sprintf "expr %02d: %s" i e) ("let v = " ^ e))
    [
      "42";
      "3.14";
      "true";
      "'c'";
      "\"s\"";
      "()";
      "x";
      "~-1";
      "~-(1 + 2)";
      "f x";
      "f x y z";
      "f (g x) (h y)";
      "(f x) y";
      "fn x -> x";
      "fn x -> fn y -> x";
      "fn (x : Int) -> x";
      "fn (a, b) -> a";
      "fn _ -> 0";
      "fn () -> 0";
      "fn [] -> 0";
      "fn {x: a} -> a";
      "let a = 1 in a";
      "let a = 1 in let b = 2 in a + b";
      "let f x = x in f 1";
      "let (a, b) = (1, 2) in a";
      "let rec f n = n in f 1";
      "let rec f n = g n and g n = n in f 1";
      "if a then b else c";
      "if a then if b then c else d else e";
      "if a then b else if c then d else e";
      "case x do | 1 -> a | _ -> b";
      "case x do | [] -> 0 | h :: t -> 1";
      "case x do | (a, b) -> a";
      "case x do | Some y -> y | None -> 0";
      "case x do | Some (a, b) -> a | None -> 0";
      "case x do | {x: 1} -> 0 | _ -> 1";
      "case x do | \"a\" -> 0 | _ -> 1";
      "case x do | 'a' -> 0 | _ -> 1";
      "case x do | true -> 0 | false -> 1";
      "case x do | () -> 0";
      "case x do | a :: b :: rest -> a";
      "case x do | [] -> case y do | [] -> 0 | _ -> 1";
      "{ 1 }";
      "{ 1; 2; 3 }";
      "{ let a = 1; a }";
      "{ let a = 1; let b = 2; a + b }";
      "{ print \"a\"; print \"b\" }";
      "{}";
      "[]";
      "[1]";
      "[1, 2, 3]";
      "[[1], [2, 3]]";
      "[1...10]";
      "[1...n]";
      "[a ... b + 1]";
      "[x | x <- xs]";
      "[x + y | x <- xs, y <- ys]";
      "[a | (a, 1) <- ps]";
      "(1, 2)";
      "(1, (2, 3), [4])";
      "{x: 1, y: 2}";
      "{x: 1}";
      "{ r with x = 2 }";
      "{ r with x = 2, y = 3 }";
      "r.x";
      "r.x.y";
      "(f r).x";
      "1 :: 2 :: []";
      "x |> f";
      "x |> f |> g";
      "(+)";
      "(::)";
      "(+) 1 2";
      "(==) 1";
      "m!(1, 2)";
      "m![1, 2]";
      "m!{1, 2}";
      "m!()";
      "a ^ b ^ c";
      "a == b";
      "a != b";
      "a <= b && c >= d || e";
      "a ++ b";
      "M.x";
      "M.f 1";
      "Int";
      "not true";
    ]

(* ---- precedence & associativity, checked by evaluation (prelude loaded) -- *)

let t_value name expr expected =
  name >:: fun _ ->
  match repl_eval [ expr ] with
  | Language.Repl_kernel.Ev_expr { value; _ } ->
      assert_equal ~printer:Fun.id ~msg:("evaluating " ^ expr) expected value
  | Language.Repl_kernel.Ev_error m -> assert_failure ("error evaluating " ^ expr ^ ": " ^ m)
  | Language.Repl_kernel.Ev_defs _ -> assert_failure ("got definitions for " ^ expr)

let precedence_tests =
  [
    t_value "sub is left assoc" "10 - 3 - 2" "5";
    t_value "div is left assoc" "100 / 10 / 5" "2";
    t_value "mod is left assoc" "100 % 7 % 3" "2";
    t_value "mul/div mixed left assoc" "12 / 3 * 2" "8";
    t_value "mul binds tighter than add" "1 + 2 * 3" "7";
    t_value "mul binds tighter than sub" "10 - 2 * 3" "4";
    t_value "mod binds like mul" "10 - 7 % 4" "7";
    t_value "parens override" "(1 + 2) * 3" "9";
    t_value "application binds tighter than mul" "list_length [1, 2] * 3" "6";
    t_value "application binds tighter than add" "list_length [1] + list_length [2, 3]" "3";
    t_value "opposite binds tightest" "~-2 * 3" "-6";
    t_value "opposite of application arg" "int_to_str ~-4" "\"-4\"";
    t_value "double opposite" "~-(~-5)" "5";
    t_value "and binds tighter than or (1)" "true || false && false" "true";
    t_value "and binds tighter than or (2)" "false && true || true" "true";
    t_value "and binds tighter than or (3)" "false && false || true && true" "true";
    t_value "relational binds tighter than and" "1 < 2 && 3 < 4" "true";
    t_value "relational binds tighter than or" "1 > 2 || 3 > 2" "true";
    t_value "add binds tighter than relational" "1 + 1 == 2" "true";
    t_value "add binds tighter than lt" "1 + 1 < 1 + 2" "true";
    t_value "relational left assoc" "1 < 2 == true" "true";
    t_value "concat left/right" "\"a\" ^ \"b\" ^ \"c\"" "\"abc\"";
    t_value "concat with application" "\"n=\" ^ int_to_str 5" "\"n=5\"";
    t_value "cons right assoc" "1 :: 2 :: 3 :: []" "[1, 2, 3]";
    t_value "cons looser than add" "1 + 1 :: [3]" "[2, 3]";
    t_value "cons looser than or" "true || false :: []" "[true]";
    t_value "pipeline left assoc" "[1, 2, 3] |> list_length |> int_to_str" "\"3\"";
    t_value "pipeline looser than cons" "1 :: [2] |> list_length" "2";
    t_value "pipeline looser than add" "1 + 2 |> int_to_str" "\"3\"";
    t_value "pipeline into partial application" "[1, 2, 3] |> map (fn x -> x * 10)" "[10, 20, 30]";
    t_value "if extends right" "if true then 1 else 2 + 10" "1";
    t_value "if else branch extends" "if false then 1 else 2 + 10" "12";
    t_value "fn body extends right" "(fn x -> x + 1) 1" "2";
    t_value "let body extends right" "let a = 1 in a + 1" "2";
    t_value "nested let shadowing" "let a = 1 in let a = a + 1 in a * 10" "20";
    t_value "case branch body extends" "case 1 do | 1 -> 1 + 1 | _ -> 0" "2";
    t_value "operator section add" "(+) 2 3" "5";
    t_value "operator section sub order" "(-) 10 3" "7";
    t_value "operator section div order" "(/) 10 2" "5";
    t_value "operator section cons" "(::) 1 []" "[1]";
    t_value "operator section and" "(&&) true false" "false";
    t_value "operator section partial" "map ((*) 2) [1, 2]" "[2, 4]";
    t_value "field access binds tighter than application" "int_to_str {x: 7}.x" "\"7\"";
    t_value "field access chain" "{a: {b: 3}}.a.b" "3";
    t_value "record update then access" "{ {x: 1, y: 2} with x = 5 }.x" "5";
    t_value "unit literal" "()" "()";
    t_value "tuple" "(1, \"a\")" "(1, \"a\")";
    t_value "nested tuple" "((1, 2), 3)" "((1, 2), 3)";
    t_value "range" "[1...4]" "[1, 2, 3, 4]";
    t_value "empty range" "[3...1]" "[]";
    t_value "comprehension two generators order" "[(x, y) | x <- [1, 2], y <- [3, 4]]"
      "[(1, 3), (1, 4), (2, 3), (2, 4)]";
    t_value "block value is last" "{ 1; 2; 3 }" "3";
    t_value "block with lets" "{ let a = 2; let b = a * a; b + 1 }" "5";
    t_value "custom additive operator precedence" "let (+++) a b = a * 10 + b in 1 +++ 2 * 3" "16";
    t_value "custom multiplicative operator precedence" "let ( *** ) a b = a - b in 1 + 10 *** 4" "7";
    t_value "custom relational operator precedence" "let (<=>) a b = a - b in 1 + 5 <=> 2" "4";
    t_value "custom operators left assoc" "let (+++) a b = a - b in 10 +++ 3 +++ 2" "5";
  ]

(* ---- malformed programs must fail cleanly through the CLI ---------------- *)

let t_bad name src =
  name >:: fun _ -> assert_clean_error name src (interp ~timeout:15.0 src)

let malformed_tests =
  [
    t_bad "unterminated string" "let s = \"abc";
    t_bad "unterminated block comment" "let x = 1 /* oops";
    t_bad "nested block comment" "let x = 1 /* a /* b */ c */\nlet () = print_string \"x\"";
    t_bad "bad char literal" "let c = 'ab'";
    t_bad "empty char literal" "let c = ''";
    t_bad "integer literal overflow" "let x = 123456789012345678901234567890";
    t_bad "malformed float" "let x = 1.2.3";
    t_bad "illegal character" "let x = 1 @ 2";
    t_bad "missing in" "let x = let y = 1 y";
    t_bad "missing then" "let x = if true 1 else 2";
    t_bad "missing else" "let x = if true then 1";
    t_bad "missing arrow in fn" "let f = fn x x";
    t_bad "missing body in let" "let x =";
    t_bad "missing name in let" "let = 5";
    t_bad "unbalanced paren" "let x = (1 + 2";
    t_bad "extra paren" "let x = 1 + 2)";
    t_bad "unbalanced bracket" "let x = [1, 2";
    t_bad "unbalanced brace" "let x = { 1; 2";
    t_bad "trailing operator" "let x = 1 +";
    t_bad "leading operator" "let x = * 2";
    t_bad "double comma in list" "let x = [1,, 2]";
    t_bad "case without branches" "let x = case 1 do";
    t_bad "branch without arrow" "let x = case 1 do | 1 2";
    t_bad "lowercase constructor" "type T = | foo";
    t_bad "type without constructors" "type T =";
    t_bad "trait without end" "trait T<a> where\n  val f : a -> Int";
    t_bad "impl without end" "trait T<a> where\n  val f : a -> Int\nend\nimpl T for Int where\n  f x = x";
    t_bad "impl method with let" "trait T<a> where\n  val f : a -> Int\nend\nimpl T for Int where\n  let f x = x\nend";
    t_bad "macro without arms" "macro_rules! m where\nend";
    t_bad "mod without end" "mod M where\n  let x = 1";
    t_bad "expression at top level" "1 + 2";
    t_bad "keyword as name" "let if = 1";
    t_bad "stray end" "end";
    t_bad "only operator" "+";
    t_bad "unterminated record" "let r = {x: 1";
    t_bad "bad record field" "let r = {x 1}";
    t_bad "record update without field" "let r = { x with }";
    t_bad "bad type annotation" "let x : = 1";
    t_bad "unclosed generic" "type T = Option<Int";
    t_bad "ternary chain broken" "let x = if a then b else";
    t_bad "parse error on later line is located"
      "let a = 1\nlet b = 2\nlet c = (3\nlet d = 4";
  ]

(* Parse errors should point at the offending line. *)
let location_tests =
  [
    ( "parse error message names the line" >:: fun _ ->
      let src = "let a = 1\nlet b = 2\nlet c = 3 +\nlet d = 4\n" in
      let r = interp src in
      assert_clean_error ~mentions:[ "line 3" ] "parse error line" src r );
    ( "lexer error message names the line" >:: fun _ ->
      let src = "let a = 1\n\nlet b = @\n" in
      let r = interp src in
      assert_clean_error ~mentions:[ "line 3" ] "lex error line" src r );
    ( "unterminated string names the line" >:: fun _ ->
      let src = "let a = 1\nlet s = \"abc\n" in
      let r = interp src in
      assert_clean_error ~mentions:[ "line 2" ] "string error line" src r );
  ]

(* Pathological-but-valid inputs must parse in reasonable time. *)
let stress_tests =
  let big_sum n = String.concat " + " (List.init n (fun _ -> "1")) in
  let nested_parens n = String.make n '(' ^ "1" ^ String.make n ')' in
  let nested_ifs n =
    let b = Buffer.create (n * 30) in
    for _ = 1 to n do
      Buffer.add_string b "if true then "
    done;
    Buffer.add_string b "1";
    for _ = 1 to n do
      Buffer.add_string b " else 0"
    done;
    Buffer.contents b
  in
  let long_list n = "[" ^ String.concat ", " (List.init n string_of_int) ^ "]" in
  let many_defs n =
    String.concat "\n" (List.init n (fun i -> Printf.sprintf "let v%d = %d" i i))
    ^ Printf.sprintf "\nlet () = print_string (int_to_str v%d)" (n - 1)
  in
  [
    ( "sum of 2000 terms" >:: fun _ ->
      let src = "let () = print_string (int_to_str (" ^ big_sum 2000 ^ "))" in
      assert_runs ~expected:"2000\n" "big sum" src (interp ~timeout:20.0 src) );
    ( "200 nested parentheses" >:: fun _ ->
      let src = "let () = print_string (int_to_str " ^ nested_parens 200 ^ ")" in
      assert_runs ~expected:"1\n" "nested parens" src (interp ~timeout:20.0 src) );
    ( "100 nested ifs" >:: fun _ ->
      let src = "let () = print_string (int_to_str (" ^ nested_ifs 100 ^ "))" in
      assert_runs ~expected:"1\n" "nested ifs" src (interp ~timeout:20.0 src) );
    ( "list literal with 3000 elements" >:: fun _ ->
      let src = "let () = print_string (int_to_str (list_length " ^ long_list 3000 ^ "))" in
      assert_runs ~expected:"3000\n" "long list" src (interp ~timeout:20.0 src) );
    ( "tuple nested 18 deep" >:: fun _ ->
      let rec nest d = if d = 0 then "0" else "(" ^ nest (d - 1) ^ ", " ^ string_of_int d ^ ")" in
      let src = "let t = " ^ nest 18 ^ "\nlet () = print_string \"ok\"" in
      assert_runs ~expected:"ok\n" "nested tuples" src (interp ~timeout:20.0 src) );
    ( "1000 top-level definitions" >:: fun _ ->
      let src = many_defs 1000 in
      assert_runs ~expected:"999\n" "many defs" src (interp ~timeout:20.0 src) );
  ]

let suite =
  "parser"
  >::: [
         "definitions" >::: definition_forms;
         "expressions" >::: expression_forms;
         "precedence" >::: precedence_tests;
         "malformed" >::: malformed_tests;
         "error_locations" >::: location_tests;
         "stress" >::: stress_tests;
       ]

let () = run_test_tt_main suite
