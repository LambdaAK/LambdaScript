(** Type checker tests: inferred types (compared up to renaming of type
    variables), let-polymorphism, annotations, and programs that must be
    rejected with a type error (not accepted, not a crash). *)

open OUnit2
open Forge_test_util
module RK = Language.Repl_kernel

(** Rename type variables (lowercase identifiers that are not record field
    labels) to a, b, c, … in order of first appearance. *)
let canon_type (s : string) : string =
  let n = String.length s in
  let buf = Buffer.create n in
  let mapping = Hashtbl.create 8 in
  let next = ref 0 in
  let is_lower c = c >= 'a' && c <= 'z' in
  let is_ident c = is_lower c || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_' in
  let i = ref 0 in
  while !i < n do
    let c = s.[!i] in
    if is_ident c && (!i = 0 || not (is_ident s.[!i - 1])) then (
      let j = ref !i in
      while !j < n && is_ident s.[!j] do
        incr j
      done;
      let word = String.sub s !i (!j - !i) in
      let next_non_space =
        let k = ref !j in
        while !k < n && s.[!k] = ' ' do
          incr k
        done;
        if !k < n then Some s.[!k] else None
      in
      let is_field = next_non_space = Some ':' in
      (if is_lower word.[0] && not is_field then (
         let v =
           match Hashtbl.find_opt mapping word with
           | Some v -> v
           | None ->
               let v = String.make 1 (Char.chr (Char.code 'a' + !next)) in
               incr next;
               Hashtbl.add mapping word v;
               v
         in
         Buffer.add_string buf v)
       else Buffer.add_string buf word);
      i := !j)
    else (
      Buffer.add_char buf c;
      incr i)
  done;
  Buffer.contents buf

let type_of_expr ?(setup = []) (expr : string) : string =
  match repl_eval (setup @ [ expr ]) with
  | RK.Ev_expr { typ; _ } -> typ
  | RK.Ev_error m -> assert_failure (Printf.sprintf "expected %S to typecheck, got error: %s" expr m)
  | RK.Ev_defs _ -> assert_failure ("expected an expression: " ^ expr)

let t_type ?(setup = []) name expr expected =
  name >:: fun _ ->
  let got = type_of_expr ~setup expr in
  assert_equal ~printer:Fun.id ~msg:("type of " ^ expr) (canon_type expected) (canon_type got)

(** Type of binding [name] after running definitions [defs]. *)
let t_def name defs binding expected =
  name >:: fun _ ->
  match repl_eval [ defs ] with
  | RK.Ev_defs { bindings } -> (
      match List.find_opt (fun (n, _, _) -> n = binding) bindings with
      | Some (_, _, t) ->
          assert_equal ~printer:Fun.id ~msg:("type of " ^ binding) (canon_type expected)
            (canon_type t)
      | None ->
          assert_failure
            (Printf.sprintf "binding %s not produced; got: %s" binding
               (String.concat ", " (List.map (fun (n, _, _) -> n) bindings))))
  | RK.Ev_error m -> assert_failure ("definitions rejected: " ^ m ^ show_src defs)
  | RK.Ev_expr _ -> assert_failure "expected definitions"

let leaked_internal (m : string) =
  List.exists (contains m)
    [ "Failure("; "Not_found"; "Invalid_argument"; "Stack_overflow"; "Match_failure"; "impossible" ]

(** [src] (an expression or definitions) must be rejected by the checker. *)
let t_reject ?(setup = []) name src =
  name >:: fun _ ->
  match repl_eval (setup @ [ src ]) with
  | RK.Ev_error m ->
      if leaked_internal m then assert_failure ("rejected, but with an internal error: " ^ m ^ show_src src)
  | RK.Ev_expr { typ; value } ->
      assert_failure (Printf.sprintf "accepted with type %s (value %s)%s" typ value (show_src src))
  | RK.Ev_defs { bindings } ->
      assert_failure
        (Printf.sprintf "definitions accepted: %s%s"
           (String.concat ", " (List.map (fun (n, _, t) -> n ^ " : " ^ t) bindings))
           (show_src src))
  | exception e -> assert_failure ("checker raised " ^ Printexc.to_string e ^ show_src src)

let literal_types =
  [
    t_type "int" "42" "Int";
    t_type "float" "2.5" "Float";
    t_type "bool" "false" "Bool";
    t_type "string" "\"s\"" "String";
    t_type "char" "'c'" "Char";
    t_type "unit" "()" "Unit";
    t_type "empty list" "[]" "List<a>";
    t_type "int list" "[1, 2]" "List<Int>";
    t_type "nested empty list" "[[]]" "List<List<a>>";
    t_type "tuple" "(1, \"a\", true)" "(Int, String, Bool)";
    t_type "nested tuple" "((1, 'c'), ())" "((Int, Char), Unit)";
    t_type "record" "{x: 1, y: true}" "{x: Int, y: Bool}";
    t_type "list of tuples" "[(1, \"a\")]" "List<(Int, String)>";
    t_type "range" "[1...3]" "List<Int>";
    t_type "comprehension" "[x * 2 | x <- [1, 2]]" "List<Int>";
    t_type "comprehension pair" "[(x, y) | x <- [1], y <- [\"a\"]]" "List<(Int, String)>";
    t_type "block" "{ let a = 1; a }" "Int";
    t_type "unit block" "{ }" "Unit";
    t_type "statement of any type" "{ 1 + 1; \"x\" }" "String";
    t_type "if" "if true then 1 else 2" "Int";
    t_type "concat" "\"a\" ^ \"b\"" "String";
    t_type "comparison" "1 < 2" "Bool";
    t_type "equality on strings" "\"a\" == \"b\"" "Bool";
    t_type "cons" "1 :: []" "List<Int>";
  ]

let function_types =
  [
    t_type "identity" "fn x -> x" "a -> a";
    t_type "const" "fn x -> fn y -> x" "a -> b -> a";
    t_type "flip" "fn f -> fn x -> fn y -> f y x" "(a -> b -> c) -> b -> a -> c";
    t_type "compose" "fn f -> fn g -> fn x -> f (g x)" "(a -> b) -> (c -> a) -> c -> b";
    t_type "twice" "fn f -> fn x -> f (f x)" "(a -> a) -> a -> a";
    t_type "apply" "fn f -> fn x -> f x" "(a -> b) -> a -> b";
    t_type "int function" "fn x -> x + 1" "Int -> Int";
    t_type "string function" "fn s -> s ^ \"!\"" "String -> String";
    t_type "bool function" "fn b -> if b then 1 else 0" "Bool -> Int";
    t_type "pair constructor" "fn x -> fn y -> (x, y)" "a -> b -> (a, b)";
    t_type "tuple pattern" "fn p -> case p do | (a, b) -> a" "(a, b) -> a";
    t_type "tuple param pattern" "fn (a, b) -> b" "(a, b) -> b";
    t_type "list head" "fn l -> case l do | h :: _ -> h" "List<a> -> a";
    t_type "list sum" "fn l -> case l do | [] -> 0 | h :: _ -> h" "List<Int> -> Int";
    t_type "singleton" "fn x -> [x]" "a -> List<a>";
    t_type "field access" "fn r -> r.x + 1" "{x: Int} -> Int";
    t_type "option match" "fn o -> case o do | Some x -> x | None -> 0" "Option<Int> -> Int";
    t_type "option wrap" "fn x -> Some x" "a -> Option<a>";
    t_type "equality" "fn x -> fn y -> x == y" "a -> a -> Bool";
    t_type "partial builtin" "map (fn x -> x + 1)" "List<Int> -> List<Int>";
    t_type "str_slice partial" "str_slice \"abc\" 1" "Int -> String";
    t_type "annotated param" "fn (x : Int) -> x" "Int -> Int";
    t_type "annotated tyvar param" "fn (x : a) -> x" "a -> a";
    t_type "annotated list param" "fn (x : [Int]) -> x" "List<Int> -> List<Int>";
    t_type "annotated function param" "fn (f : Int -> Bool) -> f 1" "(Int -> Bool) -> Bool";
    t_type "pipeline" "fn x -> x |> int_to_str" "Int -> String";
    t_type "unit param" "fn () -> 1" "Unit -> Int";
    t_type "wildcard param" "fn _ -> 1" "a -> Int";
  ]

let builtin_types =
  [
    t_type "print_string" "print_string" "String -> Unit";
    t_type "print" "print" "String -> Unit";
    t_type "int_to_str" "int_to_str" "Int -> String";
    t_type "int_to_float" "int_to_float" "Int -> Float";
    t_type "float_to_int" "float_to_int" "Float -> Int";
    t_type "string_to_list" "string_to_list" "String -> List<Char>";
    t_type "list_of_string" "list_of_string" "String -> List<Char>";
    t_type "str_length" "str_length" "String -> Int";
    t_type "str_concat" "str_concat" "String -> String -> String";
    t_type "str_slice" "str_slice" "String -> Int -> Int -> String";
    t_type "not" "not" "Bool -> Bool";
    t_type "map" "map" "(a -> b) -> List<a> -> List<b>";
    t_type "filter" "filter" "(a -> Bool) -> List<a> -> List<a>";
    t_type "reduce_left" "reduce_left" "(a -> b -> a) -> a -> List<b> -> a";
    t_type "reduce_right" "reduce_right" "(a -> b -> b) -> List<a> -> b -> b";
    t_type "list_length" "list_length" "List<a> -> Int";
    t_type "list_head" "list_head" "List<a> -> a";
    t_type "list_tail" "list_tail" "List<a> -> List<a>";
    t_type "list_nth" "list_nth" "List<a> -> Int -> a";
    t_type "tuple_fst" "tuple_fst" "(a, b) -> a";
    t_type "tuple_snd" "tuple_snd" "(a, b) -> b";
    t_type "plus section" "(+)" "Int -> Int -> Int";
    t_type "concat section" "(^)" "String -> String -> String";
    t_type "cons section" "(::)" "a -> List<a> -> List<a>";
    t_type "and section" "(&&)" "Bool -> Bool -> Bool";
    t_type "int_eq" "int_eq" "Int -> Int -> Bool";
    t_type "str_compare" "str_compare" "String -> String -> Ordering";
    t_type "show" "show" "Show a => a -> String";
    t_type "println" "println" "Show a => a -> Unit";
    t_type "compare" "compare" "Ord a => a -> a -> Ordering";
    t_type "fmap" "fmap" "Functor f => (a -> b) -> f<a> -> f<b>";
    t_type "pure" "pure" "Applicative f => a -> f<a>";
    t_type "bind" "bind" "Monad f => f<a> -> (a -> f<b>) -> f<b>";
    t_type "mappend" "mappend" "Semigroup a => a -> a -> a";
    t_type "empty" "empty" "Monoid a => a";
  ]

let polymorphism_tests =
  [
    t_type "let-polymorphism with fn" "let id = fn x -> x in (id 1, id true)" "(Int, Bool)";
    t_type "let-polymorphism with sugar" "let id x = x in (id 1, id \"a\")" "(Int, String)";
    t_type "let-polymorphic pair maker" "let p x = (x, x) in (p 1, p true)" "((Int, Int), (Bool, Bool))";
    t_type "let-polymorphism inside lambda" "fn y -> let id = fn x -> x in (id 1, id true)"
      "a -> (Int, Bool)";
    t_type "let-polymorphism inside lambda using param" "fn y -> let k = fn x -> y in (k 1, k true)"
      "a -> (a, a)";
    t_type "let-polymorphism inside let rec" "let rec f n = let id = fn x -> x in (id n, id true) in f 1"
      "(Int, Bool)";
    t_type "lambda-bound is monomorphic" "fn f -> f 1" "(Int -> a) -> a";
    t_type "recursive length" "let rec len l = case l do | [] -> 0 | _ :: t -> 1 + len t in len"
      "List<a> -> Int";
    t_type "recursive diverging" "let rec f x = f x in f" "a -> b";
    t_type "polymorphic recursion via let rec use" "let rec len l = case l do | [] -> 0 | _ :: t -> 1 + len t in (len [1], len [true])"
      "(Int, Int)";
    t_type ~setup:[ "let id x = x" ] "top-level polymorphism" "(id 1, id \"s\", id [true])"
      "(Int, String, List<Bool>)";
    t_type ~setup:[ "let compose f g x = f (g x)" ] "top-level compose" "compose"
      "(a -> b) -> (c -> a) -> c -> b";
    t_type ~setup:[ "let k = fn x -> fn y -> x" ] "top-level const instantiated twice"
      "(k 1 \"a\", k \"b\" 2)" "(Int, String)";
    t_type ~setup:[ "let rec map2 f l = case l do | [] -> [] | h :: t -> f h :: map2 f t" ]
      "top-level recursive map" "map2" "(a -> b) -> List<a> -> List<b>";
    t_type ~setup:[ "let rec even n = if n == 0 then true else odd (n - 1)\nand odd n = if n == 0 then false else even (n - 1)" ]
      "mutual recursion even" "even" "Int -> Bool";
    t_type ~setup:[ "let rec even n = if n == 0 then true else odd (n - 1)\nand odd n = if n == 0 then false else even (n - 1)" ]
      "mutual recursion odd" "odd" "Int -> Bool";
    t_type "let tuple destructuring types each part" "let (a, b) = (1, \"s\") in a" "Int";
    t_type "let tuple destructuring second" "let (a, b) = (1, \"s\") in b" "String";
    t_type "let tuple destructuring in function" "fn p -> let (a, b) = p in a + 1" "(Int, a) -> Int";
    t_type "let nested destructuring" "let (a, (b, c)) = (1, (true, \"s\")) in (c, b, a)"
      "(String, Bool, Int)";
    t_type "let cons destructuring" "let h :: t = [1, 2] in t" "List<Int>";
    t_type "let record destructuring" "let {x: a, y: b} = {x: 1, y: true} in b" "Bool";
  ]

let definition_types =
  [
    t_def "pair" "let pair x y = (x, y)" "pair" "a -> b -> (a, b)";
    t_def "annotated param" "let f (x : Int) = x" "f" "Int -> Int";
    t_def "annotated return" "let f (x : Int) : String = int_to_str x" "f" "Int -> String";
    t_def "value annotation" "let n : Int = 5" "n" "Int";
    t_def "function value annotation" "let f : Int -> Int = fn x -> x" "f" "Int -> Int";
    t_def "type variable annotation" "let g (x : a) = x" "g" "a -> a";
    t_def "tuple destructuring def a" "let (a, b) = (1, \"s\")" "a" "Int";
    t_def "tuple destructuring def b" "let (a, b) = (1, \"s\")" "b" "String";
    t_def "constrained def" "let twice_show x = show x ^ show x" "twice_show" "Show a => a -> String";
    t_def "explicit constraint" "let shw [Show a] (x : a) = show x" "shw" "Show a => a -> String";
    t_def "generic adt ctor" "type Box<a> = | Box of a\nlet b = Box 1" "b" "Box<Int>";
    t_def "generic adt fn" "type Box<a> = | Box of a\nlet unbox b = case b do | Box x -> x" "unbox" "Box<a> -> a";
    t_def "recursive adt" "type rec T = | L | N of (T, Int, T)\nlet t = N (L, 1, L)" "t" "T";
    t_def "recursive generic adt" "type rec Tree<a> = | Leaf | Node of (Tree<a>, a, Tree<a>)\nlet rec size t = case t do | Leaf -> 0 | Node (l, _, r) -> size l + 1 + size r"
      "size" "Tree<a> -> Int";
    t_def "either" "type Either<a, b> = | Left of a | Right of b\nlet l = Left 1" "l" "Either<Int, a>";
    t_def "record field fn" "let get_x r = r.x" "get_x" "{x: a} -> a";
    t_def "record update fn" "let set_x r = { r with x = 1 }" "set_x" "{x: Int} -> {x: Int}";
    t_def "alias in annotation" "type Pt = (Int, Int)\nlet origin : Pt = (0, 0)" "origin" "(Int, Int)";
  ]

let rejection_tests =
  [
    t_reject "int plus bool" "1 + true";
    t_reject "ill-typed statement in a block" "{ 1 + true; 2 }";
    t_reject "ill-typed statement after a local definition" "{ let x = 1; x ^ \"a\"; x }";
    t_reject "string concat int" "\"a\" ^ 1";
    t_reject "non-bool condition" "if 1 then 2 else 3";
    t_reject "branch mismatch" "if true then 1 else \"a\"";
    t_reject "heterogeneous list" "[1, \"a\"]";
    t_reject "cons type mismatch" "1 :: [\"a\"]";
    t_reject "too many arguments" "(fn x -> x + 1) 1 2";
    t_reject "applying a non-function" "1 2";
    t_reject "occurs check self application" "fn x -> x x";
    t_reject "occurs check recursive" "let rec f x = f in f";
    t_reject "occurs check list" "fn x -> x :: x";
    t_reject "unbound variable" "undefined_name + 1";
    t_reject "unbound in body" "let a = 1 in b";
    t_reject "let is not recursive" "let f n = f n in f 1";
    t_reject "pattern type mismatch string vs int" "case 1 do | \"a\" -> 0 | _ -> 1";
    t_reject "tuple pattern on list" "case [1] do | (a, b) -> a";
    t_reject "payload pattern type" "case Some 1 do | Some \"a\" -> 0 | _ -> 1";
    t_reject "unknown constructor in pattern" "case Some 1 do | Foo -> 0 | _ -> 1";
    t_reject "missing payload in pattern" "case Some 1 do | Some -> 0 | _ -> 1";
    t_reject "extra payload in pattern" "case None do | None x -> 0 | _ -> 1";
    t_reject "branch result mismatch" "case 1 do | 1 -> 1 | _ -> \"x\"";
    t_reject "tuple pattern arity" "case (1, 2) do | (a, b, c) -> a";
    t_reject "not on int" "not 1";
    t_reject "and on int" "true && 1";
    t_reject "tuple size mismatch in equality" "(1, 2) == (1, 2, 3)";
    t_reject "equality of different types" "1 == \"1\"";
    t_reject "no rank-2 polymorphism" "fn f -> (f 1, f true)";
    t_reject "value annotation mismatch" "let (x : Int) = \"a\"";
    t_reject "value annotation mismatch top-level" "let x : String = 1";
    t_reject "return annotation mismatch" "let f (x : Int) : String = x";
    t_reject "param annotation mismatch" "let f (x : Bool) = x + 1";
    t_reject "show of a function" "show (fn x -> x)";
    t_reject "compare functions" "compare (fn x -> x) (fn x -> x)";
    t_reject "functor on int" "fmap (fn x -> x) 5";
    t_reject "missing record field" "{x: 1}.y";
    t_reject "missing field through function" "(fn r -> r.x) {y: 1}";
    t_reject "missing field via annotated function" "let f r = r.x + 1\nlet v = f {y: 1}";
    t_reject "record update unknown field" "{ {x: 1} with y = 2 }";
    t_reject "record field type mismatch" "{x: 1}.x ^ \"a\"";
    t_reject "record update type mismatch" "{ {x: 1} with x = \"a\" }";
    t_reject "records with different fields" "if true then {x: 1} else {y: 1}";
    t_reject "duplicate record field" "{x: 1, x: 2}";
    t_reject "range with bool bound" "let b = true in [1...b]";
    t_type "range with variable bounds" "let a = 2 in let b = 4 in [a...b]" "List<Int>";
    t_reject "generator over non-list" "[x | x <- 5]";
    t_reject "option of different payloads" "Some 1 == Some \"a\"";
    t_reject "str_length of int" "str_length 5";
    t_reject "list_length of int" "list_length 5";
    t_reject "int_to_str of string" "int_to_str \"a\"";
    t_reject "constructor arity" "Some 1 2";
    t_reject "nullary constructor applied" "None 1";
    t_reject "unknown constructor" "Frobnicate 1";
    t_reject "mutual recursion mismatch" "let rec f x = g x + 1\nand g y = f y ^ \"a\"";
    t_reject "unknown type in annotation" "let x : Foo = 1";
    t_reject "type constructor arity" "let x : Option<Int, Int> = None";
    t_reject "self reference in non-rec let" "let x = x + 1";
    t_reject "map with wrong function" "map (fn x -> x + 1) [\"a\"]";
    t_reject "filter with non-bool predicate" "filter (fn x -> x) [1]";
    t_reject "applying unit" "() 1";
    t_reject "concat of lists with ^" "[1] ^ [2]";
    t_reject "pipeline into non-function" "1 |> 2";
    t_reject "char plus" "'a' + 1";
    t_reject "float plus int" "1.5 + 1";
    t_reject "semigroup on int" "1 ++ 2";
    t_reject "println of a function" "println (fn x -> x)";
  ]

(* A few rejections end-to-end: exit 1 and a clean type-error message. *)
let cli_rejections =
  List.map
    (fun (name, src) -> name >:: fun _ -> assert_clean_error name src (interp src))
    [
      ("cli: type error", "let x = 1 + true");
      ("cli: unbound variable", "let () = print_string y");
      ("cli: occurs check", "let f x = x x");
      ("cli: missing instance", "let () = println (fn x -> x)");
      ("cli: annotation mismatch", "let x : Int = \"a\"");
      ("cli: missing field", "let v = {x: 1}.y");
    ]

let suite =
  "typecheck"
  >::: [
         "literals" >::: literal_types;
         "functions" >::: function_types;
         "builtins" >::: builtin_types;
         "polymorphism" >::: polymorphism_tests;
         "definitions" >::: definition_types;
         "rejections" >::: rejection_tests;
         "cli" >::: cli_rejections;
       ]

let () = run_test_tt_main suite
