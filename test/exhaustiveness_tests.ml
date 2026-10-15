(** Exhaustiveness and redundancy checking of [case] expressions.

    Contract tested here (through the interpreter executable):
    - a non-exhaustive [case] produces a warning on stderr that names a missing
      pattern, and the program still runs;
    - an arm that can never match is reported as redundant;
    - exhaustive matches produce no warning at all (no false positives),
      including everything in the prelude. *)

open OUnit2
open Forge_test_util

let run_ok src =
  let r = interp src in
  (match r.status with
  | Exited 0 -> ()
  | _ -> assert_failure ("program should still run: " ^ describe r ^ show_src src));
  r

(** Non-exhaustive: warning on stderr mentioning [missing]. *)
let t_missing name ?(missing = []) src =
  name >:: fun _ ->
  let r = run_ok src in
  if not (lowercase_contains r.err "warning") then
    assert_failure ("expected a warning on stderr, got: " ^ describe r ^ show_src src);
  if not (lowercase_contains r.err "exhaustive") then
    assert_failure ("warning should say the match is not exhaustive: " ^ r.err ^ show_src src);
  List.iter
    (fun m ->
      if not (contains r.err m) then
        assert_failure (Printf.sprintf "warning should mention missing case %S: %s%s" m r.err (show_src src)))
    missing

let t_redundant name src =
  name >:: fun _ ->
  let r = run_ok src in
  if not (lowercase_contains r.err "redundant") then
    assert_failure ("expected a redundant-arm warning: " ^ describe r ^ show_src src)

let t_clean name src =
  name >:: fun _ ->
  let r = run_ok src in
  if String.trim r.err <> "" then
    assert_failure ("expected no warnings, got stderr:\n" ^ r.err ^ show_src src)

let colors = "type Color = | Red | Green | Blue\n"

let missing_tests =
  [
    t_missing "bool missing false" ~missing:[ "false" ]
      "let f b = case b do | true -> 1\nlet () = println (f true)";
    t_missing "bool missing true" ~missing:[ "true" ]
      "let f b = case b do | false -> 1\nlet () = println (f false)";
    t_missing "adt missing constructor" ~missing:[ "Blue" ]
      (colors ^ "let f c = case c do | Red -> 1 | Green -> 2\nlet () = println (f Red)");
    t_missing "adt missing two constructors" ~missing:[ "Green" ]
      (colors ^ "let f c = case c do | Red -> 1\nlet () = println (f Red)");
    t_missing "option missing None" ~missing:[ "None" ]
      "let f o = case o do | Some x -> x\nlet () = println (f (Some 1))";
    t_missing "option missing Some" ~missing:[ "Some" ]
      "let f o = case o do | None -> 0\nlet () = println (f None)";
    t_missing "list missing nil" ~missing:[ "[]" ]
      "let f l = case l do | h :: _ -> h\nlet () = println (f [1])";
    t_missing "list missing cons" ~missing:[ "::" ]
      "let f l = case l do | [] -> 0\nlet () = println (f [])";
    t_missing "int literals without wildcard"
      "let f n = case n do | 0 -> \"z\" | 1 -> \"o\"\nlet () = print_string (f 0)";
    t_missing "string literals without wildcard"
      "let f s = case s do | \"a\" -> 1\nlet () = println (f \"a\")";
    t_missing "char literals without wildcard"
      "let f c = case c do | 'a' -> 1\nlet () = println (f 'a')";
    t_missing "nested option" ~missing:[ "None" ]
      "let f o = case o do | Some (Some x) -> x | None -> 0\nlet () = println (f None)";
    t_missing "tuple of bools" ~missing:[ "false" ]
      "let f p = case p do | (true, true) -> 1 | (false, _) -> 2\nlet () = println (f (true, true))";
    t_missing "list of at most one element"
      "let f l = case l do | [] -> 0 | _ :: [] -> 1\nlet () = println (f [])";
    t_missing "record field literal" ~missing:[ "false" ]
      "let f r = case r do | {on: true} -> 1\nlet () = println (f {on: true})";
    t_missing "inside a lambda" ~missing:[ "None" ]
      "let g = fn o -> case o do | Some x -> x\nlet () = println (g (Some 2))";
    t_missing "nested case" ~missing:[ "Blue" ]
      (colors
     ^ "let f a b = case a do | true -> (case b do | Red -> 1 | Green -> 2) | false -> 0\nlet () = println (f false Red)"
      );
    t_missing "recursive type" ~missing:[ "Node" ]
      "type rec T = | Leaf | Node of (T, T)\nlet f t = case t do | Leaf -> 0\nlet () = println (f Leaf)";
    t_missing "payload constructor" ~missing:[ "Square" ]
      "type S = | Circle of Int | Square of Int\nlet f s = case s do | Circle r -> r\nlet () = println (f (Circle 1))";
    t_missing "ordering" ~missing:[ "GT" ]
      "let f o = case o do | LT -> 1 | EQ -> 2\nlet () = println (f LT)";
    t_missing "constructor with literal payload" ~missing:[ "Some" ]
      "let f o = case o do | Some 1 -> 1 | None -> 0\nlet () = println (f None)";
    t_missing "top-level expression" ~missing:[ "None" ]
      "let v = case Some 3 do | Some x -> x\nlet () = println v";
    t_missing "in a block" ~missing:[ "false" ]
      "let v = { let b = true; case b do | true -> 1 }\nlet () = println v";
    t_missing "generic adt" ~missing:[ "Right" ]
      "type E<a, b> = | Left of a | Right of b\nlet f e = case e do | Left x -> x\nlet () = println (f (Left 1))";
  ]

let redundant_tests =
  [
    t_redundant "arm after wildcard" "let f n = case n do | _ -> 0 | 1 -> 1\nlet () = println (f 1)";
    t_redundant "arm after variable" "let f o = case o do | x -> 0 | None -> 1\nlet () = println (f None)";
    t_redundant "duplicate constructor arm"
      (colors ^ "let f c = case c do | Red -> 1 | Red -> 2 | Green -> 3 | Blue -> 4\nlet () = println (f Red)");
    t_redundant "duplicate literal arm" "let f n = case n do | 1 -> 1 | 1 -> 2 | _ -> 3\nlet () = println (f 1)";
    t_redundant "wildcard after complete bool"
      "let f b = case b do | true -> 1 | false -> 0 | _ -> 2\nlet () = println (f true)";
    t_redundant "specific after general payload"
      "let f o = case o do | Some x -> x | Some 1 -> 2 | None -> 0\nlet () = println (f None)";
    t_redundant "nil after list wildcard" "let f l = case l do | _ -> 0 | [] -> 1\nlet () = println (f [])";
    t_redundant "tuple arm covered" "let f p = case p do | (_, _) -> 0 | (1, 2) -> 1\nlet () = println (f (1, 2))";
  ]

let clean_tests =
  [
    t_clean "bool both" "let f b = case b do | true -> 1 | false -> 0\nlet () = println (f true)";
    t_clean "adt all constructors"
      (colors ^ "let f c = case c do | Red -> 1 | Green -> 2 | Blue -> 3\nlet () = println (f Blue)");
    t_clean "option both" "let f o = case o do | Some x -> x | None -> 0\nlet () = println (f None)";
    t_clean "list nil and cons" "let f l = case l do | [] -> 0 | h :: _ -> h\nlet () = println (f [4])";
    t_clean "final wildcard" "let f n = case n do | 0 -> 1 | _ -> 2\nlet () = println (f 0)";
    t_clean "final variable" "let f n = case n do | 0 -> 1 | m -> m\nlet () = println (f 5)";
    t_clean "nested option covered"
      "let f o = case o do | Some (Some x) -> x | Some None -> 1 | None -> 0\nlet () = println (f None)";
    t_clean "tuple of bools all combinations"
      "let f p = case p do | (true, true) -> 1 | (true, false) -> 2 | (false, true) -> 3 | (false, false) -> 4\nlet () = println (f (false, true))";
    t_clean "list by length" "let f l = case l do | [] -> 0 | _ :: [] -> 1 | _ :: _ :: _ -> 2\nlet () = println (f [1, 2])";
    t_clean "record binders" "let f r = case r do | {x: a, y: b} -> a + b\nlet () = println (f {x: 1, y: 2})";
    t_clean "record bool field both" "let f r = case r do | {on: true} -> 1 | {on: false} -> 0\nlet () = println (f {on: false})";
    t_clean "unit" "let f u = case u do | () -> 1\nlet () = println (f ())";
    t_clean "ordering" "let f o = case o do | LT -> 1 | EQ -> 2 | GT -> 3\nlet () = println (f GT)";
    t_clean "generic adt" "type Box<a> = | Box of a\nlet f b = case b do | Box x -> x\nlet () = println (f (Box 1))";
    t_clean "recursive tree"
      "type rec T = | Leaf | Node of (T, Int, T)\nlet rec s t = case t do | Leaf -> 0 | Node (l, v, r) -> s l + v + s r\nlet () = println (s Leaf)";
    t_clean "int with wildcard" "let f n = case n do | 1 -> 1 | 2 -> 2 | _ -> 3\nlet () = println (f 9)";
    t_clean "tuple single arm" "let f p = case p do | (x, y) -> x + y\nlet () = println (f (1, 2))";
    t_clean "cons literal head then wildcard" "let f l = case l do | 1 :: _ -> 1 | _ -> 0\nlet () = println (f [1])";
    t_clean "string with wildcard" "let f s = case s do | \"a\" -> 1 | _ -> 0\nlet () = println (f \"b\")";
    t_clean "payload constructors"
      "type S = | Circle of Int | Square of Int\nlet f s = case s do | Circle r -> r | Square w -> w\nlet () = println (f (Square 2))";
    t_clean "mutually recursive types"
      "type rec A = | A0 | A1 of B\nand B = | B0 | B1 of A\nlet rec fa a = case a do | A0 -> 0 | A1 b -> fb b\nand fb b = case b do | B0 -> 1 | B1 a -> fa a\nlet () = println (fa (A1 B0))";
    t_clean "nested tuple and option"
      "let f p = case p do | (Some a, _) -> a | (None, b) -> b\nlet () = println (f (None, 3))";
    t_clean "empty program with prelude" "let () = print_string \"ok\"";
    t_clean "prelude-heavy program"
      "let () = println (show 1 ^ show true)\nlet () = println (compare 1 2)\nlet () = println ([1] ++ [2])";
  ]

let behaviour_tests =
  [
    ( "warning does not change program output" >:: fun _ ->
      let src = "let f b = case b do | true -> \"yes\"\nlet () = print_string (f true)" in
      let r = run_ok src in
      assert_equal ~printer:(Printf.sprintf "%S") "yes\n" r.out );
    ( "non-exhaustive match still fails at run time when it falls through" >:: fun _ ->
      let src = "let f b = case b do | true -> \"yes\"\nlet () = print_string (f false)" in
      let r = interp src in
      match r.status with
      | Exited 0 -> assert_failure ("should fail at run time: " ^ describe r)
      | _ -> () );
  ]

let suite =
  "exhaustiveness"
  >::: [
         "missing" >::: missing_tests;
         "redundant" >::: redundant_tests;
         "no_false_positives" >::: clean_tests;
         "behaviour" >::: behaviour_tests;
       ]

let () = run_test_tt_main suite
