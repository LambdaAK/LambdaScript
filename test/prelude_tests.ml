(** Every name exported by [prelude/prelude.forge] and every builtin listed in
    the README, exercised through both back ends. *)

open OUnit2
open Forge_test_util

let ok name src expected = name >:: fun _ -> assert_both ~expected name src
let pr e = Printf.sprintf "let () = println (%s)" e
let ps e = Printf.sprintf "let () = print_string (%s)" e

let opt_int = "let oi o = case o do | Some v -> \"Some \" ^ int_to_str v | None -> \"None\"\n"

let show_tests =
  [
    ok "show Int" (ps "show 42") "42\n";
    ok "show negative Int" (ps "show ~-7") "-7\n";
    ok "show zero" (ps "show 0") "0\n";
    ok "show Bool true" (ps "show true") "true\n";
    ok "show Bool false" (ps "show false") "false\n";
    ok "show Unit" (ps "show ()") "()\n";
    ok "show String is identity" (ps "show \"hi there\"") "hi there\n";
    ok "show empty String" (ps "show \"\"") "\n";
    ok "show empty list" (ps "show (list_tail [1])") "[]\n";
    ok "show singleton list" (ps "show [1]") "[1]\n";
    ok "show list" (ps "show [1, 2, 3]") "[1, 2, 3]\n";
    ok "show nested list" (ps "show [[1, 2], [3]]") "[[1, 2], [3]]\n";
    ok "show list of bools" (ps "show [true, false]") "[true, false]\n";
    ok "show list of strings" (ps "show [\"a\", \"b\"]") "[a, b]\n";
    ok "show list of units" (ps "show [(), ()]") "[(), ()]\n";
    ok "show list of orderings" (ps "show [LT, EQ, GT]") "[LT, EQ, GT]\n";
    ok "show Ordering LT" (ps "show LT") "LT\n";
    ok "show Ordering EQ" (ps "show EQ") "EQ\n";
    ok "show Ordering GT" (ps "show GT") "GT\n";
    ok "show Char" (ps "show 'x'") "x\n";
    ok "show Option Some" (ps "show (Some 3)") "Some 3\n";
    ok "show Option None" (ps "show (list_head [None, Some 1])") "None\n";
    ok "show tuple" (ps "show (1, true)") "(1, true)\n";
    ok "show triple" (ps "show (1, \"a\", 'c')") "(1, a, c)\n";
    ok "show nested option" (ps "show (Some (Some 2))") "Some Some 2\n";
    ok "show Float" (ps "show 2.5") "2.5\n";
    ok "show integral Float" (ps "show 3.0") "3.0\n";
    ok "show list of options" (ps "show [Some 1, None]") "[Some 1, None]\n";
  ]

let println_tests =
  [
    ok "println Int" (pr "1 + 1") "2\n";
    ok "println String" (pr "\"text\"") "text\n";
    ok "println Bool" (pr "1 < 2") "true\n";
    ok "println Unit" (pr "()") "()\n";
    ok "println list" (pr "[3, 2, 1]") "[3, 2, 1]\n";
    ok "println Ordering" (pr "compare 2 1") "GT\n";
    ok "println twice" "let () = println 1\nlet () = println 2" "1\n2\n";
    ok "print_string adds newline" (ps "\"a\"") "a\n";
    ok "print does not add newline" "let () = print \"a\"\nlet () = print \"b\"\nlet () = print_string \"\"" "ab\n";
  ]

let eq_tests =
  [
    ok "Eq Int ==" (pr "(==) 3 3") "true\n";
    ok "Eq Int !=" (pr "(!=) 3 4") "true\n";
    ok "Eq Bool ==" (pr "(==) true false") "false\n";
    ok "Eq Unit ==" (pr "(==) () ()") "true\n";
    ok "Eq String ==" (pr "(==) \"a\" \"a\"") "true\n";
    ok "Eq String !=" (pr "(!=) \"a\" \"a\"") "false\n";
    ok "Eq Float ==" (pr "(==) 1.5 1.5") "true\n";
    ok "Eq Char ==" (pr "(==) 'a' 'b'") "false\n";
    ok "Eq Ordering ==" (pr "(==) LT LT") "true\n";
    ok "Eq Ordering !=" (pr "(!=) LT GT") "true\n";
    ok "int_eq" (pr "int_eq 4 4") "true\n";
    ok "float_eq" (pr "float_eq 0.5 0.25") "false\n";
    ok "str_eq" (pr "str_eq \"x\" \"x\"") "true\n";
    ok "char_eq" (pr "char_eq 'q' 'q'") "true\n";
    ok "bool_eq" (pr "bool_eq true true") "true\n";
    ok "unit_eq" (pr "unit_eq () ()") "true\n";
  ]

let ord_tests =
  [
    ok "compare Int lt" (pr "compare 1 2") "LT\n";
    ok "compare Int eq" (pr "compare 2 2") "EQ\n";
    ok "compare Int gt" (pr "compare 3 2") "GT\n";
    ok "compare negative" (pr "compare ~-1 1") "LT\n";
    ok "compare Bool" (pr "compare true false") "GT\n";
    ok "compare Unit" (pr "compare () ()") "EQ\n";
    ok "compare String" (pr "compare \"apple\" \"banana\"") "LT\n";
    ok "compare String prefix" (pr "compare \"ab\" \"a\"") "GT\n";
    ok "compare Float" (pr "compare 0.5 0.25") "GT\n";
    ok "compare Char" (pr "compare 'a' 'b'") "LT\n";
    ok "Ord default <" (pr "(<) 1 2") "true\n";
    ok "Ord default <=" (pr "(<=) 2 2") "true\n";
    ok "Ord default >" (pr "(>) 1 2") "false\n";
    ok "Ord default >=" (pr "(>=) \"b\" \"a\"") "true\n";
    ok "int_compare" (pr "int_compare 5 9") "LT\n";
    ok "str_compare" (pr "str_compare \"z\" \"a\"") "GT\n";
    ok "char_compare" (pr "char_compare 'm' 'm'") "EQ\n";
    ok "bool_compare" (pr "bool_compare false true") "LT\n";
    ok "unit_compare" (pr "unit_compare () ()") "EQ\n";
    ok "float_compare" (pr "float_compare 1.0 2.0") "LT\n";
    ok "sort with compare"
      "let rec insert x l = case l do | [] -> [x] | h :: t -> (case compare x h do | GT -> h :: insert x t | _ -> x :: l)\nlet rec sort l = case l do | [] -> [] | h :: t -> insert h (sort t)\nlet () = println (sort [3, 1, 2])"
      "[1, 2, 3]\n";
  ]

let semigroup_tests =
  [
    ok "String mappend" (ps "mappend \"ab\" \"cd\"") "abcd\n";
    ok "String ++" (ps "\"ab\" ++ \"cd\"") "abcd\n";
    ok "String empty" (ps "empty ++ \"z\"") "z\n";
    ok "List mappend" (pr "mappend [1] [2, 3]") "[1, 2, 3]\n";
    ok "List ++" (pr "[1, 2] ++ [3] ++ []") "[1, 2, 3]\n";
    ok "List ++ empty left" (pr "list_length (list_tail [0] ++ [5])") "1\n";
    ok "List empty" (pr "list_length (empty ++ [1, 2])") "2\n";
    ok "Option mappend first Some wins" (opt_int ^ ps "oi (mappend (Some 1) (Some 2))") "Some 1\n";
    ok "Option mappend None left" (opt_int ^ ps "oi (mappend None (Some 2))") "Some 2\n";
    ok "Option ++" (opt_int ^ ps "oi (None ++ None ++ Some 7)") "Some 7\n";
    ok "Option empty" (opt_int ^ ps "oi (empty ++ Some 4)") "Some 4\n";
  ]

let monad_tests =
  [
    ok "fmap Some" (opt_int ^ ps "oi (fmap (fn x -> x + 1) (Some 1))") "Some 2\n";
    ok "fmap None" (opt_int ^ ps "oi (fmap (fn x -> x + 1) None)") "None\n";
    ok "pure" (opt_int ^ ps "oi (pure 5)") "Some 5\n";
    ok "ap Some Some" (opt_int ^ ps "oi (ap (Some (fn x -> x * 3)) (Some 4))") "Some 12\n";
    ok "ap None" (opt_int ^ ps "oi (ap None (Some 4))") "None\n";
    ok "ap Some None" (opt_int ^ ps "oi (ap (Some (fn x -> x * 3)) None)") "None\n";
    ok "bind Some" (opt_int ^ ps "oi (bind (Some 3) (fn x -> Some (x * x)))") "Some 9\n";
    ok "bind None" (opt_int ^ ps "oi (bind None (fn x -> Some (x * x)))") "None\n";
    ok "bind to None" (opt_int ^ ps "oi (bind (Some 3) (fn x -> None))") "None\n";
    ok ">>= chain" (opt_int ^ ps "oi (Some 2 >>= (fn x -> Some (x + 1)) >>= (fn y -> Some (y * 10)))") "Some 30\n";
    ok "aempty" (opt_int ^ ps "oi aempty") "None\n";
    ok "<|> left" (opt_int ^ ps "oi (Some 1 <|> Some 2)") "Some 1\n";
    ok "<|> right" (opt_int ^ ps "oi (None <|> Some 2)") "Some 2\n";
    ok "<|> both None" (opt_int ^ ps "oi (None <|> None)") "None\n";
  ]

let types_tests =
  [
    ok "List constructors" (pr "list_length ((::) 1 [])") "1\n";
    ok "Option constructors" "let () = println (case Some \"s\" do | Some s -> str_length s | None -> 0)" "1\n";
    ok "Ordering constructors" "let o = GT\nlet () = println (case o do | GT -> 1 | _ -> 0)" "1\n";
    ok "user can shadow prelude println" "let println x = print_string \"mine\"\nlet () = println 1" "mine\n";
  ]

(* Builtins from the README "Built-in Functions" section not covered elsewhere. *)
let builtin_tests =
  [
    ok "list_of_string" (pr "list_length (list_of_string \"four\")") "4\n";
    ok "string_to_list then compare" (pr "list_head (string_to_list \"zq\") == 'z'") "true\n";
    ok "map identity" (pr "map (fn x -> x) [4, 5]") "[4, 5]\n";
    ok "map to strings" (pr "map int_to_str [1, 2]") "[1, 2]\n";
    ok "map empty" (pr "list_length (map (fn x -> x) [])") "0\n";
    ok "filter none" (pr "list_length (filter (fn x -> false) [1, 2])") "0\n";
    ok "filter keeps order" (pr "filter (fn x -> x > 1) [3, 1, 2]") "[3, 2]\n";
    ok "reduce_left strings" (ps "reduce_left (fn a -> fn b -> a ^ b) \"\" [\"x\", \"y\", \"z\"]") "xyz\n";
    ok "reduce_right strings" (ps "reduce_right (fn a -> fn b -> a ^ b) [\"x\", \"y\", \"z\"] \"\"") "xyz\n";
    ok "reduce_right builds list" (pr "reduce_right (fn x -> fn acc -> x * 2 :: acc) [1, 2] []") "[2, 4]\n";
    ok "list_nth last" (pr "list_nth [1, 2, 3] 2") "3\n";
    ok "tuple_fst of nested" (pr "tuple_fst (tuple_snd (1, (2, 3)))") "2\n";
    ok "int_to_float" (pr "float_to_int (int_to_float 12)") "12\n";
    ok "float_to_str shortest round trip" (ps "float_to_str 0.1 ^ \" \" ^ float_to_str 0.333333333333 ^ \" \" ^ float_to_str (-2.25)") "0.1 0.333333333333 -2.25\n";
    ok "float arithmetic operators" (ps "float_to_str (1.5 +. 2.25) ^ \" \" ^ float_to_str (1.0 -. 4.0) ^ \" \" ^ float_to_str (2.5 *. 4.0) ^ \" \" ^ float_to_str (1.0 /. 8.0)") "3.75 -3.0 10.0 0.125\n";
    ok "float operators: precedence like + and *" (ps "float_to_str (3.0 *. 2.0 -. 1.0 /. 4.0 +. 1.0)") "6.75\n";
    ok "float operators as values" "let () = println (reduce_left (+.) 0.0 [0.5, 1.5, 2.0], map ((*.) 2.0) [1.0, 3.0])" "(4.0, [2.0, 6.0])\n";
    ok "sqrt exp log" (ps "float_to_str (sqrt 16.0) ^ \" \" ^ float_to_str (exp 0.0) ^ \" \" ^ float_to_str (log 1.0) ^ \" \" ^ float_to_str (log (exp 2.0))") "4.0 1.0 0.0 2.0\n";
    ok "float_to_str: integral values keep their digits"
      (ps "float_to_str (-10.0) ^ \" \" ^ float_to_str 123.0 ^ \" \" ^ float_to_str 1000000000000000.0 ^ \" \" ^ float_to_str 99.5")
      "-10.0 123.0 1000000000000000.0 99.5\n";
    ok "float_to_str integral and large" (ps "float_to_str 7.0 ^ \" \" ^ float_to_str 100000000000000000000000.0 ^ \" \" ^ float_to_str 0.0000001") "7.0 1e+23 1e-07\n";
    ok "char_to_str" (ps "char_to_str 'a' ^ char_to_str 'b'") "ab\n";
    ok "not false" (pr "not false") "true\n";
    ok "not as value" (pr "map not [true, false]") "[false, true]\n";
    ok "str_concat partial" (ps "map (str_concat \"<\") [\"a\", \"b\"] |> reduce_left (fn a -> fn b -> a ^ b) \"\"") "<a<b\n";
    ok "str_slice partial" (ps "(str_slice \"abcdef\" 2) 2") "cd\n";
    ok "operators as values" (pr "reduce_left (+) 0 [1, 2, 3] + reduce_left (*) 1 [2, 3]") "12\n";
    ok "comparison operators as values" (pr "filter ((<) 2) [1, 2, 3, 4]") "[3, 4]\n";
    ok "division operator as value" (pr "map ((/) 100) [10, 20]") "[10, 5]\n";
    ok "cons operator as value" (pr "reduce_right (::) [1, 2] []") "[1, 2]\n";
    ok "or operator as value" (pr "reduce_left (||) false [false, true]") "true\n";
  ]

let suite =
  "prelude"
  >::: [
         "show" >::: show_tests;
         "println" >::: println_tests;
         "eq" >::: eq_tests;
         "ord" >::: ord_tests;
         "semigroup_monoid" >::: semigroup_tests;
         "functor_monad" >::: monad_tests;
         "types" >::: types_tests;
         "builtins" >::: builtin_tests;
       ]

let () = run_test_tt_main suite
