open OUnit2
open Language.Ceval
open Language.Typecheck
open Language.Condense
open Language.Ctostringtree.CToStringTree
open Language.Parse
open Language.Lex
open Language.Env
open Language.Ceval_defn

let modify_tests : bool = false

module type TestModifier = sig
  type test_type

  val modify_tests : test_type list -> test_type list
end

module type TestModifierInput = sig
  type test_type

  val modifiers : (test_type -> test_type) list
end

module MakeTestModifier (Input : TestModifierInput) :
  TestModifier with type test_type = Input.test_type = struct
  type test_type = Input.test_type

  let modify_tests (tests : test_type list) : test_type list =
    if not modify_tests then tests
    else
      List.map
        (fun expression ->
          List.map (fun modifier -> modifier expression) Input.modifiers)
        tests
      |> List.flatten
end

module IntTestModifierInput :
  TestModifierInput with type test_type = string * string = struct
  type test_type = string * string

  let modifiers =
    [
      (fun (x, y) -> (x, y));
      (fun (x, y) ->
        (x ^ " + 1", y |> int_of_string |> ( + ) 1 |> string_of_int));
      (fun (x, y) ->
        (x ^ " + 2", y |> int_of_string |> ( + ) 2 |> string_of_int));
      (fun (x, y) ->
        (x ^ " + 3", y |> int_of_string |> ( + ) 3 |> string_of_int));
      (fun (x, y) ->
        (x ^ " + 4", y |> int_of_string |> ( + ) 4 |> string_of_int));
      (fun (x, y) ->
        (x ^ " + 5", y |> int_of_string |> ( + ) 5 |> string_of_int));
      (fun (x, y) ->
        ( "( " ^ x ^ " ) " ^ "* 2",
          y |> int_of_string |> ( * ) 2 |> string_of_int ));
      (fun (x, y) ->
        ( "( " ^ x ^ " ) " ^ "* 3",
          y |> int_of_string |> ( * ) 3 |> string_of_int ));
      (* division *)
      (fun (x, y) ->
        ( "( " ^ x ^ " ) " ^ "/ 2",
          y |> int_of_string |> (fun x -> x / 2) |> string_of_int ));
      (fun (x, y) ->
        ( "( " ^ x ^ " ) " ^ "/ 3",
          y |> int_of_string |> (fun x -> x / 3) |> string_of_int ));
      (fun (x, y) ->
        ( "( " ^ x ^ " ) " ^ "/ 4",
          y |> int_of_string |> (fun x -> x / 4) |> string_of_int ));
      (fun (x, y) ->
        ( "~- ( " ^ x ^ " )",
          y |> int_of_string |> (fun x -> -x) |> string_of_int ));
      (* ternary *)
      (fun (x, y) -> ("if true then " ^ x ^ " else 0", y));
      (fun (x, y) -> ("if false then 0 else " ^ x, y));
      (* more ternary *)
      (fun (x, _) -> ("if false then " ^ x ^ " else 0", "0"));
      (fun (x, _) -> ("if true then 0 else " ^ x, "0"));
      (* more complicated tests *)
      (fun (x, y) ->
        ( x ^ " + 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10",
          y |> int_of_string |> ( + ) 55 |> string_of_int ));
      (* tests involving functions *)
      (fun (x, y) -> ("(\\ n -> n) (" ^ x ^ " )", y));
      (fun (x, y) ->
        ( "(\\ n -> n + 1) (" ^ x ^ " )",
          y |> int_of_string |> ( + ) 1 |> string_of_int ));
      (fun (x, y) ->
        ( "(\\ a -> \\ b -> a + b) (" ^ x ^ " )" ^ " ( " ^ x ^ " )",
          y |> int_of_string |> ( * ) 2 |> string_of_int ));
      (* tests involving let expressions *)
      (fun (x, y) -> ("let a = " ^ x ^ " in a", y));
      (fun (x, y) ->
        ( "let a = " ^ x ^ " in a + 1",
          y |> int_of_string |> ( + ) 1 |> string_of_int ));
    ]
end

module EvalTestModifierInput :
  TestModifierInput with type test_type = string * string = struct
  type test_type = string * string

  let modifiers : (test_type -> test_type) list =
    [
      (* identity *)
      (fun (x, y) -> (x, y));
      (* let expression *)
      (fun (x, y) -> ("let a = " ^ x ^ " in a", y));
      (fun (x, y) -> ("let rec a = " ^ x ^ " in a", y));
      (fun (x, y) -> ("( \\ a -> a ) ( " ^ x ^ " )", y));
    ]
end

module TypeTestModifierInput : TestModifierInput with type test_type = string =
struct
  type test_type = string

  let modifiers : (test_type -> test_type) list =
    [
      (* identity *)
      (fun x -> x);
      (* let expression *)
      (fun x -> "let q = " ^ x ^ " in q");
      (fun x -> "let rec q = " ^ x ^ " in q");
      (fun x -> "( \\ a -> a ) ( " ^ x ^ " )");
    ]
end

module IntTypeTestModifierInput :
  TestModifierInput with type test_type = string = struct
  type test_type = string

  let modifiers : (string -> string) list =
    [
      (fun x -> x);
      (fun x -> x ^ " + 1");
      (fun x -> x ^ " + 2");
      (fun x -> x ^ " + 3");
      (fun x -> x ^ " + 4");
      (fun x -> x ^ " + 5");
      (fun x -> x ^ " + 6");
      (fun x -> x ^ " + 7");
      (fun x -> x ^ " + 8");
      (fun x -> x ^ " + 9");
      (fun x -> x ^ " + 10");
      (* multiply *)
      (fun x -> x ^ " * 2");
      (fun x -> x ^ " * 3");
      (fun x -> x ^ " * 4");
      (fun x -> x ^ " * 5");
      (fun x -> x ^ " * 6");
      (fun x -> x ^ " * 7");
      (fun x -> x ^ " * 8");
      (fun x -> x ^ " * 9");
      (fun x -> x ^ " * 10");
      (* divide *)
      (fun x -> x ^ " / 2");
      (fun x -> x ^ " / 3");
      (fun x -> x ^ " / 4");
      (fun x -> x ^ " / 5");
      (fun x -> x ^ " / 6");
      (fun x -> x ^ " / 7");
      (* paren *)
      (fun x -> "(" ^ x ^ ")");
      (* ternary *)
      (fun x -> "if true then " ^ x ^ " else 0");
      (fun x -> "if false then 0 else " ^ x);
      (* tests that involve functions that will evaluate to integers *)
      (fun x -> "(\\ n -> n) (" ^ x ^ " )");
      (fun x -> "(\\ n -> n + 1) (" ^ x ^ " )");
      (fun x -> "(\\ a -> \\ b -> a + b) (" ^ x ^ " )" ^ " ( " ^ x ^ " )");
    ]
end

module BoolTypeTestModifierInput :
  TestModifierInput with type test_type = string = struct
  type test_type = string

  let modifiers =
    [
      (fun x -> x);
      (fun x -> x ^ " || true");
      (* prepend *)
      (fun x -> "true || ( " ^ x ^ " )");
      (fun x -> "false || ( " ^ x ^ " )");
      (fun x -> "true && ( " ^ x ^ " )");
      (fun x -> "false && ( " ^ x ^ " )");
      (* append *)
      (fun x -> x ^ " || false");
      (fun x -> x ^ " && true");
      (fun x -> x ^ " && false");
      (fun x -> "not ( " ^ x ^ " )");
    ]
end

module FunctionTypeTestModifierInput :
  TestModifierInput with type test_type = string * string = struct
  type test_type = string * string

  let modifiers =
    [ (fun x -> x); (fun (x, y) -> ("(\\ m -> m) ( " ^ x ^ " )", y)) ]
end

module PairTypeTestModiferInput :
  TestModifierInput with type test_type = string * string = struct
  type test_type = string * string

  let modifiers : (string * string -> string * string) list = [ (fun x -> x) ]
end

module VectorTypeTestModifierInput :
  TestModifierInput with type test_type = string * string = struct
  type test_type = string * string

  let modifiers : (string * string -> string * string) list = [ (fun x -> x) ]
end

module ListTypeTestModifierInput :
  TestModifierInput with type test_type = string * string = struct
  type test_type = string * string

  let modifiers : (string * string -> string * string) list = [ (fun x -> x) ]
end

module SwitchTypeTestModifierInput :
  TestModifierInput with type test_type = string * string = struct
  type test_type = string * string

  let modifiers : (string * string -> string * string) list = [ (fun x -> x) ]
end

module PolymorphismTypeTestModifierInput :
  TestModifierInput with type test_type = string * string = struct
  type test_type = string * string

  let modifiers : (string * string -> string * string) list = [ (fun x -> x) ]
end

module IntTestModifier = MakeTestModifier (IntTestModifierInput)
module IntTypeTestModifier = MakeTestModifier (IntTypeTestModifierInput)
module BoolTypeTestModifier = MakeTestModifier (BoolTypeTestModifierInput)
module EvalTestModifier = MakeTestModifier (EvalTestModifierInput)
module TypeTestModifier = MakeTestModifier (TypeTestModifierInput)

module FunctionTypeTestModifier =
  MakeTestModifier (FunctionTypeTestModifierInput)

module PairTypeTestModifier = MakeTestModifier (PairTypeTestModiferInput)
module VectorTypeTestModifier = MakeTestModifier (VectorTypeTestModifierInput)
module ListTypeTestModifier = MakeTestModifier (ListTypeTestModifierInput)
module SwitchTypeTestModifier = MakeTestModifier (SwitchTypeTestModifierInput)

module PolymorphismTypeTestModifier =
  MakeTestModifier (PolymorphismTypeTestModifierInput)

let defn_type_test (defns_string : string) (expr_string : string)
    (expected_output : string) : test =
  let defns : Language.Cexpr.c_defn list =
    defns_string |> list_of_string |> lex |> parse_program |> condense_program
  in
  let expr : Language.Cexpr.c_expr =
    expr_string |> list_of_string |> lex |> parse_expr |> fst |> condense_expr
  in
  let description = "\ndefns: \n" ^ defns_string ^ "\nexpr: \n" ^ expr_string in
  description >:: fun _ ->
  (* First, evaluate the definitions to get the environments set up *)
  let _, static_env, type_env =
    List.fold_left
      (fun (env, static_env, type_env) defn ->
        let new_env, new_static_env, new_type_env, _, _, _ =
          eval_defn defn env static_env type_env []
        in
        (new_env, new_static_env, new_type_env))
      ([], [], []) defns
  in

  let type_of_expr = type_of_c_expr expr static_env type_env in
  let result_string = string_of_c_type type_of_expr in
  assert_equal result_string expected_output

let kind_of_type_test (defns_string : string) (type_string : string)
    (expected_output : string) : test =
  let defns : Language.Cexpr.c_defn list =
    defns_string |> list_of_string |> lex |> parse_program |> condense_program
  in
  let t : Language.Cexpr.c_type =
    type_string |> list_of_string |> lex |> parse_compound_type |> fst
    |> condense_type
  in

  let description =
    "\ndefns: \n" ^ defns_string ^ "\ntype: " ^ type_string
    ^ "\n\nexpected kind: " ^ expected_output
  in
  description >:: fun _ ->
  (* First, evaluate the definitions to get the environments set up *)
  let _, _, type_env, static_type_env =
    List.fold_left
      (fun (env, static_env, type_env, static_type_env) defn ->
        let new_env, new_static_env, new_type_env, new_static_type_env, _, _ =
          eval_defn defn env static_env type_env static_type_env
        in
        (new_env, new_static_env, new_type_env, new_static_type_env))
      ([], [], [], []) defns
  in

  let kind = kind_of_type t type_env "" static_type_env in
  let result_string = string_of_c_kind kind in
  assert_equal result_string expected_output

let defn_expr_test (defns_string : string) (expr_string : string)
    (expected_output : string) : test =
  let defns : Language.Cexpr.c_defn list =
    defns_string |> list_of_string |> lex |> parse_program |> condense_program
  in
  let expr : Language.Cexpr.c_expr =
    expr_string |> list_of_string |> lex |> parse_expr |> fst |> condense_expr
  in
  let description = "\ndefns: \n" ^ defns_string ^ "\nexpr: \n" ^ expr_string in
  description >:: fun _ ->
  (* First, evaluate the definitions to get the environments set up *)
  let env, _, _ =
    List.fold_left
      (fun (env, static_env, type_env) defn ->
        let new_env, new_static_env, new_type_env, _, _, _ =
          eval_defn defn env static_env type_env []
        in
        (new_env, new_static_env, new_type_env))
      ([], [], []) defns
  in

  (* Then, evaluate the expressino under those environments *)
  let result = eval_c_expr expr env in
  let result_string = string_of_value result in
  assert_equal result_string expected_output

let eval_test (expr : string) (expected_output : string) : test =
  expr ^ " SHOULD YIELD " ^ expected_output >:: fun _ ->
  let result : string = c_eval expr in
  assert_equal result expected_output

let type_test (expr : string) (expected_output : string) : test =
  expr ^ " SHOULD BE OF TYPE " ^ expected_output >:: fun _ ->
  let static_env =
    List.map
      (fun (id, code) ->
        (* get the type of the value *)
        let tokens : token list = code |> list_of_string |> lex in
        let e, _ = parse_expr tokens in
        let c_e = condense_expr e in
        let t = type_of_c_expr c_e [] [] in
        (id, t))
      code_mapping
    @ built_ins_types
  in
  let result : string =
    type_of_c_expr
      (expr |> list_of_string |> lex |> parse_expr |> fst |> condense_expr)
      static_env []
    |> string_of_c_type
  in
  assert_equal result expected_output

let type_is_bool (program : string) = type_test program "Bool"
let type_is_int (program : string) = type_test program "Int"
let type_is_string (program : string) = type_test program "Str"
let () = ignore type_is_bool
let () = ignore type_is_int
let () = ignore type_is_string

let int_types : string list =
  [
    "0";
    "1";
    "~-1";
    "~-2";
    "1 + 2";
    "1 + 2 + 3 + 4";
    "1 + 2 * 3 + 4";
    "100 / 30 + 5";
    "10 % 4";
    "5 + 2 * 3";
    "2 + 3 * 4";
    "1 + 2 + 3 + 4 + 5";
    "10 - 2 * 3";
    "8 - 2 - 3 - 4 - 5";
    "6 * 2 + 3";
    "3 * 4 * 5";
    "15 / 3 - 2";
    "20 / 4 / 5";
    "10 * 2 / 4";
    "15 - 3 + 2";
    "2 + 4 * 6 - 8";
    "20 / 5 * 2 + 3";
    "7 - 3 * 2 / 4";
    "9 + 3 * 2 - 4 / 2";
    "5 * (3 + 2)";
    "12 / (4 - 2)";
    "3 + 4 * 2 / (1 - 5)";
    "(5 + 2) * 3 - 4";
    "2 * (10 - 8) + 1";
    "100 / 10 % 3";
    "100 % 30 / 2 * 3";
    "100 % 31 / 2";
    "100 % 31 / 2 * 3";
    "100 % 31 / 2 * 3 + 1";
    "100 % 31 / 2 * 3 + 1 - 1";
    "1 + 2 * 3 + 4 * 5 + 6 * 7 + 8 * 9 + 10";
    "1 - 1 - 1";
    "~-1 - 1 - 2";
    "10 - 20 - 30 - 40";
    "1 + 5 - 4 - 3";
    "10 - 5 + 5";
    "~-10 - 5 + 5 - 5 - 5";
    "1 - 2 - 3 - 4 - 5";
    "1 - 2 - 3 - 4 - 5 - 6";
    "1 + 2 - 3 + 4 - 5 + 6";
    "1 + 2 + 3 - 4 - 5 - 6";
    "1 * 2 * 3 * 4";
    "10 / 2 * 3";
  ]

let bool_types =
  [
    "true";
    "false";
    "true || true";
    "true || false";
    "false || true";
    "false || false";
    "true && true";
    "true && false";
    "false && true";
    "false && false";
    "true && true || false";
    "true || false || false || false || (true || false && true)";
    "true && false || false || false || (true || false && true)";
    "true && false || false || false || (false || false && true)";
    "true && false || false || false || (false || false && true)";
    "not true";
    "not false";
    "not (not true )";
    "not (not false)";
    "not true || false";
    "not true || true";
    "true && not true";
    "true && not false";
    "1 < 2";
    "1 > 2";
    "1 <= 2";
    "1 <= 1";
    "1 >= 1";
    "2 >= 1";
    "1 < 1";
    "1 < 2 && 13414 < 11413413";
    "1 < 2 && 13414 > 11413413";
    "1 < 2 && false";
    "1 == 1";
    "1 <> 1";
    "if true then true else false";
    "true || true";
    "1 < 2 || false";
    "1 == 1";
    "1 <> 1";
    "if true then true else false";
    "if false then true else false";
    "if 1 < 2 then true else false";
    "if 1 > 2 then true else false";
    "if not true then true else false";
    "if not false then true else false";
    "if not false || not true then true else false";
    "if true && true then true else false";
    "if true && false then true else false";
    "if false && true then true else false";
    "if false && false then true else false";
    "if true && true || false then true else false";
    "if true || false || false || false || (true || false && true) then true \
     else false";
    "if true && false || false || false || (true || false && true) then true \
     else false";
    "if true && false || false || false || (false || false && true) then true \
     else false";
    "if true && false || false || false || (false || false && true) then true \
     else false";
    "if not true then true else false";
    "if not false then true else false";
    "if not (not true ) then true else false";
    "if not (not false) then true else false";
    "if not true || false then true else false";
    "if not true || true then true else false";
    "if true && not true then true else false";
    "if true && not false then true else false";
    "if true && true || false then true else false";
    "if true || false || false || false || (true || false && true) then true \
     else false";
    "if true && false || false || false || (true || false && true) then true \
     else false";
    "if true && false || false || false || (false || false && true) then true \
     else false";
    "if true && false || false || false || (false || false && true) then true \
     else false";
    "if true && true || false then true else false";
    "if true || false || false || false || (true || false && true) then true \
     else false";
  ]

let string_types = [ {|""|}; {|"hello"|} ]

let function_type_tests =
  [
    ("\\ n -> n", "a -> a");
    ("\\ n -> n + 1", "Int -> Int");
    ("\\ a -> \\ b -> a + b", "Int -> Int -> Int");
    ("\\ n [Int] -> n", "Int -> Int");
    ("\\ n [Int] -> n + 1", "Int -> Int");
    ("\\ a [Int] -> \\ b [Int] -> a + b", "Int -> Int -> Int");
    ("\\ a -> \\ b -> a", "a -> b -> a");
    ("\\ a -> \\ b -> b", "a -> b -> b");
    ("\\ a [Int] -> \\ b -> b", "Int -> a -> a");
    ("\\ a -> \\ b [Int] -> b", "a -> Int -> Int");
    ("\\ a [Int] -> \\ b [Int] -> b", "Int -> Int -> Int");
    ("\\ a [Int] -> \\ b [Int] -> a", "Int -> Int -> Int");
    ("\\ a [Int] -> \\ b [Int] -> a + b", "Int -> Int -> Int");
    ("\\ a [Int] -> \\ b [Int] -> a + b + 1", "Int -> Int -> Int");
    ("\\ a [Int] -> \\ b [Int] -> a + b + 1 + 2", "Int -> Int -> Int");
    ("\\ (a, b) [(Int, Int)] -> a + b", "(Int, Int) -> Int");
    ("\\ (a, _) -> \\ (_, b) -> a + b", "(Int, a) -> (b, Int) -> Int");
    ("\\ (a, _) -> \\ (_, b) -> a || b", "(Bool, a) -> (b, Bool) -> Bool");
    ( {|\ (a, b) ->
    \ (c, d) ->
    if a then b
    else if c then d
    else 1|},
      "(Bool, Int) -> (Bool, Int) -> Int" );
    (* more complicated function type tests *)
    ("\\ a -> \\ b -> \\ c -> a ( b ( c ) )", "(a -> b) -> (c -> a) -> c -> b");
    (* tests with syntax sugar let expressions *)
    ("let f x = x in f", "a -> a");
    ("let f x = x + 1 in f", "Int -> Int");
    ("let f x = x + 1 in f 1", "Int");
    ("let f x = x + 1 in f 1 + 1", "Int");
    ("let f x = x + 1 in f (1 + 1)", "Int");
    (* add three numbers *)
    ("let f a b c = a + b + c in f", "Int -> Int -> Int -> Int");
    ("let f a b c = a + b + c in f 1", "Int -> Int -> Int");
    ("let f a b c = a + b + c in f 1 2", "Int -> Int");
    ("let f a b c = a + b + c in f 1 2 3", "Int");
    ("let f a b c d = a in f", "a -> b -> c -> d -> a");
    (* typed arguments *)
    ( "let f a [Int] b [Int] c [Int] d [Int] = a in f",
      "Int -> Int -> Int -> Int -> Int" );
    ( "let f a [Int] b [Int] c [Int] d [Int] = a in f 1",
      "Int -> Int -> Int -> Int" );
    ("let f a [Int] b [Int] c [Int] d [Int] = a in f 1 2", "Int -> Int -> Int");
    ("let f a [Int] b [Int] c [Int] d [Int] = a in f 1 2 3", "Int -> Int");
    ("let f a [Int] b [Int] c [Int] d [Int] = a in f 1 2 3 4", "Int");
    (* with type variables *)
    ("\\ a [a] -> a", "a -> a");
    ("\\ a [a] -> a + 1", "Int -> Int");
    ("\\ a [a] -> \\ b [a] -> a", "a -> a -> a");
    ("\\ a [a] -> \\ b [a] -> b", "a -> a -> a");
    ("\\ a [a] -> \\ b [b] -> a", "a -> b -> a");
    ("\\ a [a] -> \\ b [a] -> a + b", "Int -> Int -> Int");
    ("\\ a [a] -> \\ b [a] -> a + b + 1", "Int -> Int -> Int");
    ("\\ f [a -> b] -> \\ x [b] -> f x", "(a -> a) -> a -> a");
    (* this is an Interesting example because it turns out that a = b here *)
    ("\\ f [a -> b] -> \\ x [a] -> f x", "(a -> b) -> a -> b");
    (* on the other hand, there is no constraInt generated in this expression
       saying that a = b, so they are different *)
    ("let f a b [Int] c [Int] d [Int] = a in f", "a -> Int -> Int -> Int -> a");
    ("let f a b [Int] c [Int] d = a in f", "a -> Int -> Int -> b -> a");
    ("let f a b [Int] c d [Int] = a in f", "a -> Int -> b -> Int -> a");
    ("let f a b [Int] c d = a in f", "a -> Int -> b -> c -> a");
    ("let f a b c [Int] d [Int] = b in f", "a -> b -> Int -> Int -> b");
    ("let f a b c [Int] d = b in f", "a -> b -> Int -> c -> b");
    ("let f a b c d [Str] = c in f", "a -> b -> c -> Str -> c");
    ("let f a b c d = c in f", "a -> b -> c -> d -> c");
    ("let f a b c d [Str] = d in f", "a -> b -> c -> Str -> Str");
    ("\\ (a, _) -> a", "(a, b) -> a");
    ("\\ (a, _) -> a + 1", "(Int, a) -> Int");
    ("\\ f -> \\ x -> f x", "(a -> b) -> a -> b");
    ( {|\ f [a -> b -> c] ->
    \ a [a] ->
    \ b [b] ->
    f a b|},
      "(a -> b -> c) -> a -> b -> c" );
    ("\\ a [(a, b)] -> a", "(a, b) -> (a, b)");
    ("\\ (a, _) [(a, b)] -> a", "(a, b) -> a");
    ("\\ (_, a) [(a, b)] -> a", "(a, b) -> b");
    ("\\ (a, b, c) [(a, b, c)] -> a", "(a, b, c) -> a");
    (* higher order function *)
    ( {|\ f [(a, b) -> c] ->
    \ a [a] ->
    \ b [b] ->
    f (a, b)|},
      "((a, b) -> c) -> a -> b -> c" );
    ( {|\ f [a -> b -> c] ->
    \ (a, b) ->
    f a b|},
      "(a -> b -> c) -> (a, b) -> c" );
    (* long function with 10 arguments and return the first *)
    ( "let f a b c d e f g h i j = a in f",
      "a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> a" );
    (* long function with 20 arguments *)
    ( "let f a b c d e f g h i j k l m n o p q r s t = a in f",
      "a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n -> o \
       -> p -> q -> r -> s -> t -> a" );
    (* long function with 30 arguments. name the arguments the word of the
       number *)
    ( "let f one two three four five six seven eight nine ten eleven twelve \
       thirteen fourteen fifteen sixteen seventeen eighteen nineteen twenty \
       twentyone twentytwo twentythree twentyfour twentyfive twentysix \
       twentyseven twentyeight twentynine thirty = one in f",
      "a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n -> o \
       -> p -> q -> r -> s -> t -> u -> v -> w -> x -> y -> z -> a1 -> b1 -> \
       c1 -> d1 -> a" );
    (* long function with 40 arguments. name the arguments the word of the
       number *)
    ( "let f one two three four five six seven eight nine ten eleven twelve \
       thirteen fourteen fifteen sixteen seventeen eighteen nineteen twenty \
       twentyone twentytwo twentythree twentyfour twentyfive twentysix \
       twentyseven twentyeight twentynine thirty thirtyone thirtytwo \
       thirtythree thirtyfour thirtyfive thirtysix thirtyseven thirtyeight \
       thirtynine forty = one in f",
      {|a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n -> o -> p -> q -> r -> s -> t -> u -> v -> w -> x -> y -> z -> a1 -> b1 -> c1 -> d1 -> e1 -> f1 -> g1 -> h1 -> i1 -> j1 -> k1 -> l1 -> m1 -> n1 -> a|}
    );
    (* long function with 50 arguments. name the arguments the word of the
       number *)
    ( "let f one two three four five six seven eight nine ten eleven twelve \
       thirteen fourteen fifteen sixteen seventeen eighteen nineteen twenty \
       twentyone twentytwo twentythree twentyfour twentyfive twentysix \
       twentyseven twentyeight twentynine thirty thirtyone thirtytwo \
       thirtythree thirtyfour thirtyfive thirtysix thirtyseven thirtyeight \
       thirtynine forty fortyone fortytwo fortythree fortyfour fortyfive \
       fortysix fortyseven fortyeight fortynine fifty = one in f",
      {|a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n -> o -> p -> q -> r -> s -> t -> u -> v -> w -> x -> y -> z -> a1 -> b1 -> c1 -> d1 -> e1 -> f1 -> g1 -> h1 -> i1 -> j1 -> k1 -> l1 -> m1 -> n1 -> o1 -> p1 -> q1 -> r1 -> s1 -> t1 -> u1 -> v1 -> w1 -> x1 -> a|}
    );
    (* long function with 60 arguments. name the arguments the word of the
       number *)
    ( {|let f one two three four five six seven eight nine ten eleven twelve
      thirteen fourteen fifteen sixteen seventeen eighteen nineteen twenty
      twentyone twentytwo twentythree twentyfour twentyfive twentysix
      twentyseven twentyeight twentynine thirty thirtyone thirtytwo
      thirtythree thirtyfour thirtyfive thirtysix thirtyseven thirtyeight
      thirtynine forty fortyone fortytwo fortythree fortyfour fortyfive
      fortysix fortyseven fortyeight fortynine fifty fiftyone fiftytwo fiftythree fiftyfour fiftyfive
      fiftysix fiftyseven fiftyeight fiftynine sixty = one in f|},
      {|a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n -> o -> p -> q -> r -> s -> t -> u -> v -> w -> x -> y -> z -> a1 -> b1 -> c1 -> d1 -> e1 -> f1 -> g1 -> h1 -> i1 -> j1 -> k1 -> l1 -> m1 -> n1 -> o1 -> p1 -> q1 -> r1 -> s1 -> t1 -> u1 -> v1 -> w1 -> x1 -> y1 -> `2 -> a2 -> b2 -> c2 -> d2 -> e2 -> f2 -> g2 -> h2 -> a|}
    );
    (* recursive functions *)
    ("let rec f x = x in f", "a -> a");
    ("let rec f x [Unit] = x in f", "Unit -> Unit");
    ("let rec f x [Int -> Int] = x in f", "(Int -> Int) -> Int -> Int");
    ("\\ a [[Int]] -> a", "[Int] -> [Int]");
    ("let rec f x = if x == 0 then 0 else f (x - 1) in f", "Int -> Int");
    (* factorial *)
    ("let rec f x = if x == 0 then 1 else x * f (x - 1) in f", "Int -> Int");
    (* fibonacci *)
    ( "let rec f x = if x == 0 then 0 else if x == 1 then 1 else f (x - 1) + f \
       (x - 2) in f",
      "Int -> Int" );
    (* sum of first n numbers *)
    ("let rec f x = if x == 0 then 0 else x + f (x - 1) in f", "Int -> Int");
    (* sum of first n odd numbers *)
    ( "let rec f x = if x == 0 then 0 else if x == 1 then 1 else 2 * x - 1 + f \
       (x - 1) in f",
      "Int -> Int" );
    (* sum of first n even numbers *)
    ( "let rec f x = if x == 0 then 0 else if x == 1 then 2 else 2 * x + f (x \
       - 1) in f",
      "Int -> Int" );
    (* sum of first n squares *)
    ("let rec f x = if x == 0 then 0 else x * x + f (x - 1) in f", "Int -> Int");
    (* sum of first n cubes *)
    ( "let rec f x = if x == 0 then 0 else x * x * x + f (x - 1) in f",
      "Int -> Int" );
    (* sum of first n fourth powers *)
    ( "let rec f x = if x == 0 then 0 else x * x * x * x + f (x - 1) in f",
      "Int -> Int" );
    (* sum of first n fifth powers *)
    ( "let rec f x = if x == 0 then 0 else x * x * x * x * x + f (x - 1) in f",
      "Int -> Int" );
    (* recursive function with Boolean inputs *)
    ("let rec f x = if x then 1 else 0 in f", "Bool -> Int");
    ("let rec f x = if x then 1 else f (not x) in f", "Bool -> Int");
    (* big recursive function like fibonacci but with third order recurrence
       relation *)
    ( "let rec f x = if x == 0 then 0 else if x == 1 then 1 else if x == 2 \
       then 2 else f (x - 1) + f (x - 2) + f (x - 3) in f",
      "Int -> Int" );
    (* big recursive function like fibonacci but with fourth order recurrence
       relation *)
    ( "let rec f x = if x == 0 then 0 else if x == 1 then 1 else if x == 2 \
       then 2 else if x == 3 then 3 else f (x - 1) + f (x - 2) + f (x - 3) + f \
       (x - 4) in f",
      "Int -> Int" );
    (* big recursive function like fibonacci but with fifth order recurrence
       relation *)
    ( "let rec f x = if x == 0 then 0 else if x == 1 then 1 else if x == 2 \
       then 2 else if x == 3 then 3 else if x == 4 then 4 else f (x - 1) + f \
       (x - 2) + f (x - 3) + f (x - 4) + f (x - 5) in f",
      "Int -> Int" );
    (* map implemented using fold_right *)
    ( "let rec fold op lst acc =\n\
      \    switch lst =>\n\
      \    | [] -> acc\n\
      \    | h :: t -> op h (fold op t acc)\n\
      \    end\n\
      \  in\n\
      \  \n\
      \  let rec map f lst =\n\
      \    fold (\\ x -> \\ acc -> f x :: acc) lst []\n\
      \  \n\
      \  in\n\
      \  \n\
      \  map",
      "(a -> b) -> [a] -> [b]" );
    (* filter implemented using fold_right *)
    ( {|let rec fold op lst acc =
    switch lst =>
    | [] -> acc
    | h :: t -> op h (fold op t acc)
    end
  in
  
  let filter pred = fold (\ x -> \ acc -> if pred x then x :: acc else acc) []
  
  in filter|},
      "(a -> Bool) -> [a] -> [a]" );
    (* filter implemented using fold_left *)
    ( {|let rec fold op acc lst =
    switch lst =>
    | [] -> acc
    | h :: t -> fold op t (op h acc)
    end
  in
  
  let filter pred = fold (\ x -> \ acc -> if pred x then x :: acc else acc) []
  
  in filter|},
      "(a -> Bool) -> [a] -> [a]" );
  ]

let pair_type_tests =
  [
    ("(1, 1)", "(Int, Int)");
    ("(1, true)", "(Int, Bool)");
    ("(1, 1 + 1)", "(Int, Int)");
    ("(1 - 1, 1 + 1 + 1)", "(Int, Int)");
    ("(1 - 1, 1 + 1 + 1 + 1)", "(Int, Int)");
    (* nested *)
    ("(1 - 1, (1 + 1, 1 + 1 + 1))", "(Int, (Int, Int))");
    ("(1 - 1, (1 + 1, (1 + 1 + 1, 1 + 1 + 1 + 1)))", "(Int, (Int, (Int, Int)))");
  ]

let function_to_string_tests =
  [
    ("\\ a -> a", "function");
    ("\\ () -> ()", "function");
    ("\\ () [Unit] -> ()", "function");
    ("let a [(Int -> Int) -> Int] = \\ f -> f 1 in a", "function");
  ]

let vector_type_tests =
  [
    ("(1, 2, 3)", "(Int, Int, Int)");
    ("(1, 2, 3, 4)", "(Int, Int, Int, Int)");
    ("(1, 2, 3, 4, 5)", "(Int, Int, Int, Int, Int)");
    ("(1, 2, 3, 4, 5, 6)", "(Int, Int, Int, Int, Int, Int)");
    ("(1, 2, 3, 4, 5, 6, 7)", "(Int, Int, Int, Int, Int, Int, Int)");
    (* with other types *)
    ("(1, 2, true)", "(Int, Int, Bool)");
    ("(1, 2, true, false)", "(Int, Int, Bool, Bool)");
    ("(1, 2, true, false, 1 + 1)", "(Int, Int, Bool, Bool, Int)");
    ("(1, 2, true, false, 1 + 1, 1 + 1 + 1)", "(Int, Int, Bool, Bool, Int, Int)");
    (* very complicated nested vector *)
    ( "(1, 2, true, false, 1 + 1, 1 + 1 + 1, (1, 2, true, false, 1 + 1, 1 + 1 \
       + 1))",
      "(Int, Int, Bool, Bool, Int, Int, (Int, Int, Bool, Bool, Int, Int))" );
  ]

let list_type_tests =
  [
    ("[]", "[a]");
    ("1 :: []", "[Int]");
    ("1 :: 2 :: []", "[Int]");
    ("1 :: 2 :: 3 :: []", "[Int]");
    ("1 :: 2 :: 3 :: 4 :: []", "[Int]");
    ("(1, 2) :: []", "[(Int, Int)]");
    ("(1, 2) :: (3, 4) :: []", "[(Int, Int)]");
    (* with other types *)
    ("true :: []", "[Bool]");
    (* nested list *)
    ("(1 :: []) :: []", "[[Int]]");
    ("(1 :: 2 :: []) :: []", "[[Int]]");
    ("[] :: []", "[[a]]");
    ("[] :: [] :: []", "[[a]]");
    ("([] :: []) :: []", "[[[a]]]");
    ("(([] :: []) :: []) :: []", "[[[[a]]]]");
    ("[1 ... 10000]", "[Int]");
    ("[1 ... 10000000]", "[Int]");
    ("[1 ... 0]", "[Int]");
    ("[x * x | x <- [1, 2, 3, 4, 5]]", "[Int]");
    ( {|[(x, y, z) | x <- [1, 2, 3], y <- ["hello", "world"], z <- [true, false]]|},
      "[(Int, Str, Bool)]" );
    ({|
      [x | x <- [1, 2, 3, 4, 5], x <- [true, false]]
      |}, "[Bool]");
    ({|[x | x <- [1, 2, 3, 4, 5], x <- []]|}, "[a]");
  ]

let polymorphism_tests =
  [
    ("let f x = x in f f", "a -> a");
    ("let f x = x in f f f", "a -> a");
    ("let f x = x in f f f f", "a -> a");
    ("let f x = x in f f f f f", "a -> a");
    ("let f x = x in f 1 < 5 || f true", "Bool");
    ("let f x = x in let g = f in g g", "a -> a");
    ("let f x = x in let g = f in g f", "a -> a");
    ("let f x = x in let g = f in f g f g", "a -> a");
    ("let f x = x in let a = f 1 in f true", "Bool");
    ("(\\ f -> f 1 < 5 || f true) (\\ x -> x)", "Bool");
    ("(\\ f -> (f 0 1) < 5 || (f true 0)) (\\ x -> \\ y -> x)", "Bool");
    ("(\\ f -> (f 0 1) < 5 || (f true false)) (\\ x -> \\ y -> y)", "Bool");
    ( {|
    let f x = x in
    let g = f in
    let h = g in
    h h
  |},
      "a -> a" );
    ( {|
    let f x = x in
    let g = f in
    let h = g in
    let i = h in
    (f f f f g g g g g g h h h h h h h i i i i i i f f f f f f f g g g g g g h h h h h i i i i i f f f f f f f f f f f f f f f f f f f g g g g g g g) 1 < 2 || (f g h f) true
  |},
      "Bool" );
    ( {|
    (\ f -> 
      (\ g -> 
        g 1 < 5 || g true) 
      f) (\ x -> x)
  |},
      "Bool" );
    ( {|
    (\ x -> \ y -> \ z -> x y || x 1 < 2) (\ x -> x) true ()
  |},
      "Bool" );
    ( {|
    (\ x -> \ y -> \ z -> x y || x 1 < 2) (\ x -> x) true
  |},
      "a -> Bool" );
  ]

let switch_type_tests =
  [
    ("switch () => | () -> 1 end", "Int");
    ("switch () => | () -> true end", "Bool");
    ("switch () => | () -> () end", "Unit");
    ("switch () => | () -> (1, 2) end", "(Int, Int)");
    ("switch () => | () -> (1, 2, 3) end", "(Int, Int, Int)");
    ("switch 1 => | 1 -> 1 end", "Int");
    ("switch 1 => | 1 -> true end", "Bool");
    ("switch 1 => | 1 -> () end", "Unit");
    ("switch 5 => | 1 -> 1 | 2 -> 2 | 3 -> 3 | 4 -> 4 | 5 -> 5 end", "Int");
  ]

let arithmetic_tests =
  [
    ("1 + 2", "3");
    ("1 + 2 + 3 + 4", "10");
    ("1 + 2 * 3 + 4", "11");
    ("100 / 30 + 5", "8");
    ("10 % 4", "2");
    ("5 + 2 * 3", "11");
    ("2 + 3 * 4", "14");
    ("1 + 2 + 3 + 4 + 5", "15");
    ("10 - 2 * 3", "4");
    ("8 - 2 - 3 - 4 - 5", "-6");
    ("6 * 2 + 3", "15");
    ("3 * 4 * 5", "60");
    ("15 / 3 - 2", "3");
    ("20 / 4 / 5", "1");
    ("10 * 2 / 4", "5");
    ("15 - 3 + 2", "14");
    ("2 + 4 * 6 - 8", "18");
    ("20 / 5 * 2 + 3", "11");
    ("7 - 3 * 2 / 4", "6");
    ("9 + 3 * 2 - 4 / 2", "13");
    ("5 * (3 + 2)", "25");
    ("12 / (4 - 2)", "6");
    ("3 + 4 * 2 / (1 - 5)", "1");
    ("(5 + 2) * 3 - 4", "17");
    ("2 * (10 - 8) + 1", "5");
    ("100 / 10 % 3", "1");
    ("100 % 30 / 2 * 3", "15");
    ("100 % 31 / 2", "3");
    ("100 % 31 / 2 * 3", "9");
    ("100 % 31 / 2 * 3 + 1", "10");
    ("100 % 31 / 2 * 3 + 1 - 1", "9");
    (* very complicated test *)
    ("1 + 2 * 3 + 4 * 5 + 6 * 7 + 8 * 9 + 10", "151");
    ("1 - 1 - 1", "-1");
    ("~-1 - 1 - 2", "-4");
    ("10 - 20 - 30 - 40", "-80");
    ("1 + 5 - 4 - 3", "-1");
    ("10 - 5 + 5", "10");
    ("~-10 - 5 + 5 - 5 - 5", "-20");
    ("1 - 2 - 3 - 4 - 5", "-13");
    ("1 - 1 - 1 * 5 * 100", "-500");
  ]

let boolean_tests =
  [
    ("true", "true");
    ("false", "false");
    ("true || true", "true");
    ("true || false", "true");
    ("false || true", "true");
    ("false || false", "false");
    ("true && true", "true");
    ("true && false", "false");
    ("false && true", "false");
    ("false && false", "false");
    (* more complicated ones *)
    ("true && true || false", "true");
    ("true || false || false || false || (true || false && true)", "true");
    ("true && false || false || false || (true || false && true)", "true");
    ("true && false || false || false || (false || false && true)", "false");
    ("true && false || false || false || (false || false && true)", "false");
    ("not true", "false");
    ("not false", "true");
    ("not (not true )", "true");
    ("not (not false)", "false");
    ("not true || false", "false");
    ("not true || true", "true");
    ("true && not true", "false");
    ("true && not false", "true");
    (* some relations *)
    ("1 < 2", "true");
    ("1 > 2", "false");
    ("1 <= 2", "true");
    ("1 <= 1", "true");
    ("1 >= 1", "true");
    ("2 >= 1", "true");
    ("1 < 1", "false");
    ("1 < 2 && 13414 < 11413413", "true");
    ("1 < 2 && 13414 > 11413413", "false");
    ("1 < 2 || false", "true");
    ("1 < 2 && false", "false");
    (* equality *)
    ("1 == 1", "true");
    ("1 <> 1", "false");
    (* big tests with just + and - *)
    ("1 + 2 - 3 + 4 - 5 + 6", "5");
    ("1 + 2 + 3 - 4 - 5 - 6", "-9");
  ]

let ternary_tests =
  [
    ("if true then 0 else 1", "0");
    ("if false then 0 else 1", "1");
    ("if 1 < 2 then 0 else 1", "0");
    ("if 1 > 2 then 0 else 1", "1");
    ("if not true then 0 else 1", "1");
    ("if not false then 0 else 1", "0");
    ("if not false || not true then 0 else 1", "0");
  ]

let switch_tests =
  [
    ("switch () => | () -> 1 end", "1");
    ("switch () => | () -> true end", "true");
    ("switch () => | () -> () end", "()");
    ("switch () => | () -> (1, 2) end", "(1, 2)");
    ("switch () => | () -> (1, 2, 3) end", "(1, 2, 3)");
    ("switch 1 => | 1 -> 1 end", "1");
    ("switch 1 => | 1 -> true end", "true");
  ]

let minus_tests =
  [
    ("1 - 1 - 1", "-1");
    ("1 - 1 - 2", "-2");
    ("10 - 20 - 30 - 40", "-80");
    ("1 + 5 - 4 - 3", "-1");
    ("10 - 5 + 5", "10");
    ("~-10 - 5 + 5 - 5 - 5", "-20");
    ("1 - 2 - 3 - 4 - 5", "-13");
    ("1 - 2 - 3 - 4 - 5 - 6", "-19");
    ("1 + 2 - 3 + 4 - 5 + 6", "5");
    ("1 + 2 + 3 - 4 - 5 - 6", "-9");
  ]

let mult_div_mod_tests =
  [
    ("1 * 2 * 3 * 4", "24");
    ("10 / 2 * 3", "15");
    ("10 / 3 * 4", "12");
    ("100 / 2 / 5", "10");
    ("500 / 100 / 2", "2");
    ("100 / 2 % 49", "1");
    ("2 * 5 % 4", "2");
    ("11 % 3 % 1", "0");
  ]

let list_tests =
  [
    ("[]", "[]");
    ("1 :: []", "[1]");
    ("1 :: 2 :: []", "[1, 2]");
    ("1 :: 2 :: 3 :: []", "[1, 2, 3]");
    ("1 :: 2 :: 3 :: 4 :: []", "[1, 2, 3, 4]");
    (* with other types *)
    ("true :: []", "[true]");
    (* nested list *)
    ("(1 :: []) :: []", "[[1]]");
    ("(1 :: 2 :: []) :: []", "[[1, 2]]");
    (* do some operations in the list *)
    ("(1 + 2) :: []", "[3]");
    ("(1 + 2) :: (3 + 4) :: []", "[3, 7]");
    ("(1, 2) :: []", "[(1, 2)]");
    ( {|
  let (a, b) = (1, 2) in
  let res = a + b in
  a :: b :: res :: []
  |},
      "[1, 2, 3]" );
    ({|[1,2,3,4,5]|}, "[1, 2, 3, 4, 5]");
    ({|let x = 1 in let y = 2 in [x,y]|}, "[1, 2]");
    ( {|
  let rec fold_right op lst acc =
    switch lst =>
    | [] -> acc
    | h :: t -> op h (fold_right op t acc)
  end
in

fold_right (\x -> \y -> x + y) [1,2,3,4,5,6,7,8,9,10] 0
  |},
      "55" );
    ({|
  [1 ... 10]
  |}, "[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]");
    ({|[15 ... 15]|}, "[15]");
    ({|[1 ... 1]|}, "[1]");
    ( {|
    [(x, y) | x <- [1, 2, 3], y <- [4, 5, 6]]
    |},
      "[(1, 4), (1, 5), (1, 6), (2, 4), (2, 5), (2, 6), (3, 4), (3, 5), (3, 6)]"
    );
    ({|[x + y | x <- [1, 2, 3], y <- [4, 5, 6]]|}, "[5, 6, 7, 6, 7, 8, 7, 8, 9]");
    ( {|
    let rec fold_right op lst acc =
      switch lst =>
      | [] -> acc
      | h :: t -> op h (fold_right op t acc)
    end
    in

    fold_right (\x -> \y -> x + y) [1 ... 10] 0
    
    |},
      "55" );
  ]

let fold_type_tests =
  [
    ( {|
    let rec fold op arr acc =
      switch arr =>
      | [] -> acc
      | h :: t -> fold op t (op acc h)
      end
    in
    fold
  |},
      "(a -> b -> a) -> [b] -> a -> a" );
    ( {|
  let rec fold op arr acc =
    switch arr =>
    | [] -> acc
    | h :: t -> op h (fold op t acc)
    end
  in
  fold
  |},
      "(a -> b -> b) -> [a] -> b -> b" );
  ]

let complex_tests =
  [
    ( {|
    let succ [Int -> Int] =
      \ n [Int] -> n + 1
    in
    
    let sum [Int -> Int -> Int] =
      \ a [Int] ->
      \ b [Int] ->
      a + b
    in
    
    sum (succ 4) (sum 1 2)
    |},
      "8" );
    ( {|
    let succ [Int-> Int] =
      \ n [Int] -> n + 1
    in

    succ(succ (succ (succ (succ (succ (succ (succ (succ (0)))))))))
    |},
      "9" );
    ({|
    let a = 1 in
    let a = a in
    a
    |}, "1");
    ({|
    let f =
      \ a [str] -> a
    in
    f ""
    |}, {|""|});
    ({|
    let f =
      \ () -> ()
    in
    f ()
    |}, {|()|});
    (* add some tests involving functions *)
    ("let f a b c d = a + b + c + d in f 1 1 1 1", "4");
    ("let f a b c d = a - b + c + d in f 1 2 1 1", "1");
    ("let f a b c d = a in f 1 () 100 100000", "1");
    ("let f a b c d = a in f (f 5 () () ()) () 100 100000", "5");
    (* function that uses ternary statement *)
    ("let f a b c d = if a < b then c else d in f 1 2 3 4", "3");
    (* function that applies one function twice to another function *)
    ( {|
    let succ n = n + 1 in
    let apply_twice f x = f (f x) in
    apply_twice succ 2
    |},
      "4" );
    (* make a similar test *)
    ( {|
    let square n = n * n in
    let apply_twice f x = f (f x) in
    apply_twice square 3
    |},
      "81" );
    (* complicated function tests *)
    (* function that takes a function and applies it to 1 *)
    ({|
    let apply_one f = f 1 in
    apply_one (\ n -> n + 1)
    |}, "2");
    ( {|
    let f a b c d e f g h i j k l m n o p q r s t = a in
    f 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 true false 1 2
    |},
      "1" );
    ( {|
    let rec f x = if x == 0 then 1 else x * f (x - 1) in
    f 5
    |},
      "120" );
    (* fibonacci *)
    ( {|
    let rec f x = if x == 0 then 0 else if x == 1 then 1 else f (x - 1) + f (x - 2) in
    f 10
    |},
      "55" );
    ( {|
    let rec f x = if x == 0 then 0 else if x == 1 then 1 else f (x - 1) + f (x - 2) in
    f 20
    |},
      "6765" );
    ( {|
    let rec f x = if x == 1 then 1 else if x == 2 then 1 else f (x - 1) + f (x - 2) in
    f 1
    |},
      "1" );
    ( {|
    let rec f x = if x == 1 then 1 else if x == 2 then 1 else f (x - 1) + f (x - 2) in
    f 2
    |},
      "1" );
    ( {|
    let rec f x = if x == 1 then 1 else if x == 2 then 1 else f (x - 1) + f (x - 2) in
    f 3
    |},
      "2" );
    ( {|

    let rec f x = if x == 1 then 1 else if x == 2 then 1 else f (x - 1) + f (x - 2) in
    f 4
    |},
      "3" );
    ( {|
    let rec f x = if x == 1 then 1 else if x == 2 then 1 else f (x - 1) + f (x - 2) in
    f 5
    |},
      "5" );
    ( {|
    let rec f x = if x == 1 then 1 else if x == 2 then 1 else f (x - 1) + f (x - 2) in
    f 6
    |},
      "8" );
    ( {|
    let rec f x = if x == 1 then 1 else if x == 2 then 1 else f (x - 1) + f (x - 2) in
    f 7
    |},
      "13" );
    ( {|
    let rec f x = if x == 1 then 1 else if x == 2 then 1 else f (x - 1) + f (x - 2) in
    f 8
    |},
      "21" );
    (* sum *)
    ( {|
    let rec f x = if x == 0 then 0 else x + f (x - 1) in
    f 10
    |},
      "55" );
    (* sum of first n odd numbers *)
    ( {|
    let rec f x = if x == 0 then 0 else if x == 1 then 1 else 2 * x - 1 + f (x - 1) in
    f 10
    |},
      "100" );
    (* sum of first n even numbers *)
    ({|
    let rec f x = if x then 1 else f (not x) in
    f true

    |}, "1");
    ({|
    let rec f x = if x then 1 else f (not x) in
    f false
    |}, "1");
    ( {|
    let rec f x = if x == 0 then true else false || f (x - 1) in
    f 100
    |},
      "true" );
    ( {|
    let rec f x = if x == 0 then false else true && f (x - 1) in
    f 100
    |},
      "false" );
  ]

let defn_expr_test_data : (string * string * string) list =
  [
    ("let x = 1", "x", "1");
    ("let x = 2", "x", "2");
    ({|let x = 1
  let y = 2
  let z = 3
  let w = 4
  |}, "x + y + z + w", "10");
    ({|
  let f x = x + 1
  |}, "f (f 2)", "4");
  ]

let defn_type_test_data : (string * string * string) list =
  [
    ("let x = 1", "x", "Int");
    ("let x = 2", "x", "Int");
    ({|
  let f x = [x]
  let y = true
  
  |}, "f y", "[Bool]");
    ( {|
  type T =
    | A
    | B
  let swap x =
    switch x =>
    | A -> B
    | B -> A
  end
  
  |},
      "swap",
      "T -> T" );
    ( {|
  type List a =
    | Nil
    | Cons (a, List a)
  
  |},
      "Nil",
      "List a" );
    ( {|
  type List a =
    | Nil
    | Cons (a, List a)
  |},
      "Cons",
      "(a, List a) -> List a" );
    ( {|
  type List a =
    | Nil
    | Cons (a, List a)

  let rec length lst =
    switch lst =>
    | Nil -> 0
    | Cons (h, t) -> 1 + length t
    end
  
  
  |},
      "length",
      "List a -> Int" );
    ( {|
    type List a =
      | Nil
      | Cons (a, List a)

    let rec fold_left op acc lst =
      switch lst =>
      | Nil -> acc
      | Cons (h, t) -> fold_left op (op acc h) t
    end
    |},
      "fold_left",
      "(a -> b -> a) -> a -> List b -> a" );
    ( {|
      type List a =
        | Nil
        | Cons (a, List a)

      let rec fold_right op lst acc =
        switch lst =>
        | Nil -> acc
        | Cons (h, t) -> op h (fold_right op t acc)
      end
    |},
      "fold_right",
      "(a -> b -> b) -> List a -> b -> b" );
  ]

let kind_of_type_tests_data : (string * string * string) list =
  [
    ( {|
        type List a =
          | Nil
          | Cons (a, List a)
      |},
      "List",
      "* -> *" );
    ( {|
    type Either a b =
      | Left (a)
      | Right (b)
    |},
      "Either",
      "* -> * -> *" );
    ({|
      type App a b = a b
    |}, "App", "(A -> B) -> A -> B");
    ( {|
      type App a b c d = a b c d
    |},
      "App",
      "(A -> B -> C -> D) -> A -> B -> C -> D" );
    ( {|
    type T a b c d = a ( b c ) d
    |},
      "T",
      "(A -> B -> C) -> (D -> A) -> D -> B -> C" );
    ({|
      type T a = a
    |}, "T", "A -> A");
    ({|
      type T a b = a
    |}, "T", "A -> B -> A");
    ({|
    type T a b = b
    |}, "T", "A -> B -> B");
    ( {|
    type T a b c d e f g = e
    
    |},
      "T",
      "A -> B -> C -> D -> E -> F -> G -> E" );
    ( {|
    type Id a = a
    type App a b = a b
    type T = App Id

    |},
      "T",
      "A -> A" );
    ( {|
    type Id a = a
    type App a b = a b
    type T = App Id Int

    |},
      "T",
      "*" );
    ( {|
      type T a b c = a ( b c )
    
    |},
      "T",
      "(A -> B) -> (C -> A) -> C -> B" );
    ( {|
      type App a b c = a b c
      type T a b = App a b Int
    |},
      "T",
      "(A -> * -> B) -> A -> B" );
    ({|
    type T a b = (a, b)
    |}, "T", "* -> * -> *");
    ({|
      type T a b = (a, a)
    |}, "T", "* -> A -> *");
    ({|
      type T a b c d = Bool
    |}, "T", "A -> B -> C -> D -> *");
    ( {|
      type T a b c d e = (a b, c d, e)
    |},
      "T",
      "(A -> *) -> A -> (B -> *) -> B -> * -> *" );
    ( {|
      type Id a = a
      type IntType = Id (Id (Id (Id (Id (Int)))))
    |},
      "IntType",
      "*" );
    ( {|
      type List a =
        | Nil
        | Cons (a, List a)

      type ListInt = List Int
    |},
      "ListInt",
      "*" );
    ( {|
      type List a =
        | Nil
        | Cons (a, List a)
      type Id a = a
      type ListInt = Id (Id (List (Id (Id Int))))
    |},
      "ListInt",
      "*" );
    ( {|
      type Either a b = | Left (a) | Right (b)
      type EitherLeftInt a = Either Int a
      type EitherLeftIntRightBool = EitherLeftInt Bool
    |},
      "EitherLeftIntRightBool",
      "*" );
    ( {|
      type List a =
        | Nil
        | Cons (a, List a)
      type Either a b = | Left (a) | Right (b)
      type T = List (Either Int Bool)
    |},
      "T",
      "*" );
    ( {|
      type List a =
        | Nil
        | Cons (a, List a)
      type Either a b = | Left (a) | Right (b)
      type T = Either (List (Either (List Int) (List Bool)))
    |},
      "T",
      "* -> *" );
    ( {|
      type List a =
        | Nil
        | Cons (a, List a)
      type Either a b = | Left (a) | Right (b)
      type T = Either (List (Either (List Int) (List Bool))) (List (Either Int Bool))
    |},
      "T",
      "*" );
    ( {|
      type List a =
        | Nil
        | Cons (a, List a)

      type List2 = List
    |},
      "List2",
      "* -> *" );
    ( {|
      type List a =
        | Nil
        | Cons (a, List a)

      type List2 = List
      type ListInt = List2 Int
    |},
      "ListInt",
      "*" );
    ( {|
      type SL a b =
        | Nil
        | Cons (a b, SL a b)
    
    |},
      "SL",
      "(A -> *) -> A -> *" );
    ( {|
      type SL a b c d =
        | Nil
        | Cons (a b, SL a b c d)
    
    |},
      "SL",
      "(A -> *) -> A -> B -> C -> *" );
    ( {|
      type SL a b c d =
        | Nil
        | Cons (a b c d, SL a b c d)
    
    |},
      "SL",
      "(A -> B -> C -> *) -> A -> B -> C -> *" );
    ( {|
      type BT a =
        | Leaf
        | Node (a, BT a, BT a)
    
    |},
      "BT",
      "* -> *" );
    ( {|
      type BT a =
        | Leaf
        | Node (a, BT a, BT a)

      type BTInt = BT Int
    
    |},
      "BTInt",
      "*" );
    ( {|

      type List a =
        | Nil
        | Cons (a, List a)

      type BT a =
        | Leaf
        | Node (a, BT a, BT a)

      type T a = List (BT a)
    
    |},
      "T",
      "* -> *" );
    ( {|
    type T a b c d e f g = a b (c d e) (f g)
    |},
      "T",
      "(A -> B -> C -> D) -> A -> (E -> F -> B) -> E -> F -> (G -> C) -> G -> D"
    );
    ( {|
      type Option a = | None | Some (a)
      type EffectOption = Option (Unit -> Unit)
    |},
      "EffectOption",
      "*" );
    ({|
        type Function a b = a -> b
    |}, "Function", "* -> * -> *");
    ({|
      type List a = [a]
    |}, "List", "* -> *");
    ({|
      type List a b = [(a, b)]
    |}, "List", "* -> * -> *");
    ({|
      type List a b = [(a -> b, b -> a)]
    |}, "List", "* -> * -> *");
    ( {|
      type List a b c d =
        (a -> b -> c -> d, d -> c -> b -> a, a -> b -> c -> d, d -> c -> b -> a)
    |},
      "List",
      "* -> * -> * -> * -> *" );
    ({|
      type List a b = a (b -> b)
    |}, "List", "(* -> A) -> * -> A");
    ( {|
    type T a b c d e = a b b b c c d d d d
  |},
      "T",
      "(A -> A -> A -> B -> B -> C -> C -> C -> C -> D) -> A -> B -> C -> E -> \
       D" );
  ]

let defn_expr_tests =
  List.map (fun (a, b, c) -> defn_expr_test a b c) defn_expr_test_data

let defn_type_tests =
  List.map (fun (a, b, c) -> defn_type_test a b c) defn_type_test_data

let kind_of_type_tests =
  List.map (fun (a, b, c) -> kind_of_type_test a b c) kind_of_type_tests_data

let int_type_tests : test list =
  List.map
    (fun expression -> type_is_int expression)
    (int_types |> TypeTestModifier.modify_tests
   |> IntTypeTestModifier.modify_tests)

let bool_type_tests : test list =
  List.map
    (fun expression -> type_is_bool expression)
    (bool_types |> TypeTestModifier.modify_tests
   |> BoolTypeTestModifier.modify_tests)

let string_type_tests : test list =
  List.map (fun expression -> type_is_string expression) string_types

let function_type_tests : test list =
  List.map
    (fun (a, b) -> type_test a b)
    (function_type_tests @ fold_type_tests
    |> FunctionTypeTestModifier.modify_tests)

let pair_type_tests : test list =
  List.map
    (fun (a, b) -> type_test a b)
    (pair_type_tests |> PairTypeTestModifier.modify_tests)

let vector_type_tests : test list =
  List.map
    (fun (a, b) -> type_test a b)
    (vector_type_tests |> VectorTypeTestModifier.modify_tests)

let list_type_tests : test list =
  List.map
    (fun (a, b) -> type_test a b)
    (list_type_tests |> ListTypeTestModifier.modify_tests)

let switch_type_tests : test list =
  List.map
    (fun (a, b) -> type_test a b)
    (switch_type_tests |> SwitchTypeTestModifier.modify_tests)

let polymorphism_tests : test list =
  List.map
    (fun (a, b) -> type_test a b)
    (polymorphism_tests |> PolymorphismTypeTestModifier.modify_tests)

let eval_test_data =
  [
    arithmetic_tests |> IntTestModifier.modify_tests;
    boolean_tests;
    complex_tests;
    minus_tests;
    mult_div_mod_tests;
    ternary_tests;
    function_to_string_tests;
    list_tests;
    switch_tests;
  ]
  |> List.flatten |> EvalTestModifier.modify_tests

let eval_tests = List.map (fun (a, b) -> eval_test a b) eval_test_data
let () = ignore kind_of_type_tests

let all_tests =
  List.flatten
    [
      eval_tests;
      int_type_tests;
      bool_type_tests;
      string_type_tests;
      function_type_tests;
      pair_type_tests;
      vector_type_tests;
      list_type_tests;
      switch_type_tests;
      polymorphism_tests;
      defn_expr_tests;
      defn_type_tests;
      kind_of_type_tests;
    ]

let suite = "suite" >::: all_tests
let () = run_test_tt_main suite
