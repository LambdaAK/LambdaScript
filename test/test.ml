open OUnit2
open Language.Ceval
open Language.Typecheck
open Language.Condense
open Language.C_to_string
open Language.Lex
open Language.Env
open Language.Parser.ExprParser

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

let eval_test (expr : string) (expected_output : string) : test =
  expr ^ " SHOULD YIELD " ^ expected_output >:: fun _ ->
  let result : string = c_eval expr |> unwrap_eval_result in
  assert_equal result expected_output

let type_test (expr : string) (expected_output : string) : test =
  expr ^ " SHOULD BE OF TYPE " ^ expected_output >:: fun _ ->
  let static_env =
    List.map
      (fun (id, code) ->
        (* get the type of the value *)
        let tokens : token list = code |> list_of_string |> lex in

        let a = expr_parser (List.map (fun t -> t.token_type) tokens) in

        match a with
        | None -> failwith "Could not parse the expression"
        | Some (e, _) ->
            let c_e : Language.Cexpr.c_expr = condense_expr e in

            let t =
              match type_of_c_expr built_ins_types [] c_e with
              | Ok t -> t
              | Error e -> failwith (string_of_type_check_error e)
            in
            (id, t))
      code_mapping
    @ built_ins_types
  in
  (* let result : string = type_of_c_expr (expr |> list_of_string |> lex |>
     parse_expr |> fst |> condense_expr) static_env |> string_of_c_type in *)

  let tokens = expr |> list_of_string |> lex in
  let token_types = List.map (fun t -> t.token_type) tokens in
  let parser_result = expr_parser token_types in
  let parsed_expr =
    match parser_result with
    | None -> failwith "Could not parse the expression"
    | Some (e, _) -> e
  in
  let condensed_expr = condense_expr parsed_expr in
  let type_result =
    match type_of_c_expr static_env [] condensed_expr with
    | Ok t -> t
    | _ -> failwith "type failureeeeeeeeee"
  in
  let type_string = string_of_c_type type_result in

  assert_equal type_string expected_output

let type_is_bool (program : string) = type_test program "bool"
let type_is_int (program : string) = type_test program "int"
let type_is_string (program : string) = type_test program "str"
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
    "1 > 2";
    "1 <= 2";
    "1 <= 1";
    "1 >= 1";
    "2 >= 1";
    "1 < 1";
    "1 < 2 && 13414 < 11413413";
    "1 < 2 && 13414 > 11413413";
    "1 < 2 && false";
    "if true then true else false";
    "1 < 2 || false";
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
    "if true && true || false then true else false";
    "if true || false || false || false || (true || false && true) then true \
     else false";
  ]

let string_types = [ {|""|}; {|"hello"|} ]

let function_type_tests =
  [
    ("\\ n -> n", "'a -> 'a");
    ("\\ n -> n + 1", "int -> int");
    ("\\ a -> \\ b -> a + b", "int -> int -> int");
    ("\\ n [int] -> n", "int -> int");
    ("\\ n [int] -> n + 1", "int -> int");
    ("\\ a [int] -> \\ b [int] -> a + b", "int -> int -> int");
    ("\\ a -> \\ b -> a", "'a -> 'b -> 'a");
    ("\\ a -> \\ b -> b", "'a -> 'b -> 'b");
    ("\\ a [int] -> \\ b -> b", "int -> 'a -> 'a");
    ("\\ a -> \\ b [int] -> b", "'a -> int -> int");
    ("\\ a [int] -> \\ b [int] -> b", "int -> int -> int");
    ("\\ a [int] -> \\ b [int] -> a", "int -> int -> int");
    ("\\ a [int] -> \\ b [int] -> a + b", "int -> int -> int");
    ("\\ a [int] -> \\ b [int] -> a + b + 1", "int -> int -> int");
    ("\\ a [int] -> \\ b [int] -> a + b + 1 + 2", "int -> int -> int");
    ("\\ (a, b) [(int, int)] -> a + b", "(int, int) -> int");
    ("\\ (a, _) -> \\ (_, b) -> a + b", "(int, 'a) -> ('b, int) -> int");
    ("\\ (a, _) -> \\ (_, b) -> a || b", "(bool, 'a) -> ('b, bool) -> bool");
    ( {|\ (a, b) ->
    \ (c, d) ->
    if a then b
    else if c then d
    else 1|},
      "(bool, int) -> (bool, int) -> int" );
    (* more complicated function type tests *)
    ( "\\ a -> \\ b -> \\ c -> a ( b ( c ) )",
      "('a -> 'b) -> ('c -> 'a) -> 'c -> 'b" );
    (* tests with syntax sugar let expressions *)
    ("let f x = x in f", "'a -> 'a");
    ("let f x = x + 1 in f", "int -> int");
    ("let f x = x + 1 in f 1", "int");
    ("let f x = x + 1 in f 1 + 1", "int");
    ("let f x = x + 1 in f (1 + 1)", "int");
    (* add three numbers *)
    ("let f a b c = a + b + c in f", "int -> int -> int -> int");
    ("let f a b c = a + b + c in f 1", "int -> int -> int");
    ("let f a b c = a + b + c in f 1 2", "int -> int");
    ("let f a b c = a + b + c in f 1 2 3", "int");
    ("let f a b c d = a in f", "'a -> 'b -> 'c -> 'd -> 'a");
    (* typed arguments *)
    ( "let f a [int] b [int] c [int] d [int] = a in f",
      "int -> int -> int -> int -> int" );
    ( "let f a [int] b [int] c [int] d [int] = a in f 1",
      "int -> int -> int -> int" );
    ("let f a [int] b [int] c [int] d [int] = a in f 1 2", "int -> int -> int");
    ("let f a [int] b [int] c [int] d [int] = a in f 1 2 3", "int -> int");
    ("let f a [int] b [int] c [int] d [int] = a in f 1 2 3 4", "int");
    (* with type variables *)
    ("\\ a ['a] -> a", "'a -> 'a");
    ("\\ a ['a] -> a + 1", "int -> int");
    ("\\ a ['a] -> \\ b ['a] -> a", "'a -> 'a -> 'a");
    ("\\ a ['a] -> \\ b ['a] -> b", "'a -> 'a -> 'a");
    ("\\ a ['a] -> \\ b ['b] -> a", "'a -> 'b -> 'a");
    ("\\ a ['a] -> \\ b ['a] -> a + b", "int -> int -> int");
    ("\\ a ['a] -> \\ b ['a] -> a + b + 1", "int -> int -> int");
    ("\\ f ['e -> 'f] -> \\ x ['f] -> f x", "('a -> 'a) -> 'a -> 'a");
    (* this is an interesting example because it turns out that a = b here *)
    ("\\ f ['e -> 'f] -> \\ x ['e] -> f x", "('a -> 'b) -> 'a -> 'b");
    (* on the other hand, there is no constraint generated in this expression
       saying that a = b, so they are different *)
    ("let f a b [int] c [int] d [int] = a in f", "'a -> int -> int -> int -> 'a");
    ("let f a b [int] c [int] d = a in f", "'a -> int -> int -> 'b -> 'a");
    ("let f a b [int] c d [int] = a in f", "'a -> int -> 'b -> int -> 'a");
    ("let f a b [int] c d = a in f", "'a -> int -> 'b -> 'c -> 'a");
    ("let f a b c [int] d [int] = b in f", "'a -> 'b -> int -> int -> 'b");
    ("let f a b c [int] d = b in f", "'a -> 'b -> int -> 'c -> 'b");
    ("let f a b c d [str] = c in f", "'a -> 'b -> 'c -> str -> 'c");
    ("let f a b c d = c in f", "'a -> 'b -> 'c -> 'd -> 'c");
    ("let f a b c d [str] = d in f", "'a -> 'b -> 'c -> str -> str");
    ("\\ (a, _) -> a", "('a, 'b) -> 'a");
    ("\\ (a, _) -> a + 1", "(int, 'a) -> int");
    ("\\ f -> \\ x -> f x", "('a -> 'b) -> 'a -> 'b");
    ( {|\ f ['e -> 'f -> 'g] ->
    \ a ['e] ->
    \ b ['f] ->
    f a b|},
      "('a -> 'b -> 'c) -> 'a -> 'b -> 'c" );
    ("\\ a [('a, 'b)] -> a", "('a, 'b) -> ('a, 'b)");
    ("\\ (a, _) [('a, 'b)] -> a", "('a, 'b) -> 'a");
    ("\\ (_, a) [('a, 'b)] -> a", "('a, 'b) -> 'b");
    ("\\ (a, b, c) [('a, 'b, 'c)] -> a", "('a, 'b, 'c) -> 'a");
    (* higher order function *)
    ( {|\ f [('e, 'f) -> 'g] ->
    \ a ['e] ->
    \ b ['f] ->
    f (a, b)|},
      "(('a, 'b) -> 'c) -> 'a -> 'b -> 'c" );
    ( {|\ f ['e -> 'f -> 'g] ->
    \ (a, b) ->
    f a b|},
      "('a -> 'b -> 'c) -> ('a, 'b) -> 'c" );
    (* long function with 10 arguments and return the first *)
    ( "let f a b c d e f g h i j = a in f",
      "'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> 'a" );
    (* long function with 20 arguments *)
    ( "let f a b c d e f g h i j k l m n o p q r s t = a in f",
      "'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> 'k -> 'l -> \
       'm -> 'n -> 'o -> 'p -> 'q -> 'r -> 's -> 't -> 'a" );
    (* long function with 30 arguments. name the arguments the word of the
       number *)
    ( "let f one two three four five six seven eight nine ten eleven twelve \
       thirteen fourteen fifteen sixteen seventeen eighteen nineteen twenty \
       twentyone twentytwo twentythree twentyfour twentyfive twentysix \
       twentyseven twentyeight twentynine thirty = one in f",
      "'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> 'k -> 'l -> \
       'm -> 'n -> 'o -> 'p -> 'q -> 'r -> 's -> 't -> 'u -> 'v -> 'w -> 'x -> \
       'y -> 'z -> 'aa -> 'ab -> 'ac -> 'ad -> 'a" );
    (* recursive functions *)
    ("let rec f x = x in f", "'a -> 'a");
    ("let rec f x [unit] = x in f", "unit -> unit");
    ("let rec f x [int -> int] = x in f", "(int -> int) -> int -> int");
    ("\\ a [[int]] -> a", "[int] -> [int]");
    ("let rec f x = if x == 0 then 0 else f (x - 1) in f", "int -> int");
    (* factorial *)
    ("let rec f x = if x == 0 then 1 else x * f (x - 1) in f", "int -> int");
    (* fibonacci *)
    ( "let rec f x = if x == 0 then 0 else if x == 1 then 1 else f (x - 1) + f \
       (x - 2) in f",
      "int -> int" );
    (* sum of first n numbers *)
    ("let rec f x = if x == 0 then 0 else x + f (x - 1) in f", "int -> int");
    (* sum of first n odd numbers *)
    ( "let rec f x = if x == 0 then 0 else if x == 1 then 1 else 2 * x - 1 + f \
       (x - 1) in f",
      "int -> int" );
    (* sum of first n even numbers *)
    ( "let rec f x = if x == 0 then 0 else if x == 1 then 2 else 2 * x + f (x \
       - 1) in f",
      "int -> int" );
    (* sum of first n squares *)
    ("let rec f x = if x == 0 then 0 else x * x + f (x - 1) in f", "int -> int");
    (* sum of first n cubes *)
    ( "let rec f x = if x == 0 then 0 else x * x * x + f (x - 1) in f",
      "int -> int" );
    (* sum of first n fourth powers *)
    ( "let rec f x = if x == 0 then 0 else x * x * x * x + f (x - 1) in f",
      "int -> int" );
    (* sum of first n fifth powers *)
    ( "let rec f x = if x == 0 then 0 else x * x * x * x * x + f (x - 1) in f",
      "int -> int" );
    (* recursive function with boolean inputs *)
    ("let rec f x = if x then 1 else 0 in f", "bool -> int");
    ("let rec f x = if x then 1 else f (not x) in f", "bool -> int");
    (* big recursive function like fibonacci but with third order recurrence
       relation *)
    ( "let rec f x = if x == 0 then 0 else if x == 1 then 1 else if x == 2 \
       then 2 else f (x - 1) + f (x - 2) + f (x - 3) in f",
      "int -> int" );
    (* big recursive function like fibonacci but with fourth order recurrence
       relation *)
    ( "let rec f x = if x == 0 then 0 else if x == 1 then 1 else if x == 2 \
       then 2 else if x == 3 then 3 else f (x - 1) + f (x - 2) + f (x - 3) + f \
       (x - 4) in f",
      "int -> int" );
    (* big recursive function like fibonacci but with fifth order recurrence
       relation *)
    ( "let rec f x = if x == 0 then 0 else if x == 1 then 1 else if x == 2 \
       then 2 else if x == 3 then 3 else if x == 4 then 4 else f (x - 1) + f \
       (x - 2) + f (x - 3) + f (x - 4) + f (x - 5) in f",
      "int -> int" );
    (* map implemented using fold_right *)
    ( "let rec fold op lst acc =\n\
      \    switch lst =>\n\
      \    | [] -> acc\n\
      \    | h :: t -> op h (fold op t acc)\n\
      \    \n\
      \  in\n\
      \  \n\
      \  let rec map f lst =\n\
      \    fold (\\ x -> \\ acc -> f x :: acc) lst []\n\
      \  \n\
      \  in\n\
      \  \n\
      \  map",
      "('a -> 'b) -> ['a] -> ['b]" );
    (* filter implemented using fold_right *)
    ( {|let rec fold op lst acc =
    switch lst =>
    | [] -> acc
    | h :: t -> op h (fold op t acc)
    
  in
  
  let filter pred = fold (\ x -> \ acc -> if pred x then x :: acc else acc) []
  
  in filter|},
      "('a -> bool) -> ['a] -> ['a]" );
    (* filter implemented using fold_left *)
    ( {|let rec fold op acc lst =
    switch lst =>
    | [] -> acc
    | h :: t -> fold op t (op h acc)
    
  in
  
  let filter pred = fold (\ x -> \ acc -> if pred x then x :: acc else acc) []
  
  in filter|},
      "('a -> bool) -> ['a] -> ['a]" );
  ]

let pair_type_tests =
  [
    ("(1, 1)", "(int, int)");
    ("(1, true)", "(int, bool)");
    ("(1, 1 + 1)", "(int, int)");
    ("(1 - 1, 1 + 1 + 1)", "(int, int)");
    ("(1 - 1, 1 + 1 + 1 + 1)", "(int, int)");
    (* nested *)
    ("(1 - 1, (1 + 1, 1 + 1 + 1))", "(int, (int, int))");
    ("(1 - 1, (1 + 1, (1 + 1 + 1, 1 + 1 + 1 + 1)))", "(int, (int, (int, int)))");
  ]

let function_to_string_tests =
  [
    ("\\ a -> a", "function");
    ("\\ () -> ()", "function");
    ("\\ () [unit] -> ()", "function");
    ("let a [(int -> int) -> int] = \\ f -> f 1 in a", "function");
  ]

let vector_type_tests =
  [
    ("(1, 2, 3)", "(int, int, int)");
    ("(1, 2, 3, 4)", "(int, int, int, int)");
    ("(1, 2, 3, 4, 5)", "(int, int, int, int, int)");
    ("(1, 2, 3, 4, 5, 6)", "(int, int, int, int, int, int)");
    ("(1, 2, 3, 4, 5, 6, 7)", "(int, int, int, int, int, int, int)");
    (* with other types *)
    ("(1, 2, true)", "(int, int, bool)");
    ("(1, 2, true, false)", "(int, int, bool, bool)");
    ("(1, 2, true, false, 1 + 1)", "(int, int, bool, bool, int)");
    ("(1, 2, true, false, 1 + 1, 1 + 1 + 1)", "(int, int, bool, bool, int, int)");
    (* very complicated nested vector *)
    ( "(1, 2, true, false, 1 + 1, 1 + 1 + 1, (1, 2, true, false, 1 + 1, 1 + 1 \
       + 1))",
      "(int, int, bool, bool, int, int, (int, int, bool, bool, int, int))" );
  ]

let list_type_tests =
  [
    ("[]", "['a]");
    ("1 :: []", "[int]");
    ("1 :: 2 :: []", "[int]");
    ("1 :: 2 :: 3 :: []", "[int]");
    ("1 :: 2 :: 3 :: 4 :: []", "[int]");
    ("(1, 2) :: []", "[(int, int)]");
    ("(1, 2) :: (3, 4) :: []", "[(int, int)]");
    (* with other types *)
    ("true :: []", "[bool]");
    (* nested list *)
    ("(1 :: []) :: []", "[[int]]");
    ("(1 :: 2 :: []) :: []", "[[int]]");
    ("[] :: []", "[['a]]");
    ("[] :: [] :: []", "[['a]]");
    ("([] :: []) :: []", "[[['a]]]");
    ("(([] :: []) :: []) :: []", "[[[['a]]]]");
    ("[1 ... 10000]", "[int]");
    ("[1 ... 10000000]", "[int]");
    ("[1 ... 0]", "[int]");
    ("[x * x | x <- [1, 2, 3, 4, 5]]", "[int]");
    ( {|[(x, y, z) | x <- [1, 2, 3], y <- ["hello", "world"], z <- [true, false]]|},
      "[(int, str, bool)]" );
    ({|
      [x | x <- [1, 2, 3, 4, 5], x <- [true, false]]
      |}, "[bool]");
    ({|[x | x <- [1, 2, 3, 4, 5], x <- []]|}, "['a]");
  ]

let polymorphism_tests =
  [
    ("let f x = x in f f", "'a -> 'a");
    ("let f x = x in f f f", "'a -> 'a");
    ("let f x = x in f f f f", "'a -> 'a");
    ("let f x = x in f f f f f", "'a -> 'a");
    ("let f x = x in f 1 < 5 || f true", "bool");
    ("let f x = x in let g = f in g g", "'a -> 'a");
    ("let f x = x in let g = f in g f", "'a -> 'a");
    ("let f x = x in let g = f in f g f g", "'a -> 'a");
    ("let f x = x in let a = f 1 in f true", "bool");
    ( {|
    let f x = x in
    let g = f in
    let h = g in
    h h
  |},
      "'a -> 'a" );
    ( {|
    let f x = x in
    let g = f in
    let h = g in
    let i = h in
    (f f f f g g g g g g h h h h h h h i i i i i i f f f f f f f g g g g g g h h h h h i i i i i f f f f f f f f f f f f f f f f f f f g g g g g g g) 1 < 2 || (f g h f) true
  |},
      "bool" );
  ]

let switch_type_tests =
  [
    ("switch () => | () -> 1", "int");
    ("switch () => | () -> true", "bool");
    ("switch () => | () -> ()", "unit");
    ("switch () => | () -> (1, 2)", "(int, int)");
    ("switch () => | () -> (1, 2, 3)", "(int, int, int)");
    ("switch 1 => | 1 -> 1", "int");
    ("switch 1 => | 1 -> true", "bool");
    ("switch 1 => | 1 -> ()", "unit");
    ("switch 5 => | 1 -> 1 | 2 -> 2 | 3 -> 3 | 4 -> 4 | 5 -> 5", "int");
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
    ("switch () => | () -> 1", "1");
    ("switch () => | () -> true", "true");
    ("switch () => | () -> ()", "()");
    ("switch () => | () -> (1, 2)", "(1, 2)");
    ("switch () => | () -> (1, 2, 3)", "(1, 2, 3)");
    ("switch 1 => | 1 -> 1", "1");
    ("switch 1 => | 1 -> true", "true");
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
    in

    fold_right (\x -> \y -> x + y) [1,2,3,4,5,6,7,8,9,10] 0
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
      
    in
    fold
  |},
      "('a -> 'b -> 'a) -> ['b] -> 'a -> 'a" );
    ( {|
  let rec fold op arr acc =
    switch arr =>
    | [] -> acc
    | h :: t -> op h (fold op t acc)
    
  in
  fold
  |},
      "('a -> 'b -> 'b) -> ['a] -> 'b -> 'b" );
  ]

let complex_tests =
  [
    ( {|
    let succ [int -> int] =
      \ n [int] -> n + 1
    in
    
    let sum [int -> int -> int] =
      \ a [int] ->
      \ b [int] ->
      a + b
    in
    
    sum (succ 4) (sum 1 2)
    |},
      "8" );
    ( {|
    let succ [int-> int] =
      \ n [int] -> n + 1
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

let block_tests =
  [
    (* Empty block *)
    ("{}", "()");
    (* Block with single expression *)
    ("{1}", "1");
    (* Block with multiple expressions *)
    ("{1; 2; 3}", "3");
    (* Block with definitions *)
    ("{let x = 1; let y = 2}", "()");
    (* Block with definitions and expressions *)
    ("{let x = 1; let y = 2; x + y}", "3");
    (* Block with nested blocks *)
    ("{{1; 2}; {3; 4}}", "4");
    (* Block with recursive definitions *)
    ("{let rec f x = if x == 0 then 1 else x * f (x - 1); f 5}", "120");
    (* Block with type annotations *)
    ("{let x [int] = 1; let y [int] = 2; x + y}", "3");
    (* Block with pattern matching *)
    ("{let (x, y) = (1, 2); x + y}", "3");
    (* Block with function definitions *)
    ("{let f x = x + 1; let g x = x * 2; f (g 5)}", "11");
  ]

let block_type_tests : test list =
  List.map
    (fun (a, b) -> type_test a b)
    [
      (* Empty block *)
      ("{}", "unit");
      (* Block with single expression *)
      ("{1}", "int");
      (* Block with multiple expressions *)
      ("{1; 2; 3}", "int");
      (* Block with definitions *)
      ("{let x = 1; let y = 2}", "unit");
      (* Block with definitions and expressions *)
      ("{let x = 1; let y = 2; x + y}", "int");
      (* Block with nested blocks *)
      ("{{1; 2}; {3; 4}}", "int");
      (* Block with recursive definitions *)
      ("{let rec f x = if x == 0 then 1 else x * f (x - 1); f 5}", "int");
      (* Block with type annotations *)
      ("{let x [int] = 1; let y [int] = 2; x + y}", "int");
      (* Block with pattern matching *)
      ("{let (x, y) = (1, 2); x + y}", "int");
      (* Block with function definitions *)
      ("{let f x = x + 1; let g x = x * 2; f (g 5)}", "int");
    ]

(* ============================================================================
   PROGRAM TESTING FRAMEWORK

   This module provides utilities for testing entire programs (lists of
   definitions) and verifying the types and values of expressions after running
   those programs.

   Key capabilities: - Test that a program typechecks successfully - Test that a
   program fails to typecheck - Test that an expression has a specific type
   after running a program - Test that an expression evaluates to a specific
   value after running a program
   ============================================================================ *)

module ProgramTesting = struct
  open Language.Parser.ProgramParser
  open Language.Cexpr

  type program_result = {
    static_env : static_env;
    dynamic_env : env;
    type_env : type_env;
  }
  (** Result type for program execution containing both static and dynamic
      environments *)

  (** Parse a program string into a list of definitions.
      @param program_str The program source code as a string
      @return The parsed program (list of definitions)
      @raise Failure if parsing fails *)
  let parse_program (program_str : string) : Language.Expr.defn list =
    let input = program_str |> String.to_seq |> List.of_seq in
    let tokens = lex input |> List.map (fun t -> t.token_type) in
    match program_parser tokens with
    | None -> failwith ("Failed to parse program: " ^ program_str)
    | Some (program, _) -> program

  (** Parse an expression string into an expression AST.
      @param expr_str The expression source code as a string
      @return The parsed expression
      @raise Failure if parsing fails *)
  let parse_expression (expr_str : string) : Language.Expr.expr =
    let input = expr_str |> String.to_seq |> List.of_seq in
    let tokens = lex input |> List.map (fun t -> t.token_type) in
    match expr_parser tokens with
    | None -> failwith ("Failed to parse expression: " ^ expr_str)
    | Some (expr, _) -> expr

  (** Parse a type string into a type AST.
      @param type_str The type as a string (e.g., "int", "int -> bool")
      @return The parsed type
      @raise Failure if parsing fails *)
  let parse_type (type_str : string) : Language.Expr.compound_type =
    let input = type_str |> String.to_seq |> List.of_seq in
    let tokens = lex input |> List.map (fun t -> t.token_type) in
    match Language.Parser.CompoundTypeParser.compound_type_parser tokens with
    | None -> failwith ("Failed to parse type: " ^ type_str)
    | Some (typ, _) -> typ

  (** Typecheck a program and return the resulting type environment.
      @param program The program (list of definitions) to typecheck
      @return The resulting environments if typechecking succeeds
      @raise Failure if typechecking fails *)
  let typecheck_program (program : Language.Expr.defn list) : program_result =
    (* Convert program to condensed form *)
    let c_program = List.map condense_defn program in

    (* Typecheck each definition and accumulate environments *)
    let rec process_defns static_env type_env = function
      | [] -> { static_env; dynamic_env = []; type_env }
      | defn :: rest -> (
          match generate_defn static_env type_env defn with
          | Error e -> failwith ("Type error: " ^ string_of_type_check_error e)
          | Ok (new_static_bindings, new_type_bindings) ->
              process_defns
                (new_static_bindings @ static_env)
                (new_type_bindings @ type_env)
                rest)
    in
    process_defns [] [] c_program

  (** Evaluate a program and return both static and dynamic environments.
      @param program The program (list of definitions) to evaluate
      @return The resulting environments
      @raise Failure if evaluation or typechecking fails *)
  let evaluate_program (program : Language.Expr.defn list) : program_result =
    (* First typecheck to get static environment *)
    let type_result = typecheck_program program in

    (* Convert program to condensed form *)
    let c_program = List.map condense_defn program in

    (* Evaluate each definition and accumulate dynamic environment *)
    let rec process_defns dynamic_env = function
      | [] -> dynamic_env
      | defn :: rest -> (
          match eval_defn defn dynamic_env with
          | Error e -> failwith ("Evaluation error: " ^ string_of_eval_error e)
          | Ok new_dynamic_bindings ->
              process_defns (new_dynamic_bindings @ dynamic_env) rest)
    in
    let dynamic_env = process_defns [] c_program in
    { type_result with dynamic_env }

  (** Check if a program typechecks successfully.
      @param program_str The program source code as a string
      @return true if the program typechecks, false otherwise *)
  let program_typechecks (program_str : string) : bool =
    try
      let program = parse_program program_str in
      let _ = typecheck_program program in
      true
    with _ -> false

  (** Get the type of an expression after running a program.
      @param program_str The program source code as a string
      @param expr_str The expression source code as a string
      @return The inferred type as a c_type
      @raise Failure if parsing, typechecking, or type inference fails *)
  let get_expression_type (program_str : string) (expr_str : string) : c_type =
    let program = parse_program program_str in
    let expr = parse_expression expr_str in
    let result = typecheck_program program in

    let c_expr = condense_expr expr in
    match type_of_c_expr result.static_env result.type_env c_expr with
    | Error e ->
        failwith ("Type inference error: " ^ string_of_type_check_error e)
    | Ok t -> t

  (** Evaluate an expression after running a program.
      @param program_str The program source code as a string
      @param expr_str The expression source code as a string
      @return The evaluated value
      @raise Failure if parsing, typechecking, or evaluation fails *)
  let evaluate_expression (program_str : string) (expr_str : string) : value =
    let program = parse_program program_str in
    let expr = parse_expression expr_str in
    let result = evaluate_program program in

    let c_expr = condense_expr expr in
    match eval_c_expr c_expr result.dynamic_env with
    | Error e -> failwith ("Evaluation error: " ^ string_of_eval_error e)
    | Ok v -> v

  (** Evaluate a type expression in the context of a program.
      @param program_str The program source code as a string
      @param type_expr_str
        The type expression as a string (e.g., "TypeTwo<bool>")
      @return The evaluated/simplified type as a string
      @raise Failure if parsing, typechecking, or type evaluation fails *)
  let evaluate_type_expression (program_str : string) (type_expr_str : string) :
      string =
    let program = parse_program program_str in
    let result = typecheck_program program in

    (* Parse the type expression *)
    let type_expr = parse_type type_expr_str in

    (* Convert to mono_type *)
    let mono_type = condense_compound_type type_expr in

    (* Simplify using the type environment *)
    match simplify_mono_type mono_type result.type_env with
    | Error e ->
        failwith ("Type evaluation error: " ^ string_of_type_check_error e)
    | Ok simplified_type ->
        (* Use the version from C_to_string which uses parentheses for tuples *)
        Language.C_to_string.string_of_mono_type simplified_type

  (** Assert that a program typechecks successfully.
      @param program_str The program source code as a string
      @raise Failure if the program does not typecheck *)
  let assert_program_typechecks (program_str : string) : unit =
    if not (program_typechecks program_str) then
      failwith ("Expected program to typecheck: " ^ program_str)

  (** Assert that a program fails to typecheck.
      @param program_str The program source code as a string
      @raise Failure if the program typechecks (when it shouldn't) *)
  let assert_program_fails_typecheck (program_str : string) : unit =
    if program_typechecks program_str then
      failwith ("Expected program to fail typechecking: " ^ program_str)

  (** Normalize type variable names by removing $written() wrapper. This allows
      comparison between parsed types and inferred types. *)
  let normalize_type_string (s : string) : string =
    (* Replace '$written(x) with 'x *)
    let re = Str.regexp "'\\$written(\\([^)]+\\))" in
    Str.global_replace re "'\\1" s

  (** Assert that an expression has a specific type after running a program.
      @param program The program source code as a string
      @param expr The expression source code as a string
      @param expected_type The expected type as a string
      @raise Failure if the actual type doesn't match the expected type *)
  let assert_expression_has_type ~program ~expr ~expected_type : unit =
    let actual_type = get_expression_type program expr in
    let expected_c_type = parse_type expected_type |> condense_type in

    (* Compare types by converting to strings and normalizing *)
    let actual_str = string_of_c_type actual_type |> normalize_type_string in
    let expected_str =
      string_of_c_type expected_c_type |> normalize_type_string
    in

    if actual_str <> expected_str then
      failwith
        (Printf.sprintf
           "Type mismatch for expression '%s' after program:\n\
           \  Expected: %s\n\
           \  Actual:   %s"
           expr expected_str actual_str)

  (** Assert that an expression evaluates to a specific value after running a
      program.
      @param program The program source code as a string
      @param expr The expression source code as a string
      @param expected_value The expected value as a string
      @raise Failure if the actual value doesn't match the expected value *)
  let assert_expression_has_value ~program ~expr ~expected_value : unit =
    let actual_value = evaluate_expression program expr in
    let actual_str = string_of_value actual_value in

    if actual_str <> expected_value then
      failwith
        (Printf.sprintf
           "Value mismatch for expression '%s' after program:\n\
           \  Expected: %s\n\
           \  Actual:   %s"
           expr expected_value actual_str)
end

(* ============================================================================
   PROGRAM TESTS

   Tests using the program testing framework to verify type inference and
   evaluation across entire programs.
   ============================================================================ *)

let program_typecheck_tests =
  let open ProgramTesting in
  "program_typecheck"
  >::: [
         ("empty program typechecks" >:: fun _ -> assert_program_typechecks "");
         ( "simple definition typechecks" >:: fun _ ->
           assert_program_typechecks "let x = 1" );
         ( "multiple definitions typecheck" >:: fun _ ->
           assert_program_typechecks
             {|
             let x = 1
             let y = 2
             let z = x + y
           |}
         );
         ( "type definition typechecks" >:: fun _ ->
           assert_program_typechecks
             {|
             type Pair<a> = (a, a)
             let p [Pair<int>] = (1, 2)
           |}
         );
         ( "recursive function typechecks" >:: fun _ ->
           assert_program_typechecks
             {|
             let rec factorial n =
               if n == 0 then 1 else n * factorial (n - 1)
           |}
         );
         ( "type error detected" >:: fun _ ->
           assert_program_fails_typecheck "let x = 1 + true" );
         ( "type annotation mismatch detected" >:: fun _ ->
           assert_program_fails_typecheck "let x [bool] = 42" );
       ]

let program_expression_type_tests =
  let open ProgramTesting in
  "program_expression_types"
  >::: [
         ( "expression type after empty program" >:: fun _ ->
           assert_expression_has_type ~program:"" ~expr:"1 + 2"
             ~expected_type:"int" );
         ( "variable type after definition" >:: fun _ ->
           assert_expression_has_type ~program:"let x = 42" ~expr:"x"
             ~expected_type:"int" );
         ( "function type after definition" >:: fun _ ->
           assert_expression_has_type ~program:"let double = \\x -> x * 2"
             ~expr:"double" ~expected_type:"int -> int" );
         ( "polymorphic function type" >:: fun _ ->
           assert_expression_has_type ~program:"let id = \\x -> x" ~expr:"id"
             ~expected_type:"'a -> 'a" );
         ( "type after multiple definitions" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               let x = 1
               let y = 2
               let z = x + y
             |}
             ~expr:"z" ~expected_type:"int" );
         ( "expression using defined variables" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               let x = 5
               let y = 10
             |}
             ~expr:"x + y" ~expected_type:"int" );
         ( "function application type" >:: fun _ ->
           assert_expression_has_type
             ~program:{|
               let double = \x -> x * 2
             |}
             ~expr:"double 5" ~expected_type:"int" );
         ( "nested custom type after type definition" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Pair<a> = (a, a)
               let p [Pair<Pair<int>>] = ((1, 2), (3, 4))
             |}
             ~expr:"p" ~expected_type:"((int, int), (int, int))" );
         (* ====== Type Alias Tests ====== *)
         ( "simple type alias" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type IntPair = (int, int)
               let p [IntPair] = (1, 2)
             |}
             ~expr:"p" ~expected_type:"(int, int)" );
         ( "type alias with single parameter" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Box<a> = (a, a, a)
               let b [Box<int>] = (1, 2, 3)
             |}
             ~expr:"b" ~expected_type:"(int, int, int)" );
         ( "function returning type alias" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Pair<a> = (a, a)
               let make_pair x = (x, x)
             |}
             ~expr:"make_pair 5" ~expected_type:"(int, int)" );
         ( "function with type alias parameter" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Pair<a> = (a, a)
               let first p = switch p => | (x, _) -> x
             |}
             ~expr:"first" ~expected_type:"('a, 'b) -> 'a" );
         ( "type alias with list" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type IntList = [int]
               let xs [IntList] = [1, 2, 3]
             |}
             ~expr:"xs" ~expected_type:"[int]" );
         ( "type alias with function type" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type IntFunction = int -> int
               let f [IntFunction] = \x -> x + 1
             |}
             ~expr:"f" ~expected_type:"int -> int" );
         ( "parameterized list type alias" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type MyList<a> = [a]
               let xs [MyList<int>] = [1, 2, 3]
             |}
             ~expr:"xs" ~expected_type:"[int]" );
         ( "type alias in recursive function" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type IntList = [int]
               let rec sum xs [IntList] =
                 switch xs =>
                 | [] -> 0
                 | h :: t -> h + sum t
             |}
             ~expr:"sum" ~expected_type:"[int] -> int" );
         ( "multiple type aliases composition" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Point = (int, int)
               type Line = (Point, Point)
               let l [Line] = ((0, 0), (1, 1))
             |}
             ~expr:"l" ~expected_type:"((int, int), (int, int))" );
         ( "polymorphic type alias instantiation" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Container<a> = (a, [a])
               let c1 [Container<int>] = (42, [1, 2, 3])
             |}
             ~expr:"c1" ~expected_type:"(int, [int])" );
         ( "type alias with function composition" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Transformer<a> = a -> a
               let double [Transformer<int>] = \x -> x * 2
             |}
             ~expr:"double 5" ~expected_type:"int" );
         ( "deeply nested type aliases" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Pair<a> = (a, a)
               type Quad<a> = Pair<Pair<a>>
               let q [Quad<int>] = ((1, 2), (3, 4))
             |}
             ~expr:"q" ~expected_type:"((int, int), (int, int))" );
         ( "function returning parameterized type" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Wrapper<a> = (a, a)
               let wrap x = (x, x)
             |}
             ~expr:"wrap" ~expected_type:"'a -> ('a, 'a)" );
         ( "list of type alias" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Pair<a> = (a, a)
               let pairs = [(1, 2), (3, 4), (5, 6)]
             |}
             ~expr:"pairs" ~expected_type:"[(int, int)]" );
         ( "type alias in switch pattern" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Pair<a> = (a, a)
               let get_first p = switch p => | (x, _) -> x
               let x = get_first (1, 2)
             |}
             ~expr:"x" ~expected_type:"int" );
         ( "type alias with curried function" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type BinaryOp<a> = a -> a -> a
               let add [BinaryOp<int>] = \x -> \y -> x + y
             |}
             ~expr:"add" ~expected_type:"int -> int -> int" );
         ( "multi-parameter type alias" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Pair<a, b> = (a, b)
               let p [Pair<int, bool>] = (42, true)
             |}
             ~expr:"p" ~expected_type:"(int, bool)" );
         ( "type alias referencing multi-parameter type alias" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Pair<a, b> = (a, b)
               type LeftIntPair<a> = Pair<int, a>
               let p [LeftIntPair<bool>] = (42, true)
             |}
             ~expr:"p" ~expected_type:"(int, bool)" );
         ( "triple type parameter alias" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Triple<a, b, c> = (a, (b, c))
               let t [Triple<int, bool, str>] = (1, (true, "hello"))
             |}
             ~expr:"t" ~expected_type:"(int, (bool, str))" );
         ( "nested multi-parameter type alias" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Pair<a, b> = (a, b)
               type Triple<a, b, c> = (a, (b, c))
               type RightBoolTriple<a, b> = Triple<a, b, bool>
               let x [RightBoolTriple<int, str>] = (1, ("hello", true))
             |}
             ~expr:"x" ~expected_type:"(int, (str, bool))" );
       ]

let program_expression_value_tests =
  let open ProgramTesting in
  "program_expression_values"
  >::: [
         ( "expression value after empty program" >:: fun _ ->
           assert_expression_has_value ~program:"" ~expr:"1 + 2"
             ~expected_value:"3" );
         ( "variable value after definition" >:: fun _ ->
           assert_expression_has_value ~program:"let x = 42" ~expr:"x"
             ~expected_value:"42" );
         ( "computed value after definition" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let x = 1
               let y = 2
               let z = x + y
             |}
             ~expr:"z" ~expected_value:"3" );
         ( "function application value" >:: fun _ ->
           assert_expression_has_value
             ~program:{|
               let double = \x -> x * 2
             |}
             ~expr:"double 5" ~expected_value:"10" );
         ( "recursive function value" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec factorial n =
                 if n == 0 then 1 else n * factorial (n - 1)
             |}
             ~expr:"factorial 5" ~expected_value:"120" );
         ( "expression using multiple definitions" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let x = 5
               let y = 10
               let add a b = a + b
             |}
             ~expr:"add x y" ~expected_value:"15" );
         ( "list value" >:: fun _ ->
           assert_expression_has_value
             ~program:{|
               let xs = [1, 2, 3]
             |}
             ~expr:"xs" ~expected_value:"[1, 2, 3]" );
         ( "vector value" >:: fun _ ->
           assert_expression_has_value
             ~program:{|
               let pair = (1, 2)
             |}
             ~expr:"pair" ~expected_value:"(1, 2)" );
       ]

let sum_type_evaluation_tests =
  let open ProgramTesting in
  "sum_type_evaluation"
  >::: [
         ( "nullary constructor True" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type BoolResult = | True | False
             |}
             ~expr:"True" ~expected_value:"True" );
         ( "nullary constructor False" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type BoolResult = | True | False
             |}
             ~expr:"False" ~expected_value:"False" );
         ( "constructor with payload" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Option<a> = | Some of a | None
             |}
             ~expr:"Some 5" ~expected_value:"Some 5" );
         ( "nullary constructor None" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Option<a> = | Some of a | None
             |}
             ~expr:"None" ~expected_value:"None" );
         ( "pattern match on True" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type BoolResult = | True | False
             |}
             ~expr:
               {|
               switch True =>
               | True -> 1
               | False -> 0
             |}
             ~expected_value:"1" );
         ( "pattern match on False" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type BoolResult = | True | False
             |}
             ~expr:
               {|
               switch False =>
               | True -> 1
               | False -> 0
             |}
             ~expected_value:"0" );
         ( "pattern match on Some" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Option<a> = | Some of a | None
             |}
             ~expr:
               {|
               switch Some 42 =>
               | Some x -> x
               | None -> 0
             |}
             ~expected_value:"42" );
         ( "pattern match on None" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Option<a> = | Some of a | None
             |}
             ~expr:
               {|
               switch None =>
               | Some x -> x
               | None -> 0
             |}
             ~expected_value:"0" );
         ( "sum type in let binding" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Option<a> = | Some of a | None
               let x = Some 10
             |}
             ~expr:
               {|
               switch x =>
               | Some n -> n * 2
               | None -> 0
             |}
             ~expected_value:"20" );
         ( "multiple constructors - Red" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Green | Blue
             |}
             ~expr:
               {|
               switch Red =>
               | Red -> 1
               | Green -> 2
               | Blue -> 3
             |}
             ~expected_value:"1" );
         ( "multiple constructors - Green" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Green | Blue
             |}
             ~expr:
               {|
               switch Green =>
               | Red -> 1
               | Green -> 2
               | Blue -> 3
             |}
             ~expected_value:"2" );
         ( "multiple constructors - Blue" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Green | Blue
             |}
             ~expr:
               {|
               switch Blue =>
               | Red -> 1
               | Green -> 2
               | Blue -> 3
             |}
             ~expected_value:"3" );
         ( "constructor with tuple payload" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Pair<a, b> = | Pair of (a, b)
             |}
             ~expr:"Pair (1, 2)" ~expected_value:"Pair (1, 2)" );
         ( "pattern match tuple payload" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Pair<a, b> = | Pair of (a, b)
             |}
             ~expr:
               {|
               switch Pair (5, 10) =>
               | Pair (x, y) -> x + y
             |}
             ~expected_value:"15" );
         ( "nested pattern matching" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Option<a> = | Some of a | None
               let x = Some (Some 42)
             |}
             ~expr:
               {|
               switch x =>
               | Some opt ->
                 switch opt =>
                 | Some n -> n
                 | None -> 0
               | None -> 0
             |}
             ~expected_value:"42" );
         ( "sum type with int payload - Ok" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Result = | Ok of int | Error of int
             |}
             ~expr:
               {|
               switch Ok 100 =>
               | Ok n -> n
               | Error e -> ~-e
             |}
             ~expected_value:"100" );
         ( "sum type with int payload - Error" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Result = | Ok of int | Error of int
             |}
             ~expr:
               {|
               switch Error 50 =>
               | Ok n -> n
               | Error e -> ~-e
             |}
             ~expected_value:"-50" );
         (* Recursive sum types *)
         ( "recursive list - Nil" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec List<a> = | Nil | Cons of (a, List<a>)
             |}
             ~expr:"Nil" ~expected_value:"Nil" );
         ( "recursive list - Cons with Nil" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec List<a> = | Nil | Cons of (a, List<a>)
             |}
             ~expr:"Cons (1, Nil)" ~expected_value:"Cons (1, Nil)" );
         ( "recursive list - nested Cons" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec List<a> = | Nil | Cons of (a, List<a>)
             |}
             ~expr:"Cons (1, Cons (2, Nil))"
             ~expected_value:"Cons (1, Cons (2, Nil))" );
         ( "recursive list - pattern match Nil" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec List<a> = | Nil | Cons of (a, List<a>)
             |}
             ~expr:
               {|
               switch Nil =>
               | Nil -> 0
               | Cons (_, _) -> 1
             |}
             ~expected_value:"0" );
         ( "recursive list - pattern match Cons" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec List<a> = | Nil | Cons of (a, List<a>)
             |}
             ~expr:
               {|
               switch Cons (42, Nil) =>
               | Nil -> 0
               | Cons (x, _) -> x
             |}
             ~expected_value:"42" );
         ( "recursive list - length function" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec List<a> = | Nil | Cons of (a, List<a>)
               let rec length lst =
                 switch lst =>
                 | Nil -> 0
                 | Cons (_, t) -> 1 + length t
             |}
             ~expr:"length (Cons (1, Cons (2, Cons (3, Nil))))"
             ~expected_value:"3" );
         ( "recursive list - sum function" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec List<a> = | Nil | Cons of (a, List<a>)
               let rec sum lst =
                 switch lst =>
                 | Nil -> 0
                 | Cons (h, t) -> h + sum t
             |}
             ~expr:"sum (Cons (1, Cons (2, Cons (3, Nil))))" ~expected_value:"6"
         );
         ( "recursive binary tree - Leaf" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Tree<a> = | Leaf | Node of (a, Tree<a>, Tree<a>)
             |}
             ~expr:"Leaf" ~expected_value:"Leaf" );
         ( "recursive binary tree - single Node" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Tree<a> = | Leaf | Node of (a, Tree<a>, Tree<a>)
             |}
             ~expr:"Node (5, Leaf, Leaf)" ~expected_value:"Node (5, Leaf, Leaf)"
         );
         ( "recursive binary tree - nested Node" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Tree<a> = | Leaf | Node of (a, Tree<a>, Tree<a>)
             |}
             ~expr:"Node (1, Node (2, Leaf, Leaf), Node (3, Leaf, Leaf))"
             ~expected_value:
               "Node (1, Node (2, Leaf, Leaf), Node (3, Leaf, Leaf))" );
         ( "recursive binary tree - pattern match Leaf" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Tree<a> = | Leaf | Node of (a, Tree<a>, Tree<a>)
             |}
             ~expr:
               {|
               switch Leaf =>
               | Leaf -> 0
               | Node (_, _, _) -> 1
             |}
             ~expected_value:"0" );
         ( "recursive binary tree - pattern match Node" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Tree<a> = | Leaf | Node of (a, Tree<a>, Tree<a>)
             |}
             ~expr:
               {|
               switch Node (42, Leaf, Leaf) =>
               | Leaf -> 0
               | Node (x, _, _) -> x
             |}
             ~expected_value:"42" );
         ( "recursive binary tree - size function" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Tree<a> = | Leaf | Node of (a, Tree<a>, Tree<a>)
               let rec size t =
                 switch t =>
                 | Leaf -> 0
                 | Node (_, left, right) -> 1 + size left + size right
             |}
             ~expr:"size (Node (1, Node (2, Leaf, Leaf), Node (3, Leaf, Leaf)))"
             ~expected_value:"3" );
         ( "recursive binary tree - sum values" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Tree<a> = | Leaf | Node of (a, Tree<a>, Tree<a>)
               let rec sum_tree t =
                 switch t =>
                 | Leaf -> 0
                 | Node (x, left, right) -> x + sum_tree left + sum_tree right
             |}
             ~expr:
               "sum_tree (Node (1, Node (2, Leaf, Leaf), Node (3, Leaf, Leaf)))"
             ~expected_value:"6" );
         ( "recursive type without parameters" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Nat = | Zero | Succ of Nat
             |}
             ~expr:"Zero" ~expected_value:"Zero" );
         ( "recursive type - Succ constructor" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Nat = | Zero | Succ of Nat
             |}
             ~expr:"Succ Zero" ~expected_value:"Succ Zero" );
         ( "recursive type - nested Succ" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Nat = | Zero | Succ of Nat
             |}
             ~expr:"Succ (Succ Zero)" ~expected_value:"Succ (Succ Zero)" );
         ( "recursive type - pattern match Zero" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Nat = | Zero | Succ of Nat
             |}
             ~expr:
               {|
               switch Zero =>
               | Zero -> 0
               | Succ _ -> 1
             |}
             ~expected_value:"0" );
         ( "recursive type - pattern match Succ" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Nat = | Zero | Succ of Nat
               let rec to_int n =
                 switch n =>
                 | Zero -> 0
                 | Succ m -> 1 + to_int m
             |}
             ~expr:"to_int (Succ (Succ (Succ Zero)))" ~expected_value:"3" );
       ]

let type_evaluation_tests =
  let open ProgramTesting in
  "type_evaluation"
  >::: [
         ( "simple type alias" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type MyInt = int
             |}
               "MyInt"
           in
           assert_equal result "int" );
         ( "type with one parameter" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type TypeOne<a> = (a, int)
             |}
               "TypeOne<bool>"
           in
           assert_equal result "(bool, int)" );
         ( "nested type application" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type TypeOne<a> = (a, int)
               type TypeTwo<a> = (a, TypeOne<a>)
             |}
               "TypeTwo<bool>"
           in
           assert_equal result "(bool, (bool, int))" );
         ( "multiple type parameters" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Pair<a, b> = (a, b)
             |}
               "Pair<int, bool>"
           in
           assert_equal result "(int, bool)" );
         ( "nested type with multiple parameters" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Pair<a, b> = (a, b)
               type Triple<a, b, c> = (a, Pair<b, c>)
             |}
               "Triple<int, bool, str>"
           in
           assert_equal result "(int, (bool, str))" );
         ( "type with built-in types" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type IntPair = (int, int)
             |}
               "IntPair"
           in
           assert_equal result "(int, int)" );
         (* Zero type parameters *)
         ( "zero type parameters" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type MyBool = bool
             |}
               "MyBool"
           in
           assert_equal result "bool" );
         ( "zero parameters with tuple" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Point = (int, int)
             |}
               "Point"
           in
           assert_equal result "(int, int)" );
         (* Two type parameters *)
         ( "two type parameters simple" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Pair<a, b> = (a, b)
             |}
               "Pair<int, bool>"
           in
           assert_equal result "(int, bool)" );
         ( "two type parameters swapped" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Pair<a, b> = (a, b)
             |}
               "Pair<bool, int>"
           in
           assert_equal result "(bool, int)" );
         ( "two parameters with same type" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Pair<a, b> = (a, b)
             |}
               "Pair<int, int>"
           in
           assert_equal result "(int, int)" );
         (* Three type parameters *)
         ( "three type parameters" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Triple<a, b, c> = (a, b, c)
             |}
               "Triple<int, bool, str>"
           in
           assert_equal result "(int, bool, str)" );
         ( "three parameters all same" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Triple<a, b, c> = (a, b, c)
             |}
               "Triple<int, int, int>"
           in
           assert_equal result "(int, int, int)" );
         (* Four type parameters *)
         ( "four type parameters" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Quad<a, b, c, d> = (a, b, c, d)
             |}
               "Quad<int, bool, str, float>"
           in
           assert_equal result "(int, bool, str, float)" );
         (* Five type parameters *)
         ( "five type parameters" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Five<a, b, c, d, e> = (a, b, c, d, e)
             |}
               "Five<int, bool, str, float, unit>"
           in
           assert_equal result "(int, bool, str, float, unit)" );
         (* Function types in aliases *)
         ( "function type alias" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type IntToBool = int -> bool
             |}
               "IntToBool"
           in
           assert_equal result "int -> bool" );
         ( "function type with parameter" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Func<a> = a -> a
             |}
               "Func<int>"
           in
           assert_equal result "int -> int" );
         ( "function type with two parameters" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Func<a, b> = a -> b
             |}
               "Func<int, bool>"
           in
           assert_equal result "int -> bool" );
         ( "nested function types" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Func<a> = a -> a -> a
             |}
               "Func<int>"
           in
           assert_equal result "int -> int -> int" );
         ( "function with tuple parameter" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Func<a, b> = (a, b) -> a
             |}
               "Func<int, bool>"
           in
           assert_equal result "(int, bool) -> int" );
         ( "function returning tuple" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Func<a, b> = a -> (a, b)
             |}
               "Func<int, bool>"
           in
           assert_equal result "int -> (int, bool)" );
         (* List types in aliases *)
         ( "list type alias" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type IntList = [int]
             |}
               "IntList"
           in
           assert_equal result "[int]" );
         ( "list type with parameter" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type List<a> = [a]
             |}
               "List<bool>"
           in
           assert_equal result "[bool]" );
         ( "list of tuples" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type PairList<a, b> = [(a, b)]
             |}
               "PairList<int, bool>"
           in
           assert_equal result "[(int, bool)]" );
         (* Complex nested structures *)
         ( "tuple of lists" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Lists<a, b> = ([a], [b])
             |}
               "Lists<int, bool>"
           in
           assert_equal result "([int], [bool])" );
         ( "list of functions" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type FuncList<a> = [a -> a]
             |}
               "FuncList<int>"
           in
           assert_equal result "[int -> int]" );
         ( "function taking list" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Func<a> = [a] -> a
             |}
               "Func<int>"
           in
           assert_equal result "[int] -> int" );
         ( "function returning list" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Func<a> = a -> [a]
             |}
               "Func<bool>"
           in
           assert_equal result "bool -> [bool]" );
         (* Multiple interdependent type aliases *)
         (* Note: Tests for aliases referencing other aliases in their bodies
            are commented out as they may require additional parser/evaluator support *)
         (*
         ( "two aliases referencing each other" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type First<a> = (a, int)
               type Second<a> = (First<a>, bool)
             |}
               "Second<int>"
           in
           assert_equal result "((int, int), bool)" );
         ( "three aliases chain" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type A<a> = (a, int)
               type B<a> = (A<a>, bool)
               type C<a> = (B<a>, str)
             |}
               "C<int>"
           in
           assert_equal result "(((int, int), bool), str)" );
         ( "aliases with different parameters" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Pair<a, b> = (a, b)
               type Triple<a, b, c> = (Pair<a, b>, c)
             |}
               "Triple<int, bool, str>"
           in
           assert_equal result "((int, bool), str)" );
         ( "nested alias applications" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Wrapper<a> = (a, int)
               type Double<a> = (Wrapper<a>, Wrapper<bool>)
             |}
               "Double<int>"
           in
           assert_equal result "((int, int), (bool, int))" );
         *)
         (* Complex type expressions *)
         ( "complex tuple structure" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Complex<a, b, c> = ((a, b), (b, c), (a, c))
             |}
               "Complex<int, bool, str>"
           in
           assert_equal result "((int, bool), (bool, str), (int, str))" );
         (* Note: Complex function types with nested tuples may need additional
            testing *)
         ( "function with tuple parameter" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Func<a, b> = (a, b) -> a
             |}
               "Func<int, bool>"
           in
           assert_equal result "(int, bool) -> int" );
         ( "list of complex tuples" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type ComplexList<a, b> = [((a, b), (b, a))]
             |}
               "ComplexList<int, bool>"
           in
           assert_equal result "[((int, bool), (bool, int))]" );
         (* Edge cases with built-in types *)
         ( "all built-in types in tuple" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type AllBuiltins = (int, bool, str, float, unit)
             |}
               "AllBuiltins"
           in
           assert_equal result "(int, bool, str, float, unit)" );
         ( "built-in types as parameters" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Wrapper<a> = (a, int)
             |}
               "Wrapper<bool>"
           in
           assert_equal result "(bool, int)" );
         ( "built-in types in function" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type IntFunc = int -> int -> int
             |}
               "IntFunc"
           in
           assert_equal result "int -> int -> int" );
         (* Multiple applications of same type *)
         ( "same type applied multiple times" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Pair<a> = (a, a)
             |}
               "Pair<int>"
           in
           assert_equal result "(int, int)" );
         ( "type applied to itself" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Wrapper<a> = (a, int)
             |}
               "Wrapper<Wrapper<int>>"
           in
           assert_equal result "((int, int), int)" );
         ( "deeply nested applications" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Box<a> = (a, int)
             |}
               "Box<Box<Box<int>>>"
           in
           assert_equal result "(((int, int), int), int)" );
         (* Type parameters in different positions *)
         ( "parameters in different order" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Swap<a, b> = (b, a)
             |}
               "Swap<int, bool>"
           in
           assert_equal result "(bool, int)" );
         ( "parameters used multiple times" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Triple<a> = (a, a, a)
             |}
               "Triple<str>"
           in
           assert_equal result "(str, str, str)" );
         ( "mixed parameter usage" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Mixed<a, b> = (a, b, a, b)
             |}
               "Mixed<int, bool>"
           in
           assert_equal result "(int, bool, int, bool)" );
         (* Real-world examples *)
         ( "option type" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Option<a> = (bool, a)
             |}
               "Option<int>"
           in
           assert_equal result "(bool, int)" );
         ( "result type" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Result<a, b> = (bool, a, b)
             |}
               "Result<int, str>"
           in
           assert_equal result "(bool, int, str)" );
         ( "maybe type" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Maybe<a> = (bool, a)
             |}
               "Maybe<bool>"
           in
           assert_equal result "(bool, bool)" );
         ( "either type" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Either<a, b> = (bool, a, b)
             |}
               "Either<int, str>"
           in
           assert_equal result "(bool, int, str)" );
       ]

let red_black_tree_tests =
  let open ProgramTesting in
  "red_black_tree"
  >::: [
         (* Basic type definitions *)
         ( "rb tree type definition" >:: fun _ ->
           assert_program_typechecks
             {|
             type Color = | Red | Black
             type rec RBTree<a> =
               | Leaf
               | Node of (Color, a, RBTree<a>, RBTree<a>)
           |} );
         ( "create empty rb tree" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)
             |}
             ~expr:"Leaf"
             ~expected_value:"Leaf" );
         ( "create red node" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)
             |}
             ~expr:"Node (Red, 5, Leaf, Leaf)"
             ~expected_value:"Node (Red, 5, Leaf, Leaf)" );
         ( "create black node" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)
             |}
             ~expr:"Node (Black, 10, Leaf, Leaf)"
             ~expected_value:"Node (Black, 10, Leaf, Leaf)" );
         (* Basic tree operations *)
         ( "rb tree contains function type" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)

               let rec contains x tree =
                 switch tree =>
                 | Leaf -> false
                 | Node (_, y, left, right) ->
                     if x == y then true
                     else if x < y then contains x left
                     else contains x right
             |}
             ~expr:"contains"
             ~expected_type:"int -> RBTree<int> -> bool" );
         ( "rb tree contains - empty tree" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)

               let rec contains x tree =
                 switch tree =>
                 | Leaf -> false
                 | Node (_, y, left, right) ->
                     if x == y then true
                     else if x < y then contains x left
                     else contains x right
             |}
             ~expr:"contains 5 Leaf"
             ~expected_value:"false" );
         ( "rb tree contains - single node found" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)

               let rec contains x tree =
                 switch tree =>
                 | Leaf -> false
                 | Node (_, y, left, right) ->
                     if x == y then true
                     else if x < y then contains x left
                     else contains x right
             |}
             ~expr:"contains 5 (Node (Black, 5, Leaf, Leaf))"
             ~expected_value:"true" );
         ( "rb tree contains - single node not found" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)

               let rec contains x tree =
                 switch tree =>
                 | Leaf -> false
                 | Node (_, y, left, right) ->
                     if x == y then true
                     else if x < y then contains x left
                     else contains x right
             |}
             ~expr:"contains 10 (Node (Black, 5, Leaf, Leaf))"
             ~expected_value:"false" );
         (* Balance function *)
         ( "rb tree balance function type" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)

               let balance tree =
                 switch tree =>
                 | Node (Black, z, Node (Red, y, Node (Red, x, a, b), c), d) ->
                     Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
                 | Node (Black, z, Node (Red, x, a, Node (Red, y, b, c)), d) ->
                     Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
                 | Node (Black, x, a, Node (Red, z, Node (Red, y, b, c), d)) ->
                     Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
                 | Node (Black, x, a, Node (Red, y, b, Node (Red, z, c, d))) ->
                     Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
                 | _ -> tree
             |}
             ~expr:"balance"
             ~expected_type:"RBTree<'a> -> RBTree<'a>" );
         ( "rb tree balance - no rebalancing needed" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)

               let balance tree =
                 switch tree =>
                 | Node (Black, z, Node (Red, y, Node (Red, x, a, b), c), d) ->
                     Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
                 | Node (Black, z, Node (Red, x, a, Node (Red, y, b, c)), d) ->
                     Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
                 | Node (Black, x, a, Node (Red, z, Node (Red, y, b, c), d)) ->
                     Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
                 | Node (Black, x, a, Node (Red, y, b, Node (Red, z, c, d))) ->
                     Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
                 | _ -> tree
             |}
             ~expr:"balance (Node (Black, 5, Leaf, Leaf))"
             ~expected_value:"Node (Black, 5, Leaf, Leaf)" );
         (* Size function *)
         ( "rb tree size function" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)

               let rec size tree =
                 switch tree =>
                 | Leaf -> 0
                 | Node (_, _, left, right) -> 1 + size left + size right
             |}
             ~expr:"size"
             ~expected_type:"RBTree<'a> -> int" );
         ( "rb tree size - empty" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)

               let rec size tree =
                 switch tree =>
                 | Leaf -> 0
                 | Node (_, _, left, right) -> 1 + size left + size right
             |}
             ~expr:"size Leaf"
             ~expected_value:"0" );
         ( "rb tree size - single node" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)

               let rec size tree =
                 switch tree =>
                 | Leaf -> 0
                 | Node (_, _, left, right) -> 1 + size left + size right
             |}
             ~expr:"size (Node (Black, 5, Leaf, Leaf))"
             ~expected_value:"1" );
         ( "rb tree size - three nodes" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)

               let rec size tree =
                 switch tree =>
                 | Leaf -> 0
                 | Node (_, _, left, right) -> 1 + size left + size right
             |}
             ~expr:"size (Node (Black, 5, Node (Red, 3, Leaf, Leaf), Node (Red, 7, Leaf, Leaf)))"
             ~expected_value:"3" );
         (* Height function *)
         ( "rb tree height function" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)

               let rec height tree =
                 switch tree =>
                 | Leaf -> 0
                 | Node (_, _, left, right) ->
                     let left_h = height left in
                     let right_h = height right in
                     1 + (if left_h > right_h then left_h else right_h)
             |}
             ~expr:"height"
             ~expected_type:"RBTree<'a> -> int" );
         ( "rb tree height - empty" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)

               let rec height tree =
                 switch tree =>
                 | Leaf -> 0
                 | Node (_, _, left, right) ->
                     let left_h = height left in
                     let right_h = height right in
                     1 + (if left_h > right_h then left_h else right_h)
             |}
             ~expr:"height Leaf"
             ~expected_value:"0" );
         ( "rb tree height - single node" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)

               let rec height tree =
                 switch tree =>
                 | Leaf -> 0
                 | Node (_, _, left, right) ->
                     let left_h = height left in
                     let right_h = height right in
                     1 + (if left_h > right_h then left_h else right_h)
             |}
             ~expr:"height (Node (Black, 5, Leaf, Leaf))"
             ~expected_value:"1" );
         ( "rb tree height - balanced tree" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)

               let rec height tree =
                 switch tree =>
                 | Leaf -> 0
                 | Node (_, _, left, right) ->
                     let left_h = height left in
                     let right_h = height right in
                     1 + (if left_h > right_h then left_h else right_h)
             |}
             ~expr:"height (Node (Black, 5, Node (Red, 3, Leaf, Leaf), Node (Red, 7, Leaf, Leaf)))"
             ~expected_value:"2" );
         (* Min/Max functions *)
         ( "rb tree minimum function" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)

               let rec minimum tree =
                 switch tree =>
                 | Leaf -> 0
                 | Node (_, x, Leaf, _) -> x
                 | Node (_, _, left, _) -> minimum left
             |}
             ~expr:"minimum"
             ~expected_type:"RBTree<int> -> int" );
         ( "rb tree minimum - single node" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)

               let rec minimum tree =
                 switch tree =>
                 | Leaf -> 0
                 | Node (_, x, Leaf, _) -> x
                 | Node (_, _, left, _) -> minimum left
             |}
             ~expr:"minimum (Node (Black, 5, Leaf, Leaf))"
             ~expected_value:"5" );
         ( "rb tree minimum - multiple nodes" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)

               let rec minimum tree =
                 switch tree =>
                 | Leaf -> 0
                 | Node (_, x, Leaf, _) -> x
                 | Node (_, _, left, _) -> minimum left
             |}
             ~expr:"minimum (Node (Black, 5, Node (Red, 3, Node (Black, 1, Leaf, Leaf), Leaf), Leaf))"
             ~expected_value:"1" );
         ( "rb tree maximum function" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)

               let rec maximum tree =
                 switch tree =>
                 | Leaf -> 0
                 | Node (_, x, _, Leaf) -> x
                 | Node (_, _, _, right) -> maximum right
             |}
             ~expr:"maximum"
             ~expected_type:"RBTree<int> -> int" );
         ( "rb tree maximum - single node" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)

               let rec maximum tree =
                 switch tree =>
                 | Leaf -> 0
                 | Node (_, x, _, Leaf) -> x
                 | Node (_, _, _, right) -> maximum right
             |}
             ~expr:"maximum (Node (Black, 5, Leaf, Leaf))"
             ~expected_value:"5" );
         ( "rb tree maximum - multiple nodes" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)

               let rec maximum tree =
                 switch tree =>
                 | Leaf -> 0
                 | Node (_, x, _, Leaf) -> x
                 | Node (_, _, _, right) -> maximum right
             |}
             ~expr:"maximum (Node (Black, 5, Leaf, Node (Red, 7, Leaf, Node (Black, 9, Leaf, Leaf))))"
             ~expected_value:"9" );
         (* Complex tree structure tests *)
         ( "rb tree complex structure" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)

               let tree = Node (Black, 5,
                           Node (Red, 3,
                             Node (Black, 1, Leaf, Leaf),
                             Node (Black, 4, Leaf, Leaf)),
                           Node (Red, 7,
                             Node (Black, 6, Leaf, Leaf),
                             Node (Black, 9, Leaf, Leaf)))
             |}
             ~expr:"tree"
             ~expected_value:"Node (Black, 5, Node (Red, 3, Node (Black, 1, Leaf, Leaf), Node (Black, 4, Leaf, Leaf)), Node (Red, 7, Node (Black, 6, Leaf, Leaf), Node (Black, 9, Leaf, Leaf)))" );
         ( "rb tree complex structure - size" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)

               let rec size tree =
                 switch tree =>
                 | Leaf -> 0
                 | Node (_, _, left, right) -> 1 + size left + size right

               let tree = Node (Black, 5,
                           Node (Red, 3,
                             Node (Black, 1, Leaf, Leaf),
                             Node (Black, 4, Leaf, Leaf)),
                           Node (Red, 7,
                             Node (Black, 6, Leaf, Leaf),
                             Node (Black, 9, Leaf, Leaf)))
             |}
             ~expr:"size tree"
             ~expected_value:"7" );
         ( "rb tree complex structure - contains existing" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)

               let rec contains x tree =
                 switch tree =>
                 | Leaf -> false
                 | Node (_, y, left, right) ->
                     if x == y then true
                     else if x < y then contains x left
                     else contains x right

               let tree = Node (Black, 5,
                           Node (Red, 3,
                             Node (Black, 1, Leaf, Leaf),
                             Node (Black, 4, Leaf, Leaf)),
                           Node (Red, 7,
                             Node (Black, 6, Leaf, Leaf),
                             Node (Black, 9, Leaf, Leaf)))
             |}
             ~expr:"contains 6 tree"
             ~expected_value:"true" );
         ( "rb tree complex structure - contains non-existing" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)

               let rec contains x tree =
                 switch tree =>
                 | Leaf -> false
                 | Node (_, y, left, right) ->
                     if x == y then true
                     else if x < y then contains x left
                     else contains x right

               let tree = Node (Black, 5,
                           Node (Red, 3,
                             Node (Black, 1, Leaf, Leaf),
                             Node (Black, 4, Leaf, Leaf)),
                           Node (Red, 7,
                             Node (Black, 6, Leaf, Leaf),
                             Node (Black, 9, Leaf, Leaf)))
             |}
             ~expr:"contains 10 tree"
             ~expected_value:"false" );
         (* Insert function tests *)
         ( "rb tree make_black function type" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)

               let make_black tree =
                 switch tree =>
                 | Leaf -> Leaf
                 | Node (_, x, left, right) -> Node (Black, x, left, right)
             |}
             ~expr:"make_black"
             ~expected_type:"RBTree<'a> -> RBTree<'a>" );
         ( "rb tree make_black - Leaf" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)

               let make_black tree =
                 switch tree =>
                 | Leaf -> Leaf
                 | Node (_, x, left, right) -> Node (Black, x, left, right)
             |}
             ~expr:"make_black Leaf"
             ~expected_value:"Leaf" );
         ( "rb tree make_black - Red node" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)

               let make_black tree =
                 switch tree =>
                 | Leaf -> Leaf
                 | Node (_, x, left, right) -> Node (Black, x, left, right)
             |}
             ~expr:"make_black (Node (Red, 5, Leaf, Leaf))"
             ~expected_value:"Node (Black, 5, Leaf, Leaf)" );
         ( "rb tree insert_aux function type" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)

               let balance tree =
                 switch tree =>
                 | Node (Black, z, Node (Red, y, Node (Red, x, a, b), c), d) ->
                     Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
                 | Node (Black, z, Node (Red, x, a, Node (Red, y, b, c)), d) ->
                     Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
                 | Node (Black, x, a, Node (Red, z, Node (Red, y, b, c), d)) ->
                     Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
                 | Node (Black, x, a, Node (Red, y, b, Node (Red, z, c, d))) ->
                     Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
                 | _ -> tree

               let rec insert_aux x tree =
                 switch tree =>
                 | Leaf -> Node (Red, x, Leaf, Leaf)
                 | Node (color, y, left, right) ->
                     if x < y then
                       balance (Node (color, y, insert_aux x left, right))
                     else if x > y then
                       balance (Node (color, y, left, insert_aux x right))
                     else
                       tree
             |}
             ~expr:"insert_aux"
             ~expected_type:"int -> RBTree<int> -> RBTree<int>" );
         ( "rb tree insert function type" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)

               let balance tree =
                 switch tree =>
                 | Node (Black, z, Node (Red, y, Node (Red, x, a, b), c), d) ->
                     Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
                 | Node (Black, z, Node (Red, x, a, Node (Red, y, b, c)), d) ->
                     Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
                 | Node (Black, x, a, Node (Red, z, Node (Red, y, b, c), d)) ->
                     Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
                 | Node (Black, x, a, Node (Red, y, b, Node (Red, z, c, d))) ->
                     Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
                 | _ -> tree

               let rec insert_aux x tree =
                 switch tree =>
                 | Leaf -> Node (Red, x, Leaf, Leaf)
                 | Node (color, y, left, right) ->
                     if x < y then
                       balance (Node (color, y, insert_aux x left, right))
                     else if x > y then
                       balance (Node (color, y, left, insert_aux x right))
                     else
                       tree

               let make_black tree =
                 switch tree =>
                 | Leaf -> Leaf
                 | Node (_, x, left, right) -> Node (Black, x, left, right)

               let insert x tree =
                 make_black (insert_aux x tree)
             |}
             ~expr:"insert"
             ~expected_type:"int -> RBTree<int> -> RBTree<int>" );
         ( "rb tree insert into empty tree" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)

               let balance tree =
                 switch tree =>
                 | Node (Black, z, Node (Red, y, Node (Red, x, a, b), c), d) ->
                     Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
                 | Node (Black, z, Node (Red, x, a, Node (Red, y, b, c)), d) ->
                     Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
                 | Node (Black, x, a, Node (Red, z, Node (Red, y, b, c), d)) ->
                     Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
                 | Node (Black, x, a, Node (Red, y, b, Node (Red, z, c, d))) ->
                     Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
                 | _ -> tree

               let rec insert_aux x tree =
                 switch tree =>
                 | Leaf -> Node (Red, x, Leaf, Leaf)
                 | Node (color, y, left, right) ->
                     if x < y then
                       balance (Node (color, y, insert_aux x left, right))
                     else if x > y then
                       balance (Node (color, y, left, insert_aux x right))
                     else
                       tree

               let make_black tree =
                 switch tree =>
                 | Leaf -> Leaf
                 | Node (_, x, left, right) -> Node (Black, x, left, right)

               let insert x tree =
                 make_black (insert_aux x tree)
             |}
             ~expr:"insert 5 Leaf"
             ~expected_value:"Node (Black, 5, Leaf, Leaf)" );
         ( "rb tree insert - size increases" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)

               let balance tree =
                 switch tree =>
                 | Node (Black, z, Node (Red, y, Node (Red, x, a, b), c), d) ->
                     Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
                 | Node (Black, z, Node (Red, x, a, Node (Red, y, b, c)), d) ->
                     Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
                 | Node (Black, x, a, Node (Red, z, Node (Red, y, b, c), d)) ->
                     Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
                 | Node (Black, x, a, Node (Red, y, b, Node (Red, z, c, d))) ->
                     Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
                 | _ -> tree

               let rec insert_aux x tree =
                 switch tree =>
                 | Leaf -> Node (Red, x, Leaf, Leaf)
                 | Node (color, y, left, right) ->
                     if x < y then
                       balance (Node (color, y, insert_aux x left, right))
                     else if x > y then
                       balance (Node (color, y, left, insert_aux x right))
                     else
                       tree

               let make_black tree =
                 switch tree =>
                 | Leaf -> Leaf
                 | Node (_, x, left, right) -> Node (Black, x, left, right)

               let insert x tree =
                 make_black (insert_aux x tree)

               let rec size tree =
                 switch tree =>
                 | Leaf -> 0
                 | Node (_, _, left, right) -> 1 + size left + size right

               let tree1 = insert 5 Leaf
               let tree2 = insert 3 tree1
               let tree3 = insert 7 tree2
             |}
             ~expr:"size tree3"
             ~expected_value:"3" );
         ( "rb tree insert multiple - contains all" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)

               let balance tree =
                 switch tree =>
                 | Node (Black, z, Node (Red, y, Node (Red, x, a, b), c), d) ->
                     Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
                 | Node (Black, z, Node (Red, x, a, Node (Red, y, b, c)), d) ->
                     Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
                 | Node (Black, x, a, Node (Red, z, Node (Red, y, b, c), d)) ->
                     Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
                 | Node (Black, x, a, Node (Red, y, b, Node (Red, z, c, d))) ->
                     Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
                 | _ -> tree

               let rec insert_aux x tree =
                 switch tree =>
                 | Leaf -> Node (Red, x, Leaf, Leaf)
                 | Node (color, y, left, right) ->
                     if x < y then
                       balance (Node (color, y, insert_aux x left, right))
                     else if x > y then
                       balance (Node (color, y, left, insert_aux x right))
                     else
                       tree

               let make_black tree =
                 switch tree =>
                 | Leaf -> Leaf
                 | Node (_, x, left, right) -> Node (Black, x, left, right)

               let insert x tree =
                 make_black (insert_aux x tree)

               let rec contains x tree =
                 switch tree =>
                 | Leaf -> false
                 | Node (_, y, left, right) ->
                     if x == y then true
                     else if x < y then contains x left
                     else contains x right

               let tree1 = insert 5 Leaf
               let tree2 = insert 3 tree1
               let tree3 = insert 7 tree2
               let tree4 = insert 1 tree3
               let tree5 = insert 9 tree4
             |}
             ~expr:"contains 5 tree5 && contains 3 tree5 && contains 7 tree5"
             ~expected_value:"true" );
         ( "rb tree insert - duplicate does not increase size" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)

               let balance tree =
                 switch tree =>
                 | Node (Black, z, Node (Red, y, Node (Red, x, a, b), c), d) ->
                     Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
                 | Node (Black, z, Node (Red, x, a, Node (Red, y, b, c)), d) ->
                     Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
                 | Node (Black, x, a, Node (Red, z, Node (Red, y, b, c), d)) ->
                     Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
                 | Node (Black, x, a, Node (Red, y, b, Node (Red, z, c, d))) ->
                     Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
                 | _ -> tree

               let rec insert_aux x tree =
                 switch tree =>
                 | Leaf -> Node (Red, x, Leaf, Leaf)
                 | Node (color, y, left, right) ->
                     if x < y then
                       balance (Node (color, y, insert_aux x left, right))
                     else if x > y then
                       balance (Node (color, y, left, insert_aux x right))
                     else
                       tree

               let make_black tree =
                 switch tree =>
                 | Leaf -> Leaf
                 | Node (_, x, left, right) -> Node (Black, x, left, right)

               let insert x tree =
                 make_black (insert_aux x tree)

               let rec size tree =
                 switch tree =>
                 | Leaf -> 0
                 | Node (_, _, left, right) -> 1 + size left + size right

               let tree1 = insert 5 Leaf
               let tree2 = insert 5 tree1
             |}
             ~expr:"size tree2"
             ~expected_value:"1" );
         ( "rb tree insert - min and max after inserts" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)

               let balance tree =
                 switch tree =>
                 | Node (Black, z, Node (Red, y, Node (Red, x, a, b), c), d) ->
                     Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
                 | Node (Black, z, Node (Red, x, a, Node (Red, y, b, c)), d) ->
                     Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
                 | Node (Black, x, a, Node (Red, z, Node (Red, y, b, c), d)) ->
                     Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
                 | Node (Black, x, a, Node (Red, y, b, Node (Red, z, c, d))) ->
                     Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
                 | _ -> tree

               let rec insert_aux x tree =
                 switch tree =>
                 | Leaf -> Node (Red, x, Leaf, Leaf)
                 | Node (color, y, left, right) ->
                     if x < y then
                       balance (Node (color, y, insert_aux x left, right))
                     else if x > y then
                       balance (Node (color, y, left, insert_aux x right))
                     else
                       tree

               let make_black tree =
                 switch tree =>
                 | Leaf -> Leaf
                 | Node (_, x, left, right) -> Node (Black, x, left, right)

               let insert x tree =
                 make_black (insert_aux x tree)

               let rec minimum tree =
                 switch tree =>
                 | Leaf -> 0
                 | Node (_, x, Leaf, _) -> x
                 | Node (_, _, left, _) -> minimum left

               let rec maximum tree =
                 switch tree =>
                 | Leaf -> 0
                 | Node (_, x, _, Leaf) -> x
                 | Node (_, _, _, right) -> maximum right

               let tree1 = insert 5 Leaf
               let tree2 = insert 3 tree1
               let tree3 = insert 7 tree2
               let tree4 = insert 1 tree3
               let tree5 = insert 9 tree4
             |}
             ~expr:"minimum tree5 == 1 && maximum tree5 == 9"
             ~expected_value:"true" );
       ]

(* ============================================================================
   SUM TYPE CONSTRUCTOR TYPE INFERENCE TESTS

   These tests specifically verify that sum type constructors have correct
   type inference, especially when:
   1. Constructors reference other sum types (not type parameters)
   2. Multiple sum types are defined and used together
   3. Concrete types should not become polymorphic type variables

   These tests would catch the bug where sum types were represented with
   TypeVar dummy bodies, causing constructor types to incorrectly generalize
   concrete type references.
   ============================================================================ *)

let sum_type_constructor_inference_tests =
  let open ProgramTesting in
  "sum_type_constructor_inference"
  >::: [
         (* Test that a simple sum type constructor has the correct type *)
         ( "Color constructor type - Red" >:: fun _ ->
           assert_expression_has_type
             ~program:{|
               type Color = | Red | Black
             |}
             ~expr:"Red"
             ~expected_type:"Color" );

         ( "Color constructor type - Black" >:: fun _ ->
           assert_expression_has_type
             ~program:{|
               type Color = | Red | Black
             |}
             ~expr:"Black"
             ~expected_type:"Color" );

         (* Test that a constructor with payload referencing another sum type
            has the correct type - this is the key test for the bug! *)
         ( "Node constructor type with Color parameter" >:: fun _ ->
           assert_expression_has_type
             ~program:{|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)
             |}
             ~expr:"Node"
             ~expected_type:"(Color, 'a, RBTree<'a>, RBTree<'a>) -> RBTree<'a>" );

         (* Verify that Color is NOT a type variable when used *)
         ( "Node constructor applied to Red" >:: fun _ ->
           assert_expression_has_type
             ~program:{|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)
             |}
             ~expr:"Node (Red, 5, Leaf, Leaf)"
             ~expected_type:"RBTree<int>" );

         (* Test multiple sum types referencing each other *)
         ( "constructor with multiple sum type parameters" >:: fun _ ->
           assert_expression_has_type
             ~program:{|
               type Status = | Active | Inactive
               type Priority = | High | Low
               type rec Task<a> =
                 | Task of (Status, Priority, a)
             |}
             ~expr:"Task"
             ~expected_type:"(Status, Priority, 'a) -> Task<'a>" );

         (* Verify concrete evaluation *)
         ( "Node with Red evaluates correctly" >:: fun _ ->
           assert_expression_has_value
             ~program:{|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)
             |}
             ~expr:"Node (Red, 5, Leaf, Leaf)"
             ~expected_value:"Node (Red, 5, Leaf, Leaf)" );

         (* Test that we can pattern match on the concrete Color type *)
         ( "pattern match on Color in Node" >:: fun _ ->
           assert_expression_has_value
             ~program:{|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)
             |}
             ~expr:{|
               switch Node (Red, 5, Leaf, Leaf) =>
               | Leaf -> 0
               | Node (Red, x, _, _) -> x
               | Node (Black, x, _, _) -> ~-x
             |}
             ~expected_value:"5" );

         ( "pattern match on Black in Node" >:: fun _ ->
           assert_expression_has_value
             ~program:{|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)
             |}
             ~expr:{|
               switch Node (Black, 5, Leaf, Leaf) =>
               | Leaf -> 0
               | Node (Red, x, _, _) -> x
               | Node (Black, x, _, _) -> ~-x
             |}
             ~expected_value:"-5" );

         (* Test constructor with multiple concrete sum types *)
         ( "constructor with two concrete sum types" >:: fun _ ->
           assert_expression_has_type
             ~program:{|
               type Color = | Red | Black
               type Shape = | Circle | Square
               type Decoration = | Decor of (Color, Shape)
             |}
             ~expr:"Decor"
             ~expected_type:"(Color, Shape) -> Decoration" );

         (* Test that we can use the constructor correctly *)
         ( "apply constructor with concrete sum types" >:: fun _ ->
           assert_expression_has_value
             ~program:{|
               type Color = | Red | Black
               type Shape = | Circle | Square
               type Decoration = | Decor of (Color, Shape)
             |}
             ~expr:"Decor (Red, Circle)"
             ~expected_value:"Decor (Red, Circle)" );

         (* Test nested sum types with concrete references *)
         ( "nested sum type constructors" >:: fun _ ->
           assert_expression_has_type
             ~program:{|
               type Size = | Small | Large
               type Color = | Red | Black
               type Colored<a> = | Colored of (Color, a)
             |}
             ~expr:"Colored"
             ~expected_type:"(Color, 'a) -> Colored<'a>" );

         (* Test that concrete types in tuple payloads work *)
         ( "tuple payload with multiple concrete types" >:: fun _ ->
           assert_expression_has_type
             ~program:{|
               type A = | A1 | A2
               type B = | B1 | B2
               type C = | C1 | C2
               type Combined = | Combo of (A, B, C, int)
             |}
             ~expr:"Combo"
             ~expected_type:"(A, B, C, int) -> Combined" );

         (* Test function taking constructor as argument *)
         ( "function with constructor parameter" >:: fun _ ->
           assert_expression_has_type
             ~program:{|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)

               let make_red_node x = Node (Red, x, Leaf, Leaf)
             |}
             ~expr:"make_red_node"
             ~expected_type:"'a -> RBTree<'a>" );

         (* Test that type checking rejects wrong concrete types *)
         ( "type error when using wrong sum type" >:: fun _ ->
           assert_program_fails_typecheck
             {|
               type Color = | Red | Black
               type Size = | Big | Small
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)

               let bad_node = Node (Big, 5, Leaf, Leaf)
             |} );

         (* Test with parameterized sum types *)
         ( "parameterized sum type with concrete type reference" >:: fun _ ->
           assert_expression_has_type
             ~program:{|
               type Tag = | Important | Normal
               type Wrapper<a> = | Wrap of (Tag, a)
             |}
             ~expr:"Wrap"
             ~expected_type:"(Tag, 'a) -> Wrapper<'a>" );

         (* Test complex nested structure *)
         ( "complex nested sum types" >:: fun _ ->
           assert_expression_has_type
             ~program:{|
               type Status = | Active | Inactive
               type Priority = | High | Low | Medium
               type rec TaskList<a> =
                 | Empty
                 | Task of (Status, Priority, a, TaskList<a>)
             |}
             ~expr:"Task"
             ~expected_type:"(Status, Priority, 'a, TaskList<'a>) -> TaskList<'a>" );

         (* Test that pattern matching works with concrete types *)
         ( "pattern match extracts concrete sum type" >:: fun _ ->
           assert_expression_has_value
             ~program:{|
               type Status = | Active | Inactive
               type Priority = | High | Low
               type Task = | Task of (Status, Priority, int)

               let get_priority t =
                 switch t =>
                 | Task (_, High, _) -> 1
                 | Task (_, Low, _) -> 0
             |}
             ~expr:"get_priority (Task (Active, High, 42))"
             ~expected_value:"1" );

         (* Test sum type in higher-order function *)
         ( "sum type constructor in map" >:: fun _ ->
           assert_expression_has_type
             ~program:{|
               type Color = | Red | Black
               type Colored<a> = | Colored of (Color, a)
             |}
             ~expr:{|
               let colorize c x = Colored (c, x) in
               colorize Red
             |}
             ~expected_type:"'a -> Colored<'a>" );

         (* Ensure Red-Black tree functions work correctly *)
         ( "rb tree contains function type" >:: fun _ ->
           assert_expression_has_type
             ~program:{|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)

               let rec contains x tree =
                 switch tree =>
                 | Leaf -> false
                 | Node (_, y, left, right) ->
                     if x == y then true
                     else if x < y then contains x left
                     else contains x right
             |}
             ~expr:"contains"
             ~expected_type:"int -> RBTree<int> -> bool" );

         (* Test balance function type *)
         ( "rb tree balance function type" >:: fun _ ->
           assert_expression_has_type
             ~program:{|
               type Color = | Red | Black
               type rec RBTree<a> =
                 | Leaf
                 | Node of (Color, a, RBTree<a>, RBTree<a>)

               let balance tree =
                 switch tree =>
                 | Node (Black, z, Node (Red, y, Node (Red, x, a, b), c), d) ->
                     Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
                 | _ -> tree
             |}
             ~expr:"balance"
             ~expected_type:"RBTree<'a> -> RBTree<'a>" );
       ]

(* ============================================================================
   CUSTOM INFIX OPERATOR TESTS

   Tests for custom binary operators defined with parenthesized syntax like:
     let (+++) x y = x + y + y

   Covers:
   - Type inference for custom operators
   - Evaluation of custom operators
   - Different precedence levels (additive, multiplicative, relational)
   - Partial application
   - Custom operators with various types
   ============================================================================ *)

let custom_operator_type_tests =
  let open ProgramTesting in
  "custom_operator_types"
  >::: [
         (* Additive operators (start with + or -) *)
         ( "custom additive operator type" >:: fun _ ->
           assert_expression_has_type
             ~program:{|
               let (+++) x y = x + y + y
             |}
             ~expr:"+++"
             ~expected_type:"int -> int -> int" );

         ( "custom additive operator with different implementation" >:: fun _ ->
           assert_expression_has_type
             ~program:{|
               let (+-+) a b = a + b + 1
             |}
             ~expr:"+-+"
             ~expected_type:"int -> int -> int" );

         (* Multiplicative operators (start with * / %) *)
         ( "custom multiplicative operator type" >:: fun _ ->
           assert_expression_has_type
             ~program:{|
               let (***) x y = x * x * y
             |}
             ~expr:"***"
             ~expected_type:"int -> int -> int" );

         ( "custom division-based operator type" >:: fun _ ->
           assert_expression_has_type
             ~program:{|
               let (///) x y = x / y + x % y
             |}
             ~expr:"///"
             ~expected_type:"int -> int -> int" );

         (* Relational operators (start with < > =) *)
         ( "custom relational operator type" >:: fun _ ->
           assert_expression_has_type
             ~program:{|
               let (===) x y = x == y
             |}
             ~expr:"==="
             ~expected_type:"int -> int -> bool" );

         ( "custom less-than operator type" >:: fun _ ->
           assert_expression_has_type
             ~program:{|
               let (<<) x y = x < y - 1
             |}
             ~expr:"<<"
             ~expected_type:"int -> int -> bool" );

         (* Polymorphic custom operators *)
         ( "polymorphic custom operator" >:: fun _ ->
           assert_expression_has_type
             ~program:{|
               let (<=>) x y = if x == y then 1 else 0
             |}
             ~expr:"<=>"
             ~expected_type:"int -> int -> int" );

         (* Custom operator with type annotations *)
         ( "custom operator with type annotation" >:: fun _ ->
           assert_expression_has_type
             ~program:{|
               let (+*+) [int -> int -> int] = \x [int] -> \y [int] -> x + y * 2
             |}
             ~expr:"+*+"
             ~expected_type:"int -> int -> int" );

         (* Custom operator usage in expressions *)
         ( "expression using custom additive operator" >:: fun _ ->
           assert_expression_has_type
             ~program:{|
               let (+++) x y = x + y + y
             |}
             ~expr:"5 +++ 3"
             ~expected_type:"int" );

         ( "expression using custom multiplicative operator" >:: fun _ ->
           assert_expression_has_type
             ~program:{|
               let (***) x y = x * x * y
             |}
             ~expr:"3 *** 2"
             ~expected_type:"int" );

         ( "expression using custom relational operator" >:: fun _ ->
           assert_expression_has_type
             ~program:{|
               let (===) x y = x == y
             |}
             ~expr:"5 === 5"
             ~expected_type:"bool" );

         (* Partial application *)
         ( "partial application of custom operator" >:: fun _ ->
           assert_expression_has_type
             ~program:{|
               let (+++) x y = x + y + y
               let add_six = (+++) 2
             |}
             ~expr:"add_six"
             ~expected_type:"int -> int" );

         ( "partial application result" >:: fun _ ->
           assert_expression_has_type
             ~program:{|
               let (+++) x y = x + y + y
               let add_six = (+++) 2
             |}
             ~expr:"add_six 3"
             ~expected_type:"int" );

         (* Multiple custom operators *)
         ( "multiple custom operators in program" >:: fun _ ->
           assert_expression_has_type
             ~program:{|
               let (+++) x y = x + y + y
               let (***) x y = x * x * y
               let (===) x y = x == y
             |}
             ~expr:"+++"
             ~expected_type:"int -> int -> int" );

         ( "expression with multiple custom operators" >:: fun _ ->
           assert_expression_has_type
             ~program:{|
               let (+++) x y = x + y + y
               let (***) x y = x * x * y
             |}
             ~expr:"2 *** 3 +++ 4"
             ~expected_type:"int" );

         (* Recursive custom operators *)
         ( "recursive custom operator type" >:: fun _ ->
           assert_expression_has_type
             ~program:{|
               let rec (***>) x y =
                 if x == 0 then 0
                 else if x == 1 then y
                 else y + (x - 1) ***> y
             |}
             ~expr:"***>"
             ~expected_type:"int -> int -> int" );

         (* Custom operator with bool return *)
         ( "custom operator returning bool" >:: fun _ ->
           assert_expression_has_type
             ~program:{|
               let (>><) x y = x > y && y > 0
             |}
             ~expr:">><"
             ~expected_type:"int -> int -> bool" );

         (* Mixed precedence operators *)
         ( "mixed precedence custom operators" >:: fun _ ->
           assert_expression_has_type
             ~program:{|
               let (</>) x y = x / y + 1
               let (<+>) x y = x + y * 2
             |}
             ~expr:"10 </> 3 <+> 2"
             ~expected_type:"int" );
       ]

let custom_operator_evaluation_tests =
  let open ProgramTesting in
  "custom_operator_evaluation"
  >::: [
         (* Basic evaluation *)
         ( "evaluate custom additive operator" >:: fun _ ->
           assert_expression_has_value
             ~program:{|
               let (+++) x y = x + y + y
             |}
             ~expr:"5 +++ 3"
             ~expected_value:"11" );

         ( "evaluate custom multiplicative operator" >:: fun _ ->
           assert_expression_has_value
             ~program:{|
               let (***) x y = x * x * y
             |}
             ~expr:"3 *** 2"
             ~expected_value:"18" );

         ( "evaluate custom division operator" >:: fun _ ->
           assert_expression_has_value
             ~program:{|
               let (///) x y = x / y + x % y
             |}
             ~expr:"17 /// 5"
             ~expected_value:"5" );

         ( "evaluate custom relational operator - true" >:: fun _ ->
           assert_expression_has_value
             ~program:{|
               let (===) x y = x == y
             |}
             ~expr:"5 === 5"
             ~expected_value:"true" );

         ( "evaluate custom relational operator - false" >:: fun _ ->
           assert_expression_has_value
             ~program:{|
               let (===) x y = x == y
             |}
             ~expr:"5 === 3"
             ~expected_value:"false" );

         (* Complex expressions *)
         ( "custom operator in complex expression" >:: fun _ ->
           assert_expression_has_value
             ~program:{|
               let (+++) x y = x + y + y
             |}
             ~expr:"1 + 2 +++ 3"
             ~expected_value:"9" );

         ( "multiple custom operators" >:: fun _ ->
           assert_expression_has_value
             ~program:{|
               let (+++) x y = x + y + y
               let (***) x y = x * x * y
             |}
             ~expr:"2 *** 3 +++ 4"
             ~expected_value:"16" );

         (* Partial application evaluation *)
         ( "partial application evaluation" >:: fun _ ->
           assert_expression_has_value
             ~program:{|
               let (+++) x y = x + y + y
               let add_double = (+++) 2
             |}
             ~expr:"add_double 3"
             ~expected_value:"7" );

         (* Recursive custom operators *)
         ( "recursive custom operator - base case" >:: fun _ ->
           assert_expression_has_value
             ~program:{|
               let rec (***>) x y =
                 if x == 0 then 0
                 else if x == 1 then y
                 else y + (x - 1) ***> y
             |}
             ~expr:"0 ***> 5"
             ~expected_value:"0" );

         ( "recursive custom operator - recursive case" >:: fun _ ->
           assert_expression_has_value
             ~program:{|
               let rec (***>) x y =
                 if x == 0 then 0
                 else if x == 1 then y
                 else y + (x - 1) ***> y
             |}
             ~expr:"4 ***> 3"
             ~expected_value:"12" );

         (* Custom operators with conditionals *)
         ( "custom operator with conditional logic" >:: fun _ ->
           assert_expression_has_value
             ~program:{|
               let (***) x y =
                 if x == 0 then y
                 else x * y
             |}
             ~expr:"0 *** 100"
             ~expected_value:"100" );

         ( "custom operator with conditional - non-zero" >:: fun _ ->
           assert_expression_has_value
             ~program:{|
               let (***) x y =
                 if x == 0 then y
                 else x * y
             |}
             ~expr:"5 *** 3"
             ~expected_value:"15" );

         (* Precedence testing *)
         ( "multiplicative custom operator precedence" >:: fun _ ->
           assert_expression_has_value
             ~program:{|
               let (***) x y = x * x * y
             |}
             ~expr:"2 *** 3 + 4"
             ~expected_value:"16" );

         ( "additive custom operator precedence" >:: fun _ ->
           assert_expression_has_value
             ~program:{|
               let (+++) x y = x + y + y
             |}
             ~expr:"2 * 3 +++ 4"
             ~expected_value:"14" );

         (* Custom operators in let expressions *)
         ( "custom operator in let binding" >:: fun _ ->
           assert_expression_has_value
             ~program:{|
               let (+++) x y = x + y + y
               let result = 5 +++ 3
             |}
             ~expr:"result"
             ~expected_value:"11" );

         (* Custom operators with function application *)
         ( "custom operator with function application" >:: fun _ ->
           assert_expression_has_value
             ~program:{|
               let (+++) x y = x + y + y
               let double n = n * 2
             |}
             ~expr:"double 2 +++ 3"
             ~expected_value:"10" );

         (* Chaining custom operators *)
         ( "chaining same custom operator" >:: fun _ ->
           assert_expression_has_value
             ~program:{|
               let (+++) x y = x + y + 1
             |}
             ~expr:"1 +++ 2 +++ 3"
             ~expected_value:"8" );

         (* Mixed precedence *)
         ( "mixed precedence evaluation" >:: fun _ ->
           assert_expression_has_value
             ~program:{|
               let (</>) x y = x / y + 1
               let (<+>) x y = x + y * 2
             |}
             ~expr:"10 </> 3 <+> 2"
             ~expected_value:"8" );

         (* Boolean custom operators *)
         ( "custom boolean operator - and variant" >:: fun _ ->
           assert_expression_has_value
             ~program:{|
               let (==>) x y = x == y || y > 10
             |}
             ~expr:"5 ==> 15"
             ~expected_value:"true" );

         ( "custom boolean operator - complex" >:: fun _ ->
           assert_expression_has_value
             ~program:{|
               let (<<>>) x y = x < y && y < x + 10
             |}
             ~expr:"5 <<>> 7"
             ~expected_value:"true" );

         (* Using custom operators in higher-order functions *)
         ( "custom operator in lambda" >:: fun _ ->
           assert_expression_has_value
             ~program:{|
               let (+++) x y = x + y + y
               let apply_op f a b = f a b
             |}
             ~expr:"apply_op (+++) 2 3"
             ~expected_value:"7" );
       ]

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
      List.map (fun (a, b) -> eval_test a b) block_tests;
      block_type_tests;
      [ program_typecheck_tests ];
      [ program_expression_type_tests ];
      [ program_expression_value_tests ];
      [ type_evaluation_tests ];
      [ sum_type_evaluation_tests ];
      [ sum_type_constructor_inference_tests ];
      [ red_black_tree_tests ];
      [ custom_operator_type_tests ];
      [ custom_operator_evaluation_tests ];
    ]

let suite = "suite" >::: all_tests
let () = run_test_tt_main suite
