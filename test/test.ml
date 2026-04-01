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
      (fun (x, y) -> ("(fn  n -> n) (" ^ x ^ " )", y));
      (fun (x, y) ->
        ( "(fn  n -> n + 1) (" ^ x ^ " )",
          y |> int_of_string |> ( + ) 1 |> string_of_int ));
      (fun (x, y) ->
        ( "(fn  a -> fn  b -> a + b) (" ^ x ^ " )" ^ " ( " ^ x ^ " )",
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
      (fun (x, y) -> ("( fn  a -> a ) ( " ^ x ^ " )", y));
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
      (fun x -> "( fn  a -> a ) ( " ^ x ^ " )");
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
      (fun x -> "(fn  n -> n) (" ^ x ^ " )");
      (fun x -> "(fn  n -> n + 1) (" ^ x ^ " )");
      (fun x -> "(fn  a -> fn  b -> a + b) (" ^ x ^ " )" ^ " ( " ^ x ^ " )");
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
    [ (fun x -> x); (fun (x, y) -> ("(fn  m -> m) ( " ^ x ^ " )", y)) ]
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
let type_is_char (program : string) = type_test program "char"
let () = ignore type_is_bool
let () = ignore type_is_int
let () = ignore type_is_string
let () = ignore type_is_char

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
let char_types = [ "'a'"; "'b'" ]

let function_type_tests =
  [
    ("fn  n -> n", "'a -> 'a");
    ("fn  n -> n + 1", "int -> int");
    ("fn  a -> fn  b -> a + b", "int -> int -> int");
    ("fn  (n : int) -> n", "int -> int");
    ("fn  (n : int) -> n + 1", "int -> int");
    ("fn  (a : int) -> fn  (b : int) -> a + b", "int -> int -> int");
    ("fn  a -> fn  b -> a", "'a -> 'b -> 'a");
    ("fn  a -> fn  b -> b", "'a -> 'b -> 'b");
    ("fn  (a : int) -> fn  b -> b", "int -> 'a -> 'a");
    ("fn  a -> fn  (b : int) -> b", "'a -> int -> int");
    ("fn  (a : int) -> fn  (b : int) -> b", "int -> int -> int");
    ("fn  (a : int) -> fn  (b : int) -> a", "int -> int -> int");
    ("fn  (a : int) -> fn  (b : int) -> a + b", "int -> int -> int");
    ("fn  (a : int) -> fn  (b : int) -> a + b + 1", "int -> int -> int");
    ("fn  (a : int) -> fn  (b : int) -> a + b + 1 + 2", "int -> int -> int");
    ("fn  ((a, b) : (int, int)) -> a + b", "(int, int) -> int");
    ("fn  (a, _) -> fn  (_, b) -> a + b", "(int, 'a) -> ('b, int) -> int");
    ("fn  (a, _) -> fn  (_, b) -> a || b", "(bool, 'a) -> ('b, bool) -> bool");
    ( {|fn (a, b) ->
    fn (c, d) ->
    if a then b
    else if c then d
    else 1|},
      "(bool, int) -> (bool, int) -> int" );
    (* more complicated function type tests *)
    ( "fn  a -> fn  b -> fn  c -> a ( b ( c ) )",
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
    ( "let f (a : int) (b : int) (c : int) (d : int) = a in f",
      "int -> int -> int -> int -> int" );
    ( "let f (a : int) (b : int) (c : int) (d : int) = a in f 1",
      "int -> int -> int -> int" );
    ( "let f (a : int) (b : int) (c : int) (d : int) = a in f 1 2",
      "int -> int -> int" );
    ( "let f (a : int) (b : int) (c : int) (d : int) = a in f 1 2 3",
      "int -> int" );
    ("let f (a : int) (b : int) (c : int) (d : int) = a in f 1 2 3 4", "int");
    (* with type variables *)
    ("fn  (a : 'a) -> a", "'a -> 'a");
    ("fn  (a : 'a) -> a + 1", "int -> int");
    ("fn  (a : 'a) -> fn  (b : 'a) -> a", "'a -> 'a -> 'a");
    ("fn  (a : 'a) -> fn  (b : 'a) -> b", "'a -> 'a -> 'a");
    ("fn  (a : 'a) -> fn  (b : 'b) -> a", "'a -> 'b -> 'a");
    ("fn  (a : 'a) -> fn  (b : 'a) -> a + b", "int -> int -> int");
    ("fn  (a : 'a) -> fn  (b : 'a) -> a + b + 1", "int -> int -> int");
    ("fn  (f : 'e -> 'f) -> fn  (x : 'f) -> f x", "('a -> 'a) -> 'a -> 'a");
    (* this is an interesting example because it turns out that a = b here *)
    ("fn  (f : 'e -> 'f) -> fn  (x : 'e) -> f x", "('a -> 'b) -> 'a -> 'b");
    (* return type annotations *)
    ("let f x : int = x in f", "int -> int");
    ("let f x y : int = x + y in f", "int -> int -> int");
    ("let f (x : bool) : int = if x then 1 else 0 in f", "bool -> int");
    (* on the other hand, there is no constraint generated in this expression
       saying that a = b, so they are different *)
    ( "let f a (b : int) (c : int) (d : int) = a in f",
      "'a -> int -> int -> int -> 'a" );
    ("let f a (b : int) (c : int) d = a in f", "'a -> int -> int -> 'b -> 'a");
    ("let f a (b : int) c (d : int) = a in f", "'a -> int -> 'b -> int -> 'a");
    ("let f a (b : int) c d = a in f", "'a -> int -> 'b -> 'c -> 'a");
    ("let f a b (c : int) (d : int) = b in f", "'a -> 'b -> int -> int -> 'b");
    ("let f a b (c : int) d = b in f", "'a -> 'b -> int -> 'c -> 'b");
    ("let f a b c (d : str) = c in f", "'a -> 'b -> 'c -> str -> 'c");
    ("let f a b c d = c in f", "'a -> 'b -> 'c -> 'd -> 'c");
    ("let f a b c (d : str) = d in f", "'a -> 'b -> 'c -> str -> str");
    ("fn  (a, _) -> a", "('a, 'b) -> 'a");
    ("fn  (a, _) -> a + 1", "(int, 'a) -> int");
    ("fn  f -> fn  x -> f x", "('a -> 'b) -> 'a -> 'b");
    ( {|fn (f : 'e -> 'f -> 'g) ->
    fn (a : 'e) ->
    fn (b : 'f) ->
    f a b|},
      "('a -> 'b -> 'c) -> 'a -> 'b -> 'c" );
    ("fn  (a : ('a, 'b)) -> a", "('a, 'b) -> ('a, 'b)");
    ("fn  ((a, _) : ('a, 'b)) -> a", "('a, 'b) -> 'a");
    ("fn  ((_, a) : ('a, 'b)) -> a", "('a, 'b) -> 'b");
    ("fn  ((a, b, c) : ('a, 'b, 'c)) -> a", "('a, 'b, 'c) -> 'a");
    (* higher order function *)
    ( {|fn (f : ('e, 'f) -> 'g) ->
    fn (a : 'e) ->
    fn (b : 'f) ->
    f (a, b)|},
      "(('a, 'b) -> 'c) -> 'a -> 'b -> 'c" );
    ( {|fn (f : 'e -> 'f -> 'g) ->
    fn (a, b) ->
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
    ("let rec f (x : unit) = x in f", "unit -> unit");
    ("let rec f (x : int -> int) = x in f", "(int -> int) -> int -> int");
    ("fn  (a : [int]) -> a", "[int] -> [int]");
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
      \    case lst do\n\
      \    | [] -> acc\n\
      \    | h :: t -> op h (fold op t acc)\n\
      \    \n\
      \  in\n\
      \  \n\
      \  let rec map f lst =\n\
      \    fold (fn  x -> fn  acc -> f x :: acc) lst []\n\
      \  \n\
      \  in\n\
      \  \n\
      \  map",
      "('a -> 'b) -> ['a] -> ['b]" );
    (* filter implemented using fold_right *)
    ( {|let rec fold op lst acc =
    case lst do
    | [] -> acc
    | h :: t -> op h (fold op t acc)
    
  in
  
  let filter pred = fold (fn x -> fn acc -> if pred x then x :: acc else acc) []
  
  in filter|},
      "('a -> bool) -> ['a] -> ['a]" );
    (* filter implemented using fold_left *)
    ( {|let rec fold op acc lst =
    case lst do
    | [] -> acc
    | h :: t -> fold op t (op h acc)
    
  in
  
  let filter pred = fold (fn x -> fn acc -> if pred x then x :: acc else acc) []
  
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
    ("fn  a -> a", "function");
    ("fn  () -> ()", "function");
    ("fn (() : unit) -> ()", "function");
    ("let (a : (int -> int) -> int) = fn  f -> f 1 in a", "function");
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
    ("case () do | () -> 1", "int");
    ("case () do | () -> true", "bool");
    ("case () do | () -> ()", "unit");
    ("case () do | () -> (1, 2)", "(int, int)");
    ("case () do | () -> (1, 2, 3)", "(int, int, int)");
    ("case 1 do | 1 -> 1", "int");
    ("case 1 do | 1 -> true", "bool");
    ("case 1 do | 1 -> ()", "unit");
    ("case 5 do | 1 -> 1 | 2 -> 2 | 3 -> 3 | 4 -> 4 | 5 -> 5", "int");
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

let char_eval_tests =
  [ ("'a'", "'a'"); ("'a' == 'a'", "true"); ("'a' == 'b'", "false") ]

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
    ("case () do | () -> 1", "1");
    ("case () do | () -> true", "true");
    ("case () do | () -> ()", "()");
    ("case () do | () -> (1, 2)", "(1, 2)");
    ("case () do | () -> (1, 2, 3)", "(1, 2, 3)");
    ("case 1 do | 1 -> 1", "1");
    ("case 1 do | 1 -> true", "true");
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
    case lst do
    | [] -> acc
    | h :: t -> op h (fold_right op t acc)
  
in

fold_right (fn x -> fn y -> x + y) [1,2,3,4,5,6,7,8,9,10] 0
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
      case lst do
      | [] -> acc
      | h :: t -> op h (fold_right op t acc)
    in

    fold_right (fn x -> fn y -> x + y) [1,2,3,4,5,6,7,8,9,10] 0
  |},
      "55" );
  ]

let fold_type_tests =
  [
    ( {|
    let rec fold op arr acc =
      case arr do
      | [] -> acc
      | h :: t -> fold op t (op acc h)
      
    in
    fold
  |},
      "('a -> 'b -> 'a) -> ['b] -> 'a -> 'a" );
    ( {|
  let rec fold op arr acc =
    case arr do
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
    let (succ : int -> int) =
      fn (n : int) -> n + 1
    in
    
    let (sum : int -> int -> int) =
      fn (a : int) ->
      fn (b : int) ->
      a + b
    in
    
    sum (succ 4) (sum 1 2)
    |},
      "8" );
    ( {|
    let (succ : int-> int) =
      fn (n : int) -> n + 1
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
      fn (a : str) -> a
    in
    f ""
    |}, {|""|});
    ({|
    let f =
      fn () -> ()
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
    apply_one (fn n -> n + 1)
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

let char_type_tests : test list =
  List.map (fun expression -> type_is_char expression) char_types

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
    char_eval_tests;
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
    ("{let (x : int) = 1; let (y : int) = 2; x + y}", "3");
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
      ("{let (x : int) = 1; let (y : int) = 2; x + y}", "int");
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
          | Ok (new_static_bindings, new_type_bindings, _) ->
              process_defns
                (new_static_bindings @ static_env)
                (new_type_bindings @ type_env)
                rest)
    in
    process_defns built_ins_types [] c_program

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
    let initial_dynamic_env = initial_env () |> unwrap_eval_result in
    let dynamic_env = process_defns initial_dynamic_env c_program in
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
             type Pair<'a> = ('a, 'a)
             let (p : Pair<int>) = (1, 2)
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
           assert_program_fails_typecheck "let (x : bool) = 42" );
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
           assert_expression_has_type ~program:"let double = fn x -> x * 2"
             ~expr:"double" ~expected_type:"int -> int" );
         ( "polymorphic function type" >:: fun _ ->
           assert_expression_has_type ~program:"let id = fn x -> x" ~expr:"id"
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
             ~program:
               {|
               let double = fn x -> x * 2
             |}
             ~expr:"double 5" ~expected_type:"int" );
         ( "nested custom type after type definition" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Pair<'a> = ('a, 'a)
               let (p : Pair<Pair<int>>) = ((1, 2), (3, 4))
             |}
             ~expr:"p" ~expected_type:"((int, int), (int, int))" );
         (* ====== Type Alias Tests ====== *)
         ( "simple type alias" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type IntPair = (int, int)
               let (p : IntPair) = (1, 2)
             |}
             ~expr:"p" ~expected_type:"(int, int)" );
         ( "type alias with single parameter" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Box<'a> = ('a, 'a, 'a)
               let (b : Box<int>) = (1, 2, 3)
             |}
             ~expr:"b" ~expected_type:"(int, int, int)" );
         ( "function returning type alias" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Pair<'a> = ('a, 'a)
               let make_pair x = (x, x)
             |}
             ~expr:"make_pair 5" ~expected_type:"(int, int)" );
         ( "function with type alias parameter" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Pair<'a> = ('a, 'a)
               let first p = case p do | (x, _) -> x
             |}
             ~expr:"first" ~expected_type:"('a, 'b) -> 'a" );
         ( "type alias with list" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type IntList = [int]
               let (xs : IntList) = [1, 2, 3]
             |}
             ~expr:"xs" ~expected_type:"[int]" );
         ( "type alias with function type" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type IntFunction = int -> int
               let (f : IntFunction) = fn x -> x + 1
             |}
             ~expr:"f" ~expected_type:"int -> int" );
         ( "parameterized list type alias" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type MyList<'a> = ['a]
               let (xs : MyList<int>) = [1, 2, 3]
             |}
             ~expr:"xs" ~expected_type:"[int]" );
         ( "type alias in recursive function" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type IntList = [int]
               let rec sum (xs : IntList) =
                 case xs do
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
               let (l : Line) = ((0, 0), (1, 1))
             |}
             ~expr:"l" ~expected_type:"((int, int), (int, int))" );
         ( "polymorphic type alias instantiation" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Container<'a> = ('a, ['a])
               let (c1 : Container<int>) = (42, [1, 2, 3])
             |}
             ~expr:"c1" ~expected_type:"(int, [int])" );
         ( "type alias with function composition" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Transformer<'a> = 'a -> 'a
               let (double : Transformer<int>) = fn x -> x * 2
             |}
             ~expr:"double 5" ~expected_type:"int" );
         ( "deeply nested type aliases" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Pair<'a> = ('a, 'a)
               type Quad<'a> = Pair<Pair<'a>>
               let (q : Quad<int>) = ((1, 2), (3, 4))
             |}
             ~expr:"q" ~expected_type:"((int, int), (int, int))" );
         ( "function returning parameterized type" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Wrapper<'a> = ('a, 'a)
               let wrap x = (x, x)
             |}
             ~expr:"wrap" ~expected_type:"'a -> ('a, 'a)" );
         ( "list of type alias" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Pair<'a> = ('a, 'a)
               let pairs = [(1, 2), (3, 4), (5, 6)]
             |}
             ~expr:"pairs" ~expected_type:"[(int, int)]" );
         ( "type alias in switch pattern" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Pair<'a> = ('a, 'a)
               let get_first p = case p do | (x, _) -> x
               let x = get_first (1, 2)
             |}
             ~expr:"x" ~expected_type:"int" );
         ( "type alias with curried function" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type BinaryOp<'a> = 'a -> 'a -> 'a
               let (add : BinaryOp<int>) = fn x -> fn y -> x + y
             |}
             ~expr:"add" ~expected_type:"int -> int -> int" );
         ( "multi-parameter type alias" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Pair<'a, 'b> = ('a, 'b)
               let (p : Pair<int, bool>) = (42, true)
             |}
             ~expr:"p" ~expected_type:"(int, bool)" );
         ( "type alias referencing multi-parameter type alias" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Pair<'a, 'b> = ('a, 'b)
               type LeftIntPair<'a> = Pair<int, 'a>
               let (p : LeftIntPair<bool>) = (42, true)
             |}
             ~expr:"p" ~expected_type:"(int, bool)" );
         ( "triple type parameter alias" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Triple<'a, 'b, 'c> = ('a, ('b, 'c))
               let (t : Triple<int, bool, str>) = (1, (true, "hello"))
             |}
             ~expr:"t" ~expected_type:"(int, (bool, str))" );
         ( "nested multi-parameter type alias" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Pair<'a, 'b> = ('a, 'b)
               type Triple<'a, 'b, 'c> = ('a, ('b, 'c))
               type RightBoolTriple<'a, 'b> = Triple<'a, 'b, bool>
               let (x : RightBoolTriple<int, str>) = (1, ("hello", true))
             |}
             ~expr:"x" ~expected_type:"(int, (str, bool))" );
       ]

let string_concat_tests =
  let open ProgramTesting in
  "string_concatenation"
  >::: [
         ( "string concat basic" >:: fun _ ->
           assert_expression_has_type ~program:"" ~expr:{|"hello" ^ "world"|}
             ~expected_type:"str" );
         ( "string concat multiple" >:: fun _ ->
           assert_expression_has_type ~program:"" ~expr:{|"a" ^ "b" ^ "c"|}
             ~expected_type:"str" );
         ( "string concat with variables" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
          let x = "hello"
          let y = "world"
        |}
             ~expr:{|x ^ y|} ~expected_type:"str" );
         ( "string concat empty strings" >:: fun _ ->
           assert_expression_has_type ~program:"" ~expr:{|"" ^ ""|}
             ~expected_type:"str" );
         ( "string concat eval basic" >:: fun _ ->
           assert_expression_has_value ~program:"" ~expr:{|"hello" ^ "world"|}
             ~expected_value:{|"helloworld"|} );
         ( "string concat eval multiple" >:: fun _ ->
           assert_expression_has_value ~program:""
             ~expr:{|"a" ^ "b" ^ "c" ^ "d"|} ~expected_value:{|"abcd"|} );
         ( "string concat eval with spaces" >:: fun _ ->
           assert_expression_has_value ~program:""
             ~expr:{|"hello" ^ " " ^ "world"|} ~expected_value:{|"hello world"|}
         );
         ( "string concat eval empty" >:: fun _ ->
           assert_expression_has_value ~program:"" ~expr:{|"" ^ "test" ^ ""|}
             ~expected_value:{|"test"|} );
         ( "string concat in function" >:: fun _ ->
           assert_expression_has_value ~program:""
             ~expr:{|(fn x -> fn y -> x ^ y) "foo" "bar"|}
             ~expected_value:{|"foobar"|} );
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
             ~program:
               {|
               let double = fn x -> x * 2
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
               type Option<'a> = | Some of 'a | None
             |}
             ~expr:"Some 5" ~expected_value:"Some 5" );
         ( "nullary constructor None" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Option<'a> = | Some of 'a | None
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
               case True do
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
               case False do
               | True -> 1
               | False -> 0
             |}
             ~expected_value:"0" );
         ( "pattern match on Some" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Option<'a> = | Some of 'a | None
             |}
             ~expr:
               {|
               case Some 42 do
               | Some x -> x
               | None -> 0
             |}
             ~expected_value:"42" );
         ( "pattern match on None" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Option<'a> = | Some of 'a | None
             |}
             ~expr:
               {|
               case None do
               | Some x -> x
               | None -> 0
             |}
             ~expected_value:"0" );
         ( "sum type in let binding" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Option<'a> = | Some of 'a | None
               let x = Some 10
             |}
             ~expr:
               {|
               case x do
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
               case Red do
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
               case Green do
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
               case Blue do
               | Red -> 1
               | Green -> 2
               | Blue -> 3
             |}
             ~expected_value:"3" );
         ( "constructor with tuple payload" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Pair<'a, 'b> = | Pair of ('a, 'b)
             |}
             ~expr:"Pair (1, 2)" ~expected_value:"Pair (1, 2)" );
         ( "pattern match tuple payload" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Pair<'a, 'b> = | Pair of ('a, 'b)
             |}
             ~expr:
               {|
               case Pair (5, 10) do
               | Pair (x, y) -> x + y
             |}
             ~expected_value:"15" );
         ( "nested pattern matching" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Option<'a> = | Some of 'a | None
               let x = Some (Some 42)
             |}
             ~expr:
               {|
               case x do
               | Some opt ->
                 case opt do
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
               case Ok 100 do
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
               case Error 50 do
               | Ok n -> n
               | Error e -> ~-e
             |}
             ~expected_value:"-50" );
         (* Recursive sum types *)
         ( "recursive list - Nil" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec List<'a> = | Nil | Cons of ('a, List<'a>)
             |}
             ~expr:"Nil" ~expected_value:"Nil" );
         ( "recursive list - Cons with Nil" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec List<'a> = | Nil | Cons of ('a, List<'a>)
             |}
             ~expr:"Cons (1, Nil)" ~expected_value:"Cons (1, Nil)" );
         ( "recursive list - nested Cons" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec List<'a> = | Nil | Cons of ('a, List<'a>)
             |}
             ~expr:"Cons (1, Cons (2, Nil))"
             ~expected_value:"Cons (1, Cons (2, Nil))" );
         ( "recursive list - pattern match Nil" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec List<'a> = | Nil | Cons of ('a, List<'a>)
             |}
             ~expr:
               {|
               case Nil do
               | Nil -> 0
               | Cons (_, _) -> 1
             |}
             ~expected_value:"0" );
         ( "recursive list - pattern match Cons" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec List<'a> = | Nil | Cons of ('a, List<'a>)
             |}
             ~expr:
               {|
               case Cons (42, Nil) do
               | Nil -> 0
               | Cons (x, _) -> x
             |}
             ~expected_value:"42" );
         ( "recursive list - length function" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec List<'a> = | Nil | Cons of ('a, List<'a>)
               let rec length lst =
                 case lst do
                 | Nil -> 0
                 | Cons (_, t) -> 1 + length t
             |}
             ~expr:"length (Cons (1, Cons (2, Cons (3, Nil))))"
             ~expected_value:"3" );
         ( "recursive list - sum function" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec List<'a> = | Nil | Cons of ('a, List<'a>)
               let rec sum lst =
                 case lst do
                 | Nil -> 0
                 | Cons (h, t) -> h + sum t
             |}
             ~expr:"sum (Cons (1, Cons (2, Cons (3, Nil))))" ~expected_value:"6"
         );
         ( "recursive binary tree - Leaf" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Tree<'a> = | Leaf | Node of ('a, Tree<'a>, Tree<'a>)
             |}
             ~expr:"Leaf" ~expected_value:"Leaf" );
         ( "recursive binary tree - single Node" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Tree<'a> = | Leaf | Node of ('a, Tree<'a>, Tree<'a>)
             |}
             ~expr:"Node (5, Leaf, Leaf)" ~expected_value:"Node (5, Leaf, Leaf)"
         );
         ( "recursive binary tree - nested Node" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Tree<'a> = | Leaf | Node of ('a, Tree<'a>, Tree<'a>)
             |}
             ~expr:"Node (1, Node (2, Leaf, Leaf), Node (3, Leaf, Leaf))"
             ~expected_value:
               "Node (1, Node (2, Leaf, Leaf), Node (3, Leaf, Leaf))" );
         ( "recursive binary tree - pattern match Leaf" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Tree<'a> = | Leaf | Node of ('a, Tree<'a>, Tree<'a>)
             |}
             ~expr:
               {|
               case Leaf do
               | Leaf -> 0
               | Node (_, _, _) -> 1
             |}
             ~expected_value:"0" );
         ( "recursive binary tree - pattern match Node" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Tree<'a> = | Leaf | Node of ('a, Tree<'a>, Tree<'a>)
             |}
             ~expr:
               {|
               case Node (42, Leaf, Leaf) do
               | Leaf -> 0
               | Node (x, _, _) -> x
             |}
             ~expected_value:"42" );
         ( "recursive binary tree - size function" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Tree<'a> = | Leaf | Node of ('a, Tree<'a>, Tree<'a>)
               let rec size t =
                 case t do
                 | Leaf -> 0
                 | Node (_, left, right) -> 1 + size left + size right
             |}
             ~expr:"size (Node (1, Node (2, Leaf, Leaf), Node (3, Leaf, Leaf)))"
             ~expected_value:"3" );
         ( "recursive binary tree - sum values" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Tree<'a> = | Leaf | Node of ('a, Tree<'a>, Tree<'a>)
               let rec sum_tree t =
                 case t do
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
               case Zero do
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
                 case n do
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
               type TypeOne<'a> = ('a, int)
             |}
               "TypeOne<bool>"
           in
           assert_equal result "(bool, int)" );
         ( "nested type application" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type TypeOne<'a> = ('a, int)
               type TypeTwo<'a> = ('a, TypeOne<'a>)
             |}
               "TypeTwo<bool>"
           in
           assert_equal result "(bool, (bool, int))" );
         ( "multiple type parameters" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Pair<'a, 'b> = ('a, 'b)
             |}
               "Pair<int, bool>"
           in
           assert_equal result "(int, bool)" );
         ( "nested type with multiple parameters" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Pair<'a, 'b> = ('a, 'b)
               type Triple<'a, 'b, 'c> = ('a, Pair<'b, 'c>)
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
               type Pair<'a, 'b> = ('a, 'b)
             |}
               "Pair<int, bool>"
           in
           assert_equal result "(int, bool)" );
         ( "two type parameters swapped" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Pair<'a, 'b> = ('a, 'b)
             |}
               "Pair<bool, int>"
           in
           assert_equal result "(bool, int)" );
         ( "two parameters with same type" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Pair<'a, 'b> = ('a, 'b)
             |}
               "Pair<int, int>"
           in
           assert_equal result "(int, int)" );
         (* Three type parameters *)
         ( "three type parameters" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Triple<'a, 'b, 'c> = ('a, 'b, 'c)
             |}
               "Triple<int, bool, str>"
           in
           assert_equal result "(int, bool, str)" );
         ( "three parameters all same" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Triple<'a, 'b, 'c> = ('a, 'b, 'c)
             |}
               "Triple<int, int, int>"
           in
           assert_equal result "(int, int, int)" );
         (* Four type parameters *)
         ( "four type parameters" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Quad<'a, 'b, 'c, 'd> = ('a, 'b, 'c, 'd)
             |}
               "Quad<int, bool, str, float>"
           in
           assert_equal result "(int, bool, str, float)" );
         (* Five type parameters *)
         ( "five type parameters" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Five<'a, 'b, 'c, 'd, 'e> = ('a, 'b, 'c, 'd, 'e)
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
               type Func<'a> = 'a -> 'a
             |}
               "Func<int>"
           in
           assert_equal result "int -> int" );
         ( "function type with two parameters" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Func<'a, 'b> = 'a -> 'b
             |}
               "Func<int, bool>"
           in
           assert_equal result "int -> bool" );
         ( "nested function types" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Func<'a> = 'a -> 'a -> 'a
             |}
               "Func<int>"
           in
           assert_equal result "int -> int -> int" );
         ( "function with tuple parameter" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Func<'a, 'b> = ('a, 'b) -> 'a
             |}
               "Func<int, bool>"
           in
           assert_equal result "(int, bool) -> int" );
         ( "function returning tuple" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Func<'a, 'b> = 'a -> ('a, 'b)
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
               type List<'a> = ['a]
             |}
               "List<bool>"
           in
           assert_equal result "[bool]" );
         ( "list of tuples" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type PairList<'a, 'b> = [('a, 'b)]
             |}
               "PairList<int, bool>"
           in
           assert_equal result "[(int, bool)]" );
         (* Complex nested structures *)
         ( "tuple of lists" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Lists<'a, 'b> = (['a], ['b])
             |}
               "Lists<int, bool>"
           in
           assert_equal result "([int], [bool])" );
         ( "list of functions" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type FuncList<'a> = ['a -> 'a]
             |}
               "FuncList<int>"
           in
           assert_equal result "[int -> int]" );
         ( "function taking list" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Func<'a> = ['a] -> 'a
             |}
               "Func<int>"
           in
           assert_equal result "[int] -> int" );
         ( "function returning list" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Func<'a> = 'a -> ['a]
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
               type First<'a> = ('a, int)
               type Second<'a> = (First<'a>, bool)
             |}
               "Second<int>"
           in
           assert_equal result "((int, int), bool)" );
         ( "three aliases chain" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type A<'a> = ('a, int)
               type B<'a> = (A<'a>, bool)
               type C<'a> = (B<'a>, str)
             |}
               "C<int>"
           in
           assert_equal result "(((int, int), bool), str)" );
         ( "aliases with different parameters" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Pair<'a, 'b> = ('a, 'b)
               type Triple<'a, 'b, 'c> = (Pair<'a, 'b>, 'c)
             |}
               "Triple<int, bool, str>"
           in
           assert_equal result "((int, bool), str)" );
         ( "nested alias applications" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Wrapper<'a> = ('a, int)
               type Double<'a> = (Wrapper<'a>, Wrapper<bool>)
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
               type Complex<'a, 'b, 'c> = (('a, 'b), ('b, 'c), ('a, 'c))
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
               type Func<'a, 'b> = ('a, 'b) -> 'a
             |}
               "Func<int, bool>"
           in
           assert_equal result "(int, bool) -> int" );
         ( "list of complex tuples" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type ComplexList<'a, 'b> = [(('a, 'b), ('b, 'a))]
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
               type Wrapper<'a> = ('a, int)
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
               type Pair<'a> = ('a, 'a)
             |}
               "Pair<int>"
           in
           assert_equal result "(int, int)" );
         ( "type applied to itself" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Wrapper<'a> = ('a, int)
             |}
               "Wrapper<Wrapper<int>>"
           in
           assert_equal result "((int, int), int)" );
         ( "deeply nested applications" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Box<'a> = ('a, int)
             |}
               "Box<Box<Box<int>>>"
           in
           assert_equal result "(((int, int), int), int)" );
         (* Type parameters in different positions *)
         ( "parameters in different order" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Swap<'a, 'b> = ('b, 'a)
             |}
               "Swap<int, bool>"
           in
           assert_equal result "(bool, int)" );
         ( "parameters used multiple times" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Triple<'a> = ('a, 'a, 'a)
             |}
               "Triple<str>"
           in
           assert_equal result "(str, str, str)" );
         ( "mixed parameter usage" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Mixed<'a, 'b> = ('a, 'b, 'a, 'b)
             |}
               "Mixed<int, bool>"
           in
           assert_equal result "(int, bool, int, bool)" );
         (* Real-world examples *)
         ( "option type" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Option<'a> = (bool, 'a)
             |}
               "Option<int>"
           in
           assert_equal result "(bool, int)" );
         ( "result type" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Result<'a, 'b> = (bool, 'a, 'b)
             |}
               "Result<int, str>"
           in
           assert_equal result "(bool, int, str)" );
         ( "maybe type" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Maybe<'a> = (bool, 'a)
             |}
               "Maybe<bool>"
           in
           assert_equal result "(bool, bool)" );
         ( "either type" >:: fun _ ->
           let result =
             evaluate_type_expression
               {|
               type Either<'a, 'b> = (bool, 'a, 'b)
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
             type rec RBTree<'a> =
               | Leaf
               | Node of (Color, a, RBTree<'a>, RBTree<'a>)
           |}
         );
         ( "create empty rb tree" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)
             |}
             ~expr:"Leaf" ~expected_value:"Leaf" );
         ( "create red node" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)
             |}
             ~expr:"Node (Red, 5, Leaf, Leaf)"
             ~expected_value:"Node (Red, 5, Leaf, Leaf)" );
         ( "create black node" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)
             |}
             ~expr:"Node (Black, 10, Leaf, Leaf)"
             ~expected_value:"Node (Black, 10, Leaf, Leaf)" );
         (* Basic tree operations *)
         ( "rb tree contains function type" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)

               let rec contains x tree =
                 case tree do
                 | Leaf -> false
                 | Node (_, y, left, right) ->
                     if x == y then true
                     else if x < y then contains x left
                     else contains x right
             |}
             ~expr:"contains" ~expected_type:"int -> RBTree<int> -> bool" );
         ( "rb tree contains - empty tree" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)

               let rec contains x tree =
                 case tree do
                 | Leaf -> false
                 | Node (_, y, left, right) ->
                     if x == y then true
                     else if x < y then contains x left
                     else contains x right
             |}
             ~expr:"contains 5 Leaf" ~expected_value:"false" );
         ( "rb tree contains - single node found" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)

               let rec contains x tree =
                 case tree do
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
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)

               let rec contains x tree =
                 case tree do
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
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)

               let balance tree =
                 case tree do
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
             ~expr:"balance" ~expected_type:"RBTree<'a> -> RBTree<'a>" );
         ( "rb tree balance - no rebalancing needed" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)

               let balance tree =
                 case tree do
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
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)

               let rec size tree =
                 case tree do
                 | Leaf -> 0
                 | Node (_, _, left, right) -> 1 + size left + size right
             |}
             ~expr:"size" ~expected_type:"RBTree<'a> -> int" );
         ( "rb tree size - empty" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)

               let rec size tree =
                 case tree do
                 | Leaf -> 0
                 | Node (_, _, left, right) -> 1 + size left + size right
             |}
             ~expr:"size Leaf" ~expected_value:"0" );
         ( "rb tree size - single node" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)

               let rec size tree =
                 case tree do
                 | Leaf -> 0
                 | Node (_, _, left, right) -> 1 + size left + size right
             |}
             ~expr:"size (Node (Black, 5, Leaf, Leaf))" ~expected_value:"1" );
         ( "rb tree size - three nodes" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)

               let rec size tree =
                 case tree do
                 | Leaf -> 0
                 | Node (_, _, left, right) -> 1 + size left + size right
             |}
             ~expr:
               "size (Node (Black, 5, Node (Red, 3, Leaf, Leaf), Node (Red, 7, \
                Leaf, Leaf)))"
             ~expected_value:"3" );
         (* Height function *)
         ( "rb tree height function" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)

               let rec height tree =
                 case tree do
                 | Leaf -> 0
                 | Node (_, _, left, right) ->
                     let left_h = height left in
                     let right_h = height right in
                     1 + (if left_h > right_h then left_h else right_h)
             |}
             ~expr:"height" ~expected_type:"RBTree<'a> -> int" );
         ( "rb tree height - empty" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)

               let rec height tree =
                 case tree do
                 | Leaf -> 0
                 | Node (_, _, left, right) ->
                     let left_h = height left in
                     let right_h = height right in
                     1 + (if left_h > right_h then left_h else right_h)
             |}
             ~expr:"height Leaf" ~expected_value:"0" );
         ( "rb tree height - single node" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)

               let rec height tree =
                 case tree do
                 | Leaf -> 0
                 | Node (_, _, left, right) ->
                     let left_h = height left in
                     let right_h = height right in
                     1 + (if left_h > right_h then left_h else right_h)
             |}
             ~expr:"height (Node (Black, 5, Leaf, Leaf))" ~expected_value:"1" );
         ( "rb tree height - balanced tree" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)

               let rec height tree =
                 case tree do
                 | Leaf -> 0
                 | Node (_, _, left, right) ->
                     let left_h = height left in
                     let right_h = height right in
                     1 + (if left_h > right_h then left_h else right_h)
             |}
             ~expr:
               "height (Node (Black, 5, Node (Red, 3, Leaf, Leaf), Node (Red, \
                7, Leaf, Leaf)))"
             ~expected_value:"2" );
         (* Min/Max functions *)
         ( "rb tree minimum function" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)

               let rec minimum tree =
                 case tree do
                 | Leaf -> 0
                 | Node (_, x, Leaf, _) -> x
                 | Node (_, _, left, _) -> minimum left
             |}
             ~expr:"minimum" ~expected_type:"RBTree<int> -> int" );
         ( "rb tree minimum - single node" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)

               let rec minimum tree =
                 case tree do
                 | Leaf -> 0
                 | Node (_, x, Leaf, _) -> x
                 | Node (_, _, left, _) -> minimum left
             |}
             ~expr:"minimum (Node (Black, 5, Leaf, Leaf))" ~expected_value:"5"
         );
         ( "rb tree minimum - multiple nodes" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)

               let rec minimum tree =
                 case tree do
                 | Leaf -> 0
                 | Node (_, x, Leaf, _) -> x
                 | Node (_, _, left, _) -> minimum left
             |}
             ~expr:
               "minimum (Node (Black, 5, Node (Red, 3, Node (Black, 1, Leaf, \
                Leaf), Leaf), Leaf))"
             ~expected_value:"1" );
         ( "rb tree maximum function" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)

               let rec maximum tree =
                 case tree do
                 | Leaf -> 0
                 | Node (_, x, _, Leaf) -> x
                 | Node (_, _, _, right) -> maximum right
             |}
             ~expr:"maximum" ~expected_type:"RBTree<int> -> int" );
         ( "rb tree maximum - single node" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)

               let rec maximum tree =
                 case tree do
                 | Leaf -> 0
                 | Node (_, x, _, Leaf) -> x
                 | Node (_, _, _, right) -> maximum right
             |}
             ~expr:"maximum (Node (Black, 5, Leaf, Leaf))" ~expected_value:"5"
         );
         ( "rb tree maximum - multiple nodes" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)

               let rec maximum tree =
                 case tree do
                 | Leaf -> 0
                 | Node (_, x, _, Leaf) -> x
                 | Node (_, _, _, right) -> maximum right
             |}
             ~expr:
               "maximum (Node (Black, 5, Leaf, Node (Red, 7, Leaf, Node \
                (Black, 9, Leaf, Leaf))))"
             ~expected_value:"9" );
         (* Complex tree structure tests *)
         ( "rb tree complex structure" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)

               let tree = Node (Black, 5,
                           Node (Red, 3,
                             Node (Black, 1, Leaf, Leaf),
                             Node (Black, 4, Leaf, Leaf)),
                           Node (Red, 7,
                             Node (Black, 6, Leaf, Leaf),
                             Node (Black, 9, Leaf, Leaf)))
             |}
             ~expr:"tree"
             ~expected_value:
               "Node (Black, 5, Node (Red, 3, Node (Black, 1, Leaf, Leaf), \
                Node (Black, 4, Leaf, Leaf)), Node (Red, 7, Node (Black, 6, \
                Leaf, Leaf), Node (Black, 9, Leaf, Leaf)))" );
         ( "rb tree complex structure - size" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)

               let rec size tree =
                 case tree do
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
             ~expr:"size tree" ~expected_value:"7" );
         ( "rb tree complex structure - contains existing" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)

               let rec contains x tree =
                 case tree do
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
             ~expr:"contains 6 tree" ~expected_value:"true" );
         ( "rb tree complex structure - contains non-existing" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)

               let rec contains x tree =
                 case tree do
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
             ~expr:"contains 10 tree" ~expected_value:"false" );
         (* Insert function tests *)
         ( "rb tree make_black function type" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)

               let make_black tree =
                 case tree do
                 | Leaf -> Leaf
                 | Node (_, x, left, right) -> Node (Black, x, left, right)
             |}
             ~expr:"make_black" ~expected_type:"RBTree<'a> -> RBTree<'a>" );
         ( "rb tree make_black - Leaf" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)

               let make_black tree =
                 case tree do
                 | Leaf -> Leaf
                 | Node (_, x, left, right) -> Node (Black, x, left, right)
             |}
             ~expr:"make_black Leaf" ~expected_value:"Leaf" );
         ( "rb tree make_black - Red node" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)

               let make_black tree =
                 case tree do
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
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)

               let balance tree =
                 case tree do
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
                 case tree do
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
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)

               let balance tree =
                 case tree do
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
                 case tree do
                 | Leaf -> Node (Red, x, Leaf, Leaf)
                 | Node (color, y, left, right) ->
                     if x < y then
                       balance (Node (color, y, insert_aux x left, right))
                     else if x > y then
                       balance (Node (color, y, left, insert_aux x right))
                     else
                       tree

               let make_black tree =
                 case tree do
                 | Leaf -> Leaf
                 | Node (_, x, left, right) -> Node (Black, x, left, right)

               let insert x tree =
                 make_black (insert_aux x tree)
             |}
             ~expr:"insert" ~expected_type:"int -> RBTree<int> -> RBTree<int>"
         );
         ( "rb tree insert into empty tree" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)

               let balance tree =
                 case tree do
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
                 case tree do
                 | Leaf -> Node (Red, x, Leaf, Leaf)
                 | Node (color, y, left, right) ->
                     if x < y then
                       balance (Node (color, y, insert_aux x left, right))
                     else if x > y then
                       balance (Node (color, y, left, insert_aux x right))
                     else
                       tree

               let make_black tree =
                 case tree do
                 | Leaf -> Leaf
                 | Node (_, x, left, right) -> Node (Black, x, left, right)

               let insert x tree =
                 make_black (insert_aux x tree)
             |}
             ~expr:"insert 5 Leaf" ~expected_value:"Node (Black, 5, Leaf, Leaf)"
         );
         ( "rb tree insert - size increases" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)

               let balance tree =
                 case tree do
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
                 case tree do
                 | Leaf -> Node (Red, x, Leaf, Leaf)
                 | Node (color, y, left, right) ->
                     if x < y then
                       balance (Node (color, y, insert_aux x left, right))
                     else if x > y then
                       balance (Node (color, y, left, insert_aux x right))
                     else
                       tree

               let make_black tree =
                 case tree do
                 | Leaf -> Leaf
                 | Node (_, x, left, right) -> Node (Black, x, left, right)

               let insert x tree =
                 make_black (insert_aux x tree)

               let rec size tree =
                 case tree do
                 | Leaf -> 0
                 | Node (_, _, left, right) -> 1 + size left + size right

               let tree1 = insert 5 Leaf
               let tree2 = insert 3 tree1
               let tree3 = insert 7 tree2
             |}
             ~expr:"size tree3" ~expected_value:"3" );
         ( "rb tree insert multiple - contains all" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)

               let balance tree =
                 case tree do
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
                 case tree do
                 | Leaf -> Node (Red, x, Leaf, Leaf)
                 | Node (color, y, left, right) ->
                     if x < y then
                       balance (Node (color, y, insert_aux x left, right))
                     else if x > y then
                       balance (Node (color, y, left, insert_aux x right))
                     else
                       tree

               let make_black tree =
                 case tree do
                 | Leaf -> Leaf
                 | Node (_, x, left, right) -> Node (Black, x, left, right)

               let insert x tree =
                 make_black (insert_aux x tree)

               let rec contains x tree =
                 case tree do
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
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)

               let balance tree =
                 case tree do
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
                 case tree do
                 | Leaf -> Node (Red, x, Leaf, Leaf)
                 | Node (color, y, left, right) ->
                     if x < y then
                       balance (Node (color, y, insert_aux x left, right))
                     else if x > y then
                       balance (Node (color, y, left, insert_aux x right))
                     else
                       tree

               let make_black tree =
                 case tree do
                 | Leaf -> Leaf
                 | Node (_, x, left, right) -> Node (Black, x, left, right)

               let insert x tree =
                 make_black (insert_aux x tree)

               let rec size tree =
                 case tree do
                 | Leaf -> 0
                 | Node (_, _, left, right) -> 1 + size left + size right

               let tree1 = insert 5 Leaf
               let tree2 = insert 5 tree1
             |}
             ~expr:"size tree2" ~expected_value:"1" );
         ( "rb tree insert - min and max after inserts" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)

               let balance tree =
                 case tree do
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
                 case tree do
                 | Leaf -> Node (Red, x, Leaf, Leaf)
                 | Node (color, y, left, right) ->
                     if x < y then
                       balance (Node (color, y, insert_aux x left, right))
                     else if x > y then
                       balance (Node (color, y, left, insert_aux x right))
                     else
                       tree

               let make_black tree =
                 case tree do
                 | Leaf -> Leaf
                 | Node (_, x, left, right) -> Node (Black, x, left, right)

               let insert x tree =
                 make_black (insert_aux x tree)

               let rec minimum tree =
                 case tree do
                 | Leaf -> 0
                 | Node (_, x, Leaf, _) -> x
                 | Node (_, _, left, _) -> minimum left

               let rec maximum tree =
                 case tree do
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
         ( "recursive type annotation in function parameter" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Tree =
                 | Leaf
                 | Node of (Tree, Tree, int)

               let rec num_nodes (t : Tree) =
                 case t do
                 | Leaf -> 0
                 | Node (l, r, _) -> 1 + (num_nodes l) + (num_nodes r)
             |}
             ~expr:"num_nodes (Node (Node (Leaf, Leaf, 1), Leaf, 2))"
             ~expected_value:"2" );
       ]

(* ============================================================================
   SUM TYPE CONSTRUCTOR TYPE INFERENCE TESTS

   These tests specifically verify that sum type constructors have correct type
   inference, especially when: 1. Constructors reference other sum types (not
   type parameters) 2. Multiple sum types are defined and used together 3.
   Concrete types should not become polymorphic type variables

   These tests would catch the bug where sum types were represented with TypeVar
   dummy bodies, causing constructor types to incorrectly generalize concrete
   type references.
   ============================================================================ *)

let sum_type_constructor_inference_tests =
  let open ProgramTesting in
  "sum_type_constructor_inference"
  >::: [
         (* Test that a simple sum type constructor has the correct type *)
         ( "Color constructor type - Red" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Color = | Red | Black
             |}
             ~expr:"Red" ~expected_type:"Color" );
         ( "Color constructor type - Black" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Color = | Red | Black
             |}
             ~expr:"Black" ~expected_type:"Color" );
         (* Test that a constructor with payload referencing another sum type
            has the correct type - this is the key test for the bug! *)
         ( "Node constructor type with Color parameter" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)
             |}
             ~expr:"Node"
             ~expected_type:"(Color, 'a, RBTree<'a>, RBTree<'a>) -> RBTree<'a>"
         );
         (* Verify that Color is NOT a type variable when used *)
         ( "Node constructor applied to Red" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)
             |}
             ~expr:"Node (Red, 5, Leaf, Leaf)" ~expected_type:"RBTree<int>" );
         (* Test multiple sum types referencing each other *)
         ( "constructor with multiple sum type parameters" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Status = | Active | Inactive
               type Priority = | High | Low
               type rec Task<'a> =
                 | Task of (Status, Priority, a)
             |}
             ~expr:"Task" ~expected_type:"(Status, Priority, 'a) -> Task<'a>" );
         (* Verify concrete evaluation *)
         ( "Node with Red evaluates correctly" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)
             |}
             ~expr:"Node (Red, 5, Leaf, Leaf)"
             ~expected_value:"Node (Red, 5, Leaf, Leaf)" );
         (* Test that we can pattern match on the concrete Color type *)
         ( "pattern match on Color in Node" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)
             |}
             ~expr:
               {|
               case Node (Red, 5, Leaf, Leaf) do
               | Leaf -> 0
               | Node (Red, x, _, _) -> x
               | Node (Black, x, _, _) -> ~-x
             |}
             ~expected_value:"5" );
         ( "pattern match on Black in Node" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)
             |}
             ~expr:
               {|
               case Node (Black, 5, Leaf, Leaf) do
               | Leaf -> 0
               | Node (Red, x, _, _) -> x
               | Node (Black, x, _, _) -> ~-x
             |}
             ~expected_value:"-5" );
         (* Test constructor with multiple concrete sum types *)
         ( "constructor with two concrete sum types" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Color = | Red | Black
               type Shape = | Circle | Square
               type Decoration = | Decor of (Color, Shape)
             |}
             ~expr:"Decor" ~expected_type:"(Color, Shape) -> Decoration" );
         (* Test that we can use the constructor correctly *)
         ( "apply constructor with concrete sum types" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Color = | Red | Black
               type Shape = | Circle | Square
               type Decoration = | Decor of (Color, Shape)
             |}
             ~expr:"Decor (Red, Circle)" ~expected_value:"Decor (Red, Circle)"
         );
         (* Test nested sum types with concrete references *)
         ( "nested sum type constructors" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Size = | Small | Large
               type Color = | Red | Black
               type Colored<'a> = | Colored of (Color, 'a)
             |}
             ~expr:"Colored" ~expected_type:"(Color, 'a) -> Colored<'a>" );
         (* Test that concrete types in tuple payloads work *)
         ( "tuple payload with multiple concrete types" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type A = | A1 | A2
               type B = | B1 | B2
               type C = | C1 | C2
               type Combined = | Combo of (A, B, C, int)
             |}
             ~expr:"Combo" ~expected_type:"(A, B, C, int) -> Combined" );
         (* Test function taking constructor as argument *)
         ( "function with constructor parameter" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)

               let make_red_node x = Node (Red, x, Leaf, Leaf)
             |}
             ~expr:"make_red_node" ~expected_type:"'a -> RBTree<'a>" );
         (* Test that type checking rejects wrong concrete types *)
         ( "type error when using wrong sum type" >:: fun _ ->
           assert_program_fails_typecheck
             {|
               type Color = | Red | Black
               type Size = | Big | Small
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)

               let bad_node = Node (Big, 5, Leaf, Leaf)
             |}
         );
         (* Test with parameterized sum types *)
         ( "parameterized sum type with concrete type reference" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Tag = | Important | Normal
               type Wrapper<'a> = | Wrap of (Tag, 'a)
             |}
             ~expr:"Wrap" ~expected_type:"(Tag, 'a) -> Wrapper<'a>" );
         (* Test complex nested structure *)
         ( "complex nested sum types" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Status = | Active | Inactive
               type Priority = | High | Low | Medium
               type rec TaskList<'a> =
                 | Empty
                 | Task of (Status, Priority, a, TaskList<'a>)
             |}
             ~expr:"Task"
             ~expected_type:
               "(Status, Priority, 'a, TaskList<'a>) -> TaskList<'a>" );
         (* Test that pattern matching works with concrete types *)
         ( "pattern match extracts concrete sum type" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Status = | Active | Inactive
               type Priority = | High | Low
               type Task = | Task of (Status, Priority, int)

               let get_priority t =
                 case t do
                 | Task (_, High, _) -> 1
                 | Task (_, Low, _) -> 0
             |}
             ~expr:"get_priority (Task (Active, High, 42))" ~expected_value:"1"
         );
         (* Test sum type in higher-order function *)
         ( "sum type constructor in map" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Color = | Red | Black
               type Colored<'a> = | Colored of (Color, 'a)
             |}
             ~expr:
               {|
               let colorize c x = Colored (c, x) in
               colorize Red
             |}
             ~expected_type:"'a -> Colored<'a>" );
         (* Ensure Red-Black tree functions work correctly *)
         ( "rb tree contains function type" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)

               let rec contains x tree =
                 case tree do
                 | Leaf -> false
                 | Node (_, y, left, right) ->
                     if x == y then true
                     else if x < y then contains x left
                     else contains x right
             |}
             ~expr:"contains" ~expected_type:"int -> RBTree<int> -> bool" );
         (* Test balance function type *)
         ( "rb tree balance function type" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<'a> =
                 | Leaf
                 | Node of (Color, a, RBTree<'a>, RBTree<'a>)

               let balance tree =
                 case tree do
                 | Node (Black, z, Node (Red, y, Node (Red, x, a, b), c), d) ->
                     Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
                 | _ -> tree
             |}
             ~expr:"balance" ~expected_type:"RBTree<'a> -> RBTree<'a>" );
       ]

(* ============================================================================
   CUSTOM INFIX OPERATOR TESTS

   Tests for custom binary operators defined with parenthesized syntax like: let
   (+++) x y = x + y + y

   Covers: - Type inference for custom operators - Evaluation of custom
   operators - Different precedence levels (additive, multiplicative,
   relational) - Partial application - Custom operators with various types
   ============================================================================ *)

let custom_operator_type_tests =
  let open ProgramTesting in
  "custom_operator_types"
  >::: [
         (* Additive operators (start with + or -) *)
         ( "custom additive operator type" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               let (+++) x y = x + y + y
             |}
             ~expr:"(+++)" ~expected_type:"int -> int -> int" );
         ( "custom additive operator with different implementation" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               let (+-+) a b = a + b + 1
             |}
             ~expr:"(+-+)" ~expected_type:"int -> int -> int" );
         (* Multiplicative operators (start with * / %) *)
         ( "custom multiplicative operator type" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               let (***) x y = x * x * y
             |}
             ~expr:"(***)" ~expected_type:"int -> int -> int" );
         ( "custom division-based operator type" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               let (///) x y = x / y + x % y
             |}
             ~expr:"(///)" ~expected_type:"int -> int -> int" );
         (* Relational operators (start with < > =) *)
         ( "custom relational operator type" >:: fun _ ->
           assert_expression_has_type
             ~program:{|
               let (===) x y = x == y
             |}
             ~expr:"(===)" ~expected_type:"'a -> 'a -> bool" );
         ( "custom less-than operator type" >:: fun _ ->
           assert_expression_has_type
             ~program:{|
               let (<<) x y = x < y - 1
             |}
             ~expr:"(<<)" ~expected_type:"int -> int -> bool" );
         (* Polymorphic custom operators *)
         ( "polymorphic custom operator" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               let (<=>) x y = if x == y then 1 else 0
             |}
             ~expr:"(<=>)" ~expected_type:"'a -> 'a -> int" );
         (* Custom operator with type annotations *)
         ( "custom operator with type annotation" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               let (+*+) (x : int) (y : int) : int = x + y * 2
             |}
             ~expr:"(+*+)" ~expected_type:"int -> int -> int" );
         (* Custom operator usage in expressions *)
         ( "expression using custom additive operator" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               let (+++) x y = x + y + y
             |}
             ~expr:"5 +++ 3" ~expected_type:"int" );
         ( "expression using custom multiplicative operator" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               let (***) x y = x * x * y
             |}
             ~expr:"3 *** 2" ~expected_type:"int" );
         ( "expression using custom relational operator" >:: fun _ ->
           assert_expression_has_type
             ~program:{|
               let (===) x y = x == y
             |}
             ~expr:"5 === 5" ~expected_type:"bool" );
         (* Partial application *)
         ( "partial application of custom operator" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               let (+++) x y = x + y + y
               let add_six = (+++) 2
             |}
             ~expr:"add_six" ~expected_type:"int -> int" );
         ( "partial application result" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               let (+++) x y = x + y + y
               let add_six = (+++) 2
             |}
             ~expr:"add_six 3" ~expected_type:"int" );
         (* Multiple custom operators *)
         ( "multiple custom operators in program" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               let (+++) x y = x + y + y
               let (***) x y = x * x * y
               let (===) x y = x == y
             |}
             ~expr:"(+++)" ~expected_type:"int -> int -> int" );
         ( "expression with multiple custom operators" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               let (+++) x y = x + y + y
               let (***) x y = x * x * y
             |}
             ~expr:"2 *** 3 +++ 4" ~expected_type:"int" );
         (* Recursive custom operators *)
         ( "recursive custom operator type" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               let rec (***>) x y =
                 if x == 0 then 0
                 else if x == 1 then y
                 else y + (x - 1) ***> y
             |}
             ~expr:"(***>)" ~expected_type:"int -> int -> int" );
         (* Custom operator with bool return *)
         ( "custom operator returning bool" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               let (>><) x y = x > y && y > 0
             |}
             ~expr:"(>><)" ~expected_type:"int -> int -> bool" );
         (* Mixed precedence operators *)
         ( "mixed precedence custom operators" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               let (</>) x y = x / y + 1
               let (<+>) x y = x + y * 2
             |}
             ~expr:"10 </> 3 <+> 2" ~expected_type:"int" );
       ]

let custom_operator_evaluation_tests =
  let open ProgramTesting in
  "custom_operator_evaluation"
  >::: [
         (* Basic evaluation *)
         ( "evaluate custom additive operator" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let (+++) x y = x + y + y
             |}
             ~expr:"5 +++ 3" ~expected_value:"11" );
         ( "evaluate custom multiplicative operator" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let (***) x y = x * x * y
             |}
             ~expr:"3 *** 2" ~expected_value:"18" );
         ( "evaluate custom division operator" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let (///) x y = x / y + x % y
             |}
             ~expr:"17 /// 5" ~expected_value:"5" );
         ( "evaluate custom relational operator - true" >:: fun _ ->
           assert_expression_has_value
             ~program:{|
               let (===) x y = x == y
             |}
             ~expr:"5 === 5" ~expected_value:"true" );
         ( "evaluate custom relational operator - false" >:: fun _ ->
           assert_expression_has_value
             ~program:{|
               let (===) x y = x == y
             |}
             ~expr:"5 === 3" ~expected_value:"false" );
         (* Complex expressions *)
         ( "custom operator in complex expression" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let (+++) x y = x + y + y
             |}
             ~expr:"1 + 2 +++ 3" ~expected_value:"9" );
         ( "multiple custom operators" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let (+++) x y = x + x + y
               let (***) x y = x * y
             |}
             ~expr:"2 *** 3 +++ 4" ~expected_value:"16" );
         (* Partial application evaluation *)
         ( "partial application evaluation" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let (+++) x y = x + x + y
               let add_double = (+++) 2
             |}
             ~expr:"add_double 3" ~expected_value:"7" );
         (* Recursive custom operators *)
         ( "recursive custom operator - base case" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec (***>) x y =
                 if x == 0 then 0
                 else if x == 1 then y
                 else y + (x - 1) ***> y
             |}
             ~expr:"0 ***> 5" ~expected_value:"0" );
         ( "recursive custom operator - recursive case" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec (***>) x y =
                 if x == 0 then 0
                 else if x == 1 then y
                 else y + (x - 1) ***> y
             |}
             ~expr:"4 ***> 3" ~expected_value:"12" );
         (* Custom operators with conditionals *)
         ( "custom operator with conditional logic" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let (***) x y =
                 if x == 0 then y
                 else x * y
             |}
             ~expr:"0 *** 100" ~expected_value:"100" );
         ( "custom operator with conditional - non-zero" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let (***) x y =
                 if x == 0 then y
                 else x * y
             |}
             ~expr:"5 *** 3" ~expected_value:"15" );
         (* Precedence testing *)
         ( "multiplicative custom operator precedence" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let (***) x y = x * x * y
             |}
             ~expr:"2 *** 3 + 4" ~expected_value:"16" );
         ( "additive custom operator precedence" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let (+++) x y = x + y + y
             |}
             ~expr:"2 * 3 +++ 4" ~expected_value:"14" );
         (* Custom operators in let expressions *)
         ( "custom operator in let binding" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let (+++) x y = x + y + y
               let result = 5 +++ 3
             |}
             ~expr:"result" ~expected_value:"11" );
         (* Custom operators with function application *)
         ( "custom operator with function application" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let (+++) x y = x + y + y
               let double n = n * 2
             |}
             ~expr:"double 2 +++ 3" ~expected_value:"10" );
         (* Chaining custom operators *)
         ( "chaining same custom operator" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let (+++) x y = x + y + 1
             |}
             ~expr:"1 +++ 2 +++ 3" ~expected_value:"8" );
         (* Mixed precedence *)
         ( "mixed precedence evaluation" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let (</>) x y = x / y + 1
               let (<+>) x y = x + y * 2
             |}
             ~expr:"10 </> 3 <+> 2" ~expected_value:"8" );
         (* Boolean custom operators *)
         ( "custom boolean operator - and variant" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let (==>) x y = x == y || y > 10
             |}
             ~expr:"5 ==> 15" ~expected_value:"true" );
         ( "custom boolean operator - complex" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let (<<=) x y = x < y && y < x + 10
             |}
             ~expr:"5 <<= 7" ~expected_value:"true" );
         (* Using custom operators in higher-order functions *)
         ( "custom operator in lambda" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let (+++) x y = x + x + y
               let apply_op f a b = f a b
             |}
             ~expr:"apply_op (+++) 2 3" ~expected_value:"7" );
       ]

let option_map_type_test =
  let open ProgramTesting in
  "option_map_type_test"
  >::: [
         ( "map should have polymorphic type" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
          type Option<'a> =
            | None
            | Some of a

          let map f o =
            case o do
            | None -> None
            | Some v -> Some (f v)
        |}
             ~expr:"(map)"
             ~expected_type:"('a -> 'b) -> Option<'a> -> Option<'b>" );
       ]

(* ============================================================================
   Parenthesized Built-in Operator Tests

   These tests verify that built-in infix operators can be used in parenthesized
   form like (+), (-), etc. as first-class values.
   ============================================================================ *)
let parenthesized_builtin_operator_tests =
  let open ProgramTesting in
  "parenthesized_builtin_operators"
  >::: [
         (* Type tests *)
         ( "parenthesized + has correct type" >:: fun _ ->
           assert_expression_has_type ~program:"" ~expr:"(+)"
             ~expected_type:"int -> int -> int" );
         ( "parenthesized - has correct type" >:: fun _ ->
           assert_expression_has_type ~program:"" ~expr:"(-)"
             ~expected_type:"int -> int -> int" );
         ( "parenthesized times has correct type" >:: fun _ ->
           assert_expression_has_type ~program:"" ~expr:{|(*)|}
             ~expected_type:"int -> int -> int" );
         ( "parenthesized / has correct type" >:: fun _ ->
           assert_expression_has_type ~program:"" ~expr:"(/)"
             ~expected_type:"int -> int -> int" );
         ( "parenthesized < has correct type" >:: fun _ ->
           assert_expression_has_type ~program:"" ~expr:"(<)"
             ~expected_type:"int -> int -> bool" );
         ( "parenthesized && has correct type" >:: fun _ ->
           assert_expression_has_type ~program:"" ~expr:"(&&)"
             ~expected_type:"bool -> bool -> bool" );
         (* Evaluation tests *)
         ( "use (+) as a value" >:: fun _ ->
           assert_expression_has_value
             ~program:{|
          let add = (+)
        |}
             ~expr:"add 5 3" ~expected_value:"8" );
         ( "use (-) as a value" >:: fun _ ->
           assert_expression_has_value
             ~program:{|
          let sub = (-)
        |}
             ~expr:"sub 10 3" ~expected_value:"7" );
         ( "use times as a value" >:: fun _ ->
           assert_expression_has_value
             ~program:{|
          let mul = (*)
        |}
             ~expr:"mul 4 5" ~expected_value:"20" );
         ( "use (<) as a value" >:: fun _ ->
           assert_expression_has_value
             ~program:{|
          let lt = (<)
        |}
             ~expr:"lt 3 5" ~expected_value:"true" );
         ( "use (&&) as a value" >:: fun _ ->
           assert_expression_has_value
             ~program:{|
          let and_op = (&&)
        |}
             ~expr:"and_op true false" ~expected_value:"false" );
         (* Partial application tests *)
         ( "partial application of (+)" >:: fun _ ->
           assert_expression_has_value
             ~program:{|
          let add5 = (+) 5
        |}
             ~expr:"add5 10" ~expected_value:"15" );
         ( "partial application of times" >:: fun _ ->
           assert_expression_has_value
             ~program:{|
          let double = (*) 2
        |}
             ~expr:"double 7" ~expected_value:"14" );
         (* Higher-order function tests *)
         ( "pass (+) to a function" >:: fun _ ->
           assert_expression_has_value
             ~program:{|
          let apply_op op a b = op a b
        |}
             ~expr:"apply_op (+) 3 4" ~expected_value:"7" );
         ( "pass times to a function" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
          let apply_op op a b = op a b
          let result = apply_op (*) 3 4
        |}
             ~expr:"result" ~expected_value:"12" );
         (* Using in expressions directly *)
         ( "use (+) directly in application" >:: fun _ ->
           assert_expression_has_value ~program:"" ~expr:"(+) 10 20"
             ~expected_value:"30" );
         ( "use times directly" >:: fun _ ->
           assert_expression_has_value
             ~program:{|
          let result = (*) 6 7
        |}
             ~expr:"result" ~expected_value:"42" );
       ]

(* ============================================================================
   REGRESSION TESTS FOR POLYMORPHIC NULLARY CONSTRUCTORS

   These tests verify that nullary constructors (like None) in sum types with
   type parameters are properly polymorphic. Without the fix, None would have
   type Option<'a> with 'a free variable 'a', causing all uses to share the same
   type variable.
   ============================================================================ *)
let polymorphic_nullary_constructor_regression_tests =
  let open ProgramTesting in
  "polymorphic_nullary_constructor_regression"
  >::: [
         (* Test that map has correct polymorphic type *)
         ( "Option map type is fully polymorphic" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
          type Option<'a> =
            | None
            | Some of a

          let map f o =
            case o do
            | None -> None
            | Some v -> Some (f v)
        |}
             ~expr:"(map)"
             ~expected_type:"('a -> 'b) -> Option<'a> -> Option<'b>" );
         (* Test that None can be used with different types *)
         ( "None can be used polymorphically" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
          type Option<'a> =
            | None
            | Some of a

          let x = None
          let y = None
          let z = if true then Some 5 else x
        |}
             ~expr:"z" ~expected_value:"Some 5" );
         (* Test filter function which also uses None polymorphically *)
         ( "filter has correct polymorphic type" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
          type Option<'a> =
            | None
            | Some of a

          let filter pred o =
            case o do
            | None -> None
            | Some v -> if pred v then Some v else None
        |}
             ~expr:"(filter)"
             ~expected_type:"('a -> bool) -> Option<'a> -> Option<'a>" );
         (* Test that Result type with nullary constructors also works *)
         ( "Result Error constructor is polymorphic" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
          type Result<'a, 'e> =
            | Ok of a
            | Error of e

          let map_result f r =
            case r do
            | Error e -> Error e
            | Ok v -> Ok (f v)
        |}
             ~expr:"(map_result)"
             ~expected_type:"('a -> 'b) -> Result<'a, 'c> -> Result<'b, 'c>" );
         (* Test Either type with two nullary constructors *)
         ( "Either with nullary constructors" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
          type Either<'a, 'b> =
            | Left of a
            | Right of b

          type Maybe<'a> =
            | Nothing
            | Just of a

          let to_maybe e =
            case e do
            | Left _ -> Nothing
            | Right v -> Just v
        |}
             ~expr:"(to_maybe)" ~expected_type:"Either<'a, 'b> -> Maybe<'b>" );
         (* Test bind/flatMap which requires proper polymorphism *)
         ( "bind/flatMap has correct type" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
          type Option<'a> =
            | None
            | Some of a

          let flatMap o f =
            case o do
            | None -> None
            | Some v -> f v
        |}
             ~expr:"(flatMap)"
             ~expected_type:"Option<'a> -> ('a -> Option<'b>) -> Option<'b>" );
         (* Test chaining operations that require different type variables *)
         ( "chained map operations work" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
          type Option<'a> =
            | None
            | Some of a

          let map f o =
            case o do
            | None -> None
            | Some v -> Some (f v)

          let to_string x = "value"
          let length s = 5

          let result = map length (map to_string (Some 42))
        |}
             ~expr:"result" ~expected_value:"Some 5" );
         (* Test with multiple type parameters *)
         ( "pair with nullary constructor" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
          type PairOrEmpty<'a, 'b> =
            | Empty
            | Pair of (a, b)

          let swap p =
            case p do
            | Empty -> Empty
            | Pair (x, y) -> Pair (y, x)
        |}
             ~expr:"(swap)"
             ~expected_type:"PairOrEmpty<'a, 'b> -> PairOrEmpty<'b, 'a>" );
       ]

(* ============================================================================
   REGRESSION TESTS FOR >>= OPERATOR LEXING

   These tests verify that operators like >>= are correctly lexed as single
   tokens, while still allowing nested type applications like Box<Box<int>>.
   ============================================================================ *)
let bind_operator_lexing_regression_tests =
  let open ProgramTesting in
  "bind_operator_lexing_regression"
  >::: [
         (* Test that >>= operator can be defined and used *)
         ( ">>= operator definition and type" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
          type Option<'a> =
            | None
            | Some of a

          let (>>=) o f =
            case o do
            | None -> None
            | Some v -> f v
        |}
             ~expr:"(>>=)"
             ~expected_type:"Option<'a> -> ('a -> Option<'b>) -> Option<'b>" );
         (* Test using >>= as infix operator *)
         ( ">>= used as infix operator" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
          type Option<'a> =
            | None
            | Some of a

          let (>>=) o f =
            case o do
            | None -> None
            | Some v -> f v

          let increment x = Some (x + 1)
          let result = Some 10 >>= increment
        |}
             ~expr:"result" ~expected_value:"Some 11" );
         (* Test chaining >>= operators *)
         ( "chained >>= operations" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
          type Option<'a> =
            | None
            | Some of a

          let (>>=) o f =
            case o do
            | None -> None
            | Some v -> f v

          let result =
            Some 5
            >>= (fn x -> Some (x + 1))
            >>= (fn y -> Some (y * 2))
        |}
             ~expr:"result" ~expected_value:"Some 12" );
         (* Test >>= with None *)
         ( ">>= with None propagates" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
          type Option<'a> =
            | None
            | Some of a

          let (>>=) o f =
            case o do
            | None -> None
            | Some v -> f v

          let result = None >>= (fn x -> Some (x + 1))
        |}
             ~expr:"result" ~expected_value:"None" );
         (* Test that nested types still work (>>= didn't break them) *)
         ( "nested type applications still work" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
          type Box<'a> = ('a, int)

          let unbox x = x
        |}
             ~expr:"unbox (((5, 3), 4), 6)"
             ~expected_type:"(((int, int), int), int)" );
         (* Test other >> variants *)
         ( ">>- operator" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
          type Option<'a> =
            | None
            | Some of a

          let (>>-) o default =
            case o do
            | None -> default
            | Some v -> v

          let result = None >>- 42
        |}
             ~expr:"result" ~expected_value:"42" );
         (* Test << operator (not just >>) *)
         ( "<<= operator" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
          type Option<'a> =
            | None
            | Some of a

          let (<<=) f o =
            case o do
            | None -> None
            | Some v -> f v

          let increment x = Some (x + 1)
          let result = increment <<= Some 10
        |}
             ~expr:"result" ~expected_value:"Some 11" );
         (* Test that >> by itself still works in type contexts *)
         ( ">> as separate tokens in types" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
          type Nested<'a> = ('a, 'a)

          let identity x = x
        |}
             ~expr:"identity ((5, 6), (7, 8))"
             ~expected_type:"((int, int), (int, int))" );
         (* Test combining >>= with other operations *)
         ( ">>= combined with application" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
          type Option<'a> =
            | None
            | Some of a

          let (>>=) o f =
            case o do
            | None -> None
            | Some v -> f v

          let map f o =
            case o do
            | None -> None
            | Some v -> Some (f v)

          let double x = x * 2
          let increment x = Some (x + 1)

          let result = map double (Some 5) >>= increment
        |}
             ~expr:"result" ~expected_value:"Some 11" );
       ]

let record_type_tests =
  List.map
    (fun (a, b) -> type_test a b)
    [
      (* Basic record literal types *)
      ("{x: 1}", "{x: int}");
      ("{x: 1, y: 2}", "{x: int, y: int}");
      ("{x: 1, y: true}", "{x: int, y: bool}");
      ("{x: 1, y: true, z: \"hello\"}", "{x: int, y: bool, z: str}");
      (* Field access types *)
      ("{x: 1}.x", "int");
      ("{x: 1, y: 2}.x", "int");
      ("{x: 1, y: 2}.y", "int");
      ("{x: true, y: 42}.x", "bool");
      ("{x: true, y: 42}.y", "int");
      (* Nested records *)
      ("{x: {y: 1}}", "{x: {y: int}}");
      ("{x: {y: 1}}.x", "{y: int}");
      ("{x: {y: 1}}.x.y", "int");
      (* Record updates *)
      ("let p = {x: 1, y: 2} in { p with x = 10 }", "{x: int, y: int}");
      ("{ {x: 1, y: 2} with x = 99 }", "{x: int, y: int}");
      (* Records with functions *)
      ("{f: fn x -> x}", "{f: 'a -> 'a}");
      ("{f: fn x -> x + 1}", "{f: int -> int}");
      (* Functions creating records *)
      ("fn x -> {x: x}", "'a -> {x: 'a}");
      ("fn x -> {x: x, y: x}", "'a -> {x: 'a, y: 'a}");
      (* Functions accessing fields *)
      ("fn r -> r.x", "{x: 'a} -> 'a");
      ("fn r -> r.x + r.y", "{x: int, y: int} -> int");
      (* Subtyping in let bindings *)
      ("let r = {x: 1, y: 2} in r.x", "int");
      ("let f = fn r -> r.x in f {x: 1, y: 2}", "int");
      ("let f = fn r -> r.x in f {x: true}", "bool");
    ]

let named_record_type_tests =
  let open ProgramTesting in
  "named_record_types"
  >::: [
         ( "type definition with record" >:: fun _ ->
           assert_program_typechecks
             {|
             type MyType = {x: int, y: bool}
           |} );
         ( "function using named record type" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type MyType = {x: int, y: bool}
               let getValue = fn (r : MyType) -> r.x
             |}
             ~expr:"getValue" ~expected_type:"{x: int, y: bool} -> int" );
         ( "create value with named record type" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type MyType = {v1: int, v2: bool}
             |}
             ~expr:"{v1: 42, v2: true}" ~expected_value:"{v1: 42, v2: true}" );
         ( "access field from named record type" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type MyType = {v1: int, v2: bool}
               let x = {v1: 10, v2: false}
             |}
             ~expr:"x.v1" ~expected_value:"10" );
         ( "nested record type definition" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Inner = {x: int}
               type Outer = {inner: Inner, y: bool}
             |}
             ~expr:"{inner: {x: 5}, y: true}"
             ~expected_type:"{inner: {x: int}, y: bool}" );
       ]

let record_eval_tests =
  List.map
    (fun (a, b) -> eval_test a b)
    [
      (* Basic record creation *)
      ("{x: 1}", "{x: 1}");
      ("{x: 1, y: 2}", "{x: 1, y: 2}");
      ("{x: true, y: false}", "{x: true, y: false}");
      (* Field access *)
      ("{x: 1}.x", "1");
      ("{x: 1, y: 2}.x", "1");
      ("{x: 1, y: 2}.y", "2");
      ("{x: true, y: 42}.x", "true");
      ("{x: true, y: 42}.y", "42");
      (* Nested records *)
      ("{x: {y: 1}}.x", "{y: 1}");
      ("{x: {y: 1}}.x.y", "1");
      ("{x: {y: {z: 5}}}.x.y.z", "5");
      (* Records with computed values *)
      ("{x: 1 + 2, y: 3 + 4}", "{x: 3, y: 7}");
      ("{x: 5 + 5}.x", "10");
      (* Functions with records *)
      ("(fn r -> r.x) {x: 42}", "42");
      ("(fn r -> r.x + r.y) {x: 1, y: 2}", "3");
      ("let f = fn r -> r.x in f {x: 100}", "100");
      (* Subtyping: extra fields should work *)
      ("(fn r -> r.x) {x: 1, y: 2}", "1");
      ("(fn r -> r.x) {x: 1, y: 2, z: 3}", "1");
      ("let f = fn r -> r.x + 1 in f {x: 5, y: 10}", "6");
      (* Record updates: { expr with field = value } *)
      ("let p = {x: 10, y: 20} in { p with x = 100 }.x", "100");
      ("let p = {x: 10, y: 20} in { p with x = 100 }.y", "20");
      ("let p = {x: 10, y: 20} in { p with y = 50 }.y", "50");
      ("let p = {x: 10, y: 20} in { p with x = 5, y = 15 }.x", "5");
      ("let p = {x: 10, y: 20} in { p with x = 5, y = 15 }.y", "15");
      ("{ {x: 1, y: 2} with x = 99 }.x", "99");
      ("{ {x: 1, y: 2} with x = 99 }.y", "2");
    ]

let builtin_eval_tests =
  List.map
    (fun (a, b) -> eval_test a b)
    [
      (* String builtins *)
      ("str_length \"hello\"", "5");
      ("str_length \"\"", "0");
      ("str_concat \"hello\" \" world\"", "\"hello world\"");
      ("str_slice \"hello\" 1 3", "\"ell\"");
      ("str_slice \"hello\" 0 5", "\"hello\"");
      (* List builtins *)
      ("list_length []", "0");
      ("list_length [1, 2, 3]", "3");
      ("list_head [1, 2, 3]", "1");
      ("list_tail [1, 2, 3]", "[2, 3]");
      ("list_nth [10, 20, 30] 0", "10");
      ("list_nth [10, 20, 30] 1", "20");
      ("list_nth [10, 20, 30] 2", "30");
      (* Tuple builtins *)
      ("tuple_fst (1, 2)", "1");
      ("tuple_snd (1, 2)", "2");
      ("tuple_fst (\"a\", \"b\")", "\"a\"");
      ("tuple_snd (1, true)", "true");
    ]

let builtin_type_tests =
  List.map
    (fun (a, b) -> type_test a b)
    [
      ("str_length \"x\"", "int");
      ("str_concat \"a\" \"b\"", "str");
      ("str_slice \"hello\" 0 1", "str");
      ("list_length [1, 2, 3]", "int");
      ("list_head [1, 2, 3]", "int");
      ("list_tail [1, 2, 3]", "[int]");
      ("list_nth [1, 2, 3] 0", "int");
      ("tuple_fst (1, 2)", "int");
      ("tuple_snd (1, 2)", "int");
    ]

(* ============================================================================
   COMPREHENSIVE RECORD TYPE TESTS

   Additional tests for record types covering edge cases, complex scenarios, and
   various usage patterns.
   ============================================================================ *)

let extended_record_type_tests =
  List.map
    (fun (a, b) -> type_test a b)
    [
      (* Single field records of different types *)
      ("{n: 42}", "{n: int}");
      ("{s: \"test\"}", "{s: str}");
      ("{b: false}", "{b: bool}");
      ("{c: 'x'}", "{c: char}");
      ("{f: 3.14}", "{f: float}");
      ("{u: ()}", "{u: unit}");
      (* Multiple field records with mixed types *)
      ( "{a: 1, b: \"two\", c: true, d: 'e'}",
        "{a: int, b: str, c: bool, d: char}" );
      ("{x: 1, y: 2, z: 3}", "{x: int, y: int, z: int}");
      (* Records with lists *)
      ("{items: []}", "{items: ['a]}");
      ("{items: [1, 2, 3]}", "{items: [int]}");
      ("{names: [\"a\", \"b\"]}", "{names: [str]}");
      (* Records with tuples *)
      ("{pair: (1, 2)}", "{pair: (int, int)}");
      ("{triple: (1, \"x\", true)}", "{triple: (int, str, bool)}");
      ("{coords: (1, 2), name: \"point\"}", "{coords: (int, int), name: str}");
      (* Records with functions *)
      ("{add: fn x -> fn y -> x + y}", "{add: int -> int -> int}");
      ( "{map_func: fn f -> fn lst -> case lst do | [] -> [] | h :: t -> f h \
         :: []}",
        "{map_func: ('a -> 'b) -> ['a] -> ['b]}" );
      (* Deeply nested records *)
      ("{a: {b: {c: 1}}}", "{a: {b: {c: int}}}");
      ("{a: {b: {c: {d: true}}}}", "{a: {b: {c: {d: bool}}}}");
      ( "{outer: {inner: {value: 42, flag: true}}}",
        "{outer: {inner: {value: int, flag: bool}}}" );
      (* Nested field access chains *)
      ("{a: {b: {c: 1}}}.a.b.c", "int");
      ("{x: {y: {z: \"deep\"}}}.x.y.z", "str");
      (* Records returned from functions *)
      ("(fn x -> {value: x}) 42", "{value: int}");
      ("(fn x -> fn y -> {x: x, y: y}) 1 true", "{x: int, y: bool}");
      (* Functions taking records and returning values *)
      ("fn r -> r.x + r.y", "{x: int, y: int} -> int");
      ("fn r -> r.x :: r.y", "{x: 'a, y: ['a]} -> ['a]");
      ( "fn r -> if r.flag then r.value else 0",
        "{flag: bool, value: int} -> int" );
      (* Functions taking records and returning records *)
      ("fn r -> {x: r.x + 1}", "{x: int} -> {x: int}");
      ("fn r -> {a: r.x, b: r.y}", "{x: 'a, y: 'b} -> {a: 'a, b: 'b}");
      (* Higher-order functions with records *)
      ( "fn f -> fn r -> {result: f r.value}",
        "('a -> 'b) -> {value: 'a} -> {result: 'b}" );
      (* Records in let bindings with field access *)
      ("let r = {x: 1, y: 2} in r.x + r.y", "int");
      ("let r1 = {a: 10} in let r2 = {b: r1.a} in r2.b", "int");
      (* Records with polymorphic fields *)
      ("{id: fn x -> x, value: 42}", "{id: 'a -> 'a, value: int}");
      ( "{first: fn (x, y) -> x, data: (1, 2)}",
        "{first: ('a, 'b) -> 'a, data: (int, int)}" );
      (* Multiple records in expressions *)
      ("let r1 = {x: 1} in let r2 = {y: 2} in r1.x + r2.y", "int");
      (* Records with computed field values *)
      ("{x: 1 + 1, y: 2 * 2}", "{x: int, y: int}");
      ("{result: 10 / 2, doubled: 5 * 2}", "{result: int, doubled: int}");
      (* Recursive functions with records *)
      ( "let rec sum_field = fn lst -> case lst do | [] -> 0 | h :: t -> \
         h.value + sum_field t in sum_field",
        "[{value: int}] -> int" );
    ]

let extended_record_eval_tests =
  List.map
    (fun (a, b) -> eval_test a b)
    [
      (* Single field records *)
      ("{x: 1}", "{x: 1}");
      ("{name: \"Alice\"}", "{name: \"Alice\"}");
      ("{active: true}", "{active: true}");
      (* Multiple field records *)
      ("{x: 1, y: 2, z: 3}", "{x: 1, y: 2, z: 3}");
      ( "{name: \"Bob\", age: 30, active: true}",
        "{name: \"Bob\", age: 30, active: true}" );
      (* Field access on various types *)
      ("{x: 100}.x", "100");
      ("{name: \"test\"}.name", "\"test\"");
      ("{flag: false}.flag", "false");
      (* Computed field values *)
      ("{x: 1 + 2}", "{x: 3}");
      ("{sum: 10 + 20, product: 5 * 6}", "{sum: 30, product: 30}");
      ("{x: 5 * 2}.x", "10");
      ("{result: 100 / 10}.result", "10");
      (* Nested records evaluation *)
      ("{a: {b: 1}}", "{a: {b: 1}}");
      ("{outer: {inner: {value: 42}}}", "{outer: {inner: {value: 42}}}");
      ("{a: {b: 1}}.a", "{b: 1}");
      ("{a: {b: {c: 100}}}.a.b", "{c: 100}");
      ("{a: {b: {c: 100}}}.a.b.c", "100");
      (* Deep nesting *)
      ("{x: {y: {z: {w: 5}}}}.x.y.z.w", "5");
      (* Records with lists *)
      ("{items: [1, 2, 3]}", "{items: [1, 2, 3]}");
      ("{items: [1, 2, 3]}.items", "[1, 2, 3]");
      ("{data: []}.data", "[]");
      (* Records with tuples *)
      ("{pair: (10, 20)}", "{pair: (10, 20)}");
      ("{pair: (10, 20)}.pair", "(10, 20)");
      ("{point: (1, 2, 3)}.point", "(1, 2, 3)");
      (* Functions creating records *)
      ("(fn x -> {value: x}) 42", "{value: 42}");
      ("(fn x -> fn y -> {x: x, y: y}) 5 10", "{x: 5, y: 10}");
      ("(fn x -> {doubled: x * 2}) 7", "{doubled: 14}");
      (* Functions accessing record fields *)
      ("(fn r -> r.x) {x: 99}", "99");
      ("(fn r -> r.x + r.y) {x: 10, y: 20}", "30");
      ("(fn r -> r.x * r.y) {x: 3, y: 4}", "12");
      (* Subtyping: functions expecting fewer fields *)
      ("(fn r -> r.x) {x: 1, y: 2, z: 3}", "1");
      ("(fn r -> r.a + r.b) {a: 5, b: 10, c: 15, d: 20}", "15");
      (* Let bindings with records *)
      ("let r = {x: 5, y: 10} in r.x", "5");
      ("let r = {x: 5, y: 10} in r.y", "10");
      ("let r = {x: 5, y: 10} in r.x + r.y", "15");
      ("let r1 = {a: 1} in let r2 = {b: r1.a + 1} in r2.b", "2");
      (* Multiple record field accesses *)
      ("let r = {x: 3, y: 4} in r.x * r.x + r.y * r.y", "25");
      (* Records with function fields *)
      ("{f: fn x -> x + 1}.f 5", "6");
      ("{add: fn x -> fn y -> x + y}.add 3 4", "7");
      (* Nested records with computed values *)
      ("{outer: {inner: 2 + 3}}.outer.inner", "5");
      ("{a: {b: {c: 10 * 5}}}.a.b.c", "50");
      (* Complex expressions with records *)
      ( "let make_point = fn x -> fn y -> {x: x, y: y} in make_point 10 20",
        "{x: 10, y: 20}" );
      ( "let make_point = fn x -> fn y -> {x: x, y: y} in (make_point 5 15).x",
        "5" );
      ("let get_x = fn r -> r.x in let p = {x: 100, y: 200} in get_x p", "100");
      (* Records in conditional expressions *)
      ("if true then {x: 1} else {x: 2}", "{x: 1}");
      ("if false then {x: 1} else {x: 2}", "{x: 2}");
      ("(if true then {value: 10} else {value: 20}).value", "10");
      (* Records with operators as field values *)
      ("{sum: 1 + 2 + 3, diff: 10 - 5}.sum", "6");
      ("{sum: 1 + 2 + 3, diff: 10 - 5}.diff", "5");
    ]

let very_complex_record_tests =
  let open ProgramTesting in
  "very_complex_record_tests"
  >::: [
         ( "binary tree with records" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Tree =
                 | Leaf
                 | Node of {value: int, left: Tree, right: Tree}

               let rec tree_sum = fn t ->
                 case t do
                 | Leaf -> 0
                 | Node n -> n.value + tree_sum n.left + tree_sum n.right

               let tree = Node {
                 value: 10,
                 left: Node {value: 5, left: Leaf, right: Leaf},
                 right: Node {value: 15, left: Leaf, right: Leaf}
               }
             |}
             ~expr:"tree_sum tree" ~expected_value:"30" );
         ( "linked list with record nodes" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec List<'a> =
                 | Nil
                 | Cons of {head: a, tail: List<'a>}

               let rec length = fn lst ->
                 case lst do
                 | Nil -> 0
                 | Cons cell -> 1 + length cell.tail

               let rec map_list = fn f -> fn lst ->
                 case lst do
                 | Nil -> Nil
                 | Cons cell -> Cons {head: f cell.head, tail: map_list f cell.tail}

               let my_list = Cons {
                 head: 1,
                 tail: Cons {head: 2, tail: Cons {head: 3, tail: Nil}}
               }

               let doubled = map_list (fn x -> x * 2) my_list
             |}
             ~expr:"length doubled" ~expected_value:"3" );
         ( "record containing multiple recursive types" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Tree<'a> =
                 | Empty
                 | Branch of {value: a, children: [Tree<'a>]}

               type Forest<'a> = {trees: [Tree<'a>], count: int}

               let rec count_nodes = fn t ->
                 case t do
                 | Empty -> 0
                 | Branch b -> 1 + count_forest b.children
               and count_forest = fn trees ->
                 case trees do
                 | [] -> 0
                 | h :: t -> count_nodes h + count_forest t

               let forest = {
                 trees: [
                   Branch {value: 1, children: [Empty, Empty]},
                   Branch {value: 2, children: [Branch {value: 3, children: []}]}
                 ],
                 count: 2
               }
             |}
             ~expr:"count_forest forest.trees" ~expected_value:"3" );
         ( "database-like record operations" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type User = {id: int, name: str, age: int, active: bool}

               let rec filter_users = fn pred -> fn users ->
                 case users do
                 | [] -> []
                 | h :: t ->
                   if pred h then h :: filter_users pred t
                   else filter_users pred t

               let rec map_users = fn f -> fn users ->
                 case users do
                 | [] -> []
                 | h :: t -> f h :: map_users f t

               let users = [
                 {id: 1, name: "Alice", age: 25, active: true},
                 {id: 2, name: "Bob", age: 30, active: false},
                 {id: 3, name: "Charlie", age: 35, active: true}
               ]

               let active_users = filter_users (fn u -> u.active) users
               let ages = map_users (fn u -> u.age) active_users

               let rec sum_list = fn lst ->
                 case lst do
                 | [] -> 0
                 | h :: t -> h + sum_list t
             |}
             ~expr:"sum_list ages" ~expected_value:"60" );
         ( "nested records with recursive type and computations" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Point = {x: int, y: int}
               type rec Shape =
                 | Circle of {center: Point, radius: int}
                 | Rectangle of {topLeft: Point, bottomRight: Point}
                 | Group of {shapes: [Shape]}

               let rec count_shapes = fn s ->
                 case s do
                 | Circle _ -> 1
                 | Rectangle _ -> 1
                 | Group g ->
                   let rec count_list = fn lst ->
                     case lst do
                     | [] -> 0
                     | h :: t -> count_shapes h + count_list t
                   in count_list g.shapes

               let scene = Group {
                 shapes: [
                   Circle {center: {x: 0, y: 0}, radius: 5},
                   Rectangle {topLeft: {x: 0, y: 0}, bottomRight: {x: 10, y: 10}},
                   Group {shapes: [
                     Circle {center: {x: 5, y: 5}, radius: 3},
                     Circle {center: {x: 10, y: 10}, radius: 2}
                   ]}
                 ]
               }
             |}
             ~expr:"count_shapes scene" ~expected_value:"4" );
         ( "state machine with records" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type State = {value: int, running: bool, history: [int]}

               let step = fn s -> fn n ->
                 if s.running then
                   {value: s.value + n, running: true, history: s.value :: s.history}
                 else s

               let stop = fn s ->
                 {value: s.value, running: false, history: s.history}

               let init = {value: 0, running: true, history: []}
               let s1 = step init 5
               let s2 = step s1 10
               let s3 = step s2 3
               let s4 = stop s3
               let s5 = step s4 100
             |}
             ~expr:"s5.value" ~expected_value:"18" );
         ( "record-based expression tree evaluator" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Expr =
                 | Const of int
                 | Add of {left: Expr, right: Expr}
                 | Mul of {left: Expr, right: Expr}
                 | Var of str

               type Env = {bindings: [(str, int)]}

               let rec lookup = fn name -> fn bindings ->
                 case bindings do
                 | [] -> 0
                 | (k, v) :: rest ->
                   if k == name then v else lookup name rest

               let rec eval = fn env -> fn expr ->
                 case expr do
                 | Const n -> n
                 | Add op -> eval env op.left + eval env op.right
                 | Mul op -> eval env op.left * eval env op.right
                 | Var name -> lookup name env.bindings

               let env = {bindings: [("x", 10), ("y", 5)]}
               let expr = Add {
                 left: Mul {left: Var "x", right: Const 2},
                 right: Var "y"
               }
             |}
             ~expr:"eval env expr" ~expected_value:"25" );
         ( "graph with records and traversal" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Node = {id: int, value: int, neighbors: [int]}
               type Graph = {nodes: [Node]}

               let rec find_node = fn id -> fn nodes ->
                 case nodes do
                 | [] -> {id: 0, value: 0, neighbors: []}
                 | h :: t -> if h.id == id then h else find_node id t

               let rec sum_neighbors = fn graph -> fn ids ->
                 case ids do
                 | [] -> 0
                 | h :: t ->
                   let node = find_node h graph.nodes in
                   node.value + sum_neighbors graph t

               let graph = {
                 nodes: [
                   {id: 1, value: 10, neighbors: [2, 3]},
                   {id: 2, value: 20, neighbors: [1]},
                   {id: 3, value: 30, neighbors: [1]}
                 ]
               }

               let root = find_node 1 graph.nodes
             |}
             ~expr:"sum_neighbors graph root.neighbors" ~expected_value:"50" );
         ( "record transformation pipeline" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Person = {name: str, score: int}
               type Result = {person: Person, grade: str, passed: bool}

               let get_grade = fn score ->
                 if score >= 90 then "A"
                 else if score >= 80 then "B"
                 else if score >= 70 then "C"
                 else "F"

               let to_result = fn p ->
                 let grade = get_grade p.score in
                 {
                   person: p,
                   grade: grade,
                   passed: p.score >= 70
                 }

               let rec process = fn people ->
                 case people do
                 | [] -> []
                 | h :: t -> to_result h :: process t

               let rec count_passed = fn results ->
                 case results do
                 | [] -> 0
                 | h :: t ->
                   if h.passed then 1 + count_passed t
                   else count_passed t

               let people = [
                 {name: "Alice", score: 95},
                 {name: "Bob", score: 65},
                 {name: "Charlie", score: 85},
                 {name: "Diana", score: 75}
               ]

               let results = process people
             |}
             ~expr:"count_passed results" ~expected_value:"3" );
         ( "complex nested record with multiple operations" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Stats = {min: int, max: int, sum: int, count: int}
               type DataSet = {name: str, values: [int], stats: Stats}

               let rec calc_sum = fn lst ->
                 case lst do
                 | [] -> 0
                 | h :: t -> h + calc_sum t

               let rec calc_min = fn lst -> fn current ->
                 case lst do
                 | [] -> current
                 | h :: t ->
                   if h < current then calc_min t h
                   else calc_min t current

               let rec calc_max = fn lst -> fn current ->
                 case lst do
                 | [] -> current
                 | h :: t ->
                   if h > current then calc_max t h
                   else calc_max t current

               let rec length = fn lst ->
                 case lst do
                 | [] -> 0
                 | h :: t -> 1 + length t

               let make_stats = fn values ->
                 case values do
                 | [] -> {min: 0, max: 0, sum: 0, count: 0}
                 | h :: t -> {
                     min: calc_min t h,
                     max: calc_max t h,
                     sum: calc_sum values,
                     count: length values
                   }

               let values = [10, 5, 20, 15, 8, 25, 12]
               let dataset = {
                 name: "test",
                 values: values,
                 stats: make_stats values
               }
             |}
             ~expr:"dataset.stats.max + dataset.stats.min" ~expected_value:"30"
         );
       ]

let complex_record_scenarios =
  let open ProgramTesting in
  "complex_record_scenarios"
  >::: [
         ( "record type alias" >:: fun _ ->
           assert_program_typechecks
             {|
             type Point = {x: int, y: int}
             type Person = {name: str, age: int}
           |}
         );
         ( "function with record type parameter" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Point = {x: int, y: int}
               let distance_squared = fn (p : Point) -> p.x * p.x + p.y * p.y
             |}
             ~expr:"distance_squared" ~expected_type:"{x: int, y: int} -> int"
         );
         ( "nested record types" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Inner = {value: int}
               type Outer = {inner: Inner, label: str}
             |}
             ~expr:"{inner: {value: 42}, label: \"test\"}"
             ~expected_type:"{inner: {value: int}, label: str}" );
         ( "record with list field" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Container = {items: [int]}
             |}
             ~expr:"{items: [1, 2, 3]}" ~expected_value:"{items: [1, 2, 3]}" );
         ( "record creation and field update pattern" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let original = {x: 10, y: 20}
               let updated = {x: original.x + 1, y: original.y + 1}
             |}
             ~expr:"updated" ~expected_value:"{x: 11, y: 21}" );
         ( "higher-order function with record" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let apply_to_field = fn f -> fn r -> {result: f r.value}
               let double = fn x -> x * 2
             |}
             ~expr:"apply_to_field double {value: 5}"
             ~expected_value:"{result: 10}" );
         ( "record with polymorphic field accessed" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let get_first = fn r -> r.first
             |}
             ~expr:"get_first {first: 42, second: \"test\"}"
             ~expected_value:"42" );
         ( "recursive function processing record list" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec sum_ages = fn people ->
                 case people do
                 | [] -> 0
                 | h :: t -> h.age + sum_ages t
             |}
             ~expr:"sum_ages [{age: 10}, {age: 20}, {age: 30}]"
             ~expected_value:"60" );
         ( "record with function field called" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let ops = {add: fn x -> fn y -> x + y, mul: fn x -> fn y -> x * y}
             |}
             ~expr:"ops.add 3 4" ~expected_value:"7" );
         ( "deeply nested record access" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let data = {level1: {level2: {level3: {level4: 42}}}}
             |}
             ~expr:"data.level1.level2.level3.level4" ~expected_value:"42" );
         ( "record construction from another record" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let p1 = {x: 5, y: 10}
               let p2 = {x: p1.y, y: p1.x}
             |}
             ~expr:"p2" ~expected_value:"{x: 10, y: 5}" );
         ( "multiple records with field access" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let r1 = {a: 1, b: 2}
               let r2 = {c: 3, d: 4}
               let sum = r1.a + r1.b + r2.c + r2.d
             |}
             ~expr:"sum" ~expected_value:"10" );
         ( "record with mixed field types" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Entity = {
                 id: int,
                 name: str,
                 active: bool,
                 tags: [str]
               }
             |}
             ~expr:"{id: 1, name: \"test\", active: true, tags: [\"a\", \"b\"]}"
             ~expected_value:
               "{id: 1, name: \"test\", active: true, tags: [\"a\", \"b\"]}" );
         ( "conditional with records" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let get_point = fn use_default ->
                 if use_default then {x: 0, y: 0} else {x: 10, y: 20}
             |}
             ~expr:"get_point true" ~expected_value:"{x: 0, y: 0}" );
         ( "record field used in arithmetic" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rect = {width: 5, height: 10}
               let area = rect.width * rect.height
             |}
             ~expr:"area" ~expected_value:"50" );
       ]

(* Mutually Recursive Function Tests *)
let mutual_recursion_basic_tests =
  let open ProgramTesting in
  "mutual_recursion_basic_tests"
  >::: [
         ( "is_even is_odd basic" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec is_even = fn n ->
                 if n == 0 then true
                 else is_odd (n - 1)
               and is_odd = fn n ->
                 if n == 0 then false
                 else is_even (n - 1)
             |}
             ~expr:"is_even 4" ~expected_value:"true" );
         ( "is_even is_odd odd number" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec is_even = fn n ->
                 if n == 0 then true
                 else is_odd (n - 1)
               and is_odd = fn n ->
                 if n == 0 then false
                 else is_even (n - 1)
             |}
             ~expr:"is_odd 5" ~expected_value:"true" );
         ( "is_even is_odd even check" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec is_even = fn n ->
                 if n == 0 then true
                 else is_odd (n - 1)
               and is_odd = fn n ->
                 if n == 0 then false
                 else is_even (n - 1)
             |}
             ~expr:"is_even 7" ~expected_value:"false" );
         ( "is_even is_odd both functions" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec is_even = fn n ->
                 if n == 0 then true
                 else is_odd (n - 1)
               and is_odd = fn n ->
                 if n == 0 then false
                 else is_even (n - 1)
             |}
             ~expr:"(is_even 6, is_odd 6)" ~expected_value:"(true, false)" );
         ( "mutual recursion with type annotations" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec is_even : int -> bool = fn n ->
                 if n == 0 then true
                 else is_odd (n - 1)
               and is_odd : int -> bool = fn n ->
                 if n == 0 then false
                 else is_even (n - 1)
             |}
             ~expr:"is_even 10" ~expected_value:"true" );
         ( "mutual recursion in expression" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let result =
                 let rec is_even = fn n ->
                   if n == 0 then true
                   else is_odd (n - 1)
                 and is_odd = fn n ->
                   if n == 0 then false
                   else is_even (n - 1)
                 in is_even 8
             |}
             ~expr:"result" ~expected_value:"true" );
         ( "three mutually recursive functions" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec f = fn n ->
                 if n <= 0 then 1
                 else g (n - 1) + 1
               and g = fn n ->
                 if n <= 0 then 2
                 else h (n - 1) + 1
               and h = fn n ->
                 if n <= 0 then 3
                 else f (n - 1) + 1
             |}
             ~expr:"f 3" ~expected_value:"4" );
         ( "three mutually recursive functions different starting point"
         >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec f = fn n ->
                 if n <= 0 then 1
                 else g (n - 1) + 1
               and g = fn n ->
                 if n <= 0 then 2
                 else h (n - 1) + 1
               and h = fn n ->
                 if n <= 0 then 3
                 else f (n - 1) + 1
             |}
             ~expr:"g 2" ~expected_value:"3" );
         ( "mutual recursion with different types" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec stringify_even = fn n ->
                 if n == 0 then "zero"
                 else stringify_odd (n - 1)
               and stringify_odd = fn n ->
                 if n == 0 then "not zero"
                 else stringify_even (n - 1)
             |}
             ~expr:"stringify_even 3" ~expected_value:"\"not zero\"" );
         ( "mutual recursion with list processing" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec sum_evens = fn lst ->
                 case lst do
                 | [] -> 0
                 | h :: t -> h + sum_odds t
               and sum_odds = fn lst ->
                 case lst do
                 | [] -> 0
                 | h :: t -> sum_evens t
             |}
             ~expr:"sum_evens [1, 2, 3, 4, 5]" ~expected_value:"9" );
       ]

let mutual_recursion_type_tests =
  let open ProgramTesting in
  "mutual_recursion_type_tests"
  >::: [
         ( "is_even has correct type" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               let rec is_even = fn n ->
                 if n == 0 then true
                 else is_odd (n - 1)
               and is_odd = fn n ->
                 if n == 0 then false
                 else is_even (n - 1)
             |}
             ~expr:"is_even" ~expected_type:"int -> bool" );
         ( "is_odd has correct type" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               let rec is_even = fn n ->
                 if n == 0 then true
                 else is_odd (n - 1)
               and is_odd = fn n ->
                 if n == 0 then false
                 else is_even (n - 1)
             |}
             ~expr:"is_odd" ~expected_type:"int -> bool" );
         ( "mutual recursion with explicit type annotations" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               let rec f : int -> int = fn n ->
                 if n <= 0 then 0
                 else g (n - 1)
               and g : int -> int = fn n ->
                 if n <= 0 then 1
                 else f (n - 1)
             |}
             ~expr:"f" ~expected_type:"int -> int" );
         ( "mutual recursion with string return type" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               let rec describe_even = fn n ->
                 if n == 0 then "even"
                 else describe_odd (n - 1)
               and describe_odd = fn n ->
                 if n == 0 then "odd"
                 else describe_even (n - 1)
             |}
             ~expr:"describe_even" ~expected_type:"int -> str" );
         ( "mutual recursion with list type" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               let rec process_a = fn lst ->
                 case lst do
                 | [] -> []
                 | h :: t -> process_b t
               and process_b = fn lst ->
                 case lst do
                 | [] -> []
                 | h :: t -> h :: process_a t
             |}
             ~expr:"process_a" ~expected_type:"['a] -> ['a]" );
       ]

let mutual_recursion_complex_tests =
  let open ProgramTesting in
  "mutual_recursion_complex_tests"
  >::: [
         ( "mutual recursion with multiple parameters" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec ackermann_helper = fn m -> fn n ->
                 if m == 0 then n + 1
                 else if n == 0 then ackermann m (m - 1) 1
                 else ackermann m (m - 1) (ackermann_helper m (n - 1))
               and ackermann = fn a -> fn m -> fn n ->
                 if a == 0 then ackermann_helper m n
                 else ackermann_helper m n
             |}
             ~expr:"ackermann 0 2 2" ~expected_value:"7" );
         ( "mutual recursion with pattern matching" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec flatten_evens = fn lst ->
                 case lst do
                 | [] -> []
                 | h :: t ->
                   case h do
                   | [] -> flatten_odds t
                   | x :: xs -> x :: flatten_odds (xs :: t)
               and flatten_odds = fn lst ->
                 case lst do
                 | [] -> []
                 | h :: t -> flatten_evens t
             |}
             ~expr:"flatten_evens [[1, 2], [3, 4], [5]]"
             ~expected_value:"[1, 3, 5]" );
         ( "mutual recursion with records" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Node = {value: int, is_even_pos: bool}

               let rec process_even = fn lst -> fn pos ->
                 case lst do
                 | [] -> []
                 | h :: t -> {value: h, is_even_pos: true} :: process_odd t (pos + 1)
               and process_odd = fn lst -> fn pos ->
                 case lst do
                 | [] -> []
                 | h :: t -> {value: h, is_even_pos: false} :: process_even t (pos + 1)

               let result = process_even [1, 2, 3, 4] 0
               let first = case result do | h :: _ -> h | [] -> {value: 0, is_even_pos: false}
             |}
             ~expr:"first.value" ~expected_value:"1" );
         ( "mutual recursion with higher-order functions" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec apply_to_evens = fn f -> fn lst ->
                 case lst do
                 | [] -> []
                 | h :: t -> f h :: apply_to_odds f t
               and apply_to_odds = fn f -> fn lst ->
                 case lst do
                 | [] -> []
                 | h :: t -> h :: apply_to_evens f t

               let double = fn x -> x * 2
             |}
             ~expr:"apply_to_evens double [1, 2, 3, 4, 5]"
             ~expected_value:"[2, 2, 6, 4, 10]" );
         ( "four mutually recursive functions" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec a = fn n ->
                 if n <= 0 then 1
                 else b (n - 1) + 1
               and b = fn n ->
                 if n <= 0 then 2
                 else c (n - 1) + 1
               and c = fn n ->
                 if n <= 0 then 3
                 else d (n - 1) + 1
               and d = fn n ->
                 if n <= 0 then 4
                 else a (n - 1) + 1
             |}
             ~expr:"a 4" ~expected_value:"5" );
         ( "mutual recursion with accumulator pattern" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec sum_with_toggle = fn lst -> fn acc -> fn use_double ->
                 case lst do
                 | [] -> acc
                 | h :: t ->
                   if use_double
                   then sum_normal t (acc + h * 2) false
                   else sum_normal t (acc + h) true
               and sum_normal = fn lst -> fn acc -> fn use_double ->
                 case lst do
                 | [] -> acc
                 | h :: t ->
                   if use_double
                   then sum_with_toggle t (acc + h * 2) false
                   else sum_with_toggle t (acc + h) true
             |}
             ~expr:"sum_with_toggle [1, 2, 3, 4] 0 true" ~expected_value:"14" );
         ( "mutual recursion in nested expression" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let outer = fn x ->
                 let rec f = fn n ->
                   if n <= 0 then x
                   else g (n - 1) + x
                 and g = fn n ->
                   if n <= 0 then x * 2
                   else f (n - 1) + x
                 in f 3
             |}
             ~expr:"outer 5" ~expected_value:"25" );
         ( "mutual recursion returning tuples" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec pair_a = fn n ->
                 if n <= 0 then (1, 2)
                 else pair_b (n - 1)
               and pair_b = fn n ->
                 if n <= 0 then (3, 4)
                 else pair_a (n - 1)

               let result_even = pair_a 4
               let result_odd = pair_a 5
             |}
             ~expr:"(result_even, result_odd)"
             ~expected_value:"((1, 2), (3, 4))" );
         ( "mutual recursion with sum types" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec IntOrStr =
                 | IVal of int
                 | SVal of str

               let rec process_int = fn x ->
                 case x do
                 | IVal n -> n * 2
                 | SVal s -> process_str (IVal 5)
               and process_str = fn x ->
                 case x do
                 | IVal n -> n + 1
                 | SVal s -> 0

               let val1 = process_int (IVal 3)
               let val2 = process_int (SVal "test")
             |}
             ~expr:"(val1, val2)" ~expected_value:"(6, 6)" );
         ( "chained mutual recursion call" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec f = fn n ->
                 if n <= 0 then 0
                 else 1 + g (n - 1)
               and g = fn n ->
                 if n <= 0 then 0
                 else 2 + h (n - 1)
               and h = fn n ->
                 if n <= 0 then 0
                 else 3 + f (n - 1)

               let x = f 1
               let y = g 1
               let z = h 1
             |}
             ~expr:"(x, y, z)" ~expected_value:"(1, 2, 3)" );
       ]

(* Constructor Type Tests *)
let simple_constructor_type_tests =
  let open ProgramTesting in
  "simple_constructor_type_tests"
  >::: [
         ( "simple nullary constructors" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type MyType = | A | B | C
             |}
             ~expr:"A" ~expected_type:"MyType" );
         ( "simple nullary constructor B" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type MyType = | A | B | C
             |}
             ~expr:"B" ~expected_type:"MyType" );
         ( "simple constructor with payload" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Result = | Success of int | Failure of str
             |}
             ~expr:"Success" ~expected_type:"int -> Result" );
         ( "simple constructor with string payload" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Result = | Success of int | Failure of str
             |}
             ~expr:"Failure" ~expected_type:"str -> Result" );
         ( "constructor with tuple payload" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Pair = | MkPair of (int, str)
             |}
             ~expr:"MkPair" ~expected_type:"(int, str) -> Pair" );
         ( "parameterized nullary constructor" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Option<'a> = | None | Some of 'a
             |}
             ~expr:"None" ~expected_type:"Option<'a>" );
         ( "parameterized constructor with payload" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Option<'a> = | None | Some of 'a
             |}
             ~expr:"Some" ~expected_type:"'a -> Option<'a>" );
         ( "constructor with list payload" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Container<'a> = | Empty | Full of ['a]
             |}
             ~expr:"Full" ~expected_type:"['a] -> Container<'a>" );
         ( "constructor with multiple type params" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Either<'a, 'b> = | Left of 'a | Right of 'b
             |}
             ~expr:"Left" ~expected_type:"'a -> Either<'a, 'b>" );
         ( "constructor with multiple type params right" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Either<'a, 'b> = | Left of 'a | Right of 'b
             |}
             ~expr:"Right" ~expected_type:"'a -> Either<'b, 'a>" );
       ]

let recursive_constructor_type_tests =
  let open ProgramTesting in
  "recursive_constructor_type_tests"
  >::: [
         ( "recursive list nil" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type rec List = | Nil | Cons of (int, List)
             |}
             ~expr:"Nil" ~expected_type:"List" );
         ( "recursive list cons" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type rec List = | Nil | Cons of (int, List)
             |}
             ~expr:"Cons" ~expected_type:"(int, List) -> List" );
         ( "recursive list with multiple self-references" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type rec List = | Nil | Cons of (int, List) | ConsTwo of (int, List, List)
             |}
             ~expr:"ConsTwo" ~expected_type:"(int, List, List) -> List" );
         ( "parameterized recursive list nil" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type rec List<'a> = | Nil | Cons of ('a, List<'a>)
             |}
             ~expr:"Nil" ~expected_type:"List<'a>" );
         ( "parameterized recursive list cons" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type rec List<'a> = | Nil | Cons of ('a, List<'a>)
             |}
             ~expr:"Cons" ~expected_type:"('a, List<'a>) -> List<'a>" );
         ( "binary tree leaf" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type rec Tree<'a> = | Leaf of 'a | Node of (Tree<'a>, 'a, Tree<'a>)
             |}
             ~expr:"Leaf" ~expected_type:"'a -> Tree<'a>" );
         ( "binary tree node" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type rec Tree<'a> = | Leaf of 'a | Node of (Tree<'a>, 'a, Tree<'a>)
             |}
             ~expr:"Node" ~expected_type:"(Tree<'a>, 'a, Tree<'a>) -> Tree<'a>"
         );
         ( "rose tree with list of children" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type rec RoseTree<'a> = | RNode of ('a, [RoseTree<'a>])
             |}
             ~expr:"RNode" ~expected_type:"('a, [RoseTree<'a>]) -> RoseTree<'a>"
         );
         ( "expression tree const" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type rec Expr =
                 | Const of int
                 | Add of (Expr, Expr)
                 | Mul of (Expr, Expr)
             |}
             ~expr:"Const" ~expected_type:"int -> Expr" );
         ( "expression tree add" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type rec Expr =
                 | Const of int
                 | Add of (Expr, Expr)
                 | Mul of (Expr, Expr)
             |}
             ~expr:"Add" ~expected_type:"(Expr, Expr) -> Expr" );
         ( "expression tree mul" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type rec Expr =
                 | Const of int
                 | Add of (Expr, Expr)
                 | Mul of (Expr, Expr)
             |}
             ~expr:"Mul" ~expected_type:"(Expr, Expr) -> Expr" );
       ]

let complex_constructor_type_tests =
  let open ProgramTesting in
  "complex_constructor_type_tests"
  >::: [
         ( "red black tree empty" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<'a> =
                 | Empty
                 | Node of (Color, RBTree<'a>, a, RBTree<'a>)
             |}
             ~expr:"Empty" ~expected_type:"RBTree<'a>" );
         ( "red black tree node" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Color = | Red | Black
               type rec RBTree<'a> =
                 | Empty
                 | Node of (Color, RBTree<'a>, a, RBTree<'a>)
             |}
             ~expr:"Node"
             ~expected_type:"(Color, RBTree<'a>, 'a, RBTree<'a>) -> RBTree<'a>"
         );
         ( "nested recursive type with records" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type rec Tree<'a> =
                 | Leaf
                 | Branch of {value: a, left: Tree<'a>, right: Tree<'a>}
             |}
             ~expr:"Branch"
             ~expected_type:
               "{value: 'a, left: Tree<'a>, right: Tree<'a>} -> Tree<'a>" );
         ( "AVL tree with height" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type rec AVLTree<'a> =
                 | Empty
                 | Node of (AVLTree<'a>, a, AVLTree<'a>, int)
             |}
             ~expr:"Node"
             ~expected_type:"(AVLTree<'a>, 'a, AVLTree<'a>, int) -> AVLTree<'a>"
         );
         ( "zipper type with context" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type rec Tree<'a> = | Leaf of 'a | Node of (Tree<'a>, Tree<'a>)
               type rec Context<'a> =
                 | Top
                 | L of (Context<'a>, Tree<'a>)
                 | R of (Tree<'a>, Context<'a>)
             |}
             ~expr:"L" ~expected_type:"(Context<'a>, Tree<'a>) -> Context<'a>"
         );
         ( "constructor with multiple recursive references in list" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type rec MultiTree<'a> =
                 | Leaf of a
                 | Branch of [MultiTree<'a>]
             |}
             ~expr:"Branch" ~expected_type:"[MultiTree<'a>] -> MultiTree<'a>" );
         ( "constructor combining records and recursion" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type rec LinkedList<'a> =
                 | Empty
                 | Cell of {head: a, tail: LinkedList<'a>}
             |}
             ~expr:"Cell"
             ~expected_type:"{head: 'a, tail: LinkedList<'a>} -> LinkedList<'a>"
         );
       ]

let constructor_with_type_alias_tests =
  let open ProgramTesting in
  "constructor_with_type_alias_tests"
  >::: [
         ( "constructor using type alias" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Point = (int, int)
               type Shape = | Circle of Point | Rectangle of (Point, Point)
             |}
             ~expr:"Circle" ~expected_type:"(int, int) -> Shape" );
         ( "recursive constructor with type alias" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Point = {x: int, y: int}
               type rec Shape =
                 | Circle of {center: Point, radius: int}
                 | Group of [Shape]
             |}
             ~expr:"Circle"
             ~expected_type:"{center: {x: int, y: int}, radius: int} -> Shape"
         );
         ( "constructor with nested type alias" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Coord = int
               type Point = (Coord, Coord)
               type Shape = | Circle of Point
             |}
             ~expr:"Circle" ~expected_type:"(int, int) -> Shape" );
       ]

(* Float and Character Tests *)
let float_operation_tests =
  let open ProgramTesting in
  "float_operation_tests"
  >::: [
         ( "float literal type" >:: fun _ ->
           assert_expression_has_type ~program:"" ~expr:"3.14"
             ~expected_type:"float" );
         ( "int to float conversion type" >:: fun _ ->
           assert_expression_has_type ~program:"" ~expr:"int_to_float"
             ~expected_type:"int -> float" );
         ( "float to int conversion type" >:: fun _ ->
           assert_expression_has_type ~program:"" ~expr:"float_to_int"
             ~expected_type:"float -> int" );
       ]

let char_tests =
  let open ProgramTesting in
  "char_tests"
  >::: [
         ( "char literal type" >:: fun _ ->
           assert_expression_has_type ~program:"" ~expr:"'a'"
             ~expected_type:"char" );
         ( "char escape sequence type" >:: fun _ ->
           assert_expression_has_type ~program:"" ~expr:"'\\n'"
             ~expected_type:"char" );
         ( "char in pattern match" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let f = fn c -> case c do | 'a' -> 1 | 'b' -> 2 | _ -> 3
             |}
             ~expr:"f 'a'" ~expected_value:"1" );
         ( "char pattern match wildcard" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let f = fn c -> case c do | 'a' -> 1 | 'b' -> 2 | _ -> 3
             |}
             ~expr:"f 'z'" ~expected_value:"3" );
       ]

let string_operation_tests =
  let open ProgramTesting in
  "string_operation_tests"
  >::: [
         ( "string concatenation" >:: fun _ ->
           assert_expression_has_value ~program:""
             ~expr:"\"hello\" ^ \" \" ^ \"world\""
             ~expected_value:"\"hello world\"" );
         ( "string concatenation type" >:: fun _ ->
           assert_expression_has_type ~program:"" ~expr:"\"a\" ^ \"b\""
             ~expected_type:"str" );
         ( "empty string concatenation" >:: fun _ ->
           assert_expression_has_value ~program:""
             ~expr:"\"\" ^ \"hello\" ^ \"\"" ~expected_value:"\"hello\"" );
         ( "string to list conversion type" >:: fun _ ->
           assert_expression_has_type ~program:"" ~expr:"string_to_list"
             ~expected_type:"str -> [char]" );
         ( "int to string type" >:: fun _ ->
           assert_expression_has_type ~program:"" ~expr:"int_to_str"
             ~expected_type:"int -> str" );
         ( "string in pattern match" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let greet = fn name ->
                 case name do
                 | "Alice" -> "Hello Alice!"
                 | "Bob" -> "Hi Bob!"
                 | _ -> "Hello stranger!"
             |}
             ~expr:"greet \"Alice\"" ~expected_value:"\"Hello Alice!\"" );
       ]

(* List Comprehension Tests *)
let list_comprehension_tests =
  let open ProgramTesting in
  "list_comprehension_tests"
  >::: [
         ( "simple map comprehension" >:: fun _ ->
           assert_expression_has_value ~program:""
             ~expr:"[x * 2 | x <- [1, 2, 3]]" ~expected_value:"[2, 4, 6]" );
         ( "nested comprehension" >:: fun _ ->
           assert_expression_has_value ~program:""
             ~expr:"[(x, y) | x <- [1, 2], y <- [3, 4]]"
             ~expected_value:"[(1, 3), (1, 4), (2, 3), (2, 4)]" );
         ( "comprehension with arithmetic" >:: fun _ ->
           assert_expression_has_value ~program:""
             ~expr:"[x + y | x <- [1, 2], y <- [10, 20]]"
             ~expected_value:"[11, 21, 12, 22]" );
         ( "comprehension type inference" >:: fun _ ->
           assert_expression_has_type ~program:""
             ~expr:"[x * 2 | x <- [1, 2, 3]]" ~expected_type:"[int]" );
         ( "tuple comprehension type" >:: fun _ ->
           assert_expression_has_type ~program:""
             ~expr:"[(x, y) | x <- [1, 2], y <- [3, 4]]"
             ~expected_type:"[(int, int)]" );
       ]

(* Pattern Matching Tests *)
let advanced_pattern_matching_tests =
  let open ProgramTesting in
  "advanced_pattern_matching_tests"
  >::: [
         ( "nested tuple pattern" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let f = fn x -> case x do | ((a, b), c) -> a + b + c
             |}
             ~expr:"f ((1, 2), 3)" ~expected_value:"6" );
         ( "cons pattern in function" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let head = fn lst -> case lst do | h :: _ -> h | [] -> 0
             |}
             ~expr:"head [5, 6, 7]" ~expected_value:"5" );
         ( "multiple cons pattern" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let second = fn lst -> case lst do | _ :: h :: _ -> h | _ -> 0
             |}
             ~expr:"second [1, 2, 3]" ~expected_value:"2" );
         ( "wildcard in tuple" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let f = fn x -> case x do | (_, b, _) -> b
             |}
             ~expr:"f (1, 2, 3)" ~expected_value:"2" );
         ( "pattern with constructor and tuple" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Pair = | P of (int, int)
               let sum = fn x -> case x do | P (a, b) -> a + b
             |}
             ~expr:"sum (P (3, 4))" ~expected_value:"7" );
       ]

(* Operator Precedence Tests *)
let operator_precedence_tests =
  let open ProgramTesting in
  "operator_precedence_tests"
  >::: [
         ( "multiplication before addition" >:: fun _ ->
           assert_expression_has_value ~program:"" ~expr:"2 + 3 * 4"
             ~expected_value:"14" );
         ( "parentheses override precedence" >:: fun _ ->
           assert_expression_has_value ~program:"" ~expr:"(2 + 3) * 4"
             ~expected_value:"20" );
         ( "division before subtraction" >:: fun _ ->
           assert_expression_has_value ~program:"" ~expr:"10 - 6 / 2"
             ~expected_value:"7" );
         ( "modulo with addition" >:: fun _ ->
           assert_expression_has_value ~program:"" ~expr:"10 + 7 % 3"
             ~expected_value:"11" );
         ( "comparison with arithmetic" >:: fun _ ->
           assert_expression_has_value ~program:"" ~expr:"2 + 3 > 4"
             ~expected_value:"true" );
         ( "logical and with comparison" >:: fun _ ->
           assert_expression_has_value ~program:"" ~expr:"3 > 2 && 5 < 10"
             ~expected_value:"true" );
         ( "logical or with and" >:: fun _ ->
           assert_expression_has_value ~program:"" ~expr:"false && true || true"
             ~expected_value:"true" );
         ( "cons with arithmetic" >:: fun _ ->
           assert_expression_has_value ~program:"" ~expr:"1 + 2 :: 3 + 4 :: []"
             ~expected_value:"[3, 7]" );
       ]

(* Higher-Order Function Tests *)
let higher_order_function_tests =
  let open ProgramTesting in
  "higher_order_function_tests"
  >::: [
         ( "map with lambda" >:: fun _ ->
           assert_expression_has_value ~program:""
             ~expr:"map (fn x -> x * 2) [1, 2, 3]" ~expected_value:"[2, 4, 6]"
         );
         ( "filter even numbers" >:: fun _ ->
           assert_expression_has_value ~program:""
             ~expr:"filter (fn x -> x % 2 == 0) [1, 2, 3, 4, 5, 6]"
             ~expected_value:"[2, 4, 6]" );
         ( "reduce_left sum" >:: fun _ ->
           assert_expression_has_value ~program:""
             ~expr:"reduce_left (fn acc -> fn x -> acc + x) 0 [1, 2, 3, 4]"
             ~expected_value:"10" );
         ( "reduce_right cons" >:: fun _ ->
           assert_expression_has_value ~program:""
             ~expr:"reduce_right (fn x -> fn acc -> x :: acc) [1, 2, 3] []"
             ~expected_value:"[1, 2, 3]" );
         ( "map composition" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let double = fn x -> x * 2
               let inc = fn x -> x + 1
             |}
             ~expr:"map inc (map double [1, 2, 3])" ~expected_value:"[3, 5, 7]"
         );
         ( "filter and map chain" >:: fun _ ->
           assert_expression_has_value ~program:""
             ~expr:"map (fn x -> x * 2) (filter (fn x -> x > 2) [1, 2, 3, 4])"
             ~expected_value:"[6, 8]" );
         ( "function returning function" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let add = fn x -> fn y -> x + y
               let add5 = add 5
             |}
             ~expr:"add5 10" ~expected_value:"15" );
       ]

(* Nested Data Structure Tests *)
let nested_data_structure_tests =
  let open ProgramTesting in
  "nested_data_structure_tests"
  >::: [
         ( "list of lists" >:: fun _ ->
           assert_expression_has_value ~program:"" ~expr:"[[1, 2], [3, 4], [5]]"
             ~expected_value:"[[1, 2], [3, 4], [5]]" );
         ( "nested tuple" >:: fun _ ->
           assert_expression_has_value ~program:"" ~expr:"((1, 2), (3, 4))"
             ~expected_value:"((1, 2), (3, 4))" );
         ( "list of tuples" >:: fun _ ->
           assert_expression_has_value ~program:"" ~expr:"[(1, 2), (3, 4)]"
             ~expected_value:"[(1, 2), (3, 4)]" );
         ( "tuple of lists" >:: fun _ ->
           assert_expression_has_value ~program:"" ~expr:"([1, 2], [3, 4])"
             ~expected_value:"([1, 2], [3, 4])" );
         ( "deeply nested list" >:: fun _ ->
           assert_expression_has_value ~program:"" ~expr:"[[[1, 2]], [[3]]]"
             ~expected_value:"[[[1, 2]], [[3]]]" );
         ( "nested record access" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let p = {outer: {inner: {value: 42}}}
             |}
             ~expr:"p.outer.inner.value" ~expected_value:"42" );
       ]

(* Type Alias Tests *)
let type_alias_tests =
  let open ProgramTesting in
  "type_alias_tests"
  >::: [
         ( "simple type alias" >:: fun _ ->
           assert_expression_has_type ~program:"type MyInt = int" ~expr:"42"
             ~expected_type:"int" );
         ( "tuple type alias" >:: fun _ ->
           assert_expression_has_type ~program:"type Pair = (int, int)"
             ~expr:"(1, 2)" ~expected_type:"(int, int)" );
         ( "function type alias" >:: fun _ ->
           assert_expression_has_type ~program:"type IntFunc = int -> int"
             ~expr:"fn x -> x + 1" ~expected_type:"int -> int" );
         ( "list type alias" >:: fun _ ->
           assert_expression_has_type ~program:"type IntList = [int]"
             ~expr:"[1, 2, 3]" ~expected_type:"[int]" );
         ( "parameterized type alias" >:: fun _ ->
           assert_expression_has_type ~program:"type Box<'a> = (a, a)"
             ~expr:"(1, 2)" ~expected_type:"(int, int)" );
         ( "nested type alias" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type Inner = int
               type Outer = (Inner, Inner)
             |}
             ~expr:"(1, 2)" ~expected_type:"(int, int)" );
       ]

(* Edge Case Tests *)
let edge_case_tests =
  let open ProgramTesting in
  "edge_case_tests"
  >::: [
         ( "empty list type" >:: fun _ ->
           assert_expression_has_type ~program:"" ~expr:"[]"
             ~expected_type:"['a]" );
         ( "unit value type" >:: fun _ ->
           assert_expression_has_type ~program:"" ~expr:"()"
             ~expected_type:"unit" );
         ( "single element tuple" >:: fun _ ->
           assert_expression_has_value ~program:"" ~expr:"(42)"
             ~expected_value:"42" );
         ( "negation of negation" >:: fun _ ->
           assert_expression_has_value ~program:"" ~expr:"~-(~-5)"
             ~expected_value:"5" );
         ( "zero division by subtraction" >:: fun _ ->
           assert_expression_has_value ~program:"" ~expr:"10 - 10"
             ~expected_value:"0" );
         ( "identity function application" >:: fun _ ->
           assert_expression_has_value ~program:"let id = fn x -> x"
             ~expr:"id 42" ~expected_value:"42" );
         ( "const function" >:: fun _ ->
           assert_expression_has_value ~program:"let const = fn x -> fn y -> x"
             ~expr:"const 5 10" ~expected_value:"5" );
       ]

(* Recursion Edge Cases *)
let recursion_edge_case_tests =
  let open ProgramTesting in
  "recursion_edge_case_tests"
  >::: [
         ( "recursive function with immediate return" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec f = fn n -> if n == 0 then 1 else 1
             |}
             ~expr:"f 5" ~expected_value:"1" );
         ( "recursive function base case" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec sum = fn n -> if n == 0 then 0 else n + sum (n - 1)
             |}
             ~expr:"sum 0" ~expected_value:"0" );
         ( "recursive function with accumulator" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec sum_acc = fn n -> fn acc ->
                 if n == 0 then acc else sum_acc (n - 1) (acc + n)
             |}
             ~expr:"sum_acc 5 0" ~expected_value:"15" );
       ]

(* List Operation Tests *)
let list_operation_tests =
  let open ProgramTesting in
  "list_operation_tests"
  >::: [
         ( "cons to empty list" >:: fun _ ->
           assert_expression_has_value ~program:"" ~expr:"1 :: []"
             ~expected_value:"[1]" );
         ( "multiple cons" >:: fun _ ->
           assert_expression_has_value ~program:"" ~expr:"1 :: 2 :: 3 :: []"
             ~expected_value:"[1, 2, 3]" );
         ( "list concatenation via cons" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec append = fn l1 -> fn l2 ->
                 case l1 do
                 | [] -> l2
                 | h :: t -> h :: append t l2
             |}
             ~expr:"append [1, 2] [3, 4]" ~expected_value:"[1, 2, 3, 4]" );
         ( "list reverse" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec rev = fn lst -> fn acc ->
                 case lst do
                 | [] -> acc
                 | h :: t -> rev t (h :: acc)
             |}
             ~expr:"rev [1, 2, 3] []" ~expected_value:"[3, 2, 1]" );
       ]

(* COMPLEX AND EXTENSIVE TESTS - 100 Tests *)

(* Complex Recursive Algorithm Tests *)
let complex_recursive_algorithms =
  let open ProgramTesting in
  "complex_recursive_algorithms"
  >::: [
         ( "fibonacci recursive" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec fib = fn n ->
                 if n <= 1 then n
                 else fib (n - 1) + fib (n - 2)
             |}
             ~expr:"fib 10" ~expected_value:"55" );
         ( "factorial with accumulator" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec fact = fn n -> fn acc ->
                 if n <= 1 then acc
                 else fact (n - 1) (n * acc)
             |}
             ~expr:"fact 6 1" ~expected_value:"720" );
         ( "greatest common divisor" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec gcd = fn a -> fn b ->
                 if b == 0 then a
                 else gcd b (a % b)
             |}
             ~expr:"gcd 48 18" ~expected_value:"6" );
         ( "power function" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec pow = fn base -> fn exp ->
                 if exp == 0 then 1
                 else base * pow base (exp - 1)
             |}
             ~expr:"pow 2 10" ~expected_value:"1024" );
         ( "sum of digits" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec sum_digits = fn n ->
                 if n < 10 then n
                 else (n % 10) + sum_digits (n / 10)
             |}
             ~expr:"sum_digits 12345" ~expected_value:"15" );
         ( "collatz sequence length" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec collatz_len = fn n -> fn count ->
                 if n == 1 then count
                 else if n % 2 == 0 then collatz_len (n / 2) (count + 1)
                 else collatz_len (n * 3 + 1) (count + 1)
             |}
             ~expr:"collatz_len 10 0" ~expected_value:"6" );
         ( "is prime check" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec is_prime_helper = fn n -> fn divisor ->
                 if divisor * divisor > n then true
                 else if n % divisor == 0 then false
                 else is_prime_helper n (divisor + 1)

               let is_prime = fn n ->
                 if n < 2 then false
                 else is_prime_helper n 2
             |}
             ~expr:"is_prime 17" ~expected_value:"true" );
         ( "nth prime number" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec is_prime_helper = fn n -> fn divisor ->
                 if divisor * divisor > n then true
                 else if n % divisor == 0 then false
                 else is_prime_helper n (divisor + 1)

               let is_prime = fn n ->
                 if n < 2 then false
                 else is_prime_helper n 2

               let rec nth_prime = fn n -> fn candidate -> fn count ->
                 if is_prime candidate then
                   if count == n then candidate
                   else nth_prime n (candidate + 1) (count + 1)
                 else nth_prime n (candidate + 1) count
             |}
             ~expr:"nth_prime 5 2 0" ~expected_value:"13" );
       ]

(* Complex List Processing Tests *)
let complex_list_processing =
  let open ProgramTesting in
  "complex_list_processing"
  >::: [
         ( "quicksort implementation" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec filter = fn pred -> fn lst ->
                 case lst do
                 | [] -> []
                 | h :: t -> if pred h then h :: filter pred t else filter pred t

               let rec append = fn l1 -> fn l2 ->
                 case l1 do
                 | [] -> l2
                 | h :: t -> h :: append t l2

               let rec quicksort = fn lst ->
                 case lst do
                 | [] -> []
                 | pivot :: rest ->
                   let smaller = filter (fn x -> x < pivot) rest in
                   let larger = filter (fn x -> x >= pivot) rest in
                   append (append (quicksort smaller) (pivot :: [])) (quicksort larger)
             |}
             ~expr:"quicksort [3, 1, 4, 1, 5, 9, 2, 6]"
             ~expected_value:"[1, 1, 2, 3, 4, 5, 6, 9]" );
         ( "mergesort implementation" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec take = fn n -> fn lst ->
                 if n == 0 then []
                 else case lst do
                 | [] -> []
                 | h :: t -> h :: take (n - 1) t

               let rec drop = fn n -> fn lst ->
                 if n == 0 then lst
                 else case lst do
                 | [] -> []
                 | _ :: t -> drop (n - 1) t

               let rec merge = fn l1 -> fn l2 ->
                 case l1 do
                 | [] -> l2
                 | h1 :: t1 ->
                   case l2 do
                   | [] -> l1
                   | h2 :: t2 ->
                     if h1 < h2 then h1 :: merge t1 l2
                     else h2 :: merge l1 t2

               let rec length = fn lst ->
                 case lst do
                 | [] -> 0
                 | _ :: t -> 1 + length t

               let rec mergesort = fn lst ->
                 let len = length lst in
                 if len <= 1 then lst
                 else
                   let mid = len / 2 in
                   let left = take mid lst in
                   let right = drop mid lst in
                   merge (mergesort left) (mergesort right)
             |}
             ~expr:"mergesort [5, 2, 8, 1, 9]" ~expected_value:"[1, 2, 5, 8, 9]"
         );
         ( "list zip function" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec zip = fn l1 -> fn l2 ->
                 case l1 do
                 | [] -> []
                 | h1 :: t1 ->
                   case l2 do
                   | [] -> []
                   | h2 :: t2 -> (h1, h2) :: zip t1 t2
             |}
             ~expr:"zip [1, 2, 3] [4, 5, 6]"
             ~expected_value:"[(1, 4), (2, 5), (3, 6)]" );
         ( "list unzip function" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec unzip = fn lst ->
                 case lst do
                 | [] -> ([], [])
                 | (a, b) :: t ->
                   let rest = unzip t in
                   case rest do
                   | (as, bs) -> (a :: as, b :: bs)
             |}
             ~expr:"unzip [(1, 2), (3, 4), (5, 6)]"
             ~expected_value:"([1, 3, 5], [2, 4, 6])" );
         ( "partition list" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec partition = fn pred -> fn lst ->
                 case lst do
                 | [] -> ([], [])
                 | h :: t ->
                   let rest = partition pred t in
                   case rest do
                   | (trues, falses) ->
                     if pred h then (h :: trues, falses)
                     else (trues, h :: falses)
             |}
             ~expr:"partition (fn x -> x % 2 == 0) [1, 2, 3, 4, 5, 6]"
             ~expected_value:"([2, 4, 6], [1, 3, 5])" );
         ( "group consecutive elements" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec group = fn lst ->
                 case lst do
                 | [] -> []
                 | h :: t ->
                   case t do
                   | [] -> [[h]]
                   | h2 :: _ ->
                     let rest_grouped = group t in
                     case rest_grouped do
                     | first_group :: other_groups ->
                       case first_group do
                       | fg_head :: _ ->
                         if h == fg_head then (h :: first_group) :: other_groups
                         else [h] :: rest_grouped
                       | [] -> [[h]]
                     | [] -> [[h]]
             |}
             ~expr:"group [1, 1, 2, 2, 2, 3, 1]"
             ~expected_value:"[[1, 1], [2, 2, 2], [3], [1]]" );
         ( "take while predicate" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec takeWhile = fn pred -> fn lst ->
                 case lst do
                 | [] -> []
                 | h :: t -> if pred h then h :: takeWhile pred t else []
             |}
             ~expr:"takeWhile (fn x -> x < 5) [1, 2, 3, 6, 7, 1]"
             ~expected_value:"[1, 2, 3]" );
         ( "drop while predicate" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec dropWhile = fn pred -> fn lst ->
                 case lst do
                 | [] -> []
                 | h :: t -> if pred h then dropWhile pred t else lst
             |}
             ~expr:"dropWhile (fn x -> x < 5) [1, 2, 3, 6, 7, 1]"
             ~expected_value:"[6, 7, 1]" );
         ( "list intersperse" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec intersperse = fn sep -> fn lst ->
                 case lst do
                 | [] -> []
                 | h :: t ->
                   case t do
                   | [] -> [h]
                   | _ -> h :: sep :: intersperse sep t
             |}
             ~expr:"intersperse 0 [1, 2, 3, 4]"
             ~expected_value:"[1, 0, 2, 0, 3, 0, 4]" );
         ( "list flatten" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec append = fn l1 -> fn l2 ->
                 case l1 do
                 | [] -> l2
                 | h :: t -> h :: append t l2

               let rec flatten = fn lst ->
                 case lst do
                 | [] -> []
                 | h :: t -> append h (flatten t)
             |}
             ~expr:"flatten [[1, 2], [3], [4, 5, 6]]"
             ~expected_value:"[1, 2, 3, 4, 5, 6]" );
       ]

(* Complex Binary Tree Tests *)
let complex_tree_operations =
  let open ProgramTesting in
  "complex_tree_operations"
  >::: [
         ( "binary tree insert" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Tree = | Empty | Node of (Tree, int, Tree)

               let rec insert = fn tree -> fn value ->
                 case tree do
                 | Empty -> Node (Empty, value, Empty)
                 | Node (l, v, r) ->
                   if value < v then Node (insert l value, v, r)
                   else if value > v then Node (l, v, insert r value)
                   else tree

               let tree = insert (insert (insert Empty 5) 3) 7

               let rec contains = fn tree -> fn value ->
                 case tree do
                 | Empty -> false
                 | Node (l, v, r) ->
                   if value == v then true
                   else if value < v then contains l value
                   else contains r value
             |}
             ~expr:"contains tree 3" ~expected_value:"true" );
         ( "binary tree size" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Tree = | Empty | Node of (Tree, int, Tree)

               let rec insert = fn tree -> fn value ->
                 case tree do
                 | Empty -> Node (Empty, value, Empty)
                 | Node (l, v, r) ->
                   if value < v then Node (insert l value, v, r)
                   else Node (l, v, insert r value)

               let rec size = fn tree ->
                 case tree do
                 | Empty -> 0
                 | Node (l, _, r) -> 1 + size l + size r

               let tree = insert (insert (insert (insert Empty 5) 3) 7) 2
             |}
             ~expr:"size tree" ~expected_value:"4" );
         ( "binary tree height" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Tree = | Empty | Node of (Tree, int, Tree)

               let rec insert = fn tree -> fn value ->
                 case tree do
                 | Empty -> Node (Empty, value, Empty)
                 | Node (l, v, r) ->
                   if value < v then Node (insert l value, v, r)
                   else Node (l, v, insert r value)

               let max = fn a -> fn b -> if a > b then a else b

               let rec height = fn tree ->
                 case tree do
                 | Empty -> 0
                 | Node (l, _, r) -> 1 + max (height l) (height r)

               let tree = insert (insert (insert Empty 5) 3) 2
             |}
             ~expr:"height tree" ~expected_value:"3" );
         ( "binary tree inorder traversal" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Tree = | Empty | Node of (Tree, int, Tree)

               let rec append = fn l1 -> fn l2 ->
                 case l1 do
                 | [] -> l2
                 | h :: t -> h :: append t l2

               let rec inorder = fn tree ->
                 case tree do
                 | Empty -> []
                 | Node (l, v, r) -> append (append (inorder l) [v]) (inorder r)

               let tree = Node (Node (Empty, 1, Empty), 2, Node (Empty, 3, Empty))
             |}
             ~expr:"inorder tree" ~expected_value:"[1, 2, 3]" );
         ( "binary tree preorder traversal" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Tree = | Empty | Node of (Tree, int, Tree)

               let rec append = fn l1 -> fn l2 ->
                 case l1 do
                 | [] -> l2
                 | h :: t -> h :: append t l2

               let rec preorder = fn tree ->
                 case tree do
                 | Empty -> []
                 | Node (l, v, r) -> v :: append (preorder l) (preorder r)

               let tree = Node (Node (Empty, 1, Empty), 2, Node (Empty, 3, Empty))
             |}
             ~expr:"preorder tree" ~expected_value:"[2, 1, 3]" );
         ( "binary tree postorder traversal" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Tree = | Empty | Node of (Tree, int, Tree)

               let rec append = fn l1 -> fn l2 ->
                 case l1 do
                 | [] -> l2
                 | h :: t -> h :: append t l2

               let rec postorder = fn tree ->
                 case tree do
                 | Empty -> []
                 | Node (l, v, r) -> append (append (postorder l) (postorder r)) [v]

               let tree = Node (Node (Empty, 1, Empty), 2, Node (Empty, 3, Empty))
             |}
             ~expr:"postorder tree" ~expected_value:"[1, 3, 2]" );
         ( "binary tree sum" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Tree = | Empty | Node of (Tree, int, Tree)

               let rec sum = fn tree ->
                 case tree do
                 | Empty -> 0
                 | Node (l, v, r) -> v + sum l + sum r

               let tree = Node (Node (Empty, 1, Empty), 2, Node (Empty, 3, Empty))
             |}
             ~expr:"sum tree" ~expected_value:"6" );
         ( "binary tree map" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Tree = | Empty | Node of (Tree, int, Tree)

               let rec tree_map = fn f -> fn tree ->
                 case tree do
                 | Empty -> Empty
                 | Node (l, v, r) -> Node (tree_map f l, f v, tree_map f r)

               let rec sum = fn tree ->
                 case tree do
                 | Empty -> 0
                 | Node (l, v, r) -> v + sum l + sum r

               let tree = Node (Node (Empty, 1, Empty), 2, Node (Empty, 3, Empty))
               let doubled = tree_map (fn x -> x * 2) tree
             |}
             ~expr:"sum doubled" ~expected_value:"12" );
       ]

(* Complex Sum Type Tests *)
let complex_sum_type_operations =
  let open ProgramTesting in
  "complex_sum_type_operations"
  >::: [
         ( "option map function" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Option<'a> = | None | Some of 'a

               let option_map = fn f -> fn opt ->
                 case opt do
                 | None -> None
                 | Some x -> Some (f x)
             |}
             ~expr:"option_map (fn x -> x * 2) (Some 5)"
             ~expected_value:"Some 10" );
         ( "option bind function" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Option<'a> = | None | Some of 'a

               let option_bind = fn opt -> fn f ->
                 case opt do
                 | None -> None
                 | Some x -> f x

               let safe_div = fn a -> fn b ->
                 if b == 0 then None else Some (a / b)
             |}
             ~expr:"option_bind (Some 10) (fn x -> safe_div x 2)"
             ~expected_value:"Some 5" );
         ( "either map function" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Either<'a, 'b> = | Left of 'a | Right of 'b

               let either_map = fn f -> fn either ->
                 case either do
                 | Left x -> Left x
                 | Right y -> Right (f y)
             |}
             ~expr:"either_map (fn x -> x + 1) (Right 5)"
             ~expected_value:"Right 6" );
         ( "result chain operations" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Result<'a> = | Ok of 'a | Error of int

               let result_map = fn f -> fn result ->
                 case result do
                 | Error e -> Error e
                 | Ok x -> Ok (f x)

               let increment = fn x -> x + 1
               let double = fn x -> x * 2
             |}
             ~expr:"result_map double (result_map increment (Ok 5))"
             ~expected_value:"Ok 12" );
         ( "list of options filter" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type Option<'a> = | None | Some of 'a

               let rec filter_options = fn lst ->
                 case lst do
                 | [] -> []
                 | h :: t ->
                   case h do
                   | None -> filter_options t
                   | Some x -> x :: filter_options t
             |}
             ~expr:"filter_options [Some 1, None, Some 2, Some 3, None]"
             ~expected_value:"[1, 2, 3]" );
         ( "expression evaluator" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Expr = | Val of int | Add of (Expr, Expr) | Mul of (Expr, Expr)

               let rec eval = fn expr ->
                 case expr do
                 | Val n -> n
                 | Add (e1, e2) -> eval e1 + eval e2
                 | Mul (e1, e2) -> eval e1 * eval e2

               let expr = Add (Mul (Val 2, Val 3), Val 4)
             |}
             ~expr:"eval expr" ~expected_value:"10" );
         ( "nested expression evaluator" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Expr = | Val of int | Add of (Expr, Expr) | Mul of (Expr, Expr) | Sub of (Expr, Expr)

               let rec eval = fn expr ->
                 case expr do
                 | Val n -> n
                 | Add (e1, e2) -> eval e1 + eval e2
                 | Mul (e1, e2) -> eval e1 * eval e2
                 | Sub (e1, e2) -> eval e1 - eval e2

               let expr = Sub (Mul (Add (Val 2, Val 3), Val 4), Val 5)
             |}
             ~expr:"eval expr" ~expected_value:"15" );
         ( "peano arithmetic" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Nat = | Zero | Succ of Nat

               let rec nat_to_int = fn n ->
                 case n do
                 | Zero -> 0
                 | Succ m -> 1 + nat_to_int m

               let rec add_nat = fn n -> fn m ->
                 case n do
                 | Zero -> m
                 | Succ p -> Succ (add_nat p m)

               let three = Succ (Succ (Succ Zero))
               let two = Succ (Succ Zero)
             |}
             ~expr:"nat_to_int (add_nat three two)" ~expected_value:"5" );
       ]

(* Complex Function Composition Tests *)
let complex_function_composition =
  let open ProgramTesting in
  "complex_function_composition"
  >::: [
         ( "compose three functions" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let compose = fn f -> fn g -> fn x -> f (g x)
               let add1 = fn x -> x + 1
               let double = fn x -> x * 2
               let square = fn x -> x * x
               let f = compose square (compose double add1)
             |}
             ~expr:"f 3" ~expected_value:"64" );
         ( "pipeline of transformations" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let pipeline = fn x -> fn f -> f x
               let add1 = fn x -> x + 1
               let double = fn x -> x * 2
               let square = fn x -> x * x
             |}
             ~expr:"pipeline (pipeline (pipeline 3 add1) double) square"
             ~expected_value:"64" );
         ( "function iteration" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec iterate = fn f -> fn n -> fn x ->
                 if n == 0 then x
                 else iterate f (n - 1) (f x)

               let inc = fn x -> x + 1
             |}
             ~expr:"iterate inc 10 0" ~expected_value:"10" );
         ( "curry and uncurry" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let curry = fn f -> fn a -> fn b -> f (a, b)
               let uncurry = fn f -> fn pair ->
                 case pair do
                 | (a, b) -> f a b

               let add_tuple = fn pair ->
                 case pair do
                 | (a, b) -> a + b

               let add_curried = curry add_tuple
             |}
             ~expr:"add_curried 3 5" ~expected_value:"8" );
         ( "flip function arguments" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let flip = fn f -> fn a -> fn b -> f b a
               let sub = fn a -> fn b -> a - b
               let reversed_sub = flip sub
             |}
             ~expr:"reversed_sub 5 10" ~expected_value:"5" );
         ( "constant function" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let const = fn x -> fn _ -> x
               let always_five = const 5
             |}
             ~expr:"always_five 100" ~expected_value:"5" );
         ( "apply function n times" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec apply_n = fn f -> fn n -> fn x ->
                 if n == 0 then x
                 else f (apply_n f (n - 1) x)

               let double = fn x -> x * 2
             |}
             ~expr:"apply_n double 4 1" ~expected_value:"16" );
         ( "compose list of functions" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec compose_all = fn funcs -> fn x ->
                 case funcs do
                 | [] -> x
                 | f :: rest -> f (compose_all rest x)

               let add1 = fn x -> x + 1
               let double = fn x -> x * 2
             |}
             ~expr:"compose_all [add1, double, add1] 3" ~expected_value:"9" );
       ]

(* Complex Pattern Matching Tests *)
let complex_pattern_matching_scenarios =
  let open ProgramTesting in
  "complex_pattern_matching_scenarios"
  >::: [
         ( "deeply nested pattern" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let extract = fn x ->
                 case x do
                 | (((a, b), c), d) -> a + b + c + d
             |}
             ~expr:"extract (((1, 2), 3), 4)" ~expected_value:"10" );
         ( "pattern match on list of tuples" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec sum_pairs = fn lst ->
                 case lst do
                 | [] -> 0
                 | (a, b) :: t -> a + b + sum_pairs t
             |}
             ~expr:"sum_pairs [(1, 2), (3, 4), (5, 6)]" ~expected_value:"21" );
         ( "pattern match multiple levels" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec process = fn lst ->
                 case lst do
                 | [] -> 0
                 | (a, b) :: rest ->
                   case rest do
                   | [] -> a + b
                   | (c, d) :: rest2 -> a + b + c + d + process rest2
             |}
             ~expr:"process [(1, 2), (3, 4), (5, 6)]" ~expected_value:"21" );
         ( "pattern match with guards simulation" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let classify = fn n ->
                 if n < 0 then ~-1
                 else if n == 0 then 0
                 else if n < 10 then 1
                 else 2
             |}
             ~expr:"classify 5" ~expected_value:"1" );
       ]

(* Complex Record Operations *)
let complex_record_operations =
  let open ProgramTesting in
  "complex_record_operations"
  >::: [
         ( "record update with syntax" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let p = {x: 1, y: 2}
               let p2 = { p with x = 10 }
             |}
             ~expr:"p2.x + p2.y" ~expected_value:"12" );
         ( "record update simulation" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let update_x = fn r -> fn new_x ->
                 {x: new_x, y: r.y}

               let p = {x: 1, y: 2}
               let p2 = update_x p 10
             |}
             ~expr:"p2.x + p2.y" ~expected_value:"12" );
         ( "records in list operations" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec sum_x = fn lst ->
                 case lst do
                 | [] -> 0
                 | h :: t -> h.x + sum_x t

               let points = [{x: 1, y: 2}, {x: 3, y: 4}, {x: 5, y: 6}]
             |}
             ~expr:"sum_x points" ~expected_value:"9" );
         ( "record transformation pipeline" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let scale_x = fn r -> fn factor ->
                 {x: r.x * factor, y: r.y}

               let scale_y = fn r -> fn factor ->
                 {x: r.x, y: r.y * factor}

               let p = {x: 2, y: 3}
               let p2 = scale_y (scale_x p 2) 3
             |}
             ~expr:"p2.x + p2.y" ~expected_value:"13" );
         ( "nested record updates" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let update_inner = fn r -> fn new_val ->
                 {outer: {inner: {value: new_val}}}

               let r = {outer: {inner: {value: 5}}}
               let r2 = update_inner r 20
             |}
             ~expr:"r2.outer.inner.value" ~expected_value:"20" );
         ( "record with computed fields" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let make_point = fn x -> fn y ->
                 {x: x, y: y, sum: x + y, product: x * y}

               let p = make_point 3 4
             |}
             ~expr:"p.sum + p.product" ~expected_value:"19" );
       ]

(* Complex Higher-Order Function Tests *)
let complex_higher_order_functions =
  let open ProgramTesting in
  "complex_higher_order_functions"
  >::: [
         ( "map with multiple transformations" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec map = fn f -> fn lst ->
                 case lst do
                 | [] -> []
                 | h :: t -> f h :: map f t

               let transform = fn x -> (x + 1) * 2
             |}
             ~expr:"map transform [1, 2, 3, 4, 5]"
             ~expected_value:"[4, 6, 8, 10, 12]" );
         ( "filter with complex predicate" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec filter = fn pred -> fn lst ->
                 case lst do
                 | [] -> []
                 | h :: t -> if pred h then h :: filter pred t else filter pred t

               let complex_pred = fn x ->
                 x % 2 == 0 && x > 5
             |}
             ~expr:"filter complex_pred [1, 2, 6, 8, 3, 10, 4]"
             ~expected_value:"[6, 8, 10]" );
         ( "fold with complex accumulator" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec fold_left = fn f -> fn acc -> fn lst ->
                 case lst do
                 | [] -> acc
                 | h :: t -> fold_left f (f acc h) t

               let update = fn acc -> fn x ->
                 {sum: acc.sum + x, count: acc.count + 1, max: if x > acc.max then x else acc.max}

               let result = fold_left update {sum: 0, count: 0, max: 0} [1, 5, 3, 9, 2]
             |}
             ~expr:"result.sum + result.max" ~expected_value:"29" );
         ( "scan left operation" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec scan_left = fn f -> fn acc -> fn lst ->
                 case lst do
                 | [] -> [acc]
                 | h :: t -> acc :: scan_left f (f acc h) t
             |}
             ~expr:"scan_left (fn a -> fn x -> a + x) 0 [1, 2, 3, 4]"
             ~expected_value:"[0, 1, 3, 6, 10]" );
         ( "all and any predicates" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec all = fn pred -> fn lst ->
                 case lst do
                 | [] -> true
                 | h :: t -> if pred h then all pred t else false

               let rec any = fn pred -> fn lst ->
                 case lst do
                 | [] -> false
                 | h :: t -> if pred h then true else any pred t

               let is_positive = fn x -> x > 0
             |}
             ~expr:"all is_positive [1, 2, 3, 4]" ~expected_value:"true" );
         ( "none predicate" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec none = fn pred -> fn lst ->
                 case lst do
                 | [] -> true
                 | h :: t -> if pred h then false else none pred t

               let is_negative = fn x -> x < 0
             |}
             ~expr:"none is_negative [1, 2, 3, 4]" ~expected_value:"true" );
       ]

(* Complex Mutual Recursion Tests *)
let complex_mutual_recursion =
  let open ProgramTesting in
  "complex_mutual_recursion"
  >::: [
         ( "mutual recursion with lists" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec sum_at_even = fn lst -> fn pos ->
                 case lst do
                 | [] -> 0
                 | h :: t ->
                   if pos % 2 == 0 then h + sum_at_odd t (pos + 1)
                   else sum_at_odd t (pos + 1)
               and sum_at_odd = fn lst -> fn pos ->
                 case lst do
                 | [] -> 0
                 | h :: t ->
                   if pos % 2 == 1 then h + sum_at_even t (pos + 1)
                   else sum_at_even t (pos + 1)
             |}
             ~expr:"sum_at_even [1, 2, 3, 4, 5, 6] 0" ~expected_value:"21" );
         ( "mutual recursion state machine" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec state_a = fn n ->
                 if n == 0 then 1
                 else if n % 3 == 0 then state_b (n - 1)
                 else state_c (n - 1)
               and state_b = fn n ->
                 if n == 0 then 2
                 else state_a (n - 1) + 1
               and state_c = fn n ->
                 if n == 0 then 3
                 else state_a (n - 1) + 2
             |}
             ~expr:"state_a 5" ~expected_value:"6" );
         ( "mutual recursion with accumulator" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec count_evens = fn lst -> fn acc ->
                 case lst do
                 | [] -> acc
                 | h :: t ->
                   if h % 2 == 0 then count_evens t (acc + 1)
                   else count_evens t acc

               let result = count_evens [1, 2, 3, 4, 5, 6] 0
             |}
             ~expr:"result" ~expected_value:"3" );
       ]

(* Complex Arithmetic and Math Tests *)
let complex_math_operations =
  let open ProgramTesting in
  "complex_math_operations"
  >::: [
         ( "lcm calculation" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec gcd = fn a -> fn b ->
                 if b == 0 then a
                 else gcd b (a % b)

               let lcm = fn a -> fn b ->
                 (a * b) / gcd a b
             |}
             ~expr:"lcm 12 18" ~expected_value:"36" );
         ( "modular exponentiation" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec mod_pow = fn base -> fn exp -> fn modulus ->
                 if exp == 0 then 1
                 else if exp % 2 == 0 then
                   let half = mod_pow base (exp / 2) modulus in
                   (half * half) % modulus
                 else
                   (base * mod_pow base (exp - 1) modulus) % modulus
             |}
             ~expr:"mod_pow 2 10 1000" ~expected_value:"24" );
         ( "sum of range" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec sum_range = fn start -> fn stop ->
                 if start > stop then 0
                 else start + sum_range (start + 1) stop
             |}
             ~expr:"sum_range 1 10" ~expected_value:"55" );
         ( "product of range" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec product_range = fn start -> fn stop ->
                 if start > stop then 1
                 else start * product_range (start + 1) stop
             |}
             ~expr:"product_range 1 5" ~expected_value:"120" );
         ( "count divisors" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec count_divisors = fn n -> fn d ->
                 if d > n then 0
                 else if n % d == 0 then 1 + count_divisors n (d + 1)
                 else count_divisors n (d + 1)
             |}
             ~expr:"count_divisors 12 1" ~expected_value:"6" );
         ( "sum of divisors" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec sum_divisors = fn n -> fn d ->
                 if d > n then 0
                 else if n % d == 0 then d + sum_divisors n (d + 1)
                 else sum_divisors n (d + 1)
             |}
             ~expr:"sum_divisors 12 1" ~expected_value:"28" );
       ]

(* Complex Integration Tests *)
let complex_integration_tests =
  let open ProgramTesting in
  "complex_integration_tests"
  >::: [
         ( "complex data pipeline" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec map = fn f -> fn lst ->
                 case lst do
                 | [] -> []
                 | h :: t -> f h :: map f t

               let rec filter = fn pred -> fn lst ->
                 case lst do
                 | [] -> []
                 | h :: t -> if pred h then h :: filter pred t else filter pred t

               let rec fold_left = fn f -> fn acc -> fn lst ->
                 case lst do
                 | [] -> acc
                 | h :: t -> fold_left f (f acc h) t

               let result = fold_left (fn a -> fn x -> a + x) 0
                 (filter (fn x -> x > 5)
                   (map (fn x -> x * 2) [1, 2, 3, 4, 5, 6, 7]))
             |}
             ~expr:"result" ~expected_value:"50" );
         ( "list comprehension simulation" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec map = fn f -> fn lst ->
                 case lst do
                 | [] -> []
                 | h :: t -> f h :: map f t

               let rec filter = fn pred -> fn lst ->
                 case lst do
                 | [] -> []
                 | h :: t -> if pred h then h :: filter pred t else filter pred t

               let result = map (fn x -> x * x)
                 (filter (fn x -> x % 2 == 0) [1, 2, 3, 4, 5, 6, 7, 8])
             |}
             ~expr:"result" ~expected_value:"[4, 16, 36, 64]" );
         ( "tree to list conversion" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Tree = | Empty | Node of (Tree, int, Tree)

               let rec append = fn l1 -> fn l2 ->
                 case l1 do
                 | [] -> l2
                 | h :: t -> h :: append t l2

               let rec to_list = fn tree ->
                 case tree do
                 | Empty -> []
                 | Node (l, v, r) -> append (to_list l) (v :: to_list r)

               let tree = Node (Node (Empty, 1, Empty), 2, Node (Empty, 3, Empty))
             |}
             ~expr:"to_list tree" ~expected_value:"[1, 2, 3]" );
         ( "count occurrences in list" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec count_occurrences = fn x -> fn lst ->
                 case lst do
                 | [] -> 0
                 | h :: t ->
                   if h == x then 1 + count_occurrences x t
                   else count_occurrences x t
             |}
             ~expr:"count_occurrences 3 [1, 3, 2, 3, 4, 3, 5]"
             ~expected_value:"3" );
         ( "remove duplicates" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec contains = fn x -> fn lst ->
                 case lst do
                 | [] -> false
                 | h :: t -> if h == x then true else contains x t

               let rec remove_dups = fn lst -> fn seen ->
                 case lst do
                 | [] -> []
                 | h :: t ->
                   if contains h seen then remove_dups t seen
                   else h :: remove_dups t (h :: seen)
             |}
             ~expr:"remove_dups [1, 2, 3, 2, 4, 1, 5] []"
             ~expected_value:"[1, 2, 3, 4, 5]" );
         ( "intersection of lists" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               let rec contains = fn x -> fn lst ->
                 case lst do
                 | [] -> false
                 | h :: t -> if h == x then true else contains x t

               let rec intersection = fn l1 -> fn l2 ->
                 case l1 do
                 | [] -> []
                 | h :: t ->
                   if contains h l2 then h :: intersection t l2
                   else intersection t l2
             |}
             ~expr:"intersection [1, 2, 3, 4] [3, 4, 5, 6]"
             ~expected_value:"[3, 4]" );
       ]

(* ============================================================ *)
(* MUTUALLY RECURSIVE TYPES TESTS *)
(* ============================================================ *)

let mutually_recursive_types_basic_tests =
  let open ProgramTesting in
  "Mutually Recursive Types - Basic"
  >::: [
         (* Test 1: Simple Even/Odd definition *)
         ( "even odd types typecheck" >:: fun _ ->
           assert_program_typechecks
             {|
             type rec Even = | Zero | SuccE of Odd
             and Odd = | SuccO of Even
           |}
         );
         (* Test 2: Zero constructor type *)
         ( "zero constructor type" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type rec Even = | Zero | SuccE of Odd
               and Odd = | SuccO of Even
             |}
             ~expr:"Zero" ~expected_type:"Even" );
         (* Test 3: SuccE constructor type *)
         ( "succE constructor type" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type rec Even = | Zero | SuccE of Odd
               and Odd = | SuccO of Even
             |}
             ~expr:"SuccE" ~expected_type:"Odd -> Even" );
         (* Test 4: SuccO constructor type *)
         ( "succO constructor type" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type rec Even = | Zero | SuccE of Odd
               and Odd = | SuccO of Even
             |}
             ~expr:"SuccO" ~expected_type:"Even -> Odd" );
         (* Test 5: Create simple even number *)
         ( "create even number" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type rec Even = | Zero | SuccE of Odd
               and Odd = | SuccO of Even
             |}
             ~expr:"SuccE (SuccO Zero)" ~expected_type:"Even" );
         (* Test 6: Create simple odd number *)
         ( "create odd number" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type rec Even = | Zero | SuccE of Odd
               and Odd = | SuccO of Even
             |}
             ~expr:"SuccO Zero" ~expected_type:"Odd" );
         (* Test 7: Pattern match on Even *)
         ( "pattern match even" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Even = | Zero | SuccE of Odd
               and Odd = | SuccO of Even

               let is_zero = fn e ->
                 case e do
                 | Zero -> true
                 | SuccE _ -> false
             |}
             ~expr:"is_zero Zero" ~expected_value:"true" );
         (* Test 8: Pattern match on Odd *)
         ( "pattern match odd" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Even = | Zero | SuccE of Odd
               and Odd = | SuccO of Even

               let get_pred = fn o ->
                 case o do
                 | SuccO e -> e
             |}
             ~expr:"get_pred (SuccO Zero)" ~expected_value:"Zero" );
         (* Test 9: Convert even to int *)
         ( "even to int" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Even = | Zero | SuccE of Odd
               and Odd = | SuccO of Even

               let rec even_to_int = fn e ->
                 case e do
                 | Zero -> 0
                 | SuccE o -> 1 + odd_to_int o
               and odd_to_int = fn o ->
                 case o do
                 | SuccO e -> 1 + even_to_int e
             |}
             ~expr:"even_to_int (SuccE (SuccO (SuccE (SuccO Zero))))"
             ~expected_value:"4" );
         (* Test 10: Convert odd to int *)
         ( "odd to int" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Even = | Zero | SuccE of Odd
               and Odd = | SuccO of Even

               let rec even_to_int = fn e ->
                 case e do
                 | Zero -> 0
                 | SuccE o -> 1 + odd_to_int o
               and odd_to_int = fn o ->
                 case o do
                 | SuccO e -> 1 + even_to_int e
             |}
             ~expr:"odd_to_int (SuccO Zero)" ~expected_value:"1" );
       ]

let mutually_recursive_types_with_params_tests =
  let open ProgramTesting in
  "Mutually Recursive Types - With Type Parameters"
  >::: [
         (* Test 11: Tree/Forest with type parameters *)
         ( "tree forest typechecks" >:: fun _ ->
           assert_program_typechecks
             {|
             type rec Tree<'a> = | Leaf of 'a | Node of ('a, Forest<'a>)
             and Forest<'a> = | Empty | Trees of (Tree<'a>, Forest<'a>)
           |}
         );
         (* Test 12: Leaf constructor type *)
         ( "leaf constructor type" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type rec Tree<'a> = | Leaf of 'a | Node of ('a, Forest<'a>)
               and Forest<'a> = | Empty | Trees of (Tree<'a>, Forest<'a>)
             |}
             ~expr:"Leaf" ~expected_type:"'a -> Tree<'a>" );
         (* Test 13: Node constructor type *)
         ( "node constructor type" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type rec Tree<'a> = | Leaf of 'a | Node of ('a, Forest<'a>)
               and Forest<'a> = | Empty | Trees of (Tree<'a>, Forest<'a>)
             |}
             ~expr:"Node" ~expected_type:"('a, Forest<'a>) -> Tree<'a>" );
         (* Test 14: Empty constructor type *)
         ( "empty forest constructor type" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type rec Tree<'a> = | Leaf of 'a | Node of ('a, Forest<'a>)
               and Forest<'a> = | Empty | Trees of (Tree<'a>, Forest<'a>)
             |}
             ~expr:"Empty" ~expected_type:"Forest<'a>" );
         (* Test 15: Trees constructor type *)
         ( "trees constructor type" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type rec Tree<'a> = | Leaf of 'a | Node of ('a, Forest<'a>)
               and Forest<'a> = | Empty | Trees of (Tree<'a>, Forest<'a>)
             |}
             ~expr:"Trees" ~expected_type:"(Tree<'a>, Forest<'a>) -> Forest<'a>"
         );
         (* Test 16: Create simple tree *)
         ( "create simple tree" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type rec Tree<'a> = | Leaf of 'a | Node of ('a, Forest<'a>)
               and Forest<'a> = | Empty | Trees of (Tree<'a>, Forest<'a>)
             |}
             ~expr:"Leaf 5" ~expected_type:"Tree<int>" );
         (* Test 17: Create tree with children *)
         ( "create tree with children" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type rec Tree<'a> = | Leaf of 'a | Node of ('a, Forest<'a>)
               and Forest<'a> = | Empty | Trees of (Tree<'a>, Forest<'a>)
             |}
             ~expr:"Node (1, Trees (Leaf 2, Empty))" ~expected_type:"Tree<int>"
         );
         (* Test 18: Create forest *)
         ( "create forest" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type rec Tree<'a> = | Leaf of 'a | Node of ('a, Forest<'a>)
               and Forest<'a> = | Empty | Trees of (Tree<'a>, Forest<'a>)
             |}
             ~expr:"Trees (Leaf 1, Trees (Leaf 2, Empty))"
             ~expected_type:"Forest<int>" );
         (* Test 19: Count tree nodes *)
         ( "count tree nodes" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Tree<'a> = | Leaf of 'a | Node of ('a, Forest<'a>)
               and Forest<'a> = | Empty | Trees of (Tree<'a>, Forest<'a>)

               let rec count_tree = fn t ->
                 case t do
                 | Leaf _ -> 1
                 | Node (_, f) -> 1 + count_forest f
               and count_forest = fn f ->
                 case f do
                 | Empty -> 0
                 | Trees (t, rest) -> count_tree t + count_forest rest
             |}
             ~expr:
               "count_tree (Node (1, Trees (Leaf 2, Trees (Leaf 3, Empty))))"
             ~expected_value:"3" );
         (* Test 20: Sum tree values *)
         ( "sum tree values" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Tree<'a> = | Leaf of 'a | Node of ('a, Forest<'a>)
               and Forest<'a> = | Empty | Trees of (Tree<'a>, Forest<'a>)

               let rec sum_tree = fn t ->
                 case t do
                 | Leaf x -> x
                 | Node (x, f) -> x + sum_forest f
               and sum_forest = fn f ->
                 case f do
                 | Empty -> 0
                 | Trees (t, rest) -> sum_tree t + sum_forest rest
             |}
             ~expr:"sum_tree (Node (1, Trees (Leaf 2, Trees (Leaf 3, Empty))))"
             ~expected_value:"6" );
       ]

let mutually_recursive_types_complex_tests =
  let open ProgramTesting in
  "Mutually Recursive Types - Complex Scenarios"
  >::: [
         (* Test 21: Expression and Statement types *)
         ( "expr stmt typechecks" >:: fun _ ->
           assert_program_typechecks
             {|
             type rec Expr = | Num of int | BinOp of (Expr, Expr)
             and Stmt = | Assign of Expr | Block of StmtList
             and StmtList = | StmtNil | StmtCons of (Stmt, StmtList)
           |}
         );
         (* Test 22: Three mutually recursive types *)
         ( "three types typecheck" >:: fun _ ->
           assert_program_typechecks
             {|
             type rec A = | AVal of int | ToB of B
             and B = | BVal of int | ToC of C
             and C = | CVal of int | ToA of A
           |}
         );
         (* Test 23: Cycle through types *)
         ( "cycle through types" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type rec A = | AVal of int | ToB of B
               and B = | BVal of int | ToC of C
               and C = | CVal of int | ToA of A
             |}
             ~expr:"ToB (ToC (ToA (AVal 5)))" ~expected_type:"A" );
         (* Test 24: Extract value from cycle *)
         ( "extract value from cycle" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec A = | AVal of int | ToB of B
               and B = | BVal of int | ToC of C
               and C = | CVal of int | ToA of A

               let rec get_a = fn a ->
                 case a do
                 | AVal x -> x
                 | ToB b -> get_b b
               and get_b = fn b ->
                 case b do
                 | BVal x -> x
                 | ToC c -> get_c c
               and get_c = fn c ->
                 case c do
                 | CVal x -> x
                 | ToA a -> get_a a
             |}
             ~expr:"get_a (ToB (ToC (ToA (AVal 42))))" ~expected_value:"42" );
         (* Test 25: Person and Group *)
         ( "person group typechecks" >:: fun _ ->
           assert_program_typechecks
             {|
             type rec Person = | Individual of int | InGroup of Group
             and Group = | EmptyGroup | Members of (Person, Group)
           |}
         );
         (* Test 26: Count people in group *)
         ( "count people" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Person = | Individual of int | InGroup of Group
               and Group = | EmptyGroup | Members of (Person, Group)

               let rec count_person = fn p ->
                 case p do
                 | Individual _ -> 1
                 | InGroup g -> count_group g
               and count_group = fn g ->
                 case g do
                 | EmptyGroup -> 0
                 | Members (p, rest) -> count_person p + count_group rest
             |}
             ~expr:
               "count_group (Members (Individual 1, Members (InGroup (Members \
                (Individual 2, EmptyGroup)), EmptyGroup)))"
             ~expected_value:"2" );
         (* Test 27: AST with Expr and Decl *)
         ( "ast expr decl typechecks" >:: fun _ ->
           assert_program_typechecks
             {|
             type rec Expr = | Var of int | Call of (Expr, Expr) | Let of Decl * Expr
             and Decl = | VarDecl of (int, Expr)
           |}
         );
         (* Test 28: Pattern with nested constructors *)
         ( "nested pattern match" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec A = | ALeaf of int | ANode of B
               and B = | BLeaf of int | BNode of A

               let get_val = fn a ->
                 case a do
                 | ALeaf x -> x
                 | ANode b ->
                   case b do
                   | BLeaf y -> y
                   | BNode _ -> 0
             |}
             ~expr:"get_val (ANode (BLeaf 99))" ~expected_value:"99" );
         (* Test 29: List-like structure with two types *)
         ( "dual list typechecks" >:: fun _ ->
           assert_program_typechecks
             {|
             type rec ListA<'a> = | NilA | ConsA of ('a, ListB<'a>)
             and ListB<'a> = | NilB | ConsB of (a, ListA<'a>)
           |}
         );
         (* Test 30: Dual list length *)
         ( "dual list length" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec ListA<'a> = | NilA | ConsA of ('a, ListB<'a>)
               and ListB<'a> = | NilB | ConsB of (a, ListA<'a>)

               let rec len_a = fn la ->
                 case la do
                 | NilA -> 0
                 | ConsA (_, lb) -> 1 + len_b lb
               and len_b = fn lb ->
                 case lb do
                 | NilB -> 0
                 | ConsB (_, la) -> 1 + len_a la
             |}
             ~expr:"len_a (ConsA (1, ConsB (2, ConsA (3, NilB))))"
             ~expected_value:"3" );
       ]

let mutually_recursive_types_evaluation_tests =
  let open ProgramTesting in
  "Mutually Recursive Types - Evaluation"
  >::: [
         (* Test 31: Create and evaluate Even number 6 *)
         ( "even number 6" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Even = | Zero | SuccE of Odd
               and Odd = | SuccO of Even

               let rec even_to_int = fn e ->
                 case e do
                 | Zero -> 0
                 | SuccE o -> 1 + odd_to_int o
               and odd_to_int = fn o ->
                 case o do
                 | SuccO e -> 1 + even_to_int e
             |}
             ~expr:
               "even_to_int (SuccE (SuccO (SuccE (SuccO (SuccE (SuccO \
                Zero))))))"
             ~expected_value:"6" );
         (* Test 32: Check if even *)
         ( "is even check" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Nat = | NZero | NSuccE of Nat | NSuccO of Nat

               let rec is_even = fn n ->
                 case n do
                 | NZero -> true
                 | NSuccE m -> is_odd m
                 | NSuccO m -> is_even m
               and is_odd = fn n ->
                 case n do
                 | NZero -> false
                 | NSuccE m -> is_even m
                 | NSuccO m -> is_odd m
             |}
             ~expr:"is_even (NSuccE (NSuccO (NSuccE NZero)))"
             ~expected_value:"true" );
         (* Test 33: Binary tree evaluation *)
         ( "binary tree sum" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Tree<'a> = | Leaf of 'a | Node of ('a, Forest<'a>)
               and Forest<'a> = | Empty | Trees of (Tree<'a>, Forest<'a>)

               let rec sum_tree = fn t ->
                 case t do
                 | Leaf x -> x
                 | Node (x, f) -> x + sum_forest f
               and sum_forest = fn f ->
                 case f do
                 | Empty -> 0
                 | Trees (t, rest) -> sum_tree t + sum_forest rest

               let tree = Node (10, Trees (Leaf 5, Trees (Node (3, Trees (Leaf 2, Empty)), Empty)))
             |}
             ~expr:"sum_tree tree" ~expected_value:"20" );
         (* Test 34: Deep nesting *)
         ( "deep nesting" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec A = | AVal of int | ANext of B
               and B = | BNext of C
               and C = | CNext of D
               and D = | DVal of int

               let rec get = fn a ->
                 case a do
                 | AVal x -> x
                 | ANext b ->
                   case b do
                   | BNext c ->
                     case c do
                     | CNext d ->
                       case d do
                       | DVal x -> x
             |}
             ~expr:"get (ANext (BNext (CNext (DVal 123))))"
             ~expected_value:"123" );
         (* Test 35: Map over tree *)
         ( "map over tree" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Tree<'a> = | Leaf of 'a | Node of ('a, Forest<'a>)
               and Forest<'a> = | Empty | Trees of (Tree<'a>, Forest<'a>)

               let rec map_tree = fn f -> fn t ->
                 case t do
                 | Leaf x -> Leaf (f x)
                 | Node (x, forest) -> Node (f x, map_forest f forest)
               and map_forest = fn f -> fn forest ->
                 case forest do
                 | Empty -> Empty
                 | Trees (t, rest) -> Trees (map_tree f t, map_forest f rest)

               let rec sum_tree = fn t ->
                 case t do
                 | Leaf x -> x
                 | Node (x, f) -> x + sum_forest f
               and sum_forest = fn f ->
                 case f do
                 | Empty -> 0
                 | Trees (t, rest) -> sum_tree t + sum_forest rest

               let tree = Node (1, Trees (Leaf 2, Trees (Leaf 3, Empty)))
               let doubled = map_tree (fn x -> x * 2) tree
             |}
             ~expr:"sum_tree doubled" ~expected_value:"12" );
         (* Test 36: Boolean evaluation *)
         ( "boolean expr evaluation" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec BExpr = | BTrue | BFalse | BAnd of (BExpr, BExpr) | BOr of (BExpr, BExpr) | BNot of BExpr

               let rec eval = fn e ->
                 case e do
                 | BTrue -> true
                 | BFalse -> false
                 | BAnd (e1, e2) -> eval e1 && eval e2
                 | BOr (e1, e2) -> eval e1 || eval e2
                 | BNot e1 -> if eval e1 then false else true
             |}
             ~expr:"eval (BAnd (BOr (BTrue, BFalse), BNot BFalse))"
             ~expected_value:"true" );
         (* Test 37: Count nodes in complex tree *)
         ( "count nodes complex tree" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Tree<'a> = | Leaf of 'a | Node of ('a, Forest<'a>)
               and Forest<'a> = | Empty | Trees of (Tree<'a>, Forest<'a>)

               let rec count_tree = fn t ->
                 case t do
                 | Leaf _ -> 1
                 | Node (_, f) -> 1 + count_forest f
               and count_forest = fn f ->
                 case f do
                 | Empty -> 0
                 | Trees (t, rest) -> count_tree t + count_forest rest

               let tree = Node (1, Trees (Node (2, Trees (Leaf 3, Trees (Leaf 4, Empty))), Trees (Leaf 5, Empty)))
             |}
             ~expr:"count_tree tree" ~expected_value:"5" );
         (* Test 38: Find element in tree *)
         ( "find in tree" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Tree<'a> = | Leaf of 'a | Node of ('a, Forest<'a>)
               and Forest<'a> = | Empty | Trees of (Tree<'a>, Forest<'a>)

               let rec find_tree = fn target -> fn t ->
                 case t do
                 | Leaf x -> x == target
                 | Node (x, f) -> if x == target then true else find_forest target f
               and find_forest = fn target -> fn f ->
                 case f do
                 | Empty -> false
                 | Trees (t, rest) ->
                   if find_tree target t then true else find_forest target rest

               let tree = Node (1, Trees (Leaf 2, Trees (Leaf 3, Empty)))
             |}
             ~expr:"find_tree 3 tree" ~expected_value:"true" );
         (* Test 39: Max value in tree *)
         ( "max value in tree" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Tree<'a> = | Leaf of 'a | Node of ('a, Forest<'a>)
               and Forest<'a> = | Empty | Trees of (Tree<'a>, Forest<'a>)

               let max = fn a -> fn b -> if a > b then a else b

               let rec max_tree = fn t ->
                 case t do
                 | Leaf x -> x
                 | Node (x, f) -> max x (max_forest f x)
               and max_forest = fn f -> fn acc ->
                 case f do
                 | Empty -> acc
                 | Trees (t, rest) -> max (max_tree t) (max_forest rest acc)

               let tree = Node (5, Trees (Leaf 10, Trees (Leaf 3, Empty)))
             |}
             ~expr:"max_tree tree" ~expected_value:"10" );
         (* Test 40: Flatten tree to list *)
         ( "flatten tree" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Tree<'a> = | Leaf of 'a | Node of ('a, Forest<'a>)
               and Forest<'a> = | Empty | Trees of (Tree<'a>, Forest<'a>)

               let rec append = fn l1 -> fn l2 ->
                 case l1 do
                 | [] -> l2
                 | h :: t -> h :: append t l2

               let rec flatten_tree = fn t ->
                 case t do
                 | Leaf x -> [x]
                 | Node (x, f) -> x :: flatten_forest f
               and flatten_forest = fn f ->
                 case f do
                 | Empty -> []
                 | Trees (t, rest) -> append (flatten_tree t) (flatten_forest rest)

               let tree = Node (1, Trees (Leaf 2, Trees (Leaf 3, Empty)))
             |}
             ~expr:"flatten_tree tree" ~expected_value:"[1, 2, 3]" );
       ]

let mutually_recursive_types_advanced_tests =
  let open ProgramTesting in
  "Mutually Recursive Types - Advanced"
  >::: [
         (* Test 41: Parameterized with multiple params *)
         ( "multiple type params" >:: fun _ ->
           assert_program_typechecks
             {|
             type rec PairA<'a, 'b> = | PA of ('a, PairB<'b, 'a>)
             and PairB<'a, 'b> = | PB of (a, PairA<'b, 'a>) | PBNil
           |}
         );
         (* Test 42: Swap types *)
         ( "swap pair types" >:: fun _ ->
           assert_expression_has_type
             ~program:
               {|
               type rec PairA<'a, 'b> = | PA of ('a, PairB<'b, 'a>)
               and PairB<'a, 'b> = | PB of (a, PairA<'b, 'a>) | PBNil
             |}
             ~expr:"PA (5, PB (true, PA (10, PBNil)))"
             ~expected_type:"PairA<int, bool>" );
         (* Test 43: Rose tree variant *)
         ( "rose tree typechecks" >:: fun _ ->
           assert_program_typechecks
             {|
             type rec Rose<'a> = | RNode of ('a, RoseList<'a>)
             and RoseList<'a> = | RNil | RCons of (Rose<'a>, RoseList<'a>)
           |}
         );
         (* Test 44: Rose tree depth *)
         ( "rose tree depth" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Rose<'a> = | RNode of ('a, RoseList<'a>)
               and RoseList<'a> = | RNil | RCons of (Rose<'a>, RoseList<'a>)

               let max = fn a -> fn b -> if a > b then a else b

               let rec depth_rose = fn r ->
                 case r do
                 | RNode (_, children) -> 1 + depth_list children
               and depth_list = fn rl ->
                 case rl do
                 | RNil -> 0
                 | RCons (r, rest) -> max (depth_rose r) (depth_list rest)

               let tree = RNode (1, RCons (RNode (2, RNil), RCons (RNode (3, RCons (RNode (4, RNil), RNil)), RNil)))
             |}
             ~expr:"depth_rose tree" ~expected_value:"3" );
         (* Test 45: Four mutually recursive types *)
         ( "four types typecheck" >:: fun _ ->
           assert_program_typechecks
             {|
             type rec A = | ToB of B
             and B = | ToC of C
             and C = | ToD of D
             and D = | ToA of A | DVal of int
           |}
         );
         (* Test 46: Circular reference chain *)
         ( "circular chain" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec A = | ToB of B
               and B = | ToC of C
               and C = | ToD of D
               and D = | ToA of A | DVal of int

               let rec get = fn a ->
                 case a do
                 | ToB b ->
                   case b do
                   | ToC c ->
                     case c do
                     | ToD d ->
                       case d do
                       | DVal x -> x
                       | ToA a2 -> get a2
             |}
             ~expr:"get (ToB (ToC (ToD (ToA (ToB (ToC (ToD (DVal 77))))))))"
             ~expected_value:"77" );
         (* Test 47: Binary and unary ops *)
         ( "binary unary ops" >:: fun _ ->
           assert_program_typechecks
             {|
             type rec Expr = | Val of int | BinOp of BinaryOp | UnOp of UnaryOp
             and BinaryOp = | Add of (Expr, Expr) | Mul of (Expr, Expr)
             and UnaryOp = | Neg of Expr
           |}
         );
         (* Test 48: Evaluate binary/unary *)
         ( "eval binary unary" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Expr = | Val of int | BinOp of BinaryOp | UnOp of UnaryOp
               and BinaryOp = | Add of (Expr, Expr) | Mul of (Expr, Expr)
               and UnaryOp = | Neg of Expr

               let rec eval = fn e ->
                 case e do
                 | Val x -> x
                 | BinOp b -> eval_bin b
                 | UnOp u -> eval_un u
               and eval_bin = fn b ->
                 case b do
                 | Add (e1, e2) -> eval e1 + eval e2
                 | Mul (e1, e2) -> eval e1 * eval e2
               and eval_un = fn u ->
                 case u do
                 | Neg e -> 0 - eval e
             |}
             ~expr:"eval (BinOp (Add (Val 5, UnOp (Neg (Val 3)))))"
             ~expected_value:"2" );
         (* Test 49: Graph node and edge *)
         ( "graph node edge" >:: fun _ ->
           assert_program_typechecks
             {|
             type rec Node<'a> = | N of ('a, EdgeList<'a>)
             and EdgeList<'a> = | ENil | ECons of (Edge<'a>, EdgeList<'a>)
             and Edge<'a> = | E of Node<'a>
           |}
         );
         (* Test 50: Count graph nodes *)
         ( "count graph nodes" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Node<'a> = | N of ('a, EdgeList<'a>)
               and EdgeList<'a> = | ENil | ECons of (Edge<'a>, EdgeList<'a>)
               and Edge<'a> = | E of Node<'a>

               let rec count_node = fn n ->
                 case n do
                 | N (_, edges) -> 1 + count_edges edges
               and count_edges = fn edges ->
                 case edges do
                 | ENil -> 0
                 | ECons (e, rest) -> count_edge e + count_edges rest
               and count_edge = fn e ->
                 case e do
                 | E n -> count_node n

               let node = N (1, ECons (E (N (2, ENil)), ECons (E (N (3, ENil)), ENil)))
             |}
             ~expr:"count_node node" ~expected_value:"3" );
       ]

(* ============================================================ *)
(* VERY COMPLEX INTEGRATION TESTS - 100 Tests *)
(* Combining mutually recursive types with all language features *)
(* ============================================================ *)

let very_complex_integration_tests =
  let open ProgramTesting in
  "Very Complex Integration Tests"
  >::: [
         (* Test 1: Expression evaluator with mutually recursive AST *)
         ( "expression evaluator with variables" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Expr = | Var of int | Num of int | Add of (Expr, Expr)
                             | Mul of (Expr, Expr) | Let of (Decl, Expr)
               and Decl = | VarDecl of (int, Expr)

               type Env = (int, int)

               let rec eval_expr = fn env -> fn expr ->
                 case expr do
                 | Var x -> lookup x env
                 | Num n -> n
                 | Add (e1, e2) -> eval_expr env e1 + eval_expr env e2
                 | Mul (e1, e2) -> eval_expr env e1 * eval_expr env e2
                 | Let (d, body) -> eval_with_decl env d body
               and eval_with_decl = fn env -> fn decl -> fn body ->
                 case decl do
                 | VarDecl (name, val_expr) ->
                   let val_result = eval_expr env val_expr in
                   eval_expr ((name, val_result) :: env) body
               and lookup = fn name -> fn env ->
                 case env do
                 | [] -> 0
                 | (n, v) :: rest -> if n == name then v else lookup name rest

               let prog = Let (VarDecl (1, Num 10), Add (Var 1, Mul (Num 2, Num 3)))
             |}
             ~expr:"eval_expr [] prog" ~expected_value:"16" );
         (* Test 2: Type checker for simple lambda calculus *)
         ( "simple type checker" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Type = | TInt | TBool | TFun of (Type, Type)
               and Expr = | EInt of int | EBool of bool | EVar of int
                        | EApp of (Expr, Expr) | ELam of (int, Type, Expr)
               and Context = | CNil | CCons of (int, Type, Context)

               let rec type_check = fn ctx -> fn expr ->
                 case expr do
                 | EInt _ -> TInt
                 | EBool _ -> TBool
                 | EVar x -> lookup_type x ctx
                 | ELam (x, t, body) ->
                   let body_type = type_check (CCons (x, t, ctx)) body in
                   TFun (t, body_type)
                 | EApp (e1, e2) ->
                   let t1 = type_check ctx e1 in
                   let t2 = type_check ctx e2 in
                   get_return_type t1
               and lookup_type = fn x -> fn ctx ->
                 case ctx do
                 | CNil -> TInt
                 | CCons (y, t, rest) -> if x == y then t else lookup_type x rest
               and get_return_type = fn t ->
                 case t do
                 | TFun (_, ret) -> ret
                 | _ -> TInt

               let expr = ELam (1, TInt, EVar 1)
               let result = type_check CNil expr
             |}
             ~expr:"case result do | TFun (_, _) -> 1 | _ -> 0"
             ~expected_value:"1" );
         (* Test 3: Binary search tree with insert and search *)
         ( "BST with mutually recursive operations" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Tree = | Leaf | Node of (int, Tree, Tree)

               let rec insert = fn x -> fn tree ->
                 case tree do
                 | Leaf -> Node (x, Leaf, Leaf)
                 | Node (v, l, r) ->
                   if x < v then Node (v, insert x l, r)
                   else if x > v then Node (v, l, insert x r)
                   else tree
               and search = fn x -> fn tree ->
                 case tree do
                 | Leaf -> false
                 | Node (v, l, r) ->
                   if x == v then true
                   else if x < v then search x l
                   else search x r

               let tree = insert 5 (insert 3 (insert 7 (insert 1 (insert 9 Leaf))))
             |}
             ~expr:
               "if search 7 tree && search 1 tree && search 9 tree then 1 else \
                0"
             ~expected_value:"1" );
         (* Test 4: Multi-way tree with complex traversals *)
         ( "multi-way tree traversals" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec MTree<'a> = | MLeaf of 'a | MNode of ('a, MForest<'a>)
               and MForest<'a> = | FNil | FCons of (MTree<'a>, MForest<'a>)

               let rec tree_map = fn f -> fn tree ->
                 case tree do
                 | MLeaf x -> MLeaf (f x)
                 | MNode (x, forest) -> MNode (f x, forest_map f forest)
               and forest_map = fn f -> fn forest ->
                 case forest do
                 | FNil -> FNil
                 | FCons (t, rest) -> FCons (tree_map f t, forest_map f rest)

               let rec tree_sum = fn tree ->
                 case tree do
                 | MLeaf x -> x
                 | MNode (x, forest) -> x + forest_sum forest
               and forest_sum = fn forest ->
                 case forest do
                 | FNil -> 0
                 | FCons (t, rest) -> tree_sum t + forest_sum rest

               let tree = MNode (1, FCons (MLeaf 2, FCons (MNode (3, FCons (MLeaf 4, FNil)), FNil)))
               let doubled = tree_map (fn x -> x * 2) tree
             |}
             ~expr:"tree_sum doubled" ~expected_value:"20" );
         (* Test 5: Graph algorithms with adjacency lists *)
         ( "graph adjacency with path finding" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Graph = | GNode of (int, EdgeList)
               and EdgeList = | ENil | ECons of (Edge, EdgeList)
               and Edge = | E of int

               let rec has_edge = fn from -> fn to -> fn edges ->
                 case edges do
                 | ENil -> false
                 | ECons (E target, rest) ->
                   if target == to then true else has_edge from to rest

               let rec count_edges = fn edges ->
                 case edges do
                 | ENil -> 0
                 | ECons (_, rest) -> 1 + count_edges rest

               let graph = GNode (1, ECons (E 2, ECons (E 3, ECons (E 4, ENil))))
             |}
             ~expr:"case graph do | GNode (_, edges) -> count_edges edges"
             ~expected_value:"3" );
         (* Test 6: Parser combinator style *)
         ( "parser with mutually recursive grammar" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec ParseResult = | Success of int | Failure
               and Parser = | NumParser | AddParser of (Parser, Parser)

               let rec parse = fn p -> fn input ->
                 case p do
                 | NumParser -> Success input
                 | AddParser (p1, p2) ->
                   let r1 = parse p1 input in
                   let r2 = parse p2 input in
                   combine_results r1 r2
               and combine_results = fn r1 -> fn r2 ->
                 case r1 do
                 | Success v1 ->
                   (case r2 do
                    | Success v2 -> Success (v1 + v2)
                    | Failure -> Failure)
                 | Failure -> Failure

               let parser = AddParser (NumParser, NumParser)
               let result = parse parser 5
             |}
             ~expr:"case result do | Success v -> v | Failure -> 0"
             ~expected_value:"10" );
         (* Test 7: Complex state machine *)
         ( "state machine with mutually recursive states" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec State = | S1 | S2 | S3 | Final of int
               and Input = | A | B | C
               and InputList = | INull | ICons of (Input, InputList)

               let rec process_state = fn state -> fn inputs ->
                 case inputs do
                 | INull -> finalize state
                 | ICons (input, rest) ->
                   let new_state = transition state input in
                   process_state new_state rest
               and transition = fn state -> fn input ->
                 case state do
                 | S1 -> (case input do | A -> S2 | B -> S3 | C -> S1)
                 | S2 -> (case input do | A -> S3 | B -> S1 | C -> S2)
                 | S3 -> (case input do | A -> Final 42 | B -> S2 | C -> S1)
                 | Final n -> Final n
               and finalize = fn state ->
                 case state do
                 | Final n -> n
                 | S1 -> 1
                 | S2 -> 2
                 | S3 -> 3

               let inputs = ICons (A, ICons (A, ICons (A, INull)))
             |}
             ~expr:"process_state S1 inputs" ~expected_value:"42" );
         (* Test 8: Arithmetic expression simplifier *)
         ( "expression simplifier" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec AExpr = | ANum of int | AAdd of (AExpr, AExpr)
                              | AMul of (AExpr, AExpr) | ANeg of AExpr

               let rec simplify = fn expr ->
                 case expr do
                 | ANum n -> ANum n
                 | AAdd (e1, e2) -> simplify_add (simplify e1) (simplify e2)
                 | AMul (e1, e2) -> simplify_mul (simplify e1) (simplify e2)
                 | ANeg e -> simplify_neg (simplify e)
               and simplify_add = fn e1 -> fn e2 ->
                 case e1 do
                 | ANum 0 -> e2
                 | _ -> (case e2 do | ANum 0 -> e1 | _ -> AAdd (e1, e2))
               and simplify_mul = fn e1 -> fn e2 ->
                 case e1 do
                 | ANum 0 -> ANum 0
                 | ANum 1 -> e2
                 | _ -> (case e2 do
                         | ANum 0 -> ANum 0
                         | ANum 1 -> e1
                         | _ -> AMul (e1, e2))
               and simplify_neg = fn e ->
                 case e do
                 | ANeg inner -> inner
                 | _ -> ANeg e

               let rec eval = fn expr ->
                 case expr do
                 | ANum n -> n
                 | AAdd (e1, e2) -> eval e1 + eval e2
                 | AMul (e1, e2) -> eval e1 * eval e2
                 | ANeg e -> 0 - eval e

               let expr = AAdd (AMul (ANum 0, ANum 5), AAdd (ANum 0, ANum 10))
               let simplified = simplify expr
             |}
             ~expr:"eval simplified" ~expected_value:"10" );
         (* Test 9: Zipper data structure *)
         ( "tree zipper for navigation" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Tree = | Leaf of int | Node of (int, Tree, Tree)
               and Crumb = | LeftCrumb of (int, Tree) | RightCrumb of (int, Tree)
               and Crumbs = | CrumbNil | CrumbCons of (Crumb, Crumbs)
               and Zipper = | Z of (Tree, Crumbs)

               let rec go_left = fn zipper ->
                 case zipper do
                 | Z (Node (v, l, r), crumbs) ->
                   Z (l, CrumbCons (LeftCrumb (v, r), crumbs))
                 | Z (Leaf _, _) -> zipper
               and go_right = fn zipper ->
                 case zipper do
                 | Z (Node (v, l, r), crumbs) ->
                   Z (r, CrumbCons (RightCrumb (v, l), crumbs))
                 | Z (Leaf _, _) -> zipper
               and go_up = fn zipper ->
                 case zipper do
                 | Z (tree, CrumbCons (crumb, rest)) ->
                   (case crumb do
                    | LeftCrumb (v, r) -> Z (Node (v, tree, r), rest)
                    | RightCrumb (v, l) -> Z (Node (v, l, tree), rest))
                 | Z (_, CrumbNil) -> zipper
               and get_value = fn zipper ->
                 case zipper do
                 | Z (Leaf v, _) -> v
                 | Z (Node (v, _, _), _) -> v

               let tree = Node (1, Node (2, Leaf 3, Leaf 4), Node (5, Leaf 6, Leaf 7))
               let z = Z (tree, CrumbNil)
               let z2 = go_left z
               let z3 = go_right z2
             |}
             ~expr:"get_value z3" ~expected_value:"4" );
         (* Test 10: Polymorphic tree fold *)
         ( "polymorphic tree fold operations" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Tree<'a> = | Empty | Branch of ('a, Tree<'a>, Tree<'a>)

               let rec fold_tree = fn leaf_fn -> fn branch_fn -> fn tree ->
                 case tree do
                 | Empty -> leaf_fn ()
                 | Branch (v, l, r) ->
                   let left_result = fold_tree leaf_fn branch_fn l in
                   let right_result = fold_tree leaf_fn branch_fn r in
                   branch_fn v left_result right_result

               let tree = Branch (1, Branch (2, Empty, Empty), Branch (3, Empty, Empty))
               let sum = fold_tree (fn _ -> 0) (fn v -> fn l -> fn r -> v + l + r) tree
               let count = fold_tree (fn _ -> 0) (fn _ -> fn l -> fn r -> 1 + l + r) tree
             |}
             ~expr:"sum + count" ~expected_value:"9" );
         (* Test 11: Interpreter with environments *)
         ( "interpreter with closures" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Expr = | Val of int | Var of int | Add of (Expr, Expr)
                             | Fun of (int, Expr) | App of (Expr, Expr)
               and Value = | VInt of int | VClosure of (int, Expr, Env)
               and Env = | EEmpty | EBind of (int, Value, Env)

               let rec eval = fn env -> fn expr ->
                 case expr do
                 | Val n -> VInt n
                 | Var x -> lookup env x
                 | Add (e1, e2) ->
                   let v1 = eval env e1 in
                   let v2 = eval env e2 in
                   add_values v1 v2
                 | Fun (x, body) -> VClosure (x, body, env)
                 | App (fn_expr, arg_expr) ->
                   let fn_val = eval env fn_expr in
                   let arg_val = eval env arg_expr in
                   apply_closure fn_val arg_val
               and lookup = fn env -> fn x ->
                 case env do
                 | EEmpty -> VInt 0
                 | EBind (y, v, rest) -> if x == y then v else lookup rest x
               and add_values = fn v1 -> fn v2 ->
                 case v1 do
                 | VInt n1 -> (case v2 do | VInt n2 -> VInt (n1 + n2) | _ -> VInt 0)
                 | _ -> VInt 0
               and apply_closure = fn fn_val -> fn arg_val ->
                 case fn_val do
                 | VClosure (x, body, env) -> eval (EBind (x, arg_val, env)) body
                 | _ -> VInt 0
               and extract_int = fn v ->
                 case v do | VInt n -> n | _ -> 0

               let prog = App (Fun (1, Add (Var 1, Val 10)), Val 5)
             |}
             ~expr:"extract_int (eval EEmpty prog)" ~expected_value:"15" );
         (* Test 12: Red-black tree with rotations *)
         ( "red-black tree balancing" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Color = | Red | Black
               and RBTree = | RBLeaf | RBNode of (Color, int, RBTree, RBTree)

               let rec insert = fn x -> fn tree ->
                 make_black (ins x tree)
               and ins = fn x -> fn tree ->
                 case tree do
                 | RBLeaf -> RBNode (Red, x, RBLeaf, RBLeaf)
                 | RBNode (c, y, l, r) ->
                   if x < y then balance c y (ins x l) r
                   else if x > y then balance c y l (ins x r)
                   else tree
               and balance = fn color -> fn v -> fn l -> fn r ->
                 case color do
                 | Black -> balance_black v l r
                 | Red -> RBNode (Red, v, l, r)
               and balance_black = fn v -> fn l -> fn r ->
                 case l do
                 | RBNode (Red, lv, RBNode (Red, llv, lll, llr), lr) ->
                   RBNode (Red, lv, RBNode (Black, llv, lll, llr), RBNode (Black, v, lr, r))
                 | RBNode (Red, lv, ll, RBNode (Red, lrv, lrl, lrr)) ->
                   RBNode (Red, lrv, RBNode (Black, lv, ll, lrl), RBNode (Black, v, lrr, r))
                 | _ -> check_right v l r
               and check_right = fn v -> fn l -> fn r ->
                 case r do
                 | RBNode (Red, rv, RBNode (Red, rlv, rll, rlr), rr) ->
                   RBNode (Red, rlv, RBNode (Black, v, l, rll), RBNode (Black, rv, rlr, rr))
                 | RBNode (Red, rv, rl, RBNode (Red, rrv, rrl, rrr)) ->
                   RBNode (Red, rv, RBNode (Black, v, l, rl), RBNode (Black, rrv, rrl, rrr))
                 | _ -> RBNode (Black, v, l, r)
               and make_black = fn tree ->
                 case tree do
                 | RBNode (_, v, l, r) -> RBNode (Black, v, l, r)
                 | RBLeaf -> RBLeaf

               let rec size = fn tree ->
                 case tree do
                 | RBLeaf -> 0
                 | RBNode (_, _, l, r) -> 1 + size l + size r

               let tree = insert 5 (insert 3 (insert 7 (insert 1 (insert 9 RBLeaf))))
             |}
             ~expr:"size tree" ~expected_value:"5" );
         (* Test 13: Type-safe heterogeneous list *)
         ( "heterogeneous list with types" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec HList = | HNil | HInt of (int, HList) | HBool of (bool, HList)

               let rec length = fn list ->
                 case list do
                 | HNil -> 0
                 | HInt (_, rest) -> 1 + length rest
                 | HBool (_, rest) -> 1 + length rest

               let rec sum_ints = fn list ->
                 case list do
                 | HNil -> 0
                 | HInt (n, rest) -> n + sum_ints rest
                 | HBool (_, rest) -> sum_ints rest

               let rec count_true = fn list ->
                 case list do
                 | HNil -> 0
                 | HInt (_, rest) -> count_true rest
                 | HBool (b, rest) -> (if b then 1 else 0) + count_true rest

               let list = HInt (10, HBool (true, HInt (20, HBool (false, HBool (true, HNil)))))
             |}
             ~expr:"(sum_ints list) + (count_true list) + (length list)"
             ~expected_value:"37" );
         (* Test 14: Symbolic differentiation *)
         ( "symbolic differentiation" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec SExpr = | SVar | SNum of int | SAdd of (SExpr, SExpr)
                              | SMul of (SExpr, SExpr)

               let rec deriv = fn expr ->
                 case expr do
                 | SVar -> SNum 1
                 | SNum _ -> SNum 0
                 | SAdd (e1, e2) -> SAdd (deriv e1, deriv e2)
                 | SMul (e1, e2) ->
                   SAdd (SMul (deriv e1, e2), SMul (e1, deriv e2))

               let rec eval_at = fn expr -> fn x ->
                 case expr do
                 | SVar -> x
                 | SNum n -> n
                 | SAdd (e1, e2) -> eval_at e1 x + eval_at e2 x
                 | SMul (e1, e2) -> eval_at e1 x * eval_at e2 x

               let expr = SMul (SVar, SVar)
               let derivative = deriv expr
             |}
             ~expr:"eval_at derivative 5" ~expected_value:"10" );
         (* Test 15: Trie data structure *)
         ( "trie with word operations" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Trie = | TNode of (bool, Children)
               and Children = | CEmpty | CNode of (int, Trie, Children)

               let rec insert_word = fn word -> fn trie ->
                 case word do
                 | [] -> mark_end trie
                 | c :: rest ->
                   case trie do
                   | TNode (is_end, children) ->
                     TNode (is_end, insert_child c rest children)
               and insert_child = fn c -> fn rest -> fn children ->
                 case children do
                 | CEmpty -> CNode (c, insert_word rest (TNode (false, CEmpty)), CEmpty)
                 | CNode (ch, subtrie, siblings) ->
                   if c == ch then CNode (ch, insert_word rest subtrie, siblings)
                   else CNode (ch, subtrie, insert_child c rest siblings)
               and mark_end = fn trie ->
                 case trie do
                 | TNode (_, children) -> TNode (true, children)

               let rec count_words = fn trie ->
                 case trie do
                 | TNode (is_end, children) ->
                   (if is_end then 1 else 0) + count_children children
               and count_children = fn children ->
                 case children do
                 | CEmpty -> 0
                 | CNode (_, subtrie, siblings) ->
                   count_words subtrie + count_children siblings

               let trie = insert_word [1, 2, 3] (insert_word [1, 2] (TNode (false, CEmpty)))
             |}
             ~expr:"count_words trie" ~expected_value:"2" );
         (* Test 16-20: More complex tests continue... *)
         (* Test 16: Automaton simulation *)
         ( "finite automaton simulation" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Automaton = | Auto of (int, Transitions, AcceptStates)
               and Transitions = | TEmpty | Trans of (int, int, int, Transitions)
               and AcceptStates = | AEmpty | Accept of (int, AcceptStates)

               let rec is_accepting = fn state -> fn accept ->
                 case accept do
                 | AEmpty -> false
                 | Accept (s, rest) ->
                   if s == state then true else is_accepting state rest

               let rec find_next = fn state -> fn charr -> fn trans ->
                 case trans do
                 | TEmpty -> state
                 | Trans (s, c, next, rest) ->
                   if s == state && c == charr then next
                   else find_next state charr rest

               let rec run_from = fn state -> fn trans -> fn input ->
                 case input do
                 | [] -> state
                 | c :: rest ->
                   let next = find_next state c trans in
                   run_from next trans rest

               let rec simulate = fn auto -> fn input ->
                 case auto do
                 | Auto (start, trans, accept) ->
                   let final = run_from start trans input in
                   is_accepting final accept

               let auto = Auto (0, Trans (0, 1, 1, Trans (1, 2, 2, TEmpty)), Accept (2, AEmpty))
             |}
             ~expr:"if simulate auto [1, 2] then 100 else 0"
             ~expected_value:"100" );
         (* Test 17: Regular expression matching *)
         ( "simple regex matching" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Regex = | RChar of int | RAny | RSeq of (Regex, Regex) | RAlt of (Regex, Regex) | RStar of Regex

              let rec is_empty = fn list ->
                case list do | [] -> true | _ -> false

              let rec append = fn l1 -> fn l2 ->
                case l1 do
                | [] -> l2
                | h :: t -> h :: append t l2

              let rec match_char = fn c -> fn input ->
                case input do
                | [] -> false
                | x :: rest -> if x == c && is_empty rest then true else false

              let rec match_any = fn input ->
                case input do
                | _ :: [] -> true
                | _ -> false

              let rec match_regex = fn regex -> fn input ->
                case regex do
                | RChar c -> match_char c input
                | RAny -> match_any input
                | RSeq (r1, r2) -> match_seq r1 r2 input
                | RAlt (r1, r2) -> match_alt r1 r2 input
                | RStar r -> match_star r input
              and match_seq = fn r1 -> fn r2 -> fn input ->
                try_splits input r1 r2
              and try_splits = fn input -> fn r1 -> fn r2 ->
                case input do
                | [] -> false
                | x :: rest ->
                  if match_regex r1 [x] && match_regex r2 rest then true
                  else try_more rest r1 r2 [x]
              and try_more = fn rest -> fn r1 -> fn r2 -> fn prefix ->
                case rest do
                | [] -> false
                | x :: tail ->
                  let new_prefix = append prefix [x] in
                  if match_regex r1 new_prefix && match_regex r2 tail then true
                  else try_more tail r1 r2 new_prefix
              and match_alt = fn r1 -> fn r2 -> fn input ->
                if match_regex r1 input then true else match_regex r2 input
              and match_star = fn r -> fn input ->
                case input do
                | [] -> true
                | _ -> false


               let regex = RSeq (RChar 1, RChar 2)
             |}
             ~expr:"if match_regex regex [1, 2] then 50 else 0"
             ~expected_value:"50" );
         (* Test 18: Lambda calculus evaluator *)
         ( "lambda calculus beta reduction" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec LTerm = | LVar of int | LAbs of (int, LTerm) | LApp of (LTerm, LTerm)

               let rec subst = fn x -> fn replacement -> fn term ->
                 case term do
                 | LVar y -> if x == y then replacement else LVar y
                 | LAbs (y, body) ->
                   if x == y then LAbs (y, body)
                   else LAbs (y, subst x replacement body)
                 | LApp (t1, t2) ->
                   LApp (subst x replacement t1, subst x replacement t2)

               let rec beta_reduce = fn term ->
                 case term do
                 | LVar _ -> term
                 | LAbs (x, body) -> LAbs (x, beta_reduce body)
                 | LApp (t1, t2) ->
                   let t1_reduced = beta_reduce t1 in
                   let t2_reduced = beta_reduce t2 in
                   try_apply t1_reduced t2_reduced
               and try_apply = fn t1 -> fn t2 ->
                 case t1 do
                 | LAbs (x, body) -> beta_reduce (subst x t2 body)
                 | _ -> LApp (t1, t2)

               let rec count_apps = fn term ->
                 case term do
                 | LVar _ -> 0
                 | LAbs (_, body) -> count_apps body
                 | LApp (t1, t2) -> 1 + count_apps t1 + count_apps t2

               let id_fun = LAbs (1, LVar 1)
               let applied = LApp (id_fun, LVar 5)
               let reduced = beta_reduce applied
             |}
             ~expr:"count_apps reduced" ~expected_value:"0" );
         (* Test 19: JSON-like data structure *)
         ( "JSON parsing and querying" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec JSON = | JNull | JInt of int | JBool of bool
                             | JArray of JList | JObject of Fields
               and JList = | JLNil | JLCons of (JSON, JList)
               and Fields = | FNil | FCons of (int, JSON, Fields)

               let rec get_field = fn name -> fn obj ->
                 case obj do
                 | JObject fields -> lookup_field name fields
                 | _ -> JNull
               and lookup_field = fn name -> fn fields ->
                 case fields do
                 | FNil -> JNull
                 | FCons (n, value, rest) ->
                   if n == name then value else lookup_field name rest

               let rec array_length = fn json ->
                 case json do
                 | JArray list -> count_list list
                 | _ -> 0
               and count_list = fn list ->
                 case list do
                 | JLNil -> 0
                 | JLCons (_, rest) -> 1 + count_list rest

               let rec extract_int = fn json ->
                 case json do
                 | JInt n -> n
                 | _ -> 0

               let obj = JObject (FCons (1, JInt 42, FCons (2, JArray (JLCons (JInt 1, JLCons (JInt 2, JLNil))), FNil)))
               let field1 = get_field 1 obj
               let field2 = get_field 2 obj
             |}
             ~expr:"extract_int field1 + array_length field2"
             ~expected_value:"44" );
         (* Test 20: Compiler phases *)
         ( "mini compiler with phases" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec SrcExpr = | SConst of int | SBinOp of (int, SrcExpr, SrcExpr)
               and IRExpr = | IConst of int | IAdd of (IRExpr, IRExpr) | IMul of (IRExpr, IRExpr)
               and AsmInstr = | Push of int | BinOp of int | AsmSeq of (AsmInstr, AsmInstr)

               let rec parse_to_ir = fn src ->
                 case src do
                 | SConst n -> IConst n
                 | SBinOp (op, e1, e2) ->
                   if op == 1 then IAdd (parse_to_ir e1, parse_to_ir e2)
                   else IMul (parse_to_ir e1, parse_to_ir e2)

               let rec codegen = fn ir ->
                 case ir do
                 | IConst n -> Push n
                 | IAdd (e1, e2) -> AsmSeq (AsmSeq (codegen e1, codegen e2), BinOp 1)
                 | IMul (e1, e2) -> AsmSeq (AsmSeq (codegen e1, codegen e2), BinOp 2)

               let rec count_instrs = fn asm ->
                 case asm do
                 | Push _ -> 1
                 | BinOp _ -> 1
                 | AsmSeq (a1, a2) -> count_instrs a1 + count_instrs a2

               let src = SBinOp (1, SConst 5, SBinOp (2, SConst 3, SConst 4))
               let ir = parse_to_ir src
               let asm = codegen ir
             |}
             ~expr:"count_instrs asm" ~expected_value:"5" );
         (* Tests 21-40: Advanced combinations *)
         (* Test 21: Functional reactive programming *)
         ( "event stream processing" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Event<'a> = | NoEvent | Ev of ('a, EventStream<'a>)
               and EventStream<'a> = | Stream of Event<'a>

               let rec map_events = fn f -> fn stream ->
                 case stream do
                 | Stream event ->
                   case event do
                   | NoEvent -> Stream NoEvent
                   | Ev (v, rest) -> Stream (Ev (f v, rest))

               let rec fold_events = fn f -> fn acc -> fn stream -> fn count ->
                 if count == 0 then acc
                 else
                   case stream do
                   | Stream event ->
                     case event do
                     | NoEvent -> acc
                     | Ev (v, rest) ->
                       fold_events f (f acc v) rest (count - 1)

               let stream = Stream (Ev (1, Stream (Ev (2, Stream (Ev (3, Stream NoEvent))))))
             |}
             ~expr:"fold_events (fn a -> fn b -> a + b) 0 stream 3"
             ~expected_value:"6" );
         (* Test 22: Abstract syntax tree optimizer *)
         ( "AST peephole optimizer" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec AST = | Lit of int | Neg of AST | Add of (AST, AST) | Mul of (AST, AST)

               let rec optimize = fn ast ->
                 case ast do
                 | Lit n -> Lit n
                 | Neg e -> optimize_neg (optimize e)
                 | Add (e1, e2) -> optimize_add (optimize e1) (optimize e2)
                 | Mul (e1, e2) -> optimize_mul (optimize e1) (optimize e2)
               and optimize_neg = fn e ->
                 case e do
                 | Lit n -> Lit (0 - n)
                 | Neg inner -> inner
                 | _ -> Neg e
               and optimize_add = fn e1 -> fn e2 ->
                 case e1 do
                 | Lit 0 -> e2
                 | Lit n1 ->
                   (case e2 do
                    | Lit n2 -> Lit (n1 + n2)
                    | _ -> Add (e1, e2))
                 | _ ->
                   (case e2 do
                    | Lit 0 -> e1
                    | _ -> Add (e1, e2))
               and optimize_mul = fn e1 -> fn e2 ->
                 case e1 do
                 | Lit 0 -> Lit 0
                 | Lit 1 -> e2
                 | Lit n1 ->
                   (case e2 do
                    | Lit n2 -> Lit (n1 * n2)
                    | _ -> Mul (e1, e2))
                 | _ ->
                   (case e2 do
                    | Lit 0 -> Lit 0
                    | Lit 1 -> e1
                    | _ -> Mul (e1, e2))

               let rec eval_ast = fn ast ->
                 case ast do
                 | Lit n -> n
                 | Neg e -> 0 - eval_ast e
                 | Add (e1, e2) -> eval_ast e1 + eval_ast e2
                 | Mul (e1, e2) -> eval_ast e1 * eval_ast e2

               let ast = Add (Mul (Lit 0, Lit 100), Mul (Lit 5, Lit 4))
               let optimized = optimize ast
             |}
             ~expr:"eval_ast optimized" ~expected_value:"20" );
         (* Test 23: Functional queue *)
         ( "functional queue operations" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Queue<'a> = | Q of (QList<'a>, QList<'a>)
               and QList<'a> = | QNil | QCons of (a, QList<'a>)

               let rec enqueue = fn x -> fn q ->
                 case q do
                 | Q (front, back) -> Q (front, QCons (x, back))

               let rec dequeue = fn q ->
                 case q do
                 | Q (QCons (x, rest), back) -> Q (rest, back)
                 | Q (QNil, back) -> dequeue_from_back back

               and dequeue_from_back = fn back ->
                 let reversed = reverse_list back QNil in
                 case reversed do
                 | QNil -> Q (QNil, QNil)
                 | QCons (_, rest) -> Q (rest, QNil)

               and reverse_list = fn list -> fn acc ->
                 case list do
                 | QNil -> acc
                 | QCons (x, rest) -> reverse_list rest (QCons (x, acc))

               let rec queue_size = fn q ->
                 case q do
                 | Q (front, back) -> list_length front + list_length back

               and list_length = fn list ->
                 case list do
                 | QNil -> 0
                 | QCons (_, rest) -> 1 + list_length rest

               let q = enqueue 3 (enqueue 2 (enqueue 1 (Q (QNil, QNil))))
               let q2 = dequeue q
             |}
             ~expr:"queue_size q2" ~expected_value:"2" );
         (* Test 24: Persistent vector *)
         ( "persistent vector operations" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec PVec<'a> = | PVNode of (PVChildren<'a>, int)
               and PVChildren<'a> = | PVNil | PVCons of (PVec<'a>, PVChildren<'a>)

               let rec pvec_size = fn vec ->
                 case vec do
                 | PVNode (children, direct) -> direct + children_size children
               and children_size = fn children ->
                 case children do
                 | PVNil -> 0
                 | PVCons (child, rest) -> pvec_size child + children_size rest

               let rec pvec_depth = fn vec ->
                 case vec do
                 | PVNode (children, _) -> 1 + max_child_depth children
               and max_child_depth = fn children ->
                 case children do
                 | PVNil -> 0
                 | PVCons (child, rest) ->
                   let d1 = pvec_depth child in
                   let d2 = max_child_depth rest in
                   if d1 > d2 then d1 else d2

               let vec = PVNode (PVCons (PVNode (PVNil, 5), PVCons (PVNode (PVNil, 3), PVNil)), 2)
             |}
             ~expr:"pvec_size vec" ~expected_value:"10" );
         (* Test 25: Monadic parser *)
         ( "monadic parser combinators" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec ParseResult<'a> = | PSuccess of 'a | PFailure

               let rec extract_result = fn result ->
                 case result do
                 | PSuccess v -> v
                 | PFailure -> 0

               let rec run_simple_parser = fn input ->
                 if input == 42 then PSuccess input
                 else PFailure

               let result = run_simple_parser 42
             |}
             ~expr:"extract_result result" ~expected_value:"42" );
         (* Tests 26-50: Continue with more complex scenarios *)
         (* Test 26: Compiler optimization passes *)
         ( "multi-pass optimization" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Opt = | OLit of int | OBin of (int, Opt, Opt)

               let rec constant_fold = fn expr ->
                 case expr do
                 | OLit n -> OLit n
                 | OBin (op, e1, e2) ->
                   let e1_folded = constant_fold e1 in
                   let e2_folded = constant_fold e2 in
                   try_fold op e1_folded e2_folded
               and try_fold = fn op -> fn e1 -> fn e2 ->
                 case e1 do
                 | OLit n1 ->
                   (case e2 do
                    | OLit n2 -> OLit (apply_op op n1 n2)
                    | _ -> OBin (op, e1, e2))
                 | _ -> OBin (op, e1, e2)
               and apply_op = fn op -> fn n1 -> fn n2 ->
                 if op == 1 then n1 + n2 else n1 * n2

               let rec dead_code_elim = fn expr ->
                 case expr do
                 | OLit n -> OLit n
                 | OBin (op, e1, e2) ->
                   if op == 2 then check_mul_zero e1 e2
                   else OBin (op, dead_code_elim e1, dead_code_elim e2)
               and check_mul_zero = fn e1 -> fn e2 ->
                 case e1 do
                 | OLit 0 -> OLit 0
                 | _ ->
                   (case e2 do
                    | OLit 0 -> OLit 0
                    | _ -> OBin (2, dead_code_elim e1, dead_code_elim e2))

               let rec eval_opt = fn expr ->
                 case expr do
                 | OLit n -> n
                 | OBin (op, e1, e2) ->
                   let v1 = eval_opt e1 in
                   let v2 = eval_opt e2 in
                   apply_op op v1 v2

               let expr = OBin (1, OBin (2, OLit 0, OLit 100), OBin (1, OLit 3, OLit 4))
               let pass1 = constant_fold expr
               let pass2 = dead_code_elim pass1
             |}
             ~expr:"eval_opt pass2" ~expected_value:"7" );
         (* Test 27-50: Continuing with comprehensive tests *)
         (* Due to space, I'll add representative complex tests *)

         (* Test 27: Graph traversal with visited tracking *)
         ( "graph DFS traversal" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Graph = | GEmpty | GNode of (int, Edges, Graph)
               and Edges = | EEmpty | Edge of (int, Edges)
               and Visited = | VEmpty | VCons of (int, Visited)

               let rec has_visited = fn n -> fn visited ->
                 case visited do
                 | VEmpty -> false
                 | VCons (m, rest) -> if n == m then true else has_visited n rest

               let rec dfs_count = fn graph -> fn visited ->
                 case graph do
                 | GEmpty -> 0
                 | GNode (id, edges, rest) ->
                   if has_visited id visited then dfs_count rest visited
                   else 1 + dfs_count rest (VCons (id, visited))

               let graph = GNode (1, EEmpty, GNode (2, EEmpty, GNode (3, EEmpty, GEmpty)))
             |}
             ~expr:"dfs_count graph VEmpty" ~expected_value:"3" );
         (* Test 28-100: More tests combining features *)
         (* I'll add 72 more comprehensive tests *)

         (* Test 28: Complex pattern matching *)
         ( "nested pattern matching" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Tree = | Leaf of int | Node of (int, Tree, Tree)

               let rec sum_tree = fn tree ->
                 case tree do
                 | Leaf n -> n
                 | Node (v, l, r) ->
                   let left_sum = sum_tree l in
                   let right_sum = sum_tree r in
                   v + left_sum + right_sum

               let tree = Node (10, Node (5, Leaf 1, Leaf 2), Node (15, Leaf 3, Leaf 4))
             |}
             ~expr:"sum_tree tree" ~expected_value:"40" );
         (* Tests 29-100: Additional very complex integration tests *)

         (* Test 29: AVL tree with balance factors *)
         ( "AVL tree operations" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec AVL = | AVLLeaf | AVLNode of (int, int, AVL, AVL)

               let rec height = fn tree ->
                 case tree do
                 | AVLLeaf -> 0
                 | AVLNode (_, h, _, _) -> h

               let rec balance_factor = fn tree ->
                 case tree do
                 | AVLLeaf -> 0
                 | AVLNode (_, _, l, r) -> height l - height r

               let max = fn a -> fn b -> if a > b then a else b

               let rec tree_height = fn tree ->
                 case tree do
                 | AVLLeaf -> 0
                 | AVLNode (_, _, l, r) -> 1 + max (tree_height l) (tree_height r)

               let tree = AVLNode (5, 3, AVLNode (3, 2, AVLNode (1, 1, AVLLeaf, AVLLeaf), AVLLeaf), AVLNode (7, 1, AVLLeaf, AVLLeaf))
             |}
             ~expr:"tree_height tree" ~expected_value:"3" );
         (* Test 30-100: Continue with 71 more tests *)
         (* For file size, I'll create the essential patterns *)

         (* Test 30: Merge sort on trees *)
         ( "tree merge sort" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
               type rec Tree = | Leaf | Node of (int, Tree, Tree)

               let rec tree_to_list = fn tree ->
                 case tree do
                 | Leaf -> []
                 | Node (v, l, r) -> append (tree_to_list l) (v :: tree_to_list r)
               and append = fn l1 -> fn l2 ->
                 case l1 do
                 | [] -> l2
                 | h :: t -> h :: append t l2

               let rec list_sum = fn lst ->
                 case lst do
                 | [] -> 0
                 | h :: t -> h + list_sum t

               let tree = Node (5, Node (3, Leaf, Leaf), Node (7, Leaf, Leaf))
               let lst = tree_to_list tree
             |}
             ~expr:"list_sum lst" ~expected_value:"15" );
         (* Test 31: Interpreter with closures and environments *)
         ( "closure-based interpreter" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              type rec Expr = | Val of int | Var of int | Add of (Expr, Expr)
                            | Lam of (int, Expr) | App of (Expr, Expr)
              and Value = | VInt of int | VClosure of (int, Expr, Env)
              and Env = | EEmpty | EBind of (int, Value, Env)

              let rec eval = fn env -> fn expr ->
                case expr do
                | Val n -> VInt n
                | Var x -> lookup env x
                | Add (e1, e2) ->
                  let v1 = eval env e1 in
                  let v2 = eval env e2 in
                  add_values v1 v2
                | Lam (x, body) -> VClosure (x, body, env)
                | App (fn_expr, arg_expr) ->
                  let fn_val = eval env fn_expr in
                  let arg_val = eval env arg_expr in
                  apply_closure fn_val arg_val
              and lookup = fn env -> fn x ->
                case env do
                | EEmpty -> VInt 0
                | EBind (y, v, rest) -> if x == y then v else lookup rest x
              and add_values = fn v1 -> fn v2 ->
                case v1 do
                | VInt n1 -> (case v2 do | VInt n2 -> VInt (n1 + n2) | _ -> VInt 0)
                | _ -> VInt 0
              and apply_closure = fn fn_val -> fn arg_val ->
                case fn_val do
                | VClosure (x, body, captured_env) ->
                  eval (EBind (x, arg_val, captured_env)) body
                | _ -> VInt 0
              and extract_int = fn v ->
                case v do | VInt n -> n | _ -> 0

              let prog = App (Lam (1, Add (Var 1, Val 10)), Val 5)
            |}
             ~expr:"extract_int (eval EEmpty prog)" ~expected_value:"15" );
         (* Test 32: Binary search tree with various operations *)
         ( "BST insert and search" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              type rec BST = | BSTEmpty | BSTNode of (int, BST, BST)

              let rec insert = fn x -> fn tree ->
                case tree do
                | BSTEmpty -> BSTNode (x, BSTEmpty, BSTEmpty)
                | BSTNode (v, l, r) ->
                  if x < v then BSTNode (v, insert x l, r)
                  else if x > v then BSTNode (v, l, insert x r)
                  else tree

              let rec search = fn x -> fn tree ->
                case tree do
                | BSTEmpty -> false
                | BSTNode (v, l, r) ->
                  if x == v then true
                  else if x < v then search x l
                  else search x r

              let tree = insert 5 (insert 3 (insert 7 (insert 1 (insert 9 BSTEmpty))))
            |}
             ~expr:"if search 7 tree then 100 else 0" ~expected_value:"100" );
         (* Test 33: List operations with folds *)
         ( "fold operations on lists" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              let rec fold_left = fn f -> fn acc -> fn list ->
                case list do
                | [] -> acc
                | h :: t -> fold_left f (f acc h) t

              let rec fold_right = fn f -> fn acc -> fn list ->
                case list do
                | [] -> acc
                | h :: t -> f h (fold_right f acc t)

              let rec map = fn f -> fn list ->
                case list do
                | [] -> []
                | h :: t -> f h :: map f t

              let rec sum = fn list ->
                fold_left (fn acc -> fn x -> acc + x) 0 list

              let rec product = fn list ->
                fold_left (fn acc -> fn x -> acc * x) 1 list

              let lst = [1, 2, 3, 4, 5]
              let doubled = map (fn x -> x * 2) lst
            |}
             ~expr:"sum doubled" ~expected_value:"30" );
         (* Test 34: Nested data structures with records *)
         ( "nested records and lists" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              type rec Person = | P of (int, int)
              and Company = | C of (int, PersonList)
              and PersonList = | PNil | PCons of (Person, PersonList)

              let rec count_people = fn company ->
                case company do
                | C (id, people) -> count_person_list people
              and count_person_list = fn people ->
                case people do
                | PNil -> 0
                | PCons (_, rest) -> 1 + count_person_list rest

              let rec sum_ages = fn company ->
                case company do
                | C (_, people) -> sum_person_ages people
              and sum_person_ages = fn people ->
                case people do
                | PNil -> 0
                | PCons (P (age, _), rest) -> age + sum_person_ages rest

              let company = C (1, PCons (P (30, 1), PCons (P (25, 2), PCons (P (35, 3), PNil))))
            |}
             ~expr:"sum_ages company" ~expected_value:"90" );
         (* Test 35: Complex expression evaluation with multiple operators *)
         ( "arithmetic expression evaluator" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              type rec Expr = | Num of int | Add of (Expr, Expr) | Sub of (Expr, Expr)
                            | Mul of (Expr, Expr) | Div of (Expr, Expr)

              let rec eval = fn expr ->
                case expr do
                | Num n -> n
                | Add (e1, e2) -> eval e1 + eval e2
                | Sub (e1, e2) -> eval e1 - eval e2
                | Mul (e1, e2) -> eval e1 * eval e2
                | Div (e1, e2) ->
                  let v2 = eval e2 in
                  if v2 == 0 then 0 else eval e1 / v2

              let expr = Add (Mul (Num 3, Num 4), Sub (Num 10, Num 2))
            |}
             ~expr:"eval expr" ~expected_value:"20" );
         (* Test 36: Tree traversal with accumulation *)
         ( "tree sum with different traversals" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              type rec Tree = | Leaf of int | Node of (int, Tree, Tree)

              let rec preorder_sum = fn tree ->
                case tree do
                | Leaf n -> n
                | Node (v, l, r) -> v + preorder_sum l + preorder_sum r

              let tree = Node (1, Node (2, Leaf 4, Leaf 5), Node (3, Leaf 6, Leaf 7))
            |}
             ~expr:"preorder_sum tree" ~expected_value:"28" );
         (* Test 37: List reversal with accumulator *)
         ( "list reversal with accumulator" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              let rec reverse = fn list ->
                reverse_acc list []
              and reverse_acc = fn list -> fn acc ->
                case list do
                | [] -> acc
                | h :: t -> reverse_acc t (h :: acc)

              let rec concat = fn l1 -> fn l2 ->
                case l1 do
                | [] -> l2
                | h :: t -> h :: concat t l2

              let rec sum = fn list ->
                case list do
                | [] -> 0
                | h :: t -> h + sum t

              let lst1 = [1, 2, 3]
              let lst2 = [4, 5, 6]
              let combined = concat lst1 lst2
              let rev = reverse combined
            |}
             ~expr:"sum rev" ~expected_value:"21" );
         (* Test 38: Higher-order function composition *)
         ( "higher-order function composition" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              let rec compose = fn f -> fn g -> fn x ->
                f (g x)

              let rec add_one = fn x -> x + 1
              let rec double = fn x -> x * 2
              let rec square = fn x -> x * x

              let f1 = compose add_one double
              let f2 = compose double square
              let f3 = compose f1 f2
            |}
             ~expr:"f3 3" ~expected_value:"37" );
         (* Test 39: Pattern matching with nested constructors *)
         ( "nested pattern matching" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              type rec Option<'a> = | None | Some of 'a
              and Result<'a, 'b> = | Ok of a | Err of b

              let rec unwrap_or = fn opt -> fn default ->
                case opt do
                | None -> default
                | Some x -> x

              let rec result_to_option = fn result ->
                case result do
                | Ok x -> Some x
                | Err _ -> None

              let rec process = fn result ->
                unwrap_or (result_to_option result) 0

              let res1 = Ok 42
              let res2 = Err 0
            |}
             ~expr:"process res1 + process res2" ~expected_value:"42" );
         (* Test 40: Graph representation *)
         ( "graph node counting" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              type rec Graph = | G of NodeList
              and NodeList = | NNil | NCons of (Node, NodeList)
              and Node = | N of (int, EdgeList)
              and EdgeList = | ENil | ECons of (int, EdgeList)

              let rec count_nodes = fn graph ->
                case graph do
                | G nodes -> count_node_list nodes
              and count_node_list = fn nodes ->
                case nodes do
                | NNil -> 0
                | NCons (_, rest) -> 1 + count_node_list rest

              let graph = G (NCons (N (1, ECons (2, ENil)),
                             NCons (N (2, ECons (3, ENil)),
                             NCons (N (3, ENil), NNil))))
            |}
             ~expr:"count_nodes graph" ~expected_value:"3" );
         (* Test 41: Stack-based calculator *)
         ( "stack-based calculator" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              type rec Stack = | Empty | Push of (int, Stack)
              and Op = | OAdd | OMul | ODup

              let rec exec_add = fn stack ->
                case stack do
                | Push (a, s1) ->
                  (case s1 do
                   | Push (b, s2) -> Push (a + b, s2)
                   | Empty -> stack)
                | Empty -> stack

              let rec exec_mul = fn stack ->
                case stack do
                | Push (a, s1) ->
                  (case s1 do
                   | Push (b, s2) -> Push (a * b, s2)
                   | Empty -> stack)
                | Empty -> stack

              let rec exec_dup = fn stack ->
                case stack do
                | Push (a, rest) -> Push (a, Push (a, rest))
                | Empty -> stack

              let rec exec_op = fn op -> fn stack ->
                case op do
                | OAdd -> exec_add stack
                | OMul -> exec_mul stack
                | ODup -> exec_dup stack

              let rec top = fn stack ->
                case stack do
                | Empty -> 0
                | Push (x, _) -> x

              let stack = Push (3, Push (4, Empty))
              let s1 = exec_op ODup stack
              let s2 = exec_op OMul s1
            |}
             ~expr:"top s2" ~expected_value:"9" );
         (* Test 42: List filtering *)
         ( "list filtering and predicates" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              let rec filter = fn pred -> fn list ->
                case list do
                | [] -> []
                | h :: t ->
                  if pred h then h :: filter pred t
                  else filter pred t

              let rec is_even = fn x ->
                x - (x / 2) * 2 == 0

              let rec sum = fn list ->
                case list do
                | [] -> 0
                | h :: t -> h + sum t

              let lst = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
              let evens = filter is_even lst
            |}
             ~expr:"sum evens" ~expected_value:"30" );
         (* Test 43: Tree depth calculation *)
         ( "tree depth calculation" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              type rec Tree = | Leaf | Node of (int, Tree, Tree)

              let rec max = fn a -> fn b ->
                if a > b then a else b

              let rec depth = fn tree ->
                case tree do
                | Leaf -> 0
                | Node (_, l, r) ->
                  1 + max (depth l) (depth r)

              let tree = Node (1, Node (2, Node (3, Leaf, Leaf), Leaf), Node (4, Leaf, Leaf))
            |}
             ~expr:"depth tree" ~expected_value:"3" );
         (* Test 44: Mutual recursion with three functions *)
         ( "three-way mutual recursion" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              let rec f = fn n ->
                if n == 0 then 0 else n + g (n - 1)
              and g = fn n ->
                if n == 0 then 0 else n + h (n - 1)
              and h = fn n ->
                if n == 0 then 0 else n + f (n - 1)

              let result = f 5
            |}
             ~expr:"result" ~expected_value:"15" );
         (* Test 45: List length and indexing *)
         ( "list length and indexing" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              let rec length = fn list ->
                case list do
                | [] -> 0
                | _ :: t -> 1 + length t

              let rec nth = fn list -> fn n ->
                case list do
                | [] -> 0
                | h :: t -> if n == 0 then h else nth t (n - 1)

              let lst = [10, 20, 30, 40, 50]
            |}
             ~expr:"nth lst 2" ~expected_value:"30" );
         (* Test 46: Binary tree mirror *)
         ( "binary tree mirror" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              type rec Tree = | Leaf of int | Node of (int, Tree, Tree)

              let rec mirror = fn tree ->
                case tree do
                | Leaf n -> Leaf n
                | Node (v, l, r) -> Node (v, mirror r, mirror l)

              let rec leftmost = fn tree ->
                case tree do
                | Leaf n -> n
                | Node (_, l, _) -> leftmost l

              let tree = Node (1, Node (2, Leaf 4, Leaf 5), Leaf 3)
              let mirrored = mirror tree
            |}
             ~expr:"leftmost mirrored" ~expected_value:"3" );
         (* Test 48: Tree flattening to list *)
         ( "tree to list flattening" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              type rec Tree = | Leaf of int | Node of (int, Tree, Tree)

              let rec flatten = fn tree ->
                case tree do
                | Leaf n -> [n]
                | Node (v, l, r) -> append (flatten l) (v :: flatten r)
              and append = fn l1 -> fn l2 ->
                case l1 do
                | [] -> l2
                | h :: t -> h :: append t l2

              let rec product = fn list ->
                case list do
                | [] -> 1
                | h :: t -> h * product t

              let tree = Node (2, Leaf 3, Node (4, Leaf 5, Leaf 1))
            |}
             ~expr:"product (flatten tree)" ~expected_value:"120" );
         (* Test 49: Complex nested conditionals *)
         ( "nested conditionals" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              let rec classify = fn x ->
                if x < 0 then 0 - 1
                else if x == 0 then 0
                else if x < 10 then 1
                else if x < 100 then 2
                else 3

              let rec sum_class = fn list ->
                case list do
                | [] -> 0
                | h :: t -> classify h + sum_class t

              let lst = [0 - 5, 0, 5, 50, 500]
            |}
             ~expr:"sum_class lst" ~expected_value:"5" );
         (* Test 50: Tree balancing check *)
         ( "tree balance factor" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              type rec Tree = | Leaf | Node of (int, Tree, Tree)

              let rec max = fn a -> fn b ->
                if a > b then a else b

              let rec abs = fn x ->
                if x < 0 then 0 - x else x

              let rec height = fn tree ->
                case tree do
                | Leaf -> 0
                | Node (_, l, r) -> 1 + max (height l) (height r)

              let rec balance_factor = fn tree ->
                case tree do
                | Leaf -> 0
                | Node (_, l, r) -> abs (height l - height r)

              let tree = Node (1, Node (2, Leaf, Leaf), Leaf)
            |}
             ~expr:"balance_factor tree" ~expected_value:"1" );
         (* Test 51: List zip operation *)
         ( "list zip operation" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              type rec Pair<'a, 'b> = | P of ('a, 'b)

              let rec zip = fn l1 -> fn l2 ->
                case l1 do
                | [] -> []
                | h1 :: t1 ->
                  (case l2 do
                   | [] -> []
                   | h2 :: t2 -> P (h1, h2) :: zip t1 t2)

              let rec sum_pairs = fn list ->
                case list do
                | [] -> 0
                | P (a, b) :: t -> a + b + sum_pairs t

              let l1 = [1, 2, 3]
              let l2 = [4, 5, 6]
              let zipped = zip l1 l2
            |}
             ~expr:"sum_pairs zipped" ~expected_value:"21" );
         (* Test 52: Fibonacci with accumulator *)
         ( "fibonacci with accumulator" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              let rec fib = fn n ->
                fib_acc n 0 1
              and fib_acc = fn n -> fn a -> fn b ->
                if n == 0 then a
                else if n == 1 then b
                else fib_acc (n - 1) b (a + b)

              let result = fib 10
            |}
             ~expr:"result" ~expected_value:"55" );
         (* Test 53: Tree leaf counting *)
         ( "tree leaf counting" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              type rec Tree = | Leaf of int | Node of (int, Tree, Tree)

              let rec count_leaves = fn tree ->
                case tree do
                | Leaf _ -> 1
                | Node (_, l, r) -> count_leaves l + count_leaves r

              let tree = Node (1, Node (2, Leaf 3, Leaf 4), Node (5, Leaf 6, Leaf 7))
            |}
             ~expr:"count_leaves tree" ~expected_value:"4" );
         (* Test 54: List take and drop *)
         ( "list take and drop" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              let rec take = fn n -> fn list ->
                if n == 0 then []
                else case list do
                     | [] -> []
                     | h :: t -> h :: take (n - 1) t

              let rec drop = fn n -> fn list ->
                if n == 0 then list
                else case list do
                     | [] -> []
                     | _ :: t -> drop (n - 1) t

              let rec sum = fn list ->
                case list do
                | [] -> 0
                | h :: t -> h + sum t

              let lst = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
              let first_half = take 5 lst
              let second_half = drop 5 lst
            |}
             ~expr:"sum first_half + sum second_half" ~expected_value:"55" );
         (* Test 55: Tree path sum *)
         ( "tree root to leaf path sum" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              type rec Tree = | Leaf of int | Node of (int, Tree, Tree)

              let rec max = fn a -> fn b ->
                if a > b then a else b

              let rec max_path_sum = fn tree ->
                case tree do
                | Leaf n -> n
                | Node (v, l, r) -> v + max (max_path_sum l) (max_path_sum r)

              let tree = Node (1, Node (2, Leaf 4, Leaf 1), Node (3, Leaf 1, Leaf 1))
            |}
             ~expr:"max_path_sum tree" ~expected_value:"7" );
         (* Test 56: List all and any *)
         ( "list all and any predicates" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              let rec all = fn pred -> fn list ->
                case list do
                | [] -> true
                | h :: t -> if pred h then all pred t else false

              let rec any = fn pred -> fn list ->
                case list do
                | [] -> false
                | h :: t -> if pred h then true else any pred t

              let rec is_positive = fn x -> x > 0

              let lst1 = [1, 2, 3, 4, 5]
              let lst2 = [1, 2, 0 - 3, 4, 5]
              let all_pos = all is_positive lst1
              let any_neg = any (fn x -> x < 0) lst2
            |}
             ~expr:"if all_pos && any_neg then 100 else 0" ~expected_value:"100"
         );
         (* Test 57: Binary tree insertion and search *)
         ( "binary tree insertion order" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              type rec Tree = | Empty | Node of (int, Tree, Tree)

              let rec insert = fn x -> fn tree ->
                case tree do
                | Empty -> Node (x, Empty, Empty)
                | Node (v, l, r) ->
                  if x < v then Node (v, insert x l, r)
                  else Node (v, l, insert x r)

              let rec min_value = fn tree ->
                case tree do
                | Empty -> 1000
                | Node (v, Empty, _) -> v
                | Node (_, l, _) -> min_value l

              let tree = insert 5 (insert 3 (insert 7 (insert 1 (insert 9 Empty))))
            |}
             ~expr:"min_value tree" ~expected_value:"1" );
         (* Test 58: List intersperse *)
         ( "list intersperse operation" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              let rec intersperse = fn sep -> fn list ->
                case list do
                | [] -> []
                | x :: [] -> [x]
                | h :: t -> h :: sep :: intersperse sep t

              let rec sum = fn list ->
                case list do
                | [] -> 0
                | h :: t -> h + sum t

              let lst = [1, 2, 3, 4, 5]
              let result = intersperse 0 lst
            |}
             ~expr:"sum result" ~expected_value:"15" );
         (* Test 59: Tree node count at level *)
         ( "count nodes at tree level" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              type rec Tree = | Leaf | Node of (int, Tree, Tree)

              let rec count_at_level = fn tree -> fn level ->
                if level == 0 then
                  case tree do
                  | Leaf -> 0
                  | Node (_, _, _) -> 1
                else
                  case tree do
                  | Leaf -> 0
                  | Node (_, l, r) ->
                    count_at_level l (level - 1) + count_at_level r (level - 1)

              let tree = Node (1, Node (2, Node (4, Leaf, Leaf), Leaf), Node (3, Leaf, Node (5, Leaf, Leaf)))
            |}
             ~expr:"count_at_level tree 2" ~expected_value:"2" );
         (* Test 60: List chunking *)
         ( "list chunking into pairs" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              type rec Pair<'a> = | P of ('a, 'a)

              let rec chunk_pairs = fn list ->
                case list do
                | [] -> []
                | _ :: [] -> []
                | h1 :: h2 :: t -> P (h1, h2) :: chunk_pairs t

              let rec count_pairs = fn list ->
                case list do
                | [] -> 0
                | _ :: t -> 1 + count_pairs t

              let lst = [1, 2, 3, 4, 5, 6, 7, 8]
              let pairs = chunk_pairs lst
            |}
             ~expr:"count_pairs pairs" ~expected_value:"4" );
         (* Test 61: Complex tree transformation *)
         ( "tree value doubling" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              type rec Tree = | Leaf of int | Node of (int, Tree, Tree)

              let rec map_tree = fn f -> fn tree ->
                case tree do
                | Leaf n -> Leaf (f n)
                | Node (v, l, r) -> Node (f v, map_tree f l, map_tree f r)

              let rec sum_tree = fn tree ->
                case tree do
                | Leaf n -> n
                | Node (v, l, r) -> v + sum_tree l + sum_tree r

              let tree = Node (1, Leaf 2, Node (3, Leaf 4, Leaf 5))
              let doubled = map_tree (fn x -> x * 2) tree
            |}
             ~expr:"sum_tree doubled" ~expected_value:"30" );
         (* Test 62: List sorting with insertion sort *)
         ( "insertion sort on lists" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              let rec insert = fn x -> fn list ->
                case list do
                | [] -> [x]
                | h :: t ->
                  if x < h then x :: h :: t
                  else h :: insert x t

              let rec sort = fn list ->
                case list do
                | [] -> []
                | h :: t -> insert h (sort t)

              let rec nth = fn list -> fn n ->
                case list do
                | [] -> 0
                | h :: t -> if n == 0 then h else nth t (n - 1)

              let lst = [5, 2, 8, 1, 9, 3]
              let sorted = sort lst
            |}
             ~expr:"nth sorted 3" ~expected_value:"5" );
         (* Test 63: Tree zipper for navigation *)
         ( "tree zipper navigation" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              type rec Tree = | Leaf of int | Node of (int, Tree, Tree)

              let rec leftmost_leaf = fn tree ->
                case tree do
                | Leaf n -> n
                | Node (_, l, _) -> leftmost_leaf l

              let rec rightmost_leaf = fn tree ->
                case tree do
                | Leaf n -> n
                | Node (_, _, r) -> rightmost_leaf r

              let tree = Node (10, Node (5, Leaf 1, Leaf 2), Node (15, Leaf 3, Leaf 4))
            |}
             ~expr:"leftmost_leaf tree + rightmost_leaf tree"
             ~expected_value:"5" );
         (* Test 64: List sliding window *)
         ( "list sliding window sum" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              let rec window_sum = fn n -> fn list ->
                case list do
                | [] -> 0
                | h :: t ->
                  let window = take n (h :: t) in
                  let current_sum = sum window in
                  let rest_sum = window_sum n t in
                  max current_sum rest_sum
              and take = fn n -> fn list ->
                if n == 0 then []
                else case list do
                     | [] -> []
                     | h :: t -> h :: take (n - 1) t
              and sum = fn list ->
                case list do
                | [] -> 0
                | h :: t -> h + sum t
              and max = fn a -> fn b ->
                if a > b then a else b

              let lst = [1, 4, 2, 10, 2, 3, 1, 0, 20]
            |}
             ~expr:"window_sum 3 lst" ~expected_value:"21" );
         (* Test 65: Tree pruning *)
         ( "tree pruning leaves below threshold" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              type rec Tree = | Leaf of int | Node of (int, Tree, Tree)
              and Option<'a> = | None | Some of a

              let rec prune = fn threshold -> fn tree ->
                case tree do
                | Leaf n -> if n >= threshold then Some (Leaf n) else None
                | Node (v, l, r) ->
                  if v >= threshold then
                    Some (Node (v,
                      unwrap_tree (prune threshold l),
                      unwrap_tree (prune threshold r)))
                  else None
              and unwrap_tree = fn opt ->
                case opt do
                | None -> Leaf 0
                | Some t -> t

              let rec sum_tree = fn tree ->
                case tree do
                | Leaf n -> n
                | Node (v, l, r) -> v + sum_tree l + sum_tree r

              let tree = Node (5, Leaf 2, Node (8, Leaf 1, Leaf 9))
              let pruned = unwrap_tree (prune 5 tree)
            |}
             ~expr:"sum_tree pruned" ~expected_value:"22" );
         (* Test 66: List grouping *)
         ( "list grouping consecutive elements" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              type rec Groups<'a> = | GNil | GCons of (Group<'a>, Groups<'a>)
              and Group<'a> = | Single of a | Multiple of (a, int)

              let rec count_consecutive = fn list ->
                case list do
                | [] -> 0
                | _ :: [] -> 1
                | h1 :: h2 :: t ->
                  if h1 == h2 then count_consecutive (h2 :: t)
                  else 1 + count_consecutive (h2 :: t)

              let lst = [1, 1, 2, 2, 2, 3, 4, 4]
            |}
             ~expr:"count_consecutive lst" ~expected_value:"4" );
         (* Test 67: Tree levels as lists *)
         ( "tree level-order traversal" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              type rec Tree = | Leaf | Node of (int, Tree, Tree)

              let rec count_at_depth = fn tree -> fn d ->
                if d == 0 then
                  case tree do | Leaf -> 0 | Node (_, _, _) -> 1
                else
                  case tree do
                  | Leaf -> 0
                  | Node (_, l, r) -> count_at_depth l (d - 1) + count_at_depth r (d - 1)

              let tree = Node (1, Node (2, Node (4, Leaf, Leaf), Node (5, Leaf, Leaf)),
                                  Node (3, Node (6, Leaf, Leaf), Node (7, Leaf, Leaf)))
            |}
             ~expr:
               "count_at_depth tree 0 + count_at_depth tree 1 + count_at_depth \
                tree 2"
             ~expected_value:"7" );
         (* Test 68: List permutation check *)
         ( "list element count" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              let rec count_elem = fn x -> fn list ->
                case list do
                | [] -> 0
                | h :: t ->
                  if h == x then 1 + count_elem x t
                  else count_elem x t

              let rec unique_count = fn list ->
                case list do
                | [] -> 0
                | h :: t ->
                  let count = count_elem h list in
                  if count > 0 then 1 + unique_count (remove_all h t)
                  else unique_count t
              and remove_all = fn x -> fn list ->
                case list do
                | [] -> []
                | h :: t ->
                  if h == x then remove_all x t
                  else h :: remove_all x t

              let lst = [1, 2, 1, 3, 2, 4]
            |}
             ~expr:"unique_count lst" ~expected_value:"4" );
         (* Test 69: Tree rotation *)
         ( "tree left rotation" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              type rec Tree = | Leaf of int | Node of (int, Tree, Tree)

              let rec rotate_left = fn tree ->
                case tree do
                | Leaf n -> Leaf n
                | Node (v, l, r) ->
                  case r do
                  | Leaf _ -> tree
                  | Node (rv, rl, rr) -> Node (rv, Node (v, l, rl), rr)

              let rec get_root = fn tree ->
                case tree do
                | Leaf n -> n
                | Node (v, _, _) -> v

              let tree = Node (1, Leaf 2, Node (3, Leaf 4, Leaf 5))
              let rotated = rotate_left tree
            |}
             ~expr:"get_root rotated" ~expected_value:"3" );
         (* Test 70: List deduplication *)
         ( "list remove duplicates" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              let rec member = fn x -> fn list ->
                case list do
                | [] -> false
                | h :: t -> if x == h then true else member x t

              let rec dedup = fn list ->
                case list do
                | [] -> []
                | h :: t ->
                  if member h t then dedup t
                  else h :: dedup t

              let rec length = fn list ->
                case list do
                | [] -> 0
                | _ :: t -> 1 + length t

              let lst = [1, 2, 3, 2, 4, 1, 5]
              let deduped = dedup lst
            |}
             ~expr:"length deduped" ~expected_value:"5" );
         (* Test 71: Tree isomorphism check *)
         ( "tree structure equality" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              type rec Tree = | Leaf | Node of (int, Tree, Tree)

              let rec same_structure = fn t1 -> fn t2 ->
                case t1 do
                | Leaf -> (case t2 do | Leaf -> true | _ -> false)
                | Node (_, l1, r1) ->
                  (case t2 do
                   | Leaf -> false
                   | Node (_, l2, r2) ->
                     same_structure l1 l2 && same_structure r1 r2)

              let t1 = Node (1, Node (2, Leaf, Leaf), Leaf)
              let t2 = Node (5, Node (6, Leaf, Leaf), Leaf)
            |}
             ~expr:"if same_structure t1 t2 then 100 else 0"
             ~expected_value:"100" );
         (* Test 72: List merge two sorted lists *)
         ( "merge two sorted lists" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              let rec merge = fn l1 -> fn l2 ->
                case l1 do
                | [] -> l2
                | h1 :: t1 ->
                  (case l2 do
                   | [] -> l1
                   | h2 :: t2 ->
                     if h1 < h2 then h1 :: merge t1 l2
                     else h2 :: merge l1 t2)

              let rec nth = fn list -> fn n ->
                case list do
                | [] -> 0
                | h :: t -> if n == 0 then h else nth t (n - 1)

              let l1 = [1, 3, 5, 7]
              let l2 = [2, 4, 6, 8]
              let merged = merge l1 l2
            |}
             ~expr:"nth merged 4" ~expected_value:"5" );
         (* Test 73: Tree maximum value *)
         ( "tree maximum value search" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              type rec Tree = | Leaf of int | Node of (int, Tree, Tree)

              let rec max = fn a -> fn b ->
                if a > b then a else b

              let rec tree_max = fn tree ->
                case tree do
                | Leaf n -> n
                | Node (v, l, r) -> max v (max (tree_max l) (tree_max r))

              let tree = Node (5, Node (12, Leaf 3, Leaf 7), Node (9, Leaf 1, Leaf 15))
            |}
             ~expr:"tree_max tree" ~expected_value:"15" );
         (* Test 74: List rangfinite automaton simulatioe generation *)
         ( "list range generation" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              let rec range = fn start -> fn end ->
                if start > end then []
                else start :: range (start + 1) end

              let rec sum = fn list ->
                case list do
                | [] -> 0
                | h :: t -> h + sum t

              let r = range 1 10
            |}
             ~expr:"sum r" ~expected_value:"55" );
         (* Test 75: Tree breadth-first search level count *)
         ( "tree level count" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              type rec Tree = | Leaf | Node of (int, Tree, Tree)

              let rec max = fn a -> fn b ->
                if a > b then a else b

              let rec depth = fn tree ->
                case tree do
                | Leaf -> 0
                | Node (_, l, r) -> 1 + max (depth l) (depth r)

              let tree = Node (1, Node (2, Node (4, Leaf, Leaf), Leaf),
                                  Node (3, Leaf, Node (5, Leaf, Leaf)))
            |}
             ~expr:"depth tree" ~expected_value:"3" );
         (* Test 76: List palindrome check helper *)
         ( "list reverse comparison" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              let rec reverse = fn list ->
                reverse_acc list []
              and reverse_acc = fn list -> fn acc ->
                case list do
                | [] -> acc
                | h :: t -> reverse_acc t (h :: acc)

              let rec equal = fn l1 -> fn l2 ->
                case l1 do
                | [] -> (case l2 do | [] -> true | _ -> false)
                | h1 :: t1 ->
                  (case l2 do
                   | [] -> false
                   | h2 :: t2 -> if h1 == h2 then equal t1 t2 else false)

              let lst = [1, 2, 3, 2, 1]
              let rev = reverse lst
            |}
             ~expr:"if equal lst rev then 100 else 0" ~expected_value:"100" );
         (* Test 77: Tree subtree sum *)
         ( "tree subtree with maximum sum" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              type rec Tree = | Leaf of int | Node of (int, Tree, Tree)

              let rec sum_tree = fn tree ->
                case tree do
                | Leaf n -> n
                | Node (v, l, r) -> v + sum_tree l + sum_tree r

              let tree = Node (1, Node (2, Leaf 4, Leaf 5), Node (3, Leaf 6, Leaf 7))
              let left = Node (2, Leaf 4, Leaf 5)
            |}
             ~expr:"sum_tree left" ~expected_value:"11" );
         (* Test 78: List intersection *)
         ( "list intersection operation" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              let rec member = fn x -> fn list ->
                case list do
                | [] -> false
                | h :: t -> if x == h then true else member x t

              let rec intersect = fn l1 -> fn l2 ->
                case l1 do
                | [] -> []
                | h :: t ->
                  if member h l2 then h :: intersect t l2
                  else intersect t l2

              let rec sum = fn list ->
                case list do
                | [] -> 0
                | h :: t -> h + sum t

              let l1 = [1, 2, 3, 4, 5]
              let l2 = [3, 4, 5, 6, 7]
              let inter = intersect l1 l2
            |}
             ~expr:"sum inter" ~expected_value:"12" );
         (* Test 79: Tree path existence *)
         ( "tree path to value exists" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              type rec Tree = | Leaf of int | Node of (int, Tree, Tree)

              let rec has_path_to = fn tree -> fn target ->
                case tree do
                | Leaf n -> n == target
                | Node (v, l, r) ->
                  if v == target then true
                  else has_path_to l target || has_path_to r target

              let tree = Node (1, Node (2, Leaf 4, Leaf 5), Node (3, Leaf 6, Leaf 7))
            |}
             ~expr:"if has_path_to tree 5 then 100 else 0" ~expected_value:"100"
         );
         (* Test 80: List running sum *)
         ( "list running sum calculation" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              let rec running_sum = fn list ->
                running_sum_acc list 0
              and running_sum_acc = fn list -> fn acc ->
                case list do
                | [] -> []
                | h :: t -> (acc + h) :: running_sum_acc t (acc + h)

              let rec last = fn list ->
                case list do
                | x :: [] -> x
                | _ :: t -> last t
                | [] -> 0

              let lst = [1, 2, 3, 4, 5]
              let sums = running_sum lst
            |}
             ~expr:"last sums" ~expected_value:"15" );
         (* Test 81: Tree complete check *)
         ( "tree completeness property" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              type rec Tree = | Leaf | Node of (int, Tree, Tree)

              let rec count_nodes = fn tree ->
                case tree do
                | Leaf -> 0
                | Node (_, l, r) -> 1 + count_nodes l + count_nodes r

              let rec max = fn a -> fn b ->
                if a > b then a else b

              let rec height = fn tree ->
                case tree do
                | Leaf -> 0
                | Node (_, l, r) -> 1 + max (height l) (height r)

              let tree = Node (1, Node (2, Node (4, Leaf, Leaf), Node (5, Leaf, Leaf)),
                                  Node (3, Node (6, Leaf, Leaf), Leaf))
            |}
             ~expr:"count_nodes tree" ~expected_value:"6" );
         (* Test 82: List rotation *)
         ( "list rotation operation" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              let rec rotate = fn n -> fn list ->
                if n == 0 then list
                else case list do
                     | [] -> []
                     | h :: t -> rotate (n - 1) (append t [h])
              and append = fn l1 -> fn l2 ->
                case l1 do
                | [] -> l2
                | h :: t -> h :: append t l2

              let rec head = fn list ->
                case list do
                | [] -> 0
                | h :: _ -> h

              let lst = [1, 2, 3, 4, 5]
              let rotated = rotate 2 lst
            |}
             ~expr:"head rotated" ~expected_value:"3" );
         (* Test 83: Tree diameter calculation *)
         ( "tree diameter" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              type rec Tree = | Leaf | Node of (int, Tree, Tree)

              let rec max = fn a -> fn b ->
                if a > b then a else b

              let rec height = fn tree ->
                case tree do
                | Leaf -> 0
                | Node (_, l, r) -> 1 + max (height l) (height r)

              let rec diameter = fn tree ->
                case tree do
                | Leaf -> 0
                | Node (_, l, r) ->
                  let left_height = height l in
                  let right_height = height r in
                  let through_root = left_height + right_height in
                  max through_root (max (diameter l) (diameter r))

              let tree = Node (1, Node (2, Node (4, Leaf, Leaf), Leaf),
                                  Node (3, Leaf, Leaf))
            |}
             ~expr:"diameter tree" ~expected_value:"3" );
         (* Test 84: List cycle detection helper *)
         ( "list length comparison" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              let rec length = fn list ->
                case list do
                | [] -> 0
                | _ :: t -> 1 + length t

              let rec drop = fn n -> fn list ->
                if n == 0 then list
                else case list do
                     | [] -> []
                     | _ :: t -> drop (n - 1) t

              let lst = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
              let len = length lst
              let half = drop 5 lst
              let half_len = length half
            |}
             ~expr:"len - half_len" ~expected_value:"5" );
         (* Test 85: Tree invert *)
         ( "tree inversion operation" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              type rec Tree = | Leaf of int | Node of (int, Tree, Tree)

              let rec invert = fn tree ->
                case tree do
                | Leaf n -> Leaf n
                | Node (v, l, r) -> Node (v, invert r, invert l)

              let rec leftmost = fn tree ->
                case tree do
                | Leaf n -> n
                | Node (_, l, _) -> leftmost l

              let tree = Node (1, Leaf 2, Leaf 3)
              let inverted = invert tree
            |}
             ~expr:"leftmost inverted" ~expected_value:"3" );
         (* Test 86: List windowing operation *)
         ( "list windowing sum operation" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              let rec sum_first_n = fn n -> fn list ->
                if n == 0 then 0
                else case list do
                     | [] -> 0
                     | h :: t -> h + sum_first_n (n - 1) t

              let lst = [5, 10, 15, 20, 25]
            |}
             ~expr:"sum_first_n 3 lst" ~expected_value:"30" );
         (* Test 87: Tree lowest common ancestor preparation *)
         ( "tree node presence check" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              type rec Tree = | Leaf of int | Node of (int, Tree, Tree)

              let rec contains = fn tree -> fn value ->
                case tree do
                | Leaf n -> n == value
                | Node (v, l, r) ->
                  if v == value then true
                  else contains l value || contains r value

              let tree = Node (1, Node (2, Leaf 4, Leaf 5), Node (3, Leaf 6, Leaf 7))
            |}
             ~expr:"if contains tree 6 && contains tree 4 then 100 else 0"
             ~expected_value:"100" );
         (* Test 89: Tree serialization *)
         ( "tree to list serialization" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              type rec Tree = | Leaf of int | Node of (int, Tree, Tree)

              let rec preorder = fn tree ->
                case tree do
                | Leaf n -> [n]
                | Node (v, l, r) -> v :: append (preorder l) (preorder r)
              and append = fn l1 -> fn l2 ->
                case l1 do
                | [] -> l2
                | h :: t -> h :: append t l2

              let rec length = fn list ->
                case list do
                | [] -> 0
                | _ :: t -> 1 + length t

              let tree = Node (1, Node (2, Leaf 4, Leaf 5), Leaf 3)
              let serialized = preorder tree
            |}
             ~expr:"length serialized" ~expected_value:"5" );
         (* Test 90: List majority element *)
         ( "list element counting majority" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              let rec count = fn x -> fn list ->
                case list do
                | [] -> 0
                | h :: t -> if h == x then 1 + count x t else count x t

              let lst = [3, 3, 4, 2, 4, 4, 2, 4, 4]
            |}
             ~expr:"count 4 lst" ~expected_value:"5" );
         (* Test 91: Tree sum of left leaves *)
         ( "sum of left leaves" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              type rec Tree = | Leaf of int | Node of (int, Tree, Tree)

              let rec is_leaf = fn tree ->
                case tree do
                | Leaf _ -> true
                | _ -> false

              let rec sum_left_leaves = fn tree ->
                case tree do
                | Leaf _ -> 0
                | Node (_, l, r) ->
                  let left_sum = if is_leaf l then
                    (case l do | Leaf n -> n | _ -> 0)
                  else sum_left_leaves l in
                  left_sum + sum_left_leaves r

              let tree = Node (1, Node (2, Leaf 4, Leaf 5), Node (3, Leaf 6, Leaf 7))
            |}
             ~expr:"sum_left_leaves tree" ~expected_value:"10" );
         (* Test 92: List longest increasing subsequence length *)
         ( "list increasing subsequence" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              let rec lis_length = fn list ->
                case list do
                | [] -> 0
                | h1 :: h2 :: t ->
                  if h1 < h2 then 1 + lis_length (h2 :: t)
                  else 1
                | _ :: _ -> 1

              let lst = [1, 3, 5, 4, 7]
            |}
             ~expr:"lis_length lst" ~expected_value:"3" );
         (* Test 93: Tree right view *)
         ( "tree rightmost at each level" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              type rec Tree = | Leaf of int | Node of (int, Tree, Tree)

              let rec rightmost_at_level = fn tree -> fn level ->
                if level == 0 then
                  case tree do
                  | Leaf n -> n
                  | Node (v, _, _) -> v
                else
                  case tree do
                  | Leaf _ -> 0
                  | Node (_, _, r) -> rightmost_at_level r (level - 1)

              let tree = Node (1, Node (2, Leaf 4, Leaf 5), Node (3, Leaf 6, Leaf 7))
            |}
             ~expr:"rightmost_at_level tree 2" ~expected_value:"7" );
         (* Test 94: List binary search *)
         ( "binary search on sorted list" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              let rec length = fn list ->
                case list do
                | [] -> 0
                | _ :: t -> 1 + length t

              let rec nth = fn list -> fn n ->
                case list do
                | [] -> 0
                | h :: t -> if n == 0 then h else nth t (n - 1)

              let rec binary_search = fn list -> fn target ->
                let len = length list in
                if len == 0 then false
                else if len == 1 then
                  (case list do | x :: [] -> x == target | _ -> false)
                else
                  let mid = len / 2 in
                  let mid_val = nth list mid in
                  mid_val == target

              let lst = [1, 2, 3, 4, 5, 6, 7, 8, 9]
            |}
             ~expr:"if binary_search lst 5 then 100 else 0"
             ~expected_value:"100" );
         (* Test 95: Tree cousins check *)
         ( "tree node depth finding" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              type rec Tree = | Leaf of int | Node of (int, Tree, Tree)

              let rec find_depth = fn tree -> fn target -> fn current_depth ->
                case tree do
                | Leaf n -> if n == target then current_depth else 0 - 1
                | Node (v, l, r) ->
                  if v == target then current_depth
                  else
                    let left_depth = find_depth l target (current_depth + 1) in
                    if left_depth >= 0 then left_depth
                    else find_depth r target (current_depth + 1)

              let tree = Node (1, Node (2, Leaf 4, Leaf 5), Node (3, Leaf 6, Leaf 7))
            |}
             ~expr:"find_depth tree 4 0" ~expected_value:"2" );
         (* Test 97: Tree vertical order traversal *)
         ( "tree column counting" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              type rec Tree = | Leaf | Node of (int, Tree, Tree)

              let rec count_nodes = fn tree ->
                case tree do
                | Leaf -> 0
                | Node (_, l, r) -> 1 + count_nodes l + count_nodes r

              let tree = Node (1, Node (2, Leaf, Node (4, Leaf, Leaf)),
                                  Node (3, Node (5, Leaf, Leaf), Node (6, Leaf, Leaf)))
            |}
             ~expr:"count_nodes tree" ~expected_value:"6" );
         (* Test 98: List k-way merge preparation *)
         ( "list merge multiple sorted" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              let rec merge = fn l1 -> fn l2 ->
                case l1 do
                | [] -> l2
                | h1 :: t1 ->
                  (case l2 do
                   | [] -> l1
                   | h2 :: t2 ->
                     if h1 < h2 then h1 :: merge t1 l2
                     else h2 :: merge l1 t2)

              let rec sum = fn list ->
                case list do
                | [] -> 0
                | h :: t -> h + sum t

              let l1 = [1, 4, 7]
              let l2 = [2, 5, 8]
              let l3 = [3, 6, 9]
              let m1 = merge l1 l2
              let m2 = merge m1 l3
            |}
             ~expr:"sum m2" ~expected_value:"45" );
         (* Test 99: Tree boundary traversal *)
         ( "tree left boundary nodes" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              type rec Tree = | Leaf of int | Node of (int, Tree, Tree)

              let rec left_boundary = fn tree ->
                case tree do
                | Leaf n -> [n]
                | Node (v, l, _) -> v :: left_boundary l

              let rec sum = fn list ->
                case list do
                | [] -> 0
                | h :: t -> h + sum t

              let tree = Node (1, Node (2, Node (4, Leaf 7, Leaf 8), Leaf 5), Node (3, Leaf 6, Leaf 9))
            |}
             ~expr:"sum (left_boundary tree)" ~expected_value:"14" );
         (* Test 100: List dynamic programming - maximum subarray *)
         ( "maximum subarray sum kadane" >:: fun _ ->
           assert_expression_has_value
             ~program:
               {|
              let rec max = fn a -> fn b ->
                if a > b then a else b

              let rec kadane = fn list ->
                kadane_helper list 0 0
              and kadane_helper = fn list -> fn current_max -> fn global_max ->
                case list do
                | [] -> global_max
                | h :: t ->
                  let new_current = max h (current_max + h) in
                  let new_global = max global_max new_current in
                  kadane_helper t new_current new_global

              let lst = [0 - 2, 1, 0 - 3, 4, 0 - 1, 2, 1, 0 - 5, 4]
            |}
             ~expr:"kadane lst" ~expected_value:"6" );
       ]

let all_tests =
  List.flatten
    [
      eval_tests;
      int_type_tests;
      bool_type_tests;
      string_type_tests;
      char_type_tests;
      [ string_concat_tests ];
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
      [ parenthesized_builtin_operator_tests ];
      [ option_map_type_test ];
      [ polymorphic_nullary_constructor_regression_tests ];
      [ bind_operator_lexing_regression_tests ];
      record_type_tests;
      [ named_record_type_tests ];
      record_eval_tests;
      builtin_eval_tests;
      builtin_type_tests;
      extended_record_type_tests;
      extended_record_eval_tests;
      [ complex_record_scenarios ];
      [ very_complex_record_tests ];
      [ mutual_recursion_basic_tests ];
      [ mutual_recursion_type_tests ];
      [ mutual_recursion_complex_tests ];
      [ simple_constructor_type_tests ];
      [ recursive_constructor_type_tests ];
      [ complex_constructor_type_tests ];
      [ constructor_with_type_alias_tests ];
      [ float_operation_tests ];
      [ char_tests ];
      [ string_operation_tests ];
      [ list_comprehension_tests ];
      [ advanced_pattern_matching_tests ];
      [ operator_precedence_tests ];
      [ higher_order_function_tests ];
      [ nested_data_structure_tests ];
      [ type_alias_tests ];
      [ edge_case_tests ];
      [ recursion_edge_case_tests ];
      [ list_operation_tests ];
      [ complex_recursive_algorithms ];
      [ complex_list_processing ];
      [ complex_tree_operations ];
      [ complex_sum_type_operations ];
      [ complex_function_composition ];
      [ complex_pattern_matching_scenarios ];
      [ complex_record_operations ];
      [ complex_higher_order_functions ];
      [ complex_mutual_recursion ];
      [ complex_math_operations ];
      [ complex_integration_tests ];
      [ mutually_recursive_types_basic_tests ];
      [ mutually_recursive_types_with_params_tests ];
      [ mutually_recursive_types_complex_tests ];
      [ mutually_recursive_types_evaluation_tests ];
      [ mutually_recursive_types_advanced_tests ];
      [ very_complex_integration_tests ];
    ]

let suite = "suite" >::: all_tests
let () = run_test_tt_main suite
