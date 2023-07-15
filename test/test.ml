open OUnit2
open Language.Ceval
open Language.Typecheck
open Language.Condense
open Language.Ctostring
open Language.Parse
open Language.Lex

let modify_tests: bool = false


module type TestModifier = sig
  type test_type
  val modify_tests: test_type list -> test_type list
  
end


module type TestModifierInput = sig
  type test_type
  val modifiers: (test_type -> test_type) list
end


module MakeTestModifier (Input: TestModifierInput): TestModifier with type test_type = Input.test_type = struct
  type test_type = Input.test_type
  let modify_tests (tests: test_type list): test_type list =
    if not modify_tests then tests
    else
    List.map (fun expression -> 
      (
        List.map (fun modifier -> modifier expression) Input.modifiers
      )
      ) tests |> List.flatten
end


module IntTestModifierInput: TestModifierInput with type test_type = string * string = struct
  type test_type = string * string

  let modifiers = [
    (fun (x, y) -> x, y);
    (fun (x, y) -> x ^ " + 1", y |> int_of_string |> (+) 1 |> string_of_int);
    (fun (x, y) -> x ^ " + 2", y |> int_of_string |> (+) 2 |> string_of_int);
    (fun (x, y) -> x ^ " + 3", y |> int_of_string |> (+) 3 |> string_of_int);
    (fun (x, y) -> x ^ " + 4", y |> int_of_string |> (+) 4 |> string_of_int);
    (fun (x, y) -> x ^ " + 5", y |> int_of_string |> (+) 5 |> string_of_int);
    (fun (x, y) -> "( " ^ x ^ " ) " ^ "* 2" , y |> int_of_string |> ( * ) 2 |> string_of_int);
    (fun (x, y) -> "( " ^ x ^ " ) " ^ "* 3" , y |> int_of_string |> ( * ) 3 |> string_of_int);
    (* division *)
    (fun (x, y) -> "( " ^ x ^ " ) " ^ "/ 2" , y |> int_of_string |> (fun x -> x / 2) |> string_of_int);
    (fun (x, y) -> "( " ^ x ^ " ) " ^ "/ 3" , y |> int_of_string |> (fun x -> x / 3) |> string_of_int);
    (fun (x, y) -> "( " ^ x ^ " ) " ^ "/ 4" , y |> int_of_string |> (fun x -> x / 4) |> string_of_int);
    (fun (x, y) -> "~- ( " ^ x ^ " )", y |> int_of_string |> (fun x -> -x) |> string_of_int);
    (* ternary *)
    (fun (x, y) -> "if true then " ^ x ^ " else 0", y);
    (fun (x, y) -> "if false then 0 else " ^ x, y);
    (* more ternary *)
    (fun (x, _) -> "if false then " ^ x ^ " else 0", "0");
    (fun (x, _) -> "if true then 0 else " ^ x, "0");
    
    (* more complicated tests *)
    (fun (x, y) -> x ^ " + 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10", y |> int_of_string |> (+) 55 |> string_of_int);
   
    
    (* tests involving functions *)
    (fun (x, y) -> "(fn n -> n) (" ^ x ^ " )", y);
    (fun (x, y) -> "(fn n -> n + 1) (" ^ x ^ " )", y |> int_of_string |> (+) 1 |> string_of_int);
    (fun (x, y) -> "(fn a -> fn b -> a + b) (" ^ x ^ " )" ^ " ( " ^ x ^ " )", y |> int_of_string |> ( * ) 2 |> string_of_int);


    (* tests involving bind expressions *)
    (fun (x, y) -> "bind a <- " ^ x ^ " in a", y);
    (fun (x, y) -> "bind a <- " ^ x ^ " in a + 1", y |> int_of_string |> (+) 1 |> string_of_int);

  ]
end

module EvalTestModifierInput: TestModifierInput with type test_type = string * string = struct
  type test_type = string * string

  let modifiers: (test_type -> test_type) list = [
    (* identity *)
    (fun (x, y) -> x, y);
    (* bind expression *)
    (fun (x, y) -> "bind a <- " ^ x ^ " in a", y);
    (fun (x, y) -> "bind rec a <- " ^ x ^ " in a", y);
    (fun (x, y) -> "( fn a -> a ) ( " ^ x ^ " )", y);


  ]

end

module TypeTestModifierInput: TestModifierInput with type test_type = string = struct
  type test_type = string

  let modifiers: (test_type -> test_type) list = [
    (* identity *)
    (fun x -> x);
    (* bind expression *)
    (fun x -> "bind q <- " ^ x ^ " in q");
    (fun x -> "bind rec q <- " ^ x ^ " in q");
    (fun x -> "( fn a -> a ) ( " ^ x ^ " )");

  ]

end

module IntTypeTestModifierInput: TestModifierInput with type test_type = string = struct
  type test_type = string
  let modifiers: (string -> string) list = [
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
    (fun x -> "(fn n -> n) (" ^ x ^ " )");
    (fun x -> "(fn n -> n + 1) (" ^ x ^ " )");
    (fun x -> "(fn a -> fn b -> a + b) (" ^ x ^ " )" ^ " ( " ^ x ^ " )");

  ]

end

module BoolTypeTestModifierInput: TestModifierInput with type test_type = string = struct
  
  type test_type = string

  let modifiers = [
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

module FunctionTypeTestModifierInput: TestModifierInput with type test_type = string * string = struct
  type test_type = string * string

  let modifiers = [
    (fun x -> x);
    (fun (x, y) -> "(fn m -> m) ( " ^ x ^ " )", y);
    
  ]

end

module PairTypeTestModiferInput: TestModifierInput with type test_type = string * string = struct
  type test_type = string * string

  let modifiers: (string * string -> string * string) list = [
    (fun x -> x)
  ]

end

module VectorTypeTestModifierInput: TestModifierInput with type test_type = string * string = struct
  type test_type = string * string

  let modifiers: (string * string -> string * string) list = [
    (fun x -> x)
  ]

end

module ListTypeTestModifierInput: TestModifierInput with type test_type = string * string = struct
  type test_type = string * string

  let modifiers: (string * string -> string * string) list = [
    (fun x -> x)
  ]

end

module SwitchTypeTestModifierInput: TestModifierInput with type test_type = string * string = struct
  type test_type = string * string

  let modifiers: (string * string -> string * string) list = [
    (fun x -> x)
  ]


end

module PolymorphismTypeTestModifierInput: TestModifierInput with type test_type = string * string = struct
  type test_type = string * string

  let modifiers: (string * string -> string * string) list = [
    (fun x -> x)
  ]

end


module IntTestModifier = MakeTestModifier (IntTestModifierInput)
module IntTypeTestModifier = MakeTestModifier (IntTypeTestModifierInput)
module BoolTypeTestModifier = MakeTestModifier (BoolTypeTestModifierInput)
module EvalTestModifier = MakeTestModifier (EvalTestModifierInput)
module TypeTestModifier = MakeTestModifier (TypeTestModifierInput)
module FunctionTypeTestModifier = MakeTestModifier (FunctionTypeTestModifierInput)
module PairTypeTestModifier = MakeTestModifier (PairTypeTestModiferInput)
module VectorTypeTestModifier = MakeTestModifier (VectorTypeTestModifierInput)
module ListTypeTestModifier = MakeTestModifier (ListTypeTestModifierInput)
module SwitchTypeTestModifier = MakeTestModifier (SwitchTypeTestModifierInput)
module PolymorphismTypeTestModifier = MakeTestModifier (PolymorphismTypeTestModifierInput)

let eval_test (expr: string) (expected_output: string): test =
  expr ^ " SHOULD YIELD " ^ expected_output >:: fun _ ->
    let result: string = c_eval expr in
    assert_equal result expected_output


let type_test (expr: string) (expected_output: string): test =
  expr ^ " SHOULD BE OF TYPE " ^ expected_output >:: fun _ ->
    let result: string = 
    type_of_c_expr (expr
    |> list_of_string 
    |> lex 
    |> parse_expr
    |> fst 
    |> condense_expr ) initial_env

    |> string_of_c_type 
  in
    assert_equal result expected_output


let type_is_bool (program: string) = type_test program "bool"
let type_is_int (program: string) = type_test program "int"
let type_is_string (program: string) = type_test program "str"

let () = ignore type_is_bool
let () = ignore type_is_int
let () = ignore type_is_string



let int_types: string list = [
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

let bool_types = [
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
   "1 != 1";
   "if true then true else false";
   "true || true";
   "1 < 2 || false";
   "1 == 1";
   "1 != 1";
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
   "if true || false || false || false || (true || false && true) then true else false";
   "if true && false || false || false || (true || false && true) then true else false";
   "if true && false || false || false || (false || false && true) then true else false";
   "if true && false || false || false || (false || false && true) then true else false";
   "if not true then true else false";
   "if not false then true else false";
   "if not (not true ) then true else false";
   "if not (not false) then true else false";
   "if not true || false then true else false";
   "if not true || true then true else false";
   "if true && not true then true else false";
   "if true && not false then true else false";
   "if true && true || false then true else false";
   "if true || false || false || false || (true || false && true) then true else false";
   "if true && false || false || false || (true || false && true) then true else false";
   "if true && false || false || false || (false || false && true) then true else false";
   "if true && false || false || false || (false || false && true) then true else false";
   "if true && true || false then true else false";
   "if true || false || false || false || (true || false && true) then true else false";

]

let string_types = [
  {|""|};
  {|"hello"|};

]

let function_type_tests = [
  "fn n -> n", "t1 -> t1";
  "fn n -> n + 1", "int -> int";
  "fn a -> fn b -> a + b", "int -> int -> int";
  "fn n [int] -> n", "int -> int";
  "fn n [int] -> n + 1", "int -> int";
  "fn a [int] -> fn b [int] -> a + b", "int -> int -> int";
  "fn a -> fn b -> a", "t1 -> t2 -> t1";
  "fn a -> fn b -> b", "t1 -> t2 -> t2";
  "fn a [int] -> fn b -> b", "int -> t1 -> t1";
  "fn a -> fn b [int] -> b", "t1 -> int -> int";
  "fn a [int] -> fn b [int] -> b", "int -> int -> int";
  "fn a [int] -> fn b [int] -> a", "int -> int -> int";
  "fn a [int] -> fn b [int] -> a + b", "int -> int -> int";
  "fn a [int] -> fn b [int] -> a + b + 1", "int -> int -> int";
  "fn a [int] -> fn b [int] -> a + b + 1 + 2", "int -> int -> int";
  "fn (a, b) [(int, int)] -> a + b", "(int, int) -> int";
  "fn (a, _) -> fn (_, b) -> a + b", "(int, t1) -> (t2, int) -> int";
  "fn (a, _) -> fn (_, b) -> a || b", "(bool, t1) -> (t2, bool) -> bool";
  {|fn (a, b) ->
    fn (c, d) ->
    if a then b
    else if c then d
    else 1|}, "(bool, int) -> (bool, int) -> int";

  

  (* more complicated function type tests *)
  "fn a -> fn b -> fn c -> a ( b ( c ) )", "(t1 -> t2) -> (t3 -> t1) -> t3 -> t2";

  (* tests with syntax sugar bind expressions *)

  "bind f x <- x in f", "t1 -> t1";
  "bind f x <- x + 1 in f", "int -> int";
  "bind f x <- x + 1 in f 1", "int";
  "bind f x <- x + 1 in f 1 + 1", "int";
  "bind f x <- x + 1 in f (1 + 1)", "int";
  (* add three numbers *)
  "bind f a b c <- a + b + c in f", "int -> int -> int -> int";
  "bind f a b c <- a + b + c in f 1", "int -> int -> int";
  "bind f a b c <- a + b + c in f 1 2", "int -> int";
  "bind f a b c <- a + b + c in f 1 2 3", "int";
  "bind f a b c d <- a in f", "t1 -> t2 -> t3 -> t4 -> t1";

  (* typed arguments *)

  "bind f a [int] b [int] c [int] d [int] <- a in f", "int -> int -> int -> int -> int";
  "bind f a [int] b [int] c [int] d [int] <- a in f 1", "int -> int -> int -> int";
  "bind f a [int] b [int] c [int] d [int] <- a in f 1 2", "int -> int -> int";
  "bind f a [int] b [int] c [int] d [int] <- a in f 1 2 3", "int -> int";
  "bind f a [int] b [int] c [int] d [int] <- a in f 1 2 3 4", "int";

  (* with type variables *)
  "fn a ['a] -> a", "t1 -> t1";
  "fn a ['a] -> a + 1", "int -> int";
  "fn a ['a] -> fn b ['a] -> a", "t1 -> t1 -> t1";
  "fn a ['a] -> fn b ['a] -> b", "t1 -> t1 -> t1";
  "fn a ['a] -> fn b ['b] -> a", "t1 -> t2 -> t1";
  "fn a ['a] -> fn b ['a] -> a + b", "int -> int -> int";
  "fn a ['a] -> fn b ['a] -> a + b + 1", "int -> int -> int";

  "fn f ['a -> 'b] -> fn x ['b] -> f x", "(t1 -> t1) -> t1 -> t1";
  (* this is an interesting example because it turns out that 'a = 'b here *)
  "fn f ['a -> 'b] -> fn x ['a] -> f x", "(t1 -> t2) -> t1 -> t2";
  (* on the other hand, there is no constraint generated in this expression saying that 'a = 'b, so they are different *)

  "bind f a b [int] c [int] d [int] <- a in f", "t1 -> int -> int -> int -> t1";
  "bind f a b [int] c [int] d <- a in f", "t1 -> int -> int -> t2 -> t1";
  "bind f a b [int] c d [int] <- a in f", "t1 -> int -> t2 -> int -> t1";
  "bind f a b [int] c d <- a in f", "t1 -> int -> t2 -> t3 -> t1";
  "bind f a b c [int] d [int] <- b in f", "t1 -> t2 -> int -> int -> t2";
  "bind f a b c [int] d <- b in f", "t1 -> t2 -> int -> t3 -> t2";
  "bind f a b c d [str] <- c in f", "t1 -> t2 -> t3 -> str -> t3";
  "bind f a b c d <- c in f", "t1 -> t2 -> t3 -> t4 -> t3";
  "bind f a b c d [str] <- d in f", "t1 -> t2 -> t3 -> str -> str";

  "fn (a, _) -> a", "(t1, t2) -> t1";
  "fn (a, _) -> a + 1", "(int, t1) -> int";

  "fn f -> fn x -> f x", "(t1 -> t2) -> t1 -> t2";

  {|fn f ['a -> 'b -> 'c] ->
    fn a ['a] ->
    fn b ['b] ->
    f a b|}, "(t1 -> t2 -> t3) -> t1 -> t2 -> t3";

  
  "fn a [('a, 'b)] -> a", "(t1, t2) -> (t1, t2)";
  "fn (a, _) [('a, 'b)] -> a", "(t1, t2) -> t1";
  "fn (_, a) [('a, 'b)] -> a", "(t1, t2) -> t2";
  "fn (a, b, c) [('a, 'b, 'c)] -> a", "(t1, t2, t3) -> t1";

  (* higher order function *)
  {|fn f [('a, 'b) -> 'c] ->
    fn a ['a] ->
    fn b ['b] ->
    f (a, b)|}, "((t1, t2) -> t3) -> t1 -> t2 -> t3";

  {|fn f ['a -> 'b -> 'c] ->
    fn (a, b) ->
    f a b|}, "(t1 -> t2 -> t3) -> (t1, t2) -> t3";



  (* long function with 10 arguments and return the first *)
  "bind f a b c d e f g h i j <- a in f", "t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t1";
  (* long function with 20 arguments *)
  "bind f a b c d e f g h i j k l m n o p q r s t <- a in f", "t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t1";
  (* long function with 30 arguments. name the arguments the word of the number *)
  "bind f one two three four five six seven eight nine ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen twenty twentyone twentytwo twentythree twentyfour twentyfive twentysix twentyseven twentyeight twentynine thirty <- one in f", "t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t1";
  (* long function with 40 arguments. name the arguments the word of the number *)
  "bind f one two three four five six seven eight nine ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen twenty twentyone twentytwo twentythree twentyfour twentyfive twentysix twentyseven twentyeight twentynine thirty thirtyone thirtytwo thirtythree thirtyfour thirtyfive thirtysix thirtyseven thirtyeight thirtynine forty <- one in f", "t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> t32 -> t33 -> t34 -> t35 -> t36 -> t37 -> t38 -> t39 -> t40 -> t1";
  (* long function with 50 arguments. name the arguments the word of the number *)
  "bind f one two three four five six seven eight nine ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen twenty twentyone twentytwo twentythree twentyfour twentyfive twentysix twentyseven twentyeight twentynine thirty thirtyone thirtytwo thirtythree thirtyfour thirtyfive thirtysix thirtyseven thirtyeight thirtynine forty fortyone fortytwo fortythree fortyfour fortyfive fortysix fortyseven fortyeight fortynine fifty <- one in f", "t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> t13 -> t14 -> t15 -> t16 -> t17 -> t18 -> t19 -> t20 -> t21 -> t22 -> t23 -> t24 -> t25 -> t26 -> t27 -> t28 -> t29 -> t30 -> t31 -> t32 -> t33 -> t34 -> t35 -> t36 -> t37 -> t38 -> t39 -> t40 -> t41 -> t42 -> t43 -> t44 -> t45 -> t46 -> t47 -> t48 -> t49 -> t50 -> t1";
  (* long function with 60 arguments. name the arguments the word of the number *)


  (* recursive functions *)

  "bind rec f x <- x in f", "t1 -> t1";
  "bind rec f x [ng] <- x in f", "ng -> ng";
  "bind rec f x [int -> int] <- x in f", "(int -> int) -> int -> int";

  "fn a [[int]] -> a", "[int] -> [int]";
  

  "bind rec f x <- if x == 0 then 0 else f (x - 1) in f", "int -> int";
  (* factorial *)
  "bind rec f x <- if x == 0 then 1 else x * f (x - 1) in f", "int -> int";
  (* fibonacci *)
  "bind rec f x <- if x == 0 then 0 else if x == 1 then 1 else f (x - 1) + f (x - 2) in f", "int -> int";
  (* sum of first n numbers *)
  "bind rec f x <- if x == 0 then 0 else x + f (x - 1) in f", "int -> int";
  (* sum of first n odd numbers *)
  "bind rec f x <- if x == 0 then 0 else if x == 1 then 1 else 2 * x - 1 + f (x - 1) in f", "int -> int";
  (* sum of first n even numbers *)
  "bind rec f x <- if x == 0 then 0 else if x == 1 then 2 else 2 * x + f (x - 1) in f", "int -> int";
  (* sum of first n squares *)
  "bind rec f x <- if x == 0 then 0 else x * x + f (x - 1) in f", "int -> int";
  (* sum of first n cubes *)
  "bind rec f x <- if x == 0 then 0 else x * x * x + f (x - 1) in f", "int -> int";
  (* sum of first n fourth powers *)
  "bind rec f x <- if x == 0 then 0 else x * x * x * x + f (x - 1) in f", "int -> int";
  (* sum of first n fifth powers *)
  "bind rec f x <- if x == 0 then 0 else x * x * x * x * x + f (x - 1) in f", "int -> int";
  
  (* recursive function with boolean inputs *)
  "bind rec f x <- if x then 1 else 0 in f", "bool -> int";

  "bind rec f x <- if x then 1 else f (not x) in f", "bool -> int";

  (* big recursive function like fibonacci but with third order recurrence relation *)
  "bind rec f x <- if x == 0 then 0 else if x == 1 then 1 else if x == 2 then 2 else f (x - 1) + f (x - 2) + f (x - 3) in f", "int -> int";
  (* big recursive function like fibonacci but with fourth order recurrence relation *)
  "bind rec f x <- if x == 0 then 0 else if x == 1 then 1 else if x == 2 then 2 else if x == 3 then 3 else f (x - 1) + f (x - 2) + f (x - 3) + f (x - 4) in f", "int -> int";
  (* big recursive function like fibonacci but with fifth order recurrence relation *)
  "bind rec f x <- if x == 0 then 0 else if x == 1 then 1 else if x == 2 then 2 else if x == 3 then 3 else if x == 4 then 4 else f (x - 1) + f (x - 2) + f (x - 3) + f (x - 4) + f (x - 5) in f", "int -> int";
  
]

let pair_type_tests = [
  "(1, 1)", "(int, int)";
  "(1, true)", "(int, bool)";
  "(1, 1 + 1)", "(int, int)";
  "(1 - 1, 1 + 1 + 1)", "(int, int)";
  "(1 - 1, 1 + 1 + 1 + 1)", "(int, int)";
  (* nested *)
  "(1 - 1, (1 + 1, 1 + 1 + 1))", "(int, (int, int))";
  "(1 - 1, (1 + 1, (1 + 1 + 1, 1 + 1 + 1 + 1)))", "(int, (int, (int, int)))";
]

let function_to_string_tests = [
  "fn a -> a", "function";
  "fn () -> ()", "function";
  "fn () [ng] -> ()", "function";
  "bind a [(int -> int) -> int] <- fn f -> f 1 in a", "function"
]

let vector_type_tests = [
  "(1, 2, 3)", "(int, int, int)";
  "(1, 2, 3, 4)", "(int, int, int, int)";
  "(1, 2, 3, 4, 5)", "(int, int, int, int, int)";
  "(1, 2, 3, 4, 5, 6)", "(int, int, int, int, int, int)";
  "(1, 2, 3, 4, 5, 6, 7)", "(int, int, int, int, int, int, int)";
  (* with other types *)
  "(1, 2, true)", "(int, int, bool)";
  "(1, 2, true, false)", "(int, int, bool, bool)";
  "(1, 2, true, false, 1 + 1)", "(int, int, bool, bool, int)";
  "(1, 2, true, false, 1 + 1, 1 + 1 + 1)", "(int, int, bool, bool, int, int)";
  (* very complicated nested vector *)
  "(1, 2, true, false, 1 + 1, 1 + 1 + 1, (1, 2, true, false, 1 + 1, 1 + 1 + 1))", "(int, int, bool, bool, int, int, (int, int, bool, bool, int, int))";
]

let list_type_tests = [
  "[]", "[t1]";
  "1 :: []", "[int]";
  "1 :: 2 :: []", "[int]";
  "1 :: 2 :: 3 :: []", "[int]";
  "1 :: 2 :: 3 :: 4 :: []", "[int]";
  "(1, 2) :: []", "[(int, int)]";
  "(1, 2) :: (3, 4) :: []", "[(int, int)]";
  (* with other types *)
  "true :: []", "[bool]";
  (* nested list *)
  "(1 :: []) :: []", "[[int]]";
  "(1 :: 2 :: []) :: []", "[[int]]";
  "[] :: []", "[[t1]]";
  "[] :: [] :: []", "[[t1]]";
  "([] :: []) :: []","[[[t1]]]";
  "(([] :: []) :: []) :: []", "[[[[t1]]]]";

]


let polymorphism_tests = [
  "bind f x <- x in f f", "t1 -> t1";
  "bind f x <- x in f f f", "t1 -> t1";
  "bind f x <- x in f f f f", "t1 -> t1";
  "bind f x <- x in f f f f f", "t1 -> t1";
  "bind f x <- x in f 1 < 5 || f true", "bool";
  "bind f x <- x in bind g <- f in g g", "t1 -> t1";
  "bind f x <- x in bind g <- f in g f", "t1 -> t1";
  "bind f x <- x in bind g <- f in f g", "t1 -> t1";
  "bind f x <- x in bind a <- f 1 in f true", "bool";
]



let switch_type_tests = [
  "switch () => | () -> 1 end", "int";
  "switch () => | () -> true end", "bool";
  "switch () => | () -> () end", "ng";
  "switch () => | () -> (1, 2) end", "(int, int)";
  "switch () => | () -> (1, 2, 3) end", "(int, int, int)";
  "switch 1 => | 1 -> 1 end", "int";
  "switch 1 => | 1 -> true end", "bool";
  "switch 1 => | 1 -> () end", "ng";
  "switch 5 => | 1 -> 1 | 2 -> 2 | 3 -> 3 | 4 -> 4 | 5 -> 5 end", "int";
]


let arithmetic_tests = [
   "1 + 2", "3";
   "1 + 2 + 3 + 4", "10";
   "1 + 2 * 3 + 4", "11";
   "100 / 30 + 5", "8";
   "10 % 4", "2";
   "5 + 2 * 3", "11";
   "2 + 3 * 4", "14";
   "1 + 2 + 3 + 4 + 5", "15";
   "10 - 2 * 3", "4";
   "8 - 2 - 3 - 4 - 5", "-6";
   "6 * 2 + 3", "15";
   "3 * 4 * 5", "60";
   "15 / 3 - 2", "3";
   "20 / 4 / 5", "1";
   "10 * 2 / 4", "5";
   "15 - 3 + 2", "14";
   "2 + 4 * 6 - 8", "18";
   "20 / 5 * 2 + 3", "11";
   "7 - 3 * 2 / 4", "6";
   "9 + 3 * 2 - 4 / 2", "13"; 
   "5 * (3 + 2)", "25";
   "12 / (4 - 2)", "6";
   "3 + 4 * 2 / (1 - 5)", "1";
   "(5 + 2) * 3 - 4", "17";
   "2 * (10 - 8) + 1", "5";
   "100 / 10 % 3", "1";
   "100 % 30 / 2 * 3", "15";
   "100 % 31 / 2", "3";
   "100 % 31 / 2 * 3", "9";
   "100 % 31 / 2 * 3 + 1", "10";
   "100 % 31 / 2 * 3 + 1 - 1","9";
  (* very complicated test *)
   "1 + 2 * 3 + 4 * 5 + 6 * 7 + 8 * 9 + 10", "151";
   "1 - 1 - 1", "-1";
   "~-1 - 1 - 2", "-4";
   "10 - 20 - 30 - 40", "-80";
   "1 + 5 - 4 - 3", "-1";
   "10 - 5 + 5", "10";
   "~-10 - 5 + 5 - 5 - 5", "-20";
   "1 - 2 - 3 - 4 - 5", "-13";
   "1 - 1 - 1 * 5 * 100", "-500"
]

let boolean_tests = [
   "true", "true";
   "false", "false";
   "true || true", "true";
   "true || false", "true";
   "false || true", "true";
   "false || false", "false";
   "true && true", "true";
   "true && false", "false";
   "false && true", "false";
   "false && false", "false";
  (* more complicated ones *)
   "true && true || false", "true";
   "true || false || false || false || (true || false && true)", "true";
   "true && false || false || false || (true || false && true)", "true";
   "true && false || false || false || (false || false && true)", "false";
   "true && false || false || false || (false || false && true)", "false";
   "not true", "false";
   "not false", "true";
   "not (not true )", "true";
   "not (not false)", "false";
   "not true || false", "false";
   "not true || true", "true";
   "true && not true", "false";
   "true && not false", "true";
  (* some relations *)
   "1 < 2", "true";
   "1 > 2", "false";
   "1 <= 2", "true";
   "1 <= 1", "true";
   "1 >= 1", "true";
   "2 >= 1", "true";
   "1 < 1", "false";
   "1 < 2 && 13414 < 11413413", "true";
   "1 < 2 && 13414 > 11413413", "false";
   "1 < 2 || false", "true";
   "1 < 2 && false", "false";
  (* equality *)
   "1 == 1", "true";
   "1 != 1", "false";
  (* big tests with just + and - *)
   "1 + 2 - 3 + 4 - 5 + 6", "5";
   "1 + 2 + 3 - 4 - 5 - 6", "-9";
  
]

let ternary_tests = [
   "if true then 0 else 1", "0";
   "if false then 0 else 1", "1";
   "if 1 < 2 then 0 else 1", "0";
   "if 1 > 2 then 0 else 1", "1";
   "if not true then 0 else 1", "1";
   "if not false then 0 else 1", "0";
   "if not false || not true then 0 else 1", "0";

]

let switch_tests = [
  "switch () => | () -> 1 end", "1";
  "switch () => | () -> true end", "true";
  "switch () => | () -> () end", "()";
  "switch () => | () -> (1, 2) end", "(1, 2)";
  "switch () => | () -> (1, 2, 3) end", "(1, 2, 3)";
  "switch 1 => | 1 -> 1 end", "1";
  "switch 1 => | 1 -> true end", "true";
]


let minus_tests = [
   "1 - 1 - 1", "-1";
   "1 - 1 - 2", "-2";
   "10 - 20 - 30 - 40", "-80";
   "1 + 5 - 4 - 3", "-1";
   "10 - 5 + 5", "10";
   "~-10 - 5 + 5 - 5 - 5", "-20";
   "1 - 2 - 3 - 4 - 5", "-13";
   "1 - 2 - 3 - 4 - 5 - 6", "-19";
   "1 + 2 - 3 + 4 - 5 + 6", "5";
   "1 + 2 + 3 - 4 - 5 - 6", "-9"
]

let mult_div_mod_tests = [
   "1 * 2 * 3 * 4", "24";
   "10 / 2 * 3", "15";
   "10 / 3 * 4", "12";
   "100 / 2 / 5", "10";
   "500 / 100 / 2", "2";
   "100 / 2 % 49", "1";
   "2 * 5 % 4", "2";
   "11 % 3 % 1", "0";
  
]

let list_tests = [
  "[]", "[]";
  "1 :: []", "[1]";
  "1 :: 2 :: []", "[1, 2]";
  "1 :: 2 :: 3 :: []", "[1, 2, 3]";
  "1 :: 2 :: 3 :: 4 :: []", "[1, 2, 3, 4]";
  (* with other types *)
  "true :: []", "[true]";
  (* nested list *)
  "(1 :: []) :: []", "[[1]]";
  "(1 :: 2 :: []) :: []", "[[1, 2]]";
  (* do some operations in the list *)
  "(1 + 2) :: []", "[3]";
  "(1 + 2) :: (3 + 4) :: []", "[3, 7]";
  "(1, 2) :: []", "[(1, 2)]";
  {|
  bind (a, b) <- (1, 2) in
  bind res <- a + b in
  a :: b :: res :: []
  |}, "[1, 2, 3]";
  
]




let complex_tests = [
     {|
    bind succ [int -> int] <-
      fn n [int] -> n + 1
    in
    
    bind sum [int -> int -> int] <-
      fn a [int] ->
      fn b [int] ->
      a + b
    in
    
    sum (succ 4) (sum 1 2)
    |}, "8";

     {|
    bind succ [int-> int] <-
      fn n [int] -> n + 1
    in

    succ(succ (succ (succ (succ (succ (succ (succ (succ (0)))))))))
    |}, "9";

     {|
    bind a <- 1 in
    bind a <- a in
    a
    |}, "1";

     {|
    bind f <-
      fn a [str] -> a
    in
    f ""
    |}, {|""|};

     {|
    bind f <-
      fn () -> ()
    in
    f ()
    |}, {|()|};

    (* add some tests involving functions *)
    "bind f a b c d <- a + b + c + d in f 1 1 1 1", "4";
    "bind f a b c d <- a - b + c + d in f 1 2 1 1", "1";
    "bind f a b c d <- a in f 1 () 100 100000", "1";
    "bind f a b c d <- a in f (f 5 () () ()) () 100 100000", "5";
    (* function that uses ternary statement *)
    "bind f a b c d <- if a < b then c else d in f 1 2 3 4", "3";
    (* function that applies one function twice to another function *)
    
    {|
    bind succ n <- n + 1 in
    bind apply_twice f x <- f (f x) in
    apply_twice succ 2
    |}, "4";

    (* make a similar test *)
    {|
    bind square n <- n * n in
    bind apply_twice f x <- f (f x) in
    apply_twice square 3
    |}, "81";

    (* complicated function tests *)
    (* function that takes a function and applies it to 1 *)
    {|
    bind apply_one f <- f 1 in
    apply_one (fn n -> n + 1)
    |}, "2";
  
 
    {|
    bind f a b c d e f g h i j k l m n o p q r s t <- a in
    f 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 true false 1 2
    |}, "1";

    {|
    bind rec f x <- if x == 0 then 1 else x * f (x - 1) in
    f 5
    |}, "120";

    (* fibonacci *)
    {|
    bind rec f x <- if x == 0 then 0 else if x == 1 then 1 else f (x - 1) + f (x - 2) in
    f 10
    |}, "55";

  
    {|
    bind rec f x <- if x == 0 then 0 else if x == 1 then 1 else f (x - 1) + f (x - 2) in
    f 20
    |}, "6765";

   
    {|
    bind rec f x <- if x == 1 then 1 else if x == 2 then 1 else f (x - 1) + f (x - 2) in
    f 1
    |}, "1";
  
    
      {|
    bind rec f x <- if x == 1 then 1 else if x == 2 then 1 else f (x - 1) + f (x - 2) in
    f 2
    |}, "1";

      {|
    bind rec f x <- if x == 1 then 1 else if x == 2 then 1 else f (x - 1) + f (x - 2) in
    f 3
    |}, "2";

    
      {|

    bind rec f x <- if x == 1 then 1 else if x == 2 then 1 else f (x - 1) + f (x - 2) in
    f 4
    |}, "3";

   
      {|
    bind rec f x <- if x == 1 then 1 else if x == 2 then 1 else f (x - 1) + f (x - 2) in
    f 5
    |}, "5";

     
      {|
    bind rec f x <- if x == 1 then 1 else if x == 2 then 1 else f (x - 1) + f (x - 2) in
    f 6
    |}, "8";

  
      {|
    bind rec f x <- if x == 1 then 1 else if x == 2 then 1 else f (x - 1) + f (x - 2) in
    f 7
    |}, "13";

  
      {|
    bind rec f x <- if x == 1 then 1 else if x == 2 then 1 else f (x - 1) + f (x - 2) in
    f 8
    |}, "21";


    (* sum *)
    {|
    bind rec f x <- if x == 0 then 0 else x + f (x - 1) in
    f 10
    |}, "55";

    (* sum of first n odd numbers *)
    {|
    bind rec f x <- if x == 0 then 0 else if x == 1 then 1 else 2 * x - 1 + f (x - 1) in
    f 10
    |}, "100";

    (* sum of first n even numbers *)
    
    {|
    bind rec f x <- if x then 1 else f (not x) in
    f true

    |}, "1";


    {|
    bind rec f x <- if x then 1 else f (not x) in
    f false
    |}, "1";

    {|
    bind rec f x <- if x == 0 then true else false || f (x - 1) in
    f 100
    |}, "true";

    {|
    bind rec f x <- if x == 0 then false else true && f (x - 1) in
    f 100
    |}, "false";


]


let int_type_tests: test list = List.map (fun expression -> type_is_int expression) (int_types |> TypeTestModifier.modify_tests |> IntTypeTestModifier.modify_tests)
let bool_type_tests: test list = List.map (fun expression -> type_is_bool expression) (bool_types |> TypeTestModifier.modify_tests |> BoolTypeTestModifier.modify_tests)
let string_type_tests: test list = List.map (fun expression -> type_is_string expression) string_types
let function_type_tests: test list = List.map (fun (a, b) -> type_test a b) (function_type_tests |> FunctionTypeTestModifier.modify_tests)
let pair_type_tests: test list = List.map (fun (a, b) -> type_test a b) (pair_type_tests |> PairTypeTestModifier.modify_tests)

let vector_type_tests: test list = List.map (fun (a, b) -> type_test a b) (vector_type_tests |> VectorTypeTestModifier.modify_tests)
let list_type_tests: test list = List.map (fun (a, b) -> type_test a b) (list_type_tests |> ListTypeTestModifier.modify_tests)
let switch_type_tests: test list = List.map (fun (a, b) -> type_test a b) (switch_type_tests |> SwitchTypeTestModifier.modify_tests)
let polymorphism_tests: test list = List.map (fun (a, b) -> type_test a b) (polymorphism_tests |> PolymorphismTypeTestModifier.modify_tests)
let eval_test_data = [
  arithmetic_tests |> IntTestModifier.modify_tests;
  boolean_tests;
  complex_tests;
  minus_tests;
  mult_div_mod_tests;
  ternary_tests;
  function_to_string_tests;
  list_tests;
  switch_tests;
] |> List.flatten |> EvalTestModifier.modify_tests

let eval_tests = List.map (fun (a, b) -> eval_test a b) eval_test_data


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
      polymorphism_tests
      
    ]

let suite = "suite" >::: all_tests
let () = run_test_tt_main suite

