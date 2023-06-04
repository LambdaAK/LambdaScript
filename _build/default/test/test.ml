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
    (fun (x, y) -> "(lam n -> n) (" ^ x ^ " )", y);
    (fun (x, y) -> "(lam n -> n + 1) (" ^ x ^ " )", y |> int_of_string |> (+) 1 |> string_of_int);
    (fun (x, y) -> "(lam a -> lam b -> a + b) (" ^ x ^ " )" ^ " ( " ^ x ^ " )", y |> int_of_string |> ( * ) 2 |> string_of_int);


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
    (fun (x, y) -> "( lam a -> a ) ( " ^ x ^ " )", y);

  ]

end

module TypeTestModifierInput: TestModifierInput with type test_type = string = struct
  type test_type = string

  let modifiers: (test_type -> test_type) list = [
    (* identity *)
    (fun x -> x);
    (* bind expression *)
    (fun x -> "bind q <- " ^ x ^ " in q");
    (fun x -> "( lam a -> a ) ( " ^ x ^ " )");

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
    (fun x -> "(lam n -> n) (" ^ x ^ " )");
    (fun x -> "(lam n -> n + 1) (" ^ x ^ " )");
    (fun x -> "(lam a -> lam b -> a + b) (" ^ x ^ " )" ^ " ( " ^ x ^ " )");

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
    (fun (x, y) -> "(lam m -> m) ( " ^ x ^ " )", y);
    
  ]


end


module IntTestModifier = MakeTestModifier (IntTestModifierInput)
module IntTypeTestModifier = MakeTestModifier (IntTypeTestModifierInput)
module BoolTypeTestModifier = MakeTestModifier (BoolTypeTestModifierInput)
module EvalTestModifier = MakeTestModifier (EvalTestModifierInput)
module TypeTestModifier = MakeTestModifier (TypeTestModifierInput)
module FunctionTypeTestModifier = MakeTestModifier (FunctionTypeTestModifierInput)


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
  "lam n -> n", "t1 -> t1";
  "lam n -> n + 1", "int -> int";
  "lam a -> lam b -> a + b", "int -> int -> int";
  "lam n [int] -> n", "int -> int";
  "lam n [int] -> n + 1", "int -> int";
  "lam a [int] -> lam b [int] -> a + b", "int -> int -> int";
  "lam a -> lam b -> a", "t1 -> t2 -> t1";
  "lam a -> lam b -> b", "t1 -> t2 -> t2";
  "lam a [int] -> lam b -> b", "int -> t1 -> t1";
  "lam a -> lam b [int] -> b", "t1 -> int -> int";
  "lam a [int] -> lam b [int] -> b", "int -> int -> int";
  "lam a [int] -> lam b [int] -> a", "int -> int -> int";
  "lam a [int] -> lam b [int] -> a + b", "int -> int -> int";
  "lam a [int] -> lam b [int] -> a + b + 1", "int -> int -> int";
  "lam a [int] -> lam b [int] -> a + b + 1 + 2", "int -> int -> int";

  (* more complicated function type tests *)
  "lam a -> lam b -> lam c -> a ( b ( c ) )", "(t1 -> t2) -> (t3 -> t1) -> t3 -> t2";


]

let function_to_string_tests = [
  "lam a -> a", "function";
  "lam () -> ()", "function";
  "lam () [ng] -> ()", "function";
  "bind a [(int -> int) -> int] <- lam f -> f 1 in a", "function"
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


let complex_tests = [
     {|
    bind succ [int -> int] <-
      lam n [int] -> n + 1
    in
    
    bind sum [int -> int -> int] <-
      lam a [int] ->
      lam b [int] ->
      a + b
    in
    
    sum (succ 4) (sum 1 2)
    |}, "8";

     {|
    bind succ [int-> int] <-
      lam n [int] -> n + 1
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
      lam a [str] -> a
    in
    f ""
    |}, {|""|};

     {|
    bind f <-
      lam () -> ()
    in
    f ()
    |}, {|()|};
]


let int_type_tests: test list = List.map (fun expression -> type_is_int expression) (int_types |> TypeTestModifier.modify_tests |> IntTypeTestModifier.modify_tests)
let bool_type_tests: test list = List.map (fun expression -> type_is_bool expression) (bool_types |> TypeTestModifier.modify_tests |>BoolTypeTestModifier.modify_tests)
let string_type_tests: test list = List.map (fun expression -> type_is_string expression) string_types
let function_type_tests: test list = List.map (fun (a, b) -> type_test a b) (function_type_tests |> FunctionTypeTestModifier.modify_tests)



let eval_test_data = [
  arithmetic_tests |> IntTestModifier.modify_tests;
  boolean_tests;
  complex_tests;
  minus_tests;
  mult_div_mod_tests;
  ternary_tests;
  function_to_string_tests
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
      
    ]

let suite = "suite" >::: all_tests
let () = run_test_tt_main suite
