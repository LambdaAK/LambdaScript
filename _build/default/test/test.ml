open OUnit2
open Language.Ceval
open Language.Typecheck
open Language.Condense
open Language.Ctostring
open Language.Parse
open Language.Lex


let eval_test (program: string) (expected_output: string): test =
  program >:: fun _ ->
    let result: string = c_eval program in
    assert_equal result expected_output


let type_test (program: string) (expected_output: string): test =
  program >:: fun _ ->
    let result: string = 
    program
    |> list_of_string 
    |> lex 
    |> parse_expr
    |> fst 
    |> condense_expr 
    |> type_of_c_expr 
    |> string_of_c_type 
  in
    assert_equal result expected_output


let type_is_boolean (program: string) = type_test program "bool"
let type_is_int (program: string) = type_test program "int"
let type_is_string (program: string) = type_test program "str"

let () = ignore type_is_boolean
let () = ignore type_is_int
let () = ignore type_is_string
    

let value_tests = [
  eval_test "1" "1";
]


let type_tests = [
  type_test "1" "int";
  type_test "true" "bool";
  type_test "false" "bool";
  type_test "1 + 2" "int";
  (* complicated test *)
  type_test "lam n -> n" "t1 -> t1";
  type_test "lam n -> n + 1" "int -> int";
  type_test "lam a -> lam b -> a + b" "int -> int -> int";
  type_test "true || false" "bool";


]

let int_type_tests = [
  type_is_int "1";
  type_is_int "1 + 2";
  type_is_int "1 + 2 + 3 + 4";
  type_is_int "1 + 2 * 3 + 4";
  type_is_int "100 / 30 + 5";
  type_is_int "10 % 4";
  type_is_int "5 + 2 * 3";
  type_is_int "2 + 3 * 4";
  type_is_int "1 + 2 + 3 + 4 + 5";
  type_is_int "10 - 2 * 3";
  type_is_int "8 - 2 - 3 - 4 - 5";
  type_is_int "6 * 2 + 3";
  type_is_int "3 * 4 * 5";
  type_is_int "15 / 3 - 2";
  type_is_int "20 / 4 / 5";
  type_is_int "10 * 2 / 4";
  type_is_int "15 - 3 + 2";
  type_is_int "2 + 4 * 6 - 8";
  type_is_int "20 / 5 * 2 + 3";
  type_is_int "7 - 3 * 2 / 4";
  type_is_int "9 + 3 * 2 - 4 / 2";
  type_is_int "5 * (3 + 2)";
  type_is_int "12 / (4 - 2)";
  type_is_int "3 + 4 * 2 / (1 - 5)";
  type_is_int "(5 + 2) * 3 - 4";
  type_is_int "2 * (10 - 8) + 1";
]

let bool_type_tests = [
  type_is_boolean "true";
  type_is_boolean "false";
  type_is_boolean "true || true";
  type_is_boolean "true || false";
  type_is_boolean "false || true";
  type_is_boolean "false || false";
  type_is_boolean "true && true";
  type_is_boolean "true && false";
  type_is_boolean "false && true";
  type_is_boolean "false && false";
  type_is_boolean "true && true || false";
  type_is_boolean "true || false || false || false || (true || false && true)";
  type_is_boolean "true && false || false || false || (true || false && true)";
  type_is_boolean "true && false || false || false || (false || false && true)";
  type_is_boolean "true && false || false || false || (false || false && true)";
  type_is_boolean "not true";
  type_is_boolean "not false";
  type_is_boolean "not (not true )";
  type_is_boolean "not (not false)";
  type_is_boolean "not true || false";
  type_is_boolean "not true || true";
  type_is_boolean "true && not true";
  type_is_boolean "true && not false";
  type_is_boolean "1 < 2";
  type_is_boolean "1 > 2";
  type_is_boolean "1 <= 2";
  type_is_boolean "1 <= 1";
  type_is_boolean "1 >= 1";
  type_is_boolean "2 >= 1";
  type_is_boolean "1 < 1";
  type_is_boolean "1 < 2 && 13414 < 11413413";
  type_is_boolean "1 < 2 && 13414 > 11413413";
  type_is_boolean "1 < 2 || false";
  type_is_boolean "1 < 2 && false";
  type_is_boolean "1 == 1";
  type_is_boolean "1 != 1";
  type_is_boolean "if true then true else false";
  type_is_boolean "if false then true else false";
  type_is_boolean "if 1 < 2 then true else false";
  type_is_boolean "if 1 > 2 then true else false";
  type_is_boolean "if not true then true else false";
  type_is_boolean "if not false then true else false";
  type_is_boolean "if not false || not true then true else false";
  type_is_boolean "if true && true then true else false";
  type_is_boolean "if true && false then true else false";
  type_is_boolean "if false && true then true else false";
  type_is_boolean "if false && false then true else false";
  type_is_boolean "if true && true || false then true else false";
  type_is_boolean "if true || false || false || false || (true || false && true) then true else false";
  type_is_boolean "if true && false || false || false || (true || false && true) then true else false";
  type_is_boolean "if true && false || false || false || (false || false && true) then true else false";
  type_is_boolean "if true && false || false || false || (false || false && true) then true else false";
  type_is_boolean "if not true then true else false";
  type_is_boolean "if not false then true else false";
  type_is_boolean "if not (not true ) then true else false";
  type_is_boolean "if not (not false) then true else false";
  type_is_boolean "if not true || false then true else false";
  type_is_boolean "if not true || true then true else false";
  type_is_boolean "if true && not true then true else false";
  type_is_boolean "if true && not false then true else false";
  type_is_boolean "if true && true || false then true else false";
  type_is_boolean "if true || false || false || false || (true || false && true) then true else false";
  type_is_boolean "if true && false || false || false || (true || false && true) then true else false";

]




let arithmetic_tests = [
  eval_test "1 + 2" "3";
  eval_test "1 + 2 + 3 + 4" "10";
  eval_test "1 + 2 * 3 + 4" "11";
  eval_test "100 / 30 + 5" "8";
  eval_test "10 % 4" "2";
  eval_test "5 + 2 * 3" "11";
  eval_test "2 + 3 * 4" "14";
  eval_test "1 + 2 + 3 + 4 + 5" "15";
  eval_test "10 - 2 * 3" "4";
  eval_test "8 - 2 - 3 - 4 - 5" "-6";
  eval_test "6 * 2 + 3" "15";
  eval_test "3 * 4 * 5" "60";
  eval_test "15 / 3 - 2" "3";
  eval_test "20 / 4 / 5" "1";
  eval_test "10 * 2 / 4" "5";
  eval_test "15 - 3 + 2" "14";
  eval_test "2 + 4 * 6 - 8" "18";
  eval_test "20 / 5 * 2 + 3" "11";
  eval_test "7 - 3 * 2 / 4" "6";
  eval_test "9 + 3 * 2 - 4 / 2" "13"; 
  eval_test "5 * (3 + 2)" "25";
  eval_test "12 / (4 - 2)" "6";
  eval_test "3 + 4 * 2 / (1 - 5)" "1";
  eval_test "(5 + 2) * 3 - 4" "17";
  eval_test "2 * (10 - 8) + 1" "5";
  eval_test "100 / 10 % 3" "1";
  eval_test "100 % 30 / 2 * 3" "15";
  eval_test "100 % 31 / 2" "3";
  eval_test "100 % 31 / 2 * 3" "9";
  eval_test "100 % 31 / 2 * 3 + 1" "10";
  eval_test "100 % 31 / 2 * 3 + 1 - 1" "9";
  (* very complicated test *)
  eval_test "1 + 2 * 3 + 4 * 5 + 6 * 7 + 8 * 9 + 10" "151";
]

let boolean_tests = [
  eval_test "true" "true";
  eval_test "false" "false";
  eval_test "true || true" "true";
  eval_test "true || false" "true";
  eval_test "false || true" "true";
  eval_test "false || false" "false";
  eval_test "true && true" "true";
  eval_test "true && false" "false";
  eval_test "false && true" "false";
  eval_test "false && false" "false";
  (* more complicated ones *)
  eval_test "true && true || false" "true";
  eval_test "true || false || false || false || (true || false && true)" "true";
  eval_test "true && false || false || false || (true || false && true)" "true";
  eval_test "true && false || false || false || (false || false && true)" "false";
  eval_test "true && false || false || false || (false || false && true)" "false";
  eval_test "not true" "false";
  eval_test "not false" "true";
  eval_test "not (not true )" "true";
  eval_test "not (not false)" "false";
  eval_test "not true || false" "false";
  eval_test "not true || true" "true";
  eval_test "true && not true" "false";
  eval_test "true && not false" "true";
  (* some relations *)
  eval_test "1 < 2" "true";
  eval_test "1 > 2" "false";
  eval_test "1 <= 2" "true";
  eval_test "1 <= 1" "true";
  eval_test "1 >= 1" "true";
  eval_test "2 >= 1" "true";
  eval_test "1 < 1" "false";
  eval_test "1 < 2 && 13414 < 11413413" "true";
  eval_test "1 < 2 && 13414 > 11413413" "false";
  eval_test "1 < 2 || false" "true";
  eval_test "1 < 2 && false" "false";
  (* equality *)
  eval_test "1 == 1" "true";
  eval_test "1 != 1" "false";
  
]

let ternary_tests = [
  eval_test "if true then 0 else 1" "0";
  eval_test "if false then 0 else 1" "1";
  eval_test "if 1 < 2 then 0 else 1" "0";
  eval_test "if 1 > 2 then 0 else 1" "1";
  eval_test "if not true then 0 else 1" "1";
  eval_test "if not false then 0 else 1" "0";
  eval_test "if not false || not true then 0 else 1" "0";
]


let minus_tests = [
  eval_test "1 - 1 - 1" "-1";
  eval_test "1 - 1 - 2" "-2";
  eval_test "10 - 20 - 30 - 40" "-80";
  eval_test "1 + 5 - 4 - 3" "-1";
  eval_test "10 - 5 + 5" "10";
  eval_test "~-10 - 5 + 5 - 5 - 5" "-20";
  eval_test "1 - 2 - 3 - 4 - 5" "-13";
  eval_test "1 - 2 - 3 - 4 - 5 - 6" "-19";
  eval_test "1 + 2 - 3 + 4 - 5 + 6" "5";
  eval_test "1 + 2 + 3 - 4 - 5 - 6" "-9"
]

let mult_div_mod_tests = [
  eval_test "1 * 2 * 3 * 4" "24";
  eval_test "10 / 2 * 3" "15";
  eval_test "10 / 3 * 4" "12";
  eval_test "100 / 2 / 5" "10";
  eval_test "500 / 100 / 2" "2";
  eval_test "100 / 2 % 49" "1";
  eval_test "2 * 5 % 4" "2";
  eval_test "11 % 3 % 1" "0";
  
]


let complex_tests = [
    eval_test {|
    bind succ [int -> int] <-
      lam n [int] -> n + 1
    in
    
    bind sum [int -> int -> int] <-
      lam a [int] ->
      lam b [int] ->
      a + b
    in
    
    sum (succ 4) (sum 1 2)
    |} "8";

    eval_test {|
    bind succ [int-> int] <-
      lam n [int] -> n + 1
    in

    succ(succ (succ (succ (succ (succ (succ (succ (succ (0)))))))))
    |} "9";

    eval_test {|
    bind a <- 1 in
    bind a <- a in
    a
    |} "1";

    eval_test {|
    bind f <-
      lam a [str] -> a
    in
    f ""
    |} {|""|};

    eval_test {|
    bind f <-
      lam () -> ()
    in
    f ()
    |} {|()|};
]



let all_tests =
  List.flatten
    [
      value_tests;
      arithmetic_tests;
      boolean_tests;
      complex_tests;
      minus_tests;
      mult_div_mod_tests;
      ternary_tests;
      type_tests;
      int_type_tests;
      bool_type_tests;
    ]

let suite = "suite" >::: all_tests
let () = run_test_tt_main suite
