open OUnit2
open Language.Eval



let eval_test (program: string) (expected_output: string): test =
  program >:: fun _ ->
    let result: string = eval program in
    assert_equal result expected_output
    

let value_tests = [
  eval_test "1" "1"
]

let arithmetic_tests = [
  eval_test "1 + 2" "3";
  eval_test "1 + 2 + 3 + 4" "10";
  eval_test "1 + 2 * 3 + 4" "11";
  eval_test "100 / 30 + 5" "8";
  eval_test "10 % 4" "2";
  eval_test "5 + 2 * 3" "11";
  (*a*)
  eval_test "2 + 3 * 4" "14";
  eval_test "1 + 2 + 3 + 4 + 5" "15";
  eval_test "10 - 2 * 3" "4";
  eval_test "8 - 2 - 3 - 4 - 5" "-6";
  eval_test "6 * 2 + 3" "15";
  eval_test "3 * 4 * 5" "60";
  eval_test "15 / 3 - 2" "3";
  eval_test "20 / 4 / 5" "1";
  (*b*)
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
  eval_test "100 % 31 / 2" "3"
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
  eval_test "1 <= 2" "true";
  eval_test "1 <= 1" "true";
  eval_test "1 < 1" "false";
  eval_test "1 < 2 && 13414 < 11413413" "true";
  eval_test "1 < 2 && 13414 > 11413413" "false";
  eval_test "1 < 2 || false" "true";
  eval_test "1 < 2 && false" "false";
  
]

let ternary_tests = [
  eval_test "if true then 0 else 1" "0";
  eval_test "if false then 0 else 1" "1";
  eval_test "if 1 < 2 then 0 else 1" "0";
  eval_test "if 1 > 2 then 0 else 1" "1";
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
  
]


let complex_tests = [
    eval_test {|
    bind succ [integer -> integer] <-
      lam n [integer] -> n + 1
    in
    
    bind sum [integer -> integer -> integer] <-
      lam a [integer] ->
      lam b [integer] ->
      a + b
    in
    
    sum (succ 4) (sum 1 2)
    |} "8";

    eval_test {|
    bind succ [integer -> integer] <-
      lam n [integer] -> n + 1
    in

    succ(succ (succ (succ (succ (succ (succ (succ (succ (0)))))))))
    |} "9"
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
      ternary_tests
    ]

let suite = "suite" >::: all_tests
let () = run_test_tt_main suite
