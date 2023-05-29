open OUnit2
open Language.Eval
open Language.Expr
open Language.Lex


let counter: int ref = ref 1
let () = ignore counter

let eval_test (program: string) (expected_output: string): test =
  program >:: fun _ ->
    let tokens: token list = program |> list_of_string |> lex in
    let expr: expr = parse_expr tokens |> fst in
    let result: string = eval_expr expr [] |> string_of_value in
    assert_equal result expected_output
    

let value_tests = [
  eval_test "1" "1"
]

let arithmetic_tests = [
  eval_test "1 + 2" "3";
  eval_test "1 + 2 + 3 + 4" "10";
  eval_test "1 + 2 * 3 + 4" "11";
  eval_test "100 / 30 + 5" "8";
  eval_test "10 % 4" "2"
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
  eval_test "false && false" "false"
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
      complex_tests
    ]

let suite = "suite" >::: all_tests
let () = run_test_tt_main suite
