open OUnit2
open Expr
open Lex

let parse_test (name: string) (program: string) (expected_output: expr): test = 
  name >:: fun _ ->
    let tokens: token list = program |> list_of_string |> lex in
    let expr: expr = parse_expr tokens |> fst in
    assert_equal expr expected_output



let boolean_parse_tests = [
  parse_test "parse true" "true" (BooleanExpr true);
  parse_test "parse false" "false" (BooleanExpr false)
]

let all_tests =
  List.flatten
    [
      boolean_parse_tests
    ]

let suite = "suite" >::: all_tests
let () = run_test_tt_main suite