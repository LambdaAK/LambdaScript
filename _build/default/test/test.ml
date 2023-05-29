open OUnit2
open Language.Eval
open Language.Expr
open Language.Lex


let eval_test (name: string) (program: string) (expected_output: string): test =
  name >:: fun _ ->
    let tokens: token list = program |> list_of_string |> lex in
    let expr: expr = parse_expr tokens |> fst in
    let result: string = eval_expr expr [] |> string_of_value in
    assert_equal result expected_output



let value_tests = [
  eval_test "1" "1" "1"
]

let arithmetic_tests = [
  eval_test "addition" "1 + 2" "3"
]



let all_tests =
  List.flatten
    [
      value_tests;
      arithmetic_tests
    ]

let suite = "suite" >::: all_tests
let () = run_test_tt_main suite
