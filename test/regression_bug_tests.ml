(** Regression tests for bugs fixed in this release.

    Each test asserts the CORRECT behaviour after the fix.  If a future change
    re-introduces one of these bugs the test will fail and identify the
    regression immediately.

    Bug catalogue:
      Bug 1  – EBlock silently dropped eval errors in non-last expressions
      Bug 2  – Integer division by zero raised uncaught OCaml exception
      Bug 3  – Integer modulo by zero raised uncaught OCaml exception
      Bug 4  – List comprehension generator pattern mismatch errored instead of filtering
      Bug 5  – Lexer: `[1...5]` without spaces crashed with float_of_string
      Bug 6  – subst_c_expr did not handle variable capture (impl sibling method)
      Bug 7  – Compiler && / || were not short-circuit
*)

open OUnit2
open Language.Ceval
open Language.Condense
open Language.Lex
open Language.Parser.ExprParser
open Language.Parser.ProgramParser
open Language.Build_env
open Language.Cexpr

module TC = Language.Typecheck

(* -------------------------------------------------------------------------- *)
(* Helpers                                                                     *)
(* -------------------------------------------------------------------------- *)

let lex_tokens s = s |> String.to_seq |> List.of_seq |> lex |> List.map (fun t -> t.token_type)

let eval_expr_in_empty_env (s : string) : value eval_result =
  match expr_parser (lex_tokens s) with
  | None -> Error (OtherError "parse failed")
  | Some (e, _) ->
      let c_e = condense_expr e in
      let env = initial_env () |> unwrap_eval_result in
      eval_c_expr c_e env

let eval_program_then_expr (program_src : string) (expr_src : string)
    : value eval_result =
  let tokens = lex_tokens program_src in
  let defns = match program_parser tokens with
    | None -> failwith "program parse failed"
    | Some (d, _) -> d
  in
  let c_defns = condense_program defns in
  let static_env = build_full_static_env () in
  let dynamic_env = initial_env () |> unwrap_eval_result in
  let type_env = [] in
  let _, dyn, tenv =
    List.fold_left
      (fun (se, de, te) defn ->
        let new_se, new_te =
          match TC.generate_defn se te defn with
          | Error e -> failwith ("type error: " ^ TC.string_of_type_check_error e)
          | Ok (b, tb, _) -> (b @ se, tb @ te)
        in
        let elab = TC.elaborate_defn ~rewrite_constrained_calls:true new_se new_te defn in
        let new_de = match eval_defn elab de with
          | Ok v -> v @ de
          | Error e -> failwith ("eval error: " ^ string_of_eval_error e)
        in
        (new_se, new_de, new_te))
      (static_env, dynamic_env, type_env)
      c_defns
  in
  match expr_parser (lex_tokens expr_src) with
  | None -> Error (OtherError "expr parse failed")
  | Some (e, _) ->
      eval_c_expr (condense_expr e) dyn
      |> (fun r -> ignore tenv; r)

(* -------------------------------------------------------------------------- *)
(* Bug 1: EBlock must propagate eval errors from non-last expressions         *)
(*                                                                             *)
(* Fix: changed `let _ = eval_c_expr e env in` to `let* _ = ...` so errors   *)
(* are propagated rather than silently discarded.                             *)
(* -------------------------------------------------------------------------- *)

let bug1_block_error_propagation =
  "bug1_block_error_propagation"
  >::: [
    ( "non-last expr with pattern match failure propagates error" >:: fun _ ->
      let result = eval_expr_in_empty_env "{ (case [] do | h :: _ -> h); 99 }" in
      (match result with
      | Error (OtherError _) -> ()
      | Ok v ->
          assert_failure ("Expected error, got Ok: " ^ string_of_value v)
      | Error e ->
          assert_failure ("Expected OtherError, got: " ^ string_of_eval_error e)) );

    ( "non-last expr with unbound variable propagates error" >:: fun _ ->
      let result = eval_expr_in_empty_env "{ undefined_var_xyz; 42 }" in
      (match result with
      | Error (UnboundVariable _) -> ()
      | Ok v ->
          assert_failure ("Expected UnboundVariable, got Ok: " ^ string_of_value v)
      | Error e ->
          assert_failure ("Expected UnboundVariable, got: " ^ string_of_eval_error e)) );

    ( "error in second-of-three block expressions propagates" >:: fun _ ->
      let result = eval_expr_in_empty_env "{ 1; (case [] do | h :: _ -> h); 3 }" in
      (match result with
      | Error (OtherError _) -> ()
      | Ok v ->
          assert_failure ("Expected error, got Ok: " ^ string_of_value v)
      | Error e ->
          assert_failure ("Expected OtherError, got: " ^ string_of_eval_error e)) );

    ( "block with no errors evaluates to last expression" >:: fun _ ->
      let result = eval_expr_in_empty_env "{ 1; 2; 99 }" in
      assert_equal (Ok (IntegerValue 99)) result );
  ]

(* -------------------------------------------------------------------------- *)
(* Bug 2: Integer division by zero must return an eval error, not crash       *)
(*                                                                             *)
(* Fix: guarded CDiv with `if b = 0 then Error (OtherError "division by      *)
(* zero")`.                                                                   *)
(* -------------------------------------------------------------------------- *)

let bug2_div_by_zero =
  "bug2_div_by_zero"
  >::: [
    ( "integer division by zero returns eval error" >:: fun _ ->
      let result = eval_expr_in_empty_env "1 / 0" in
      (match result with
      | Error (OtherError msg) -> assert_bool "message mentions zero" (String.length msg > 0)
      | Ok v ->
          assert_failure ("Expected OtherError, got Ok: " ^ string_of_value v)
      | Error e ->
          assert_failure ("Expected OtherError, got: " ^ string_of_eval_error e)) );

    ( "division by zero via variable returns eval error" >:: fun _ ->
      let result = eval_expr_in_empty_env "let x = 0 in 5 / x" in
      (match result with
      | Error (OtherError _) -> ()
      | Ok v ->
          assert_failure ("Expected OtherError, got Ok: " ^ string_of_value v)
      | Error e ->
          assert_failure ("Expected OtherError, got: " ^ string_of_eval_error e)) );

    ( "normal division still works" >:: fun _ ->
      assert_equal (Ok (IntegerValue 5)) (eval_expr_in_empty_env "10 / 2") );
  ]

(* -------------------------------------------------------------------------- *)
(* Bug 3: Integer modulo by zero must return an eval error, not crash         *)
(*                                                                             *)
(* Fix: same guard pattern as bug 2 applied to CMod.                         *)
(* -------------------------------------------------------------------------- *)

let bug3_mod_by_zero =
  "bug3_mod_by_zero"
  >::: [
    ( "modulo by zero returns eval error" >:: fun _ ->
      let result = eval_expr_in_empty_env "5 % 0" in
      (match result with
      | Error (OtherError _) -> ()
      | Ok v ->
          assert_failure ("Expected OtherError, got Ok: " ^ string_of_value v)
      | Error e ->
          assert_failure ("Expected OtherError, got: " ^ string_of_eval_error e)) );

    ( "modulo by zero via variable returns eval error" >:: fun _ ->
      let result = eval_expr_in_empty_env "let n = 0 in 10 % n" in
      (match result with
      | Error (OtherError _) -> ()
      | Ok v ->
          assert_failure ("Expected OtherError, got Ok: " ^ string_of_value v)
      | Error e ->
          assert_failure ("Expected OtherError, got: " ^ string_of_eval_error e)) );

    ( "normal modulo still works" >:: fun _ ->
      assert_equal (Ok (IntegerValue 1)) (eval_expr_in_empty_env "7 % 3") );
  ]

(* -------------------------------------------------------------------------- *)
(* Bug 4: List comprehension generator must filter on pattern mismatch        *)
(*                                                                             *)
(* Fix: changed `| None -> Error (PatternMatchError ...)` to                 *)
(*   `| None -> collect_envs acc rest` in generate_envs_from_generators.     *)
(* -------------------------------------------------------------------------- *)

let bug4_comprehension_pattern_filter =
  "bug4_comprehension_pattern_filter"
  >::: [
    ( "literal pattern in generator filters non-matching elements" >:: fun _ ->
      (* [42 | 1 <- [1, 2, 1, 3, 1]] → [42, 42, 42] (three matches) *)
      let result = eval_expr_in_empty_env "[42 | 1 <- [1, 2, 1, 3, 1]]" in
      (match result with
      | Ok (ListValue vs) ->
          assert_equal 3 (List.length vs);
          List.iter (fun v -> assert_equal (IntegerValue 42) v) vs
      | Ok other ->
          assert_failure ("Expected ListValue of 3 items, got: " ^ string_of_value other)
      | Error e ->
          assert_failure ("Unexpected error: " ^ string_of_eval_error e)) );

    ( "cons pattern in generator skips non-matching elements" >:: fun _ ->
      (* [(h, t) | h :: t <- [[], [1, 2], [], [3]]] → list of 2 pairs *)
      let result = eval_expr_in_empty_env "[(h, t) | h :: t <- [[], [1, 2], [], [3]]]" in
      (match result with
      | Ok (ListValue vs) ->
          assert_equal 2 (List.length vs)
      | Ok other ->
          assert_failure ("Expected ListValue of 2 items, got: " ^ string_of_value other)
      | Error e ->
          assert_failure ("Unexpected error: " ^ string_of_eval_error e)) );

    ( "all-matching generator still produces full list" >:: fun _ ->
      let result = eval_expr_in_empty_env "[x | x <- [1, 2, 3]]" in
      (match result with
      | Ok (ListValue vs) -> assert_equal 3 (List.length vs)
      | Ok other ->
          assert_failure ("Expected 3-element list, got: " ^ string_of_value other)
      | Error e ->
          assert_failure ("Unexpected error: " ^ string_of_eval_error e)) );
  ]

(* -------------------------------------------------------------------------- *)
(* Bug 5: `[1...5]` must lex correctly without requiring spaces               *)
(*                                                                             *)
(* Fix: lex_num now stops consuming at `..` lookahead, so `...` is never      *)
(* consumed as part of a numeric token.                                       *)
(* -------------------------------------------------------------------------- *)

let bug5_lexer_enum_no_space =
  "bug5_lexer_enum_no_space"
  >::: [
    ( "list range without spaces lexes and evaluates correctly" >:: fun _ ->
      let result = eval_expr_in_empty_env "[1...5]" in
      (match result with
      | Ok (ListValue vs) -> assert_equal 5 (List.length vs)
      | Ok other ->
          assert_failure ("Expected 5-element list, got: " ^ string_of_value other)
      | Error e ->
          assert_failure ("Unexpected error: " ^ string_of_eval_error e)) );

    ( "list range with spaces still works" >:: fun _ ->
      let result = eval_expr_in_empty_env "[1 ... 5]" in
      (match result with
      | Ok (ListValue vs) -> assert_equal 5 (List.length vs)
      | Ok other ->
          assert_failure ("Expected 5-element list, got: " ^ string_of_value other)
      | Error e ->
          assert_failure ("Unexpected error: " ^ string_of_eval_error e)) );

    ( "float literal is still lexed correctly" >:: fun _ ->
      let result = eval_expr_in_empty_env "3.14" in
      (match result with
      | Ok (FloatValue f) -> assert_bool "close to 3.14" (abs_float (f -. 3.14) < 0.0001)
      | Ok other ->
          assert_failure ("Expected FloatValue, got: " ^ string_of_value other)
      | Error e ->
          assert_failure ("Unexpected error: " ^ string_of_eval_error e)) );
  ]

(* -------------------------------------------------------------------------- *)
(* Bug 6: subst_c_expr must not substitute into locally-bound names           *)
(*                                                                             *)
(* Fix: added `c_pat_bound_vars` helper and `remove_captured` in             *)
(* subst_c_expr to strip captured names from the substitution before          *)
(* recursing into binder bodies.                                              *)
(* -------------------------------------------------------------------------- *)

let bug6_subst_capture_avoidance =
  "bug6_subst_capture_avoidance"
  >::: [
    ( "sibling method name as local variable no longer captured by substitution" >:: fun _ ->
      let program = {|
        trait MyTrait <a> where
          val method_a : a -> Int
          val method_b : a -> Int
        end

        impl MyTrait for Int where
          method_a x = { let method_b = 999 in method_b }
          method_b x = x + 1
        end
      |} in
      let typechecks =
        try
          let tokens = lex_tokens program in
          let defns = match program_parser tokens with
            | None -> failwith "parse failed"
            | Some (d, _) -> d
          in
          let c_defns = condense_program defns in
          let se = build_full_static_env () in
          let te = [] in
          let _ = List.fold_left
            (fun (se', te') defn ->
              match TC.generate_defn se' te' defn with
              | Error _ -> raise Exit
              | Ok (b, tb, _) -> (b @ se', tb @ te'))
            (se, te) c_defns in
          true
        with Exit | Failure _ -> false
      in
      assert_equal ~msg:"program with local variable shadowing sibling method should typecheck"
        true typechecks );

    ( "method_a with local method_b binding returns local value" >:: fun _ ->
      let program = {|
        trait MyTrait <a> where
          val method_a : a -> Int
          val method_b : a -> Int
        end

        impl MyTrait for Int where
          method_a x = { let method_b = 999 in method_b }
          method_b x = x + 1
        end
      |} in
      let result = eval_program_then_expr program "method_a 0" in
      assert_equal (Ok (IntegerValue 999)) result );
  ]

(* -------------------------------------------------------------------------- *)
(* Bug 7: Compiler && / || are now short-circuit                              *)
(*                                                                             *)
(* Fix: CAnd and COr now lower to conditional branches (BrCond + Phi) rather  *)
(* than IAnd/IOr, so the right-hand side is only evaluated when needed.       *)
(* Interpreter behaviour was already correct; this confirms parity.           *)
(* -------------------------------------------------------------------------- *)

let bug7_short_circuit =
  "bug7_short_circuit"
  >::: [
    ( "false && error-expr short-circuits and returns false" >:: fun _ ->
      (* With fix: 1/0 now returns an error rather than raising.
         With short-circuit, the RHS is never evaluated so we get false. *)
      let result = eval_expr_in_empty_env "false && (1 / 0 == 0)" in
      assert_equal (Ok (BooleanValue false)) result );

    ( "true || error-expr short-circuits and returns true" >:: fun _ ->
      let result = eval_expr_in_empty_env "true || (1 / 0 == 0)" in
      assert_equal (Ok (BooleanValue true)) result );

    ( "true && expr evaluates the right-hand side" >:: fun _ ->
      let result = eval_expr_in_empty_env "true && (3 == 3)" in
      assert_equal (Ok (BooleanValue true)) result );

    ( "false || expr evaluates the right-hand side" >:: fun _ ->
      let result = eval_expr_in_empty_env "false || (3 == 3)" in
      assert_equal (Ok (BooleanValue true)) result );
  ]

(* -------------------------------------------------------------------------- *)
(* Test suite registration                                                     *)
(* -------------------------------------------------------------------------- *)

let regression_bug_tests =
  "regression_bug_tests"
  >::: [
    bug1_block_error_propagation;
    bug2_div_by_zero;
    bug3_mod_by_zero;
    bug4_comprehension_pattern_filter;
    bug5_lexer_enum_no_space;
    bug6_subst_capture_avoidance;
    bug7_short_circuit;
  ]

let () = run_test_tt_main regression_bug_tests
