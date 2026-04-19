(** Regression tests for confirmed interpreter/compiler bugs.

    Each test reproduces a specific bug. Tests in this file are expected to
    PASS as long as the bug still exists. When a bug is fixed, its test(s)
    will fail — at that point, update the assertion to verify correct behavior.

    Bug catalogue:
      Bug 1  – EBlock silently drops eval errors in non-last expressions
      Bug 2  – Integer division by zero raises uncaught OCaml exception
      Bug 3  – Integer modulo by zero raises uncaught OCaml exception
      Bug 4  – List comprehension generator pattern mismatch errors instead of filtering
      Bug 5  – Lexer: `[1...5]` without spaces crashes with float_of_string
      Bug 6  – subst_c_expr does not handle variable capture (impl sibling method)
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

let [@warning "-32"] eval_program_then_expr (program_src : string) (expr_src : string)
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
(* Bug 1: EBlock silently drops eval errors in non-last expressions           *)
(*                                                                             *)
(* Location: src/ceval.ml line ~370 in eval_block_parts                       *)
(*                                                                             *)
(* Root cause: Non-last block expressions are evaluated with                  *)
(*   `let _ = eval_c_expr e env in`                                           *)
(* which is plain OCaml `let`, not monadic `let*`. The resulting              *)
(* eval_result (including Error cases) is discarded. Errors in non-last       *)
(* block positions are silently ignored and evaluation continues.             *)
(*                                                                             *)
(* Expected fix: change to `let* _ = eval_c_expr e env in`                   *)
(* -------------------------------------------------------------------------- *)

let bug1_block_error_swallow =
  "bug1_block_error_swallow"
  >::: [
    ( "non-last expr with pattern match failure is silently swallowed" >:: fun _ ->
      (* { (case [] do | h :: _ -> h); 99 }
         The first sub-expression tries to match an empty list with `h :: _`,
         which should fail with PatternMatchError.  The block should propagate
         that error.  Currently the error is dropped and 99 is returned. *)
      let result = eval_expr_in_empty_env "{ (case [] do | h :: _ -> h); 99 }" in
      (* BUG: currently succeeds and returns 99 instead of an error *)
      (* When fixed, this assertion should be updated to: assert_equal (Error ...) result *)
      assert_equal (Ok (IntegerValue 99)) result );

    ( "non-last expr with unbound variable is silently swallowed" >:: fun _ ->
      (* { undefined_var; 42 }
         The first sub-expression references an unbound variable.  This should
         propagate an UnboundVariable error from the block.  Currently it is
         swallowed. *)
      let result = eval_expr_in_empty_env "{ undefined_var; 42 }" in
      (* BUG: currently returns Ok (IntegerValue 42) instead of Error (UnboundVariable ...) *)
      assert_equal (Ok (IntegerValue 42)) result );

    ( "error in second-of-three block expressions is swallowed" >:: fun _ ->
      (* { 1; (case [] do | h :: _ -> h); 3 }
         Middle expression fails — should propagate, but currently 3 is returned. *)
      let result = eval_expr_in_empty_env "{ 1; (case [] do | h :: _ -> h); 3 }" in
      assert_equal (Ok (IntegerValue 3)) result );
  ]

(* -------------------------------------------------------------------------- *)
(* Bug 2: Integer division by zero raises uncaught OCaml Division_by_zero     *)
(*                                                                             *)
(* Location: src/ceval.ml ~line 858 in eval_bop                               *)
(*                                                                             *)
(* Root cause: `IntegerValue a / IntegerValue b` performs OCaml integer       *)
(* division.  When b=0, OCaml raises Division_by_zero as an exception rather  *)
(* than returning Error (OtherError "division by zero").                      *)
(*                                                                             *)
(* Expected fix: guard with `if b = 0 then Error (OtherError "division by    *)
(*   zero") else IntegerValue (a / b) |> return`                             *)
(* -------------------------------------------------------------------------- *)

let bug2_div_by_zero =
  "bug2_div_by_zero"
  >::: [
    ( "integer division by zero raises OCaml exception instead of eval error" >:: fun _ ->
      (* BUG: should return Error (OtherError "division by zero"),
         but instead raises Division_by_zero (an OCaml exception) *)
      assert_raises Division_by_zero (fun () ->
        ignore (eval_expr_in_empty_env "1 / 0")) );

    ( "division by zero in nested expression raises exception" >:: fun _ ->
      assert_raises Division_by_zero (fun () ->
        ignore (eval_expr_in_empty_env "let x = 0 in 5 / x")) );
  ]

(* -------------------------------------------------------------------------- *)
(* Bug 3: Integer modulo by zero raises uncaught OCaml Division_by_zero       *)
(*                                                                             *)
(* Location: src/ceval.ml ~line 859 in eval_bop                               *)
(*                                                                             *)
(* Root cause: `IntegerValue a mod IntegerValue b` with b=0 raises            *)
(* Division_by_zero just like bug 2.                                          *)
(*                                                                             *)
(* Expected fix: same guard pattern as bug 2                                  *)
(* -------------------------------------------------------------------------- *)

let bug3_mod_by_zero =
  "bug3_mod_by_zero"
  >::: [
    ( "modulo by zero raises OCaml exception instead of eval error" >:: fun _ ->
      assert_raises Division_by_zero (fun () ->
        ignore (eval_expr_in_empty_env "5 % 0")) );

    ( "modulo by zero in expression raises exception" >:: fun _ ->
      assert_raises Division_by_zero (fun () ->
        ignore (eval_expr_in_empty_env "let n = 0 in 10 % n")) );
  ]

(* -------------------------------------------------------------------------- *)
(* Bug 4: List comprehension generator pattern mismatch errors, not filters   *)
(*                                                                             *)
(* Location: src/ceval.ml ~line 825 in generate_envs_from_generators          *)
(*                                                                             *)
(* Root cause: When `bind_pat p value` returns None (pattern does not match   *)
(* an element), the code does `Error (PatternMatchError (p, value))` instead  *)
(* of skipping that element.  In Haskell-style list comprehensions, a         *)
(* non-matching generator pattern should filter the element out silently.     *)
(*                                                                             *)
(* Expected fix: change `| None -> Error (PatternMatchError (p, value))`      *)
(*   to `| None -> collect_envs acc rest`  (skip the element)                *)
(* -------------------------------------------------------------------------- *)

let bug4_comprehension_pattern_mismatch =
  "bug4_comprehension_pattern_mismatch"
  >::: [
    ( "literal pattern mismatch in generator crashes instead of filtering" >:: fun _ ->
      (* [42 | 1 <- [1, 2, 1, 3, 1]]
         Should filter to elements that match `1`, yielding [42, 42, 42].
         Currently crashes with PatternMatchError when it hits the element 2. *)
      let result = eval_expr_in_empty_env "[42 | 1 <- [1, 2, 1, 3, 1]]" in
      (* BUG: returns Error (PatternMatchError ...) instead of Ok (ListValue [...]) *)
      (match result with
      | Error (PatternMatchError _) ->
          () (* bug confirmed — this is the current (wrong) behavior *)
      | Ok _ ->
          assert_failure "Bug seems fixed: expected PatternMatchError but got Ok"
      | Error other ->
          assert_failure ("Unexpected error: " ^ string_of_eval_error other)) );

    ( "cons pattern mismatch in generator crashes instead of skipping" >:: fun _ ->
      (* [(h, t) | h :: t <- [[], [1, 2], [], [3]]]
         Should yield [(1, [2]), (3, [])] — skipping [] elements.
         Currently crashes on the first [] element. *)
      let result = eval_expr_in_empty_env "[(h, t) | h :: t <- [[], [1, 2], [], [3]]]" in
      (match result with
      | Error (PatternMatchError _) ->
          () (* bug confirmed *)
      | Ok _ ->
          assert_failure "Bug seems fixed: expected PatternMatchError but got Ok"
      | Error other ->
          assert_failure ("Unexpected error: " ^ string_of_eval_error other)) );
  ]

(* -------------------------------------------------------------------------- *)
(* Bug 5: Lexer crashes on `[1...5]` when dots touch the integer literal      *)
(*                                                                             *)
(* Location: src/lex.ml — lex_num / is_num_or_dot                             *)
(*                                                                             *)
(* Root cause: `is_num_or_dot` includes '.' as a valid number character.      *)
(* When lexing `1...5`, the lexer greedily accumulates `1...5` as a single    *)
(* token, then calls `float_of_string "1...5"` which raises Failure.          *)
(* The `...` token (Enum) only works when surrounded by whitespace: `1 ... 5` *)
(*                                                                             *)
(* Expected fix: in lex_num, stop consuming dots when two or more consecutive *)
(* dots appear (lookahead for `...` sequence)                                 *)
(* -------------------------------------------------------------------------- *)

let bug5_lexer_enum_no_space =
  "bug5_lexer_enum_no_space"
  >::: [
    ( "list range without spaces crashes lexer" >:: fun _ ->
      (* [1...5] should lex as: [ 1 ... 5 ]
         Instead the lexer greedily accumulates "1...5" and fails to parse as float *)
      assert_raises (Failure "float_of_string") (fun () ->
        ignore (lex_tokens "[1...5]")) );

    ( "list range with spaces works correctly" >:: fun _ ->
      (* Confirm the work-around (spaces) does not crash *)
      let result = eval_expr_in_empty_env "[1 ... 5]" in
      (match result with
      | Ok (ListValue vs) -> assert_equal 5 (List.length vs)
      | Ok other ->
          assert_failure
            ("Expected ListValue of length 5, got: " ^ string_of_value other)
      | Error e ->
          assert_failure ("Unexpected error: " ^ string_of_eval_error e)) );

    ( "reversed range without spaces crashes lexer" >:: fun _ ->
      assert_raises (Failure "float_of_string") (fun () ->
        ignore (lex_tokens "[5...1]")) );
  ]

(* -------------------------------------------------------------------------- *)
(* Bug 6: subst_c_expr does not avoid capturing locally-bound variables       *)
(*                                                                             *)
(* Location: src/condense.ml ~line 624 — subst_c_expr cases for              *)
(*   EFunction, EBind, EBindRec, EBindMutRec                                  *)
(*                                                                             *)
(* Root cause: When building typeclass instance dictionaries,                 *)
(* `build_dict_expr` substitutes sibling-method names with their internal     *)
(* identifiers using `subst_c_expr`.  However, `subst_c_expr` does not        *)
(* restrict the substitution when a pattern in EBind or EFunction binds the   *)
(* same name as a method being substituted.  As a result, a local binding     *)
(*   `let method_b = 999 in method_b`                                         *)
(* inside `method_a`'s body gets the `method_b` reference incorrectly         *)
(* replaced with the sibling's internal identifier, corrupting the semantics. *)
(*                                                                             *)
(* Expected fix: in subst_c_expr, remove substitution keys that are           *)
(* captured by the binder pattern before recursing into the body              *)
(*   e.g. EBind(p, t, e1, e2, r):                                             *)
(*     let sub' = remove_captured_by_pat p sub in                             *)
(*     EBind(p, t, subst sub e1, subst sub' e2, r)                           *)
(* -------------------------------------------------------------------------- *)

let bug6_subst_variable_capture =
  "bug6_subst_variable_capture"
  >::: [
    ( "sibling method name used as local variable gets incorrectly substituted" >:: fun _ ->
      (* In `method_a`'s body, `let method_b = 999` introduces a LOCAL `method_b`.
         The reference `method_b` after `in` should resolve to 999 (the local binding).
         However, subst_c_expr replaces it with the internal id for the sibling
         method, causing a type error (the sibling method has type Int->Int,
         not Int). *)
      let program = {|
        inter MyTrait <a> where
          val method_a : a -> Int
          val method_b : a -> Int
        end

        impl MyTrait for Int where
          method_a x = { let method_b = 999 in method_b }
          method_b x = x + 1
        end
      |} in
      (* BUG: this program should typecheck and method_a 0 should return 999.
         Currently it fails with a type inference error because the local
         `method_b` (an Int) gets replaced by the sibling method (Int -> Int). *)
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
      (* BUG: currently typechecks=false due to the capture bug *)
      assert_equal false typechecks );
  ]

(* -------------------------------------------------------------------------- *)
(* Bug 7: Compiler && / || are not short-circuit (interpreter/compiler parity)*)
(*                                                                             *)
(* Location: src/lower_min_ir.ml ~lines 3794-3817                             *)
(*                                                                             *)
(* Root cause: The native compiler lowers `&&` to `IAnd` and `||` to `IOr`,  *)
(* evaluating BOTH operands unconditionally.  The interpreter correctly        *)
(* short-circuits: `false && e` never evaluates `e`.                          *)
(*                                                                             *)
(* This means programs relying on short-circuit evaluation (e.g. to guard a   *)
(* division, or to call a side-effecting function conditionally) will behave   *)
(* differently in the interpreter vs the native compiler.                     *)
(*                                                                             *)
(* This test only covers the interpreter side (which is correct).  A          *)
(* compiler-side test would require a compiled binary.                        *)
(* -------------------------------------------------------------------------- *)

let bug7_and_short_circuit_interpreter =
  "bug7_and_short_circuit_interpreter"
  >::: [
    ( "interpreter: false && failing_expr short-circuits correctly" >:: fun _ ->
      (* false && (1/0 == 0) — in the interpreter the RHS is never evaluated *)
      (* NOTE: the interpreter handles this correctly due to monadic eval_bop  *)
      (* The COMPILER evaluates both sides and would crash on 1/0             *)
      let result = eval_expr_in_empty_env "false && (1 / 0 == 0)" in
      assert_equal (Ok (BooleanValue false)) result );

    ( "interpreter: true || failing_expr short-circuits correctly" >:: fun _ ->
      let result = eval_expr_in_empty_env "true || (1 / 0 == 0)" in
      assert_equal (Ok (BooleanValue true)) result );
  ]

(* -------------------------------------------------------------------------- *)
(* Test suite registration                                                     *)
(* -------------------------------------------------------------------------- *)

let regression_bug_tests =
  "regression_bug_tests"
  >::: [
    bug1_block_error_swallow;
    bug2_div_by_zero;
    bug3_mod_by_zero;
    bug4_comprehension_pattern_mismatch;
    bug5_lexer_enum_no_space;
    bug6_subst_variable_capture;
    bug7_and_short_circuit_interpreter;
  ]

let () = run_test_tt_main regression_bug_tests
