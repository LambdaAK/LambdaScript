open OUnit2

(* ========================================================================= *)
(* Program Harness *)
(* ========================================================================= *)

module ProgramHarness = struct
  let parse_program (source : string) : Language.Expr.defn list =
    let input = source |> String.to_seq |> List.of_seq in
    let tokens =
      Language.Lex.lex input |> List.map (fun t -> t.Language.Lex.token_type)
    in
    match Language.Parser.ProgramParser.program_parser tokens with
    | Some (program, []) -> program
    | Some (_, rem) ->
        failwith
          (Printf.sprintf "Program parse left %d trailing tokens"
             (List.length rem))
    | None -> failwith "Failed to parse program"

  let parse_expr (source : string) : Language.Expr.expr =
    let input = source |> String.to_seq |> List.of_seq in
    let tokens =
      Language.Lex.lex input |> List.map (fun t -> t.Language.Lex.token_type)
    in
    match Language.Parser.ExprParser.expr_parser tokens with
    | Some (expr, []) -> expr
    | Some (_, rem) ->
        failwith
          (Printf.sprintf "Expression parse left %d trailing tokens"
             (List.length rem))
    | None -> failwith "Failed to parse expression"

  let run_program_interpreter_style (program_src : string) :
      Language.Cexpr.static_env * Language.Cexpr.env * Language.Typecheck.type_env
      =
    let program = parse_program program_src in
    let c_program = Language.Condense.condense_program program in
    let static_env = Language.Build_env.build_full_static_env () in
    let dynamic_env =
      Language.Ceval.initial_env () |> Language.Ceval.unwrap_eval_result
    in
    let type_env = [] in
    List.fold_left
      (fun (static_env, dynamic_env, type_env) defn ->
        match Language.Typecheck.generate_defn static_env type_env defn with
        | Language.Typecheck.Error e ->
            failwith
              ("Type error: " ^ Language.Typecheck.string_of_type_check_error e)
        | Language.Typecheck.Ok (new_bindings, new_type_bindings, _) ->
            let elaborated =
              Language.Typecheck.elaborate_defn ~rewrite_constrained_calls:true
                (new_bindings @ static_env)
                (new_type_bindings @ type_env) defn
            in
            let new_dynamic_bindings =
              match Language.Ceval.eval_defn elaborated dynamic_env with
              | Language.Ceval.Ok v -> v
              | Language.Ceval.Error e ->
                  failwith
                    ("Evaluation error: "
                   ^ Language.Ceval.string_of_eval_error e)
            in
            ( new_bindings @ static_env,
              new_dynamic_bindings @ dynamic_env,
              new_type_bindings @ type_env ))
      (static_env, dynamic_env, type_env) c_program

  let eval_expr_in_state
      ((static_env, dynamic_env, type_env) :
        Language.Cexpr.static_env * Language.Cexpr.env *
        Language.Typecheck.type_env)
      (expr_src : string) : Language.Cexpr.value * Language.Cexpr.c_type =
    let expr = parse_expr expr_src |> Language.Condense.condense_expr in
    let inferred_type =
      match Language.Typecheck.type_of_c_expr static_env type_env expr with
      | Language.Typecheck.Ok t -> t
      | Language.Typecheck.Error e ->
          failwith
            ("Type error: " ^ Language.Typecheck.string_of_type_check_error e)
    in
    let elaborated =
      Language.Typecheck.elaborate_expr ~rewrite_constrained_calls:true
        static_env type_env expr
    in
    let value =
      match Language.Ceval.eval_c_expr elaborated dynamic_env with
      | Language.Ceval.Ok v -> v
      | Language.Ceval.Error e ->
          failwith
            ("Evaluation error: " ^ Language.Ceval.string_of_eval_error e)
    in
    (value, inferred_type)

  let rec strip_outer_poly (t : Language.Cexpr.c_type) : Language.Cexpr.c_type
      =
    match t with
    | Language.Cexpr.PolyType (_, inner) -> strip_outer_poly inner
    | t -> t

  let assert_expr_int
      ~state:(state :
               Language.Cexpr.static_env * Language.Cexpr.env *
               Language.Typecheck.type_env)
      ~(expr : string) ~(expected : int) : unit =
    let value, inferred_type = eval_expr_in_state state expr in
    let type_str =
      inferred_type |> strip_outer_poly |> Language.C_to_string.string_of_c_type
    in
    assert_equal ~printer:Fun.id "Int" type_str;
    let actual_value = Language.Ceval.string_of_value value in
    assert_equal ~printer:Fun.id (string_of_int expected) actual_value
end

(* ========================================================================= *)
(* REPL Harness *)
(* ========================================================================= *)

module ReplHarness = struct
  type repl_state =
    Language.Cexpr.static_env * Language.Cexpr.env * Language.Typecheck.type_env

  let fresh_repl_state () : repl_state =
    Language.Repl_kernel.clear_prelude_condense_cache ();
    let static_env = Language.Build_env.build_full_static_env () in
    let dynamic_env =
      Language.Ceval.initial_env () |> Language.Ceval.unwrap_eval_result
    in
    (static_env, dynamic_env, [])

  let run_input
      ((static_env, dynamic_env, type_env) : repl_state)
      (source : string) :
      Language.Repl_kernel.eval_outcome * repl_state =
    let outcome, static_env', dynamic_env', type_env' =
      Language.Repl_kernel.eval_user_input static_env dynamic_env type_env source
    in
    (outcome, (static_env', dynamic_env', type_env'))

  let expect_defs (outcome : Language.Repl_kernel.eval_outcome) : unit =
    match outcome with
    | Language.Repl_kernel.Ev_defs _ -> ()
    | Language.Repl_kernel.Ev_error msg ->
        assert_failure ("Expected definitions result, got error: " ^ msg)
    | Language.Repl_kernel.Ev_expr _ ->
        assert_failure "Expected definitions result, got expression"

  let expect_expr_int (outcome : Language.Repl_kernel.eval_outcome)
      ~(expected : int) : unit =
    match outcome with
    | Language.Repl_kernel.Ev_expr { typ; value } ->
        assert_equal ~printer:Fun.id "Int" typ;
        assert_equal ~printer:Fun.id (string_of_int expected) value
    | Language.Repl_kernel.Ev_error msg ->
        assert_failure ("Expected expression result, got error: " ^ msg)
    | Language.Repl_kernel.Ev_defs _ ->
        assert_failure "Expected expression result, got definitions"

  let assert_setup_then_expr_int ~(setup : string) ~(expr : string)
      ~(expected : int) : unit =
    let st0 = fresh_repl_state () in
    let setup_out, st1 = run_input st0 setup in
    expect_defs setup_out;
    let out, _st2 = run_input st1 expr in
    expect_expr_int out ~expected
end

(* ========================================================================= *)
(* Shared Test Data *)
(* ========================================================================= *)

let eq_like_program =
  {|
inter EqLike <a> {
  val (==) : a -> a -> Bool
  val (!=) : a -> a -> Bool
  let (!=) x y = if (==) x y then false else true
}

impl EqLike for Int where
  let (==) x y = x == y
end
|}

let semigroup_int_program =
  {|
inter Semigroup <a> {
  val sappend : a -> a -> a
}

impl Semigroup for Int where
  let sappend x y = x + y
end
|}

let monoid_list_int_program =
  {|
inter Monoid <a> {
  val mappend : a -> a -> a
  val mempty : a
}

impl Monoid for [Int] where
  let rec mappend xs ys =
    case xs do
    | [] -> ys
    | h :: t -> h :: mappend t ys
  let mempty = []
end

let rec len xs =
  case xs do
  | [] -> 0
  | _ :: t -> 1 + len t

let rec sum xs =
  case xs do
  | [] -> 0
  | h :: t -> h + sum t
|}

let int_values = [ 0; 1; 2; 3; 5; 8; 13; 21 ]

let repl_int_values = [ 0; 1; 2; 5; 8 ]

let list_corpus =
  [ []; [ 0 ]; [ 1; 2 ]; [ 5; 8; 13 ]; [ 3; 3; 3 ]; [ 21; 0; 1; 2 ] ]

let repl_list_corpus =
  [ []; [ 0 ]; [ 1; 2 ]; [ 5; 8; 13 ]; [ 3; 3; 3 ] ]

let list_sum (xs : int list) : int = List.fold_left ( + ) 0 xs
let list_len (xs : int list) : int = List.length xs

let list_literal (xs : int list) : string =
  match xs with
  | [] -> "[]"
  | _ ->
      "[" ^ (xs |> List.map string_of_int |> String.concat ",") ^ "]"

let list_slug (xs : int list) : string =
  match xs with
  | [] -> "nil"
  | _ -> xs |> List.map string_of_int |> String.concat "_"

(* ========================================================================= *)
(* Program Matrix Tests *)
(* ========================================================================= *)

let eq_like_state = lazy (ProgramHarness.run_program_interpreter_style eq_like_program)

let semigroup_state =
  lazy (ProgramHarness.run_program_interpreter_style semigroup_int_program)

let monoid_state =
  lazy (ProgramHarness.run_program_interpreter_style monoid_list_int_program)

let program_eq_tests : test list =
  List.concat
    (List.map
       (fun x ->
         List.concat
           [
             List.map
               (fun y ->
                 (Printf.sprintf "program_eq_%d_%d" x y) >:: fun _ ->
                 let expr = Printf.sprintf "if (==) %d %d then 1 else 0" x y in
                 let expected = if x = y then 1 else 0 in
                 ProgramHarness.assert_expr_int ~state:(Lazy.force eq_like_state)
                   ~expr ~expected)
               int_values;
             List.map
               (fun y ->
                 (Printf.sprintf "program_neq_%d_%d" x y) >:: fun _ ->
                 let expr = Printf.sprintf "if (!=) %d %d then 1 else 0" x y in
                 let expected = if x <> y then 1 else 0 in
                 ProgramHarness.assert_expr_int ~state:(Lazy.force eq_like_state)
                   ~expr ~expected)
               int_values;
           ])
       int_values)

let program_semigroup_tests : test list =
  List.concat
    (List.map
       (fun x ->
         List.map
           (fun y ->
             (Printf.sprintf "program_sappend_%d_%d" x y) >:: fun _ ->
             let expr = Printf.sprintf "sappend %d %d" x y in
             ProgramHarness.assert_expr_int
               ~state:(Lazy.force semigroup_state)
               ~expr ~expected:(x + y))
           int_values)
       int_values)

let program_monoid_len_tests : test list =
  List.concat
    (List.map
       (fun left ->
         List.map
           (fun right ->
             (Printf.sprintf "program_mappend_len_%s__%s"
                (list_slug left) (list_slug right))
             >:: fun _ ->
             let expr =
               Printf.sprintf "len (mappend %s %s)" (list_literal left)
                 (list_literal right)
             in
             let expected = list_len left + list_len right in
             ProgramHarness.assert_expr_int ~state:(Lazy.force monoid_state)
               ~expr ~expected)
           list_corpus)
       list_corpus)

let program_monoid_sum_tests : test list =
  List.concat
    (List.map
       (fun left ->
         List.map
           (fun right ->
             (Printf.sprintf "program_mappend_sum_%s__%s"
                (list_slug left) (list_slug right))
             >:: fun _ ->
             let expr =
               Printf.sprintf "sum (mappend %s %s)" (list_literal left)
                 (list_literal right)
             in
             let expected = list_sum left + list_sum right in
             ProgramHarness.assert_expr_int ~state:(Lazy.force monoid_state)
               ~expr ~expected)
           list_corpus)
       list_corpus)

let program_matrix_tests =
  "trait_impl_program_matrix"
  >::: (program_eq_tests @ program_semigroup_tests @ program_monoid_len_tests
       @ program_monoid_sum_tests)

(* ========================================================================= *)
(* REPL Matrix Tests *)
(* ========================================================================= *)

let repl_eq_tests : test list =
  List.concat
    (List.map
       (fun x ->
         List.concat
           [
             List.map
               (fun y ->
                 (Printf.sprintf "repl_eq_%d_%d" x y) >:: fun _ ->
                 let expr = Printf.sprintf "if (==) %d %d then 1 else 0" x y in
                 let expected = if x = y then 1 else 0 in
                 ReplHarness.assert_setup_then_expr_int ~setup:eq_like_program
                   ~expr ~expected)
               repl_int_values;
             List.map
               (fun y ->
                 (Printf.sprintf "repl_neq_%d_%d" x y) >:: fun _ ->
                 let expr = Printf.sprintf "if (!=) %d %d then 1 else 0" x y in
                 let expected = if x <> y then 1 else 0 in
                 ReplHarness.assert_setup_then_expr_int ~setup:eq_like_program
                   ~expr ~expected)
               repl_int_values;
           ])
       repl_int_values)

let repl_monoid_len_tests : test list =
  List.concat
    (List.map
       (fun left ->
         List.map
           (fun right ->
             (Printf.sprintf "repl_mappend_len_%s__%s" (list_slug left)
                (list_slug right))
             >:: fun _ ->
             let expr =
               Printf.sprintf "len (mappend %s %s)" (list_literal left)
                 (list_literal right)
             in
             let expected = list_len left + list_len right in
             ReplHarness.assert_setup_then_expr_int ~setup:monoid_list_int_program
               ~expr ~expected)
           repl_list_corpus)
       repl_list_corpus)

let repl_matrix_tests =
  "trait_impl_repl_matrix" >::: (repl_eq_tests @ repl_monoid_len_tests)

(* ========================================================================= *)
(* Final Suite *)
(* ========================================================================= *)

let suite = "trait_impl_massive" >::: [ program_matrix_tests; repl_matrix_tests ]
let () = run_test_tt_main suite
