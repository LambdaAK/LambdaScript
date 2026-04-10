open OUnit2

type repl_state =
  Language.Cexpr.static_env * Language.Cexpr.env * Language.Typecheck.type_env

let fail msg = assert_failure msg

let fresh_state ?(prelude = false) () : repl_state =
  Language.Repl_kernel.clear_prelude_condense_cache ();
  let static_env = Language.Build_env.build_full_static_env () in
  let dynamic_env =
    Language.Ceval.initial_env () |> Language.Ceval.unwrap_eval_result
  in
  let type_env = [] in
  if not prelude then (static_env, dynamic_env, type_env)
  else
    match Language.Repl_kernel.merge_prelude static_env dynamic_env type_env with
    | Ok (se, de, te) -> (se, de, te)
    | Error msg -> fail ("Failed to load prelude in test: " ^ msg)

let run_input ((static_env, dynamic_env, type_env) : repl_state) (source : string)
    : Language.Repl_kernel.eval_outcome * repl_state =
  let outcome, static_env', dynamic_env', type_env' =
    Language.Repl_kernel.eval_user_input static_env dynamic_env type_env source
  in
  (outcome, (static_env', dynamic_env', type_env'))

let eval_expr ~state ~(expr : string) : string * string =
  match run_input state expr with
  | Language.Repl_kernel.Ev_expr { typ; value }, _ -> (typ, value)
  | Language.Repl_kernel.Ev_error msg, _ ->
      fail ("Expected expression result for " ^ expr ^ ", got error: " ^ msg)
  | Language.Repl_kernel.Ev_defs _, _ ->
      fail ("Expected expression result for " ^ expr ^ ", got definitions")

let expect_defs ~state ~(source : string) : repl_state =
  match run_input state source with
  | Language.Repl_kernel.Ev_defs _, st -> st
  | Language.Repl_kernel.Ev_error msg, _ ->
      fail ("Expected definitions result, got error: " ^ msg)
  | Language.Repl_kernel.Ev_expr _, _ ->
      fail ("Expected definitions result, got expression")

let assert_expr_exact ~state ~(expr : string) ~(typ : string) ~(value : string) :
    unit =
  let got_typ, got_value = eval_expr ~state ~expr in
  assert_equal ~printer:Fun.id typ got_typ;
  assert_equal ~printer:Fun.id value got_value

let assert_type_has_constraint ~state ~(expr : string) ~(class_name : string) :
    unit =
  let got_typ, _ = eval_expr ~state ~expr in
  assert_bool ("Expected => in type for " ^ expr) (String.contains got_typ '=');
  assert_bool ("Expected class name in type for " ^ expr)
    (String.contains got_typ class_name.[0] && String.contains got_typ '>');
  assert_bool ("Expected class constraint head for " ^ expr)
    (String.starts_with ~prefix:(class_name ^ " ") got_typ)

let assert_type_has_no_constraint ~state ~(expr : string) ~(expected_typ : string)
    : unit =
  let got_typ, _ = eval_expr ~state ~expr in
  assert_equal ~printer:Fun.id expected_typ got_typ;
  assert_bool ("Unexpected constraint in type for " ^ expr)
    (not (String.contains got_typ '='))

let int_values = [ 0; 1; 2; 3; 5; 8; 13; 21; 34; 55; 89; 144 ]
let bool_values = [ true; false ]

let bool_lit b = if b then "true" else "false"

let list_literal (xs : int list) : string =
  match xs with
  | [] -> "[]"
  | _ -> "[" ^ String.concat "," (List.map string_of_int xs) ^ "]"

let list_slug (xs : int list) : string =
  match xs with
  | [] -> "nil"
  | _ -> String.concat "_" (List.map string_of_int xs)

let list_corpus =
  [ []; [ 0 ]; [ 1; 2 ]; [ 3; 5; 8 ]; [ 13; 21; 34; 55 ]; [ 89; 144 ] ]

let base_format_tests : test list =
  let state = lazy (fresh_state ()) in
  let mk name expr typ value =
    name >:: fun _ -> assert_expr_exact ~state:(Lazy.force state) ~expr ~typ ~value
  in
  [
    mk "fmt_int_literal" "1" "Int" "1";
    mk "fmt_int_arith" "1 + 2 * 3" "Int" "7";
    mk "fmt_bool_true" "true" "Bool" "true";
    mk "fmt_bool_false" "false" "Bool" "false";
    mk "fmt_if_expr" "if false then 0 else 42" "Int" "42";
    mk "fmt_string_literal" {|"hello"|} "String" {|"hello"|};
    mk "fmt_char_literal" "'z'" "Char" "'z'";
    mk "fmt_unit_literal" "()" "Unit" "()";
    mk "fmt_list_literal" "[1,2,3]" "List<Int>" "[1, 2, 3]";
    mk "fmt_tuple2" "(1, true)" "(Int, Bool)" "(1, true)";
    mk "fmt_tuple3" {|(1, true, "x")|} "(Int, Bool, String)"
      {|(1, true, "x")|};
    mk "fmt_typed_lambda" "fn (x : Int) -> x + 1" "Int -> Int" "function";
    mk "fmt_let_application" "let add x y = x + y in add 2 3" "Int" "5";
    mk "fmt_case_expression" "case (1,2) do | (a,b) -> a + b" "Int" "3";
    mk "fmt_cons_expression" "1 :: 2 :: []" "List<Int>" "[1, 2]";
    mk "fmt_nested_list" "[[1,2],[3]]" "List<List<Int>>" "[[1, 2], [3]]";
    mk "fmt_record_literal" "{a: 1, b: true}" "{a: Int, b: Bool}"
      "{a: 1, b: true}";
    mk "fmt_record_update" "let r = {a: 1, b: true} in {r with a = 2}"
      "{a: Int, b: Bool}" "{a: 2, b: true}";
    mk "fmt_field_access" "let r = {a: 1, b: true} in r.a" "Int" "1";
  ]

let prelude_eq_tests : test list =
  let state = lazy (fresh_state ~prelude:true ()) in
  let eq_tests =
    List.concat
      (List.map
         (fun x ->
           List.map
             (fun y ->
               (Printf.sprintf "prelude_eq_int_%d_%d" x y) >:: fun _ ->
               let expr = Printf.sprintf "if (==) %d %d then 1 else 0" x y in
               let expected = if x = y then 1 else 0 in
               assert_expr_exact ~state:(Lazy.force state) ~expr ~typ:"Int"
                 ~value:(string_of_int expected))
             int_values)
         int_values)
  in
  let neq_tests =
    List.concat
      (List.map
         (fun x ->
           List.map
             (fun y ->
               (Printf.sprintf "prelude_neq_int_%d_%d" x y) >:: fun _ ->
               let expr = Printf.sprintf "if (!=) %d %d then 1 else 0" x y in
               let expected = if x <> y then 1 else 0 in
               assert_expr_exact ~state:(Lazy.force state) ~expr ~typ:"Int"
                 ~value:(string_of_int expected))
             int_values)
         int_values)
  in
  let bool_eq_tests =
    List.concat
      (List.map
         (fun a ->
           List.map
             (fun b ->
               (Printf.sprintf "prelude_eq_bool_%b_%b" a b) >:: fun _ ->
               let expr =
                 Printf.sprintf "if (==) %s %s then 1 else 0" (bool_lit a)
                   (bool_lit b)
               in
               let expected = if a = b then 1 else 0 in
               assert_expr_exact ~state:(Lazy.force state) ~expr ~typ:"Int"
                 ~value:(string_of_int expected))
             bool_values)
         bool_values)
  in
  let bool_neq_tests =
    List.concat
      (List.map
         (fun a ->
           List.map
             (fun b ->
               (Printf.sprintf "prelude_neq_bool_%b_%b" a b) >:: fun _ ->
               let expr =
                 Printf.sprintf "if (!=) %s %s then 1 else 0" (bool_lit a)
                   (bool_lit b)
               in
               let expected = if a <> b then 1 else 0 in
               assert_expr_exact ~state:(Lazy.force state) ~expr ~typ:"Int"
                 ~value:(string_of_int expected))
             bool_values)
         bool_values)
  in
  eq_tests @ neq_tests @ bool_eq_tests @ bool_neq_tests

let prelude_show_tests : test list =
  let state = lazy (fresh_state ~prelude:true ()) in
  let show_ints =
    List.map
      (fun n ->
        (Printf.sprintf "prelude_show_int_%d" n) >:: fun _ ->
        let expr = Printf.sprintf "show %d" n in
        assert_expr_exact ~state:(Lazy.force state) ~expr ~typ:"String"
          ~value:("\"" ^ string_of_int n ^ "\""))
      int_values
  in
  let show_bools =
    List.map
      (fun b ->
        (Printf.sprintf "prelude_show_bool_%b" b) >:: fun _ ->
        let expr = Printf.sprintf "show %s" (bool_lit b) in
        let expected = if b then "\"true\"" else "\"false\"" in
        assert_expr_exact ~state:(Lazy.force state) ~expr ~typ:"String"
          ~value:expected)
      bool_values
  in
  show_ints @ show_bools

let prelude_option_format_tests : test list =
  let state = lazy (fresh_state ~prelude:true ()) in
  [
    "prelude_fmt_variant_nested" >:: fun _ ->
    assert_expr_exact ~state:(Lazy.force state)
      ~expr:"let x = Some (Some 1) in x" ~typ:"Option<Option<Int>>"
      ~value:"Some (Some 1)";
  ]

let constrained_program =
  {|
inter Tag <a> {
  val tag : a -> String
}

impl Tag for Int where
  tag x = int_to_str x
end

impl Tag for Bool where
  tag x = if x then "true" else "false"
end

let describe<Tag a> x = tag x
let wrap<Tag a> x = "[" ^ tag x ^ "]"
let pair<Tag a> x y = tag x ^ ":" ^ tag y
let id x = x
|}

let constrained_state =
  lazy
    (let st0 = fresh_state () in
     expect_defs ~state:st0 ~source:constrained_program)

let constraint_presence_tests : test list =
  let names_with_constraints = [ "tag"; "describe"; "wrap"; "pair" ] in
  let constrained_name_tests =
    List.map
      (fun name ->
        (Printf.sprintf "constraint_present_%s" name) >:: fun _ ->
        assert_type_has_constraint ~state:(Lazy.force constrained_state) ~expr:name
          ~class_name:"Tag")
      names_with_constraints
  in
  let unconstrained_name_tests =
    [ "id", "a -> a"; "id 1", "Int"; "id true", "Bool" ]
    |> List.map (fun (expr, typ) ->
           (Printf.sprintf "constraint_absent_%s"
              (String.map (fun c -> if c = ' ' then '_' else c) expr))
           >:: fun _ ->
           assert_type_has_no_constraint ~state:(Lazy.force constrained_state)
             ~expr ~expected_typ:typ)
  in
  let int_apply_tests =
    List.map
      (fun n ->
        (Printf.sprintf "constraint_apply_int_%d" n) >:: fun _ ->
        assert_type_has_no_constraint ~state:(Lazy.force constrained_state)
          ~expr:(Printf.sprintf "describe %d" n) ~expected_typ:"String";
        assert_type_has_no_constraint ~state:(Lazy.force constrained_state)
          ~expr:(Printf.sprintf "wrap %d" n) ~expected_typ:"String")
      int_values
  in
  let bool_apply_tests =
    List.map
      (fun b ->
        (Printf.sprintf "constraint_apply_bool_%b" b) >:: fun _ ->
        assert_type_has_no_constraint ~state:(Lazy.force constrained_state)
          ~expr:(Printf.sprintf "describe %s" (bool_lit b))
          ~expected_typ:"String";
        assert_type_has_no_constraint ~state:(Lazy.force constrained_state)
          ~expr:(Printf.sprintf "wrap %s" (bool_lit b))
          ~expected_typ:"String")
      bool_values
  in
  let pair_apply_tests =
    List.concat
      [
        List.map
          (fun a ->
            List.map
              (fun b ->
                (Printf.sprintf "constraint_pair_int_%d_%d" a b) >:: fun _ ->
                assert_type_has_no_constraint
                  ~state:(Lazy.force constrained_state)
                  ~expr:(Printf.sprintf "pair %d %d" a b)
                  ~expected_typ:"String")
              int_values)
          int_values
        |> List.flatten;
        List.map
          (fun a ->
            List.map
              (fun b ->
                (Printf.sprintf "constraint_pair_bool_%b_%b" a b) >:: fun _ ->
                assert_type_has_no_constraint
                  ~state:(Lazy.force constrained_state)
                  ~expr:(Printf.sprintf "pair %s %s" (bool_lit a) (bool_lit b))
                  ~expected_typ:"String")
              bool_values)
          bool_values
        |> List.flatten;
      ]
  in
  let alias_tests =
    List.map
      (fun n ->
        (Printf.sprintf "constraint_alias_roundtrip_%d" n) >:: fun _ ->
        let st0 = fresh_state () in
        let st1 = expect_defs ~state:st0 ~source:constrained_program in
        let st2 = expect_defs ~state:st1 ~source:"let alias = describe" in
        assert_type_has_constraint ~state:st2 ~expr:"alias" ~class_name:"Tag";
        assert_type_has_no_constraint ~state:st2
          ~expr:(Printf.sprintf "alias %d" n) ~expected_typ:"String")
      int_values
  in
  constrained_name_tests @ unconstrained_name_tests @ int_apply_tests
  @ bool_apply_tests @ pair_apply_tests @ alias_tests

let prelude_monoid_len_tests : test list =
  let state = lazy (fresh_state ~prelude:true ()) in
  List.concat
    (List.map
       (fun left ->
         List.map
           (fun right ->
             (Printf.sprintf "prelude_mappend_len_%s__%s"
                (list_slug left) (list_slug right))
             >:: fun _ ->
             let expr =
               Printf.sprintf "let rec len xs = case xs do | [] -> 0 | _ :: t -> 1 + len t in len (mappend %s %s)"
                 (list_literal left) (list_literal right)
             in
             assert_expr_exact ~state:(Lazy.force state) ~expr ~typ:"Int"
               ~value:(string_of_int (List.length left + List.length right)))
           list_corpus)
       list_corpus)

let suite =
  "repl_format_constraints_massive"
  >::: (base_format_tests @ prelude_eq_tests @ prelude_show_tests
       @ prelude_option_format_tests @ constraint_presence_tests
       @ prelude_monoid_len_tests)

let () = run_test_tt_main suite
