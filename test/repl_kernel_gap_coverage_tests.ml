open OUnit2

type repl_state =
  Language.Cexpr.static_env * Language.Cexpr.env * Language.Typecheck.type_env

let contains_sub (s : string) (sub : string) : bool =
  try
    ignore (Str.search_forward (Str.regexp_string sub) s 0);
    true
  with Not_found -> false

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
    | Error msg -> fail ("Failed to merge prelude in test: " ^ msg)

let run_input ((static_env, dynamic_env, type_env) : repl_state) (source : string)
    : Language.Repl_kernel.eval_outcome * repl_state =
  let outcome, static_env', dynamic_env', type_env' =
    Language.Repl_kernel.eval_user_input static_env dynamic_env type_env source
  in
  (outcome, (static_env', dynamic_env', type_env'))

let expect_defs ~state ~(source : string) : repl_state =
  match run_input state source with
  | Language.Repl_kernel.Ev_defs _, st -> st
  | Language.Repl_kernel.Ev_error msg, _ ->
      fail ("Expected defs, got error: " ^ msg ^ "\nsource:\n" ^ source)
  | Language.Repl_kernel.Ev_expr _, _ ->
      fail ("Expected defs, got expression for source:\n" ^ source)

let eval_expr ~state ~(expr : string) : string * string =
  match run_input state expr with
  | Language.Repl_kernel.Ev_expr { typ; value }, _ -> (typ, value)
  | Language.Repl_kernel.Ev_error msg, _ ->
      fail ("Expected expr result for " ^ expr ^ ", got error: " ^ msg)
  | Language.Repl_kernel.Ev_defs _, _ ->
      fail ("Expected expr result, got defs for: " ^ expr)

let eval_error ~state ~(expr : string) : string =
  match run_input state expr with
  | Language.Repl_kernel.Ev_error msg, _ -> msg
  | Language.Repl_kernel.Ev_expr { typ; value }, _ ->
      fail
        (Printf.sprintf
           "Expected error for %s, got expr typ=%s value=%s"
           expr typ value)
  | Language.Repl_kernel.Ev_defs _, _ ->
      fail ("Expected error, got defs for: " ^ expr)

let assert_expr_exact ~state ~(expr : string) ~(typ : string) ~(value : string) :
    unit =
  let got_typ, got_value = eval_expr ~state ~expr in
  assert_equal ~printer:Fun.id typ got_typ;
  assert_equal ~printer:Fun.id value got_value

let assert_type_has_constraint ~state ~(expr : string) ~(class_name : string) :
    unit =
  let typ, _ = eval_expr ~state ~expr in
  assert_bool ("Expected type to include constraint for " ^ expr)
    (contains_sub typ "=>");
  assert_bool
    (Printf.sprintf "Expected class %s in constrained type for %s (got: %s)"
       class_name expr typ)
    (contains_sub typ (class_name ^ " "))

let assert_type_has_no_constraint ~state ~(expr : string) ~(typ : string) : unit =
  let got_typ, _ = eval_expr ~state ~expr in
  assert_equal ~printer:Fun.id typ got_typ;
  assert_bool ("Unexpected constraint in type for " ^ expr)
    (not (contains_sub got_typ "=>"))

let int_values =
  [ 0; 1; 2; 3; 5; 8; 13; 21; 34; 55; 89; 144; 233; 377; 610 ]

let bool_values = [ true; false ]
let bool_lit b = if b then "true" else "false"

let list_corpus =
  [
    [];
    [ 0 ];
    [ 1; 2 ];
    [ 3; 5; 8 ];
    [ 13; 21; 34; 55 ];
    [ 89; 144; 233 ];
    [ 377; 610 ];
  ]

let list_literal (xs : int list) : string =
  match xs with
  | [] -> "[]"
  | _ -> "[" ^ String.concat "," (List.map string_of_int xs) ^ "]"

let list_value_str (xs : int list) : string =
  match xs with
  | [] -> "[]"
  | _ -> "[" ^ String.concat ", " (List.map string_of_int xs) ^ "]"

let list_slug (xs : int list) : string =
  match xs with
  | [] -> "nil"
  | _ -> String.concat "_" (List.map string_of_int xs)

let format_matrix_tests : test list =
  let st = lazy (fresh_state ()) in
  let int_literal_tests =
    List.map
      (fun n ->
        (Printf.sprintf "fmt_int_literal_%d" n) >:: fun _ ->
        assert_expr_exact ~state:(Lazy.force st)
          ~expr:(string_of_int n) ~typ:"Int" ~value:(string_of_int n))
      int_values
  in
  let bool_literal_tests =
    List.map
      (fun b ->
        (Printf.sprintf "fmt_bool_literal_%b" b) >:: fun _ ->
        let lit = bool_lit b in
        assert_expr_exact ~state:(Lazy.force st) ~expr:lit ~typ:"Bool"
          ~value:lit)
      bool_values
  in
  let list_literal_tests =
    List.map
      (fun xs ->
        (Printf.sprintf "fmt_list_literal_%s" (list_slug xs)) >:: fun _ ->
        let expected_typ = if xs = [] then "List<a>" else "List<Int>" in
        assert_expr_exact ~state:(Lazy.force st) ~expr:(list_literal xs)
          ~typ:expected_typ ~value:(list_value_str xs))
      list_corpus
  in
  let tuple_tests =
    List.map
      (fun n ->
        (Printf.sprintf "fmt_tuple3_%d" n) >:: fun _ ->
        let expr = Printf.sprintf "(%d, true, \"x\")" n in
        let value = Printf.sprintf "(%d, true, \"x\")" n in
        assert_expr_exact ~state:(Lazy.force st) ~expr
          ~typ:"(Int, Bool, String)" ~value)
      int_values
  in
  let record_tests =
    List.map
      (fun n ->
        (Printf.sprintf "fmt_record_%d" n) >:: fun _ ->
        let expr = Printf.sprintf "{a: %d, b: true}" n in
        let value = Printf.sprintf "{a: %d, b: true}" n in
        assert_expr_exact ~state:(Lazy.force st) ~expr
          ~typ:"{a: Int, b: Bool}" ~value)
      int_values
  in
  let lambda_tests =
    List.map
      (fun n ->
        (Printf.sprintf "fmt_lambda_apply_%d" n) >:: fun _ ->
        let expr = Printf.sprintf "(fn (x : Int) -> x + 1) %d" n in
        assert_expr_exact ~state:(Lazy.force st) ~expr ~typ:"Int"
          ~value:(string_of_int (n + 1)))
      int_values
  in
  int_literal_tests @ bool_literal_tests @ list_literal_tests @ tuple_tests
  @ record_tests @ lambda_tests

let error_surface_tests : test list =
  let st = lazy (fresh_state ()) in
  let parse_error_inputs =
    [
      "let = 1";
      "if then else";
      "case do";
      "fn -> 1";
      "(";
      "[1,";
      "{a:}";
      "let rec = 1";
      "type =";
      "impl for Int where";
      "inter where";
      "let x = in x";
      "if true then";
      "let x y =";
      "let (,) = 1";
      "let x = [1,2 in x";
      "let x = {a: 1 in x";
      "let rec f =";
    ]
  in
  let parse_tests =
    List.mapi
      (fun i expr ->
        (Printf.sprintf "err_parse_%03d" i) >:: fun _ ->
        let msg = eval_error ~state:(Lazy.force st) ~expr in
        assert_bool ("Expected parsing failure for: " ^ expr)
          (contains_sub msg "Parsing failed"))
      parse_error_inputs
  in
  let unbound_tests =
    List.init 40 (fun i -> "missing_symbol_" ^ string_of_int i)
    |> List.map (fun name ->
           (Printf.sprintf "err_unbound_%s" name) >:: fun _ ->
           let msg = eval_error ~state:(Lazy.force st) ~expr:name in
           assert_bool ("Expected unbound variable message for " ^ name)
             (contains_sub msg "Unbound variable:"))
  in
  let type_error_inputs =
    [
      "1 + true";
      "true + 1";
      "if 1 then 0 else 1";
      "if true then 0 else false";
      "1 && 2";
      "1 || 2";
      "not 1";
      "\"x\" + 1";
      "1 ^ 2";
      "1 < true";
      "1 > false";
      "1 <= true";
      "1 >= false";
      "1 == true";
      "1 != false";
      "(fn x -> x + 1) true";
      "(fn x -> x && true) 1";
      "case 1 do | true -> 0";
      "let (x : Bool) = 1 in x";
      "let f (x : Int) = x in f true";
      "1 :: true :: []";
      "[1, true]";
      "{a: 1}.missing";
      "let r = {a: 1} in r.b";
      "let x = 1 in x x";
    ]
  in
  let type_error_tests =
    List.mapi
      (fun i expr ->
        (Printf.sprintf "err_type_%03d" i) >:: fun _ ->
        let msg = eval_error ~state:(Lazy.force st) ~expr in
        assert_bool ("Expected type error surface for: " ^ expr)
          (contains_sub msg "Type inference failed"
          || contains_sub msg "Type mismatch"
          || contains_sub msg "Field "
          || contains_sub msg "Other error"
          || contains_sub msg "Unbound variable"))
      type_error_inputs
  in
  parse_tests @ unbound_tests @ type_error_tests

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

let constraint_visibility_tests : test list =
  let name_constraints =
    [ "tag"; "describe"; "wrap"; "pair" ]
    |> List.map (fun name ->
           (Printf.sprintf "constraint_name_%s" name) >:: fun _ ->
           assert_type_has_constraint ~state:(Lazy.force constrained_state)
             ~expr:name ~class_name:"Tag")
  in
  let unconstrained_names =
    [ ("id", "a -> a"); ("id 1", "Int"); ("id true", "Bool") ]
    |> List.map (fun (expr, typ) ->
           (Printf.sprintf "constraint_absent_%s"
              (String.map (fun c -> if c = ' ' then '_' else c) expr))
           >:: fun _ ->
           assert_type_has_no_constraint ~state:(Lazy.force constrained_state)
             ~expr ~typ)
  in
  let describe_int_tests =
    List.map
      (fun n ->
        (Printf.sprintf "constraint_describe_int_%d" n) >:: fun _ ->
        assert_type_has_no_constraint ~state:(Lazy.force constrained_state)
          ~expr:(Printf.sprintf "describe %d" n) ~typ:"String";
        assert_expr_exact ~state:(Lazy.force constrained_state)
          ~expr:(Printf.sprintf "describe %d" n) ~typ:"String"
          ~value:("\"" ^ string_of_int n ^ "\""))
      int_values
  in
  let describe_bool_tests =
    List.map
      (fun b ->
        (Printf.sprintf "constraint_describe_bool_%b" b) >:: fun _ ->
        let lit = bool_lit b in
        let value = if b then "\"true\"" else "\"false\"" in
        assert_type_has_no_constraint ~state:(Lazy.force constrained_state)
          ~expr:(Printf.sprintf "describe %s" lit) ~typ:"String";
        assert_expr_exact ~state:(Lazy.force constrained_state)
          ~expr:(Printf.sprintf "describe %s" lit) ~typ:"String" ~value)
      bool_values
  in
  let pair_int_matrix =
    List.concat
      (List.map
         (fun a ->
           List.map
             (fun b ->
               (Printf.sprintf "constraint_pair_int_%d_%d" a b) >:: fun _ ->
               let expr = Printf.sprintf "pair %d %d" a b in
               assert_expr_exact ~state:(Lazy.force constrained_state) ~expr
                 ~typ:"String"
                 ~value:("\"" ^ string_of_int a ^ ":" ^ string_of_int b ^ "\""))
             int_values)
         int_values)
  in
  let wrap_int_tests =
    List.map
      (fun n ->
        (Printf.sprintf "constraint_wrap_int_%d" n) >:: fun _ ->
        let expr = Printf.sprintf "wrap %d" n in
        let value = "\"[" ^ string_of_int n ^ "]\"" in
        assert_expr_exact ~state:(Lazy.force constrained_state) ~expr
          ~typ:"String" ~value)
      int_values
  in
  name_constraints @ unconstrained_names @ describe_int_tests
  @ describe_bool_tests @ pair_int_matrix @ wrap_int_tests

let prelude_state = lazy (fresh_state ~prelude:true ())

let prelude_behavior_tests : test list =
  let eq_matrix =
    List.concat
      (List.map
         (fun a ->
           List.map
             (fun b ->
               (Printf.sprintf "prelude_eq_%d_%d" a b) >:: fun _ ->
               let expr = Printf.sprintf "if (==) %d %d then 1 else 0" a b in
               let expected = if a = b then 1 else 0 in
               assert_expr_exact ~state:(Lazy.force prelude_state) ~expr
                 ~typ:"Int" ~value:(string_of_int expected))
             int_values)
         int_values)
  in
  let monoid_list_len =
    List.concat
      (List.map
         (fun left ->
           List.map
             (fun right ->
               (Printf.sprintf "prelude_mappend_len_%s__%s" (list_slug left)
                  (list_slug right))
               >:: fun _ ->
               let expr =
                 Printf.sprintf
                   "let rec len xs = case xs do | [] -> 0 | _ :: t -> 1 + len t in len (mappend %s %s)"
                   (list_literal left) (list_literal right)
               in
               assert_expr_exact ~state:(Lazy.force prelude_state) ~expr
                 ~typ:"Int"
                 ~value:(string_of_int (List.length left + List.length right)))
             list_corpus)
         list_corpus)
  in
  let show_ints =
    List.map
      (fun n ->
        (Printf.sprintf "prelude_show_int_%d" n) >:: fun _ ->
        let expr = Printf.sprintf "show %d" n in
        assert_expr_exact ~state:(Lazy.force prelude_state) ~expr ~typ:"String"
          ~value:("\"" ^ string_of_int n ^ "\""))
      int_values
  in
  eq_matrix @ monoid_list_len @ show_ints

let suite =
  "repl_kernel_gap_coverage"
  >::: (format_matrix_tests @ error_surface_tests @ constraint_visibility_tests
       @ prelude_behavior_tests)

let () = run_test_tt_main suite
