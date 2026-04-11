open OUnit2

let parse_program (source : string) : Language.Expr.defn list =
  let input = source |> String.to_seq |> List.of_seq in
  let tokens =
    Language.Lex.lex input |> List.map (fun t -> t.Language.Lex.token_type)
  in
  match Language.Parser.ProgramParser.program_parser tokens with
  | Some (program, []) -> program
  | Some (_, rem) ->
      failwith
        (Printf.sprintf "Program parse left %d trailing tokens" (List.length rem))
  | None -> failwith "Failed to parse program"

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
                  ("Evaluation error: " ^ Language.Ceval.string_of_eval_error e)
          in
          ( new_bindings @ static_env,
            new_dynamic_bindings @ dynamic_env,
            new_type_bindings @ type_env ))
    (static_env, dynamic_env, type_env) c_program

let lookup_value_exn (env : Language.Cexpr.env) (name : string) : string =
  match List.assoc_opt name env with
  | Some v -> Language.Ceval.string_of_value v
  | None -> failwith ("Missing runtime binding: " ^ name)

let assert_runtime_value ~env ~name ~expected =
  assert_equal ~printer:Fun.id expected (lookup_value_exn env name)

let string_contains_substring (s : string) (sub : string) : bool =
  let len_s = String.length s in
  let len_sub = String.length sub in
  if len_sub = 0 then true
  else
    let rec loop i =
      if i + len_sub > len_s then false
      else if String.sub s i len_sub = sub then true
      else loop (i + 1)
    in
    loop 0

let assert_failure_contains ~expected_substring ~f =
  match
    try
      f ();
      None
    with
    | Failure msg -> Some msg
  with
  | Some msg ->
      assert_bool
        (Printf.sprintf "expected failure containing [%s], got [%s]"
           expected_substring msg)
        (string_contains_substring msg expected_substring)
  | None ->
      assert_failure
        (Printf.sprintf "expected Failure containing [%s] but function succeeded"
           expected_substring)

let suite =
  "macro_system"
  >::: [
         ( "macro_rules_simple_expression_expansion" >:: fun _ ->
           let program =
             {|
macro_rules! add1 { (x) => x + 1 }
let out = add1!(41)
|}
           in
           let _, dynamic_env, _ = run_program_interpreter_style program in
           assert_runtime_value ~env:dynamic_env ~name:"out" ~expected:"42" );
         ( "macro_rules_nested_expansion" >:: fun _ ->
           let program =
             {|
macro_rules! add { (a, b) => a + b }
macro_rules! twice { (x) => add!(x, x) }
let out = twice!(21)
|}
           in
           let _, dynamic_env, _ = run_program_interpreter_style program in
           assert_runtime_value ~env:dynamic_env ~name:"out" ~expected:"42" );
         ( "macro_rules_module_local_scope" >:: fun _ ->
           let program =
             {|
mod M where
  macro_rules! inc { (x) => x + 1 }
  let out = inc!(41)
end
|}
           in
           let _, dynamic_env, _ = run_program_interpreter_style program in
           assert_runtime_value ~env:dynamic_env ~name:"M.out" ~expected:"42" );
         ( "macro_rules_unknown_macro_reports_error" >:: fun _ ->
           let program =
             {|
let out = missing!(1)
|}
           in
           assert_failure_contains ~expected_substring:"unknown macro missing!"
             ~f:(fun () -> ignore (run_program_interpreter_style program)) );
         ( "macro_rules_arity_mismatch_reports_error" >:: fun _ ->
           let program =
             {|
macro_rules! add { (a, b) => a + b }
let out = add!(1)
|}
           in
           assert_failure_contains ~expected_substring:"expected 2 argument(s)"
             ~f:(fun () -> ignore (run_program_interpreter_style program)) );
       ]

let () = run_test_tt_main suite
