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
        (Printf.sprintf "Program parse left %d trailing tokens: %s"
           (List.length rem)
           (String.concat " "
              (List.map Language.Lex.string_of_token_type rem)))
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
macro_rules! add1 { ($x:expr) => $x + 1 }
let out = add1!(41)
|}
           in
           let _, dynamic_env, _ = run_program_interpreter_style program in
           assert_runtime_value ~env:dynamic_env ~name:"out" ~expected:"42" );
         ( "macro_rules_nested_expansion" >:: fun _ ->
           let program =
             {|
macro_rules! add { ($a:expr, $b:expr) => $a + $b }
macro_rules! twice { ($x:expr) => add!($x, $x) }
let out = twice!(21)
|}
           in
           let _, dynamic_env, _ = run_program_interpreter_style program in
           assert_runtime_value ~env:dynamic_env ~name:"out" ~expected:"42" );
         ( "macro_rules_module_local_scope" >:: fun _ ->
           let program =
             {|
mod M where
  macro_rules! inc { ($x:expr) => $x + 1 }
  let out = inc!(41)
end
|}
           in
           let _, dynamic_env, _ = run_program_interpreter_style program in
           assert_runtime_value ~env:dynamic_env ~name:"M.out" ~expected:"42" );
         ( "macro_rules_multi_arm_dispatch" >:: fun _ ->
           let program =
             {|
macro_rules! choose {
  () => 0;
  ($x:expr) => $x;
}
let a = choose!()
let b = choose!(42)
|}
           in
           let _, dynamic_env, _ = run_program_interpreter_style program in
           assert_runtime_value ~env:dynamic_env ~name:"a" ~expected:"0";
           assert_runtime_value ~env:dynamic_env ~name:"b" ~expected:"42" );
         ( "macro_rules_support_bracket_and_brace_invocation_delimiters"
         >:: fun _ ->
           let program =
             {|
macro_rules! choose {
  () => 0;
  ($x:expr) => $x;
}
let a = choose![]
let b = choose!{42}
|}
           in
           let _, dynamic_env, _ = run_program_interpreter_style program in
           assert_runtime_value ~env:dynamic_env ~name:"a" ~expected:"0";
           assert_runtime_value ~env:dynamic_env ~name:"b" ~expected:"42" );
         ( "macro_rules_repeat_matcher_binds_list_expression" >:: fun _ ->
           let program =
             {|
macro_rules! collect {
  ($($x:expr),*) => $x;
}
let out = list_length (collect!(1, 2, 3, 4))
|}
           in
           let _, dynamic_env, _ = run_program_interpreter_style program in
           assert_runtime_value ~env:dynamic_env ~name:"out" ~expected:"4" );
         ( "macro_rules_transcriber_repetition_splices_into_args" >:: fun _ ->
           let program =
             {|
macro_rules! passthrough {
  ($($x:expr),*) => vec!($($x),*);
}
let out = list_length (passthrough!(1, 2, 3, 4))
|}
           in
           let _, dynamic_env, _ = run_program_interpreter_style program in
           assert_runtime_value ~env:dynamic_env ~name:"out" ~expected:"4" );
         ( "macro_rules_repeat_plus_requires_one_or_more" >:: fun _ ->
           let program =
             {|
macro_rules! collect1 {
  ($($x:expr),+) => $x;
}
let out = collect1!()
|}
           in
           assert_failure_contains ~expected_substring:"no matching arm"
             ~f:(fun () -> ignore (run_program_interpreter_style program)) );
         ( "macro_rules_ident_fragment" >:: fun _ ->
           let program =
             {|
macro_rules! id1 { ($x:ident) => $x }
let n = 42
let out = id1!(n)
|}
           in
           let _, dynamic_env, _ = run_program_interpreter_style program in
           assert_runtime_value ~env:dynamic_env ~name:"out" ~expected:"42" );
         ( "macro_rules_literal_fragment" >:: fun _ ->
           let program =
             {|
macro_rules! lit_id { ($x:literal) => $x }
let a = lit_id!(42)
let b = lit_id!("ok")
|}
           in
           let _, dynamic_env, _ = run_program_interpreter_style program in
           assert_runtime_value ~env:dynamic_env ~name:"a" ~expected:"42";
           assert_runtime_value ~env:dynamic_env ~name:"b" ~expected:"\"ok\"" );
         ( "macro_rules_type_fragment_accepts_type_path_like_expression"
         >:: fun _ ->
           let program =
             {|
macro_rules! show_ty { ($t:ty) => "ok" }
let out = show_ty!(Int)
|}
           in
           let _, dynamic_env, _ = run_program_interpreter_style program in
           assert_runtime_value ~env:dynamic_env ~name:"out" ~expected:"\"ok\"" );
         ( "macro_rules_path_fragment" >:: fun _ ->
           let program =
             {|
mod M where
  let v = 42
end
macro_rules! use_path { ($p:path) => $p }
let out = use_path!(M.v)
|}
           in
           let _, dynamic_env, _ = run_program_interpreter_style program in
           assert_runtime_value ~env:dynamic_env ~name:"out" ~expected:"42" );
         ( "proc_macro_count_args_builtin" >:: fun _ ->
           let program =
             {|
let out = count_args!(10, 20, 30, 40)
|}
           in
           let _, dynamic_env, _ = run_program_interpreter_style program in
           assert_runtime_value ~env:dynamic_env ~name:"out" ~expected:"4" );
         ( "proc_macro_vec_and_concat_builtins" >:: fun _ ->
           let program =
             {|
let vlen = list_length(vec!(10, 20, 30))
let s = concat!("ab", "cd", "ef")
|}
           in
           let _, dynamic_env, _ = run_program_interpreter_style program in
           assert_runtime_value ~env:dynamic_env ~name:"vlen" ~expected:"3";
           assert_runtime_value ~env:dynamic_env ~name:"s" ~expected:"\"abcdef\"" );
         ( "proc_macro_concat_accepts_nested_stringify_result" >:: fun _ ->
           let program =
             {|
macro_rules! debug_expr {
  ($e:expr) => concat!("DBG(", stringify!($e), ")");
}
let label = debug_expr!(1 + 2 * 3)
|}
           in
           let _, dynamic_env, _ = run_program_interpreter_style program in
           let got = lookup_value_exn dynamic_env "label" in
           assert_bool "stringify/concat should produce a debug label prefix"
             (string_contains_substring got "\"DBG(");
           assert_bool "stringify should include expression structure"
             (string_contains_substring got "Plus (") );
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
macro_rules! add { ($a:expr, $b:expr) => $a + $b }
let out = add!(1)
|}
           in
           assert_failure_contains ~expected_substring:"no matching arm"
             ~f:(fun () -> ignore (run_program_interpreter_style program)) );
       ]

let () = run_test_tt_main suite
