open OUnit2

let run_process_capture_stdout (prog : string) (argv : string array) : string =
  let ic = Unix.open_process_args_in prog argv in
  let contents = In_channel.input_all ic in
  match Unix.close_process_in ic with
  | Unix.WEXITED 0 -> contents
  | Unix.WEXITED n ->
      assert_failure
        (Printf.sprintf "process %S exited %d; stdout: %S" prog n contents)
  | Unix.WSIGNALED s ->
      assert_failure
        (Printf.sprintf "process %S signaled %d; stdout: %S" prog s contents)
  | Unix.WSTOPPED s ->
      assert_failure
        (Printf.sprintf "process %S stopped %d; stdout: %S" prog s contents)

let run_exe_capture_stdout exe =
  run_process_capture_stdout exe [| exe |]

let rm_rf dir =
  ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir)) : int)

let with_tmpdir f =
  let path = Filename.temp_file "ls_compile_parity_" "" in
  Unix.unlink path;
  Unix.mkdir path 0o700;
  Fun.protect ~finally:(fun () -> rm_rf path) (fun () -> f path)

let drop_cr s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | '\r' -> ()
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let find_sub_from (s : string) (sub : string) (from : int) : int option =
  let len_s = String.length s and len_sub = String.length sub in
  let rec go i =
    if i + len_sub > len_s then None
    else if String.sub s i len_sub = sub then Some i
    else go (i + 1)
  in
  go from

let parse_case_file path : string * string =
  let content =
    In_channel.with_open_bin path (fun ic -> In_channel.input_all ic) |> drop_cr
  in
  let prefix = "Expected:" in
  if not (String.starts_with ~prefix content) then
    invalid_arg (path ^ ": must start with Expected:");
  let after_label = String.length prefix in
  if after_label >= String.length content || not (content.[after_label] = '\n')
  then invalid_arg (path ^ ": Expected: must be followed by a newline");
  let body_start = after_label + 1 in
  let marker = "\nSource:\n" in
  match find_sub_from content marker body_start with
  | None ->
      invalid_arg (path ^ ": missing newline + Source: + newline delimiter")
  | Some idx ->
      let expected = String.sub content body_start (idx - body_start) in
      let source_start = idx + String.length marker in
      let source =
        if source_start >= String.length content then ""
        else
          String.sub content source_start (String.length content - source_start)
      in
      (expected, source)

let compiler_cases_dir () : string =
  let candidates =
    [
      Filename.concat (Sys.getcwd ()) "test/compiler_cases";
      Filename.concat
        (Filename.dirname Sys.executable_name)
        "test/compiler_cases";
      Filename.concat (Filename.dirname Sys.executable_name) "compiler_cases";
    ]
  in
  match List.find_opt Sys.file_exists candidates with
  | Some d -> d
  | None ->
      failwith
        "Cannot find compiler_cases — run tests from the workspace root (e.g. \
         dune runtest with (chdir %{workspace_root} ...)) or ensure \
         test/compiler_cases exists."

let capture_stdout (f : unit -> unit) : string =
  flush stdout;
  let tmp = Filename.temp_file "ls_stdout_" ".txt" in
  let stdout_fd = Unix.descr_of_out_channel stdout in
  let saved_fd = Unix.dup stdout_fd in
  let tmp_oc = open_out_bin tmp in
  let tmp_fd = Unix.descr_of_out_channel tmp_oc in
  Fun.protect
    ~finally:(fun () ->
      flush stdout;
      Unix.dup2 saved_fd stdout_fd;
      Unix.close saved_fd;
      close_out_noerr tmp_oc)
    (fun () ->
      Unix.dup2 tmp_fd stdout_fd;
      f ();
      flush stdout);
  let out = In_channel.with_open_bin tmp In_channel.input_all in
  Sys.remove tmp;
  out

let run_interpreter_capture_stdout ?(prelude = true) (src_path : string) : string
    =
  capture_stdout (fun () ->
      let file_contents =
        Language.Compile_pipeline.read_program_source ~prelude src_path
      in
      let full_tokens =
        Language.Lex.lex (file_contents |> String.to_seq |> List.of_seq)
      in
      Language.Condense.set_id_queue_from_tokens full_tokens;
      let tokens = List.map (fun t -> t.Language.Lex.token_type) full_tokens in
      let parse_out = Language.Parser.ProgramParser.program_parser tokens in
      let condensed_program =
        Fun.protect
          ~finally:Language.Condense.clear_id_queue
          (fun () ->
            match parse_out with
            | None -> assert_failure "interpreter parse failed"
            | Some (_, remaining) when remaining <> [] ->
                assert_failure
                  (Printf.sprintf
                     "interpreter parse left %d trailing tokens"
                     (List.length remaining))
            | Some (program, _) ->
                let program =
                  Language.Import_resolve.resolve_program ~root_file:src_path
                    ~base_dir:(Filename.dirname src_path)
                    program
                in
                Language.Condense.condense_program program)
      in
      let static_env = Language.Build_env.build_full_static_env () in
      let dynamic_env =
        Language.Ceval.initial_env () |> Language.Ceval.unwrap_eval_result
      in
      let type_env = [] in
      ignore
        (List.fold_left
           (fun (static_env, dynamic_env, type_env) defn ->
             match Language.Typecheck.generate_defn static_env type_env defn with
             | Language.Typecheck.Error e ->
                 assert_failure
                   ("Type error: "
                  ^ Language.Typecheck.string_of_type_check_error e)
             | Language.Typecheck.Ok
                 (new_bindings, new_type_bindings, _new_ctor_env) ->
                 let elaborated =
                   Language.Typecheck.elaborate_defn
                     ~rewrite_constrained_calls:true
                     (new_bindings @ static_env)
                     (new_type_bindings @ type_env) defn
                 in
                 let new_dynamic_bindings =
                   match Language.Ceval.eval_defn elaborated dynamic_env with
                   | Language.Ceval.Ok v -> v
                   | Language.Ceval.Error e ->
                       assert_failure
                         ("Evaluation error: "
                        ^ Language.Ceval.string_of_eval_error e)
                 in
                 ( new_bindings @ static_env,
                   new_dynamic_bindings @ dynamic_env,
                   new_type_bindings @ type_env ))
           (static_env, dynamic_env, type_env)
           condensed_program))

let compile_and_capture_stdout ?(prelude = true) (src_path : string)
    (out_path : string) : string =
  match
    Language.Compile_pipeline.compile ~quiet:true ~prelude src_path out_path
  with
  | Error msg -> assert_failure ("compile failed: " ^ msg)
  | Ok () -> run_exe_capture_stdout out_path

let parity_case_files =
  [
    "regression_elab_scope_shadow_x.forge";
    "regression_top_level_let_rec_non_function.forge";
    "regression_top_level_let_rec_wildcard_non_function.forge";
    "show_linkedlist_recursive_dispatch.forge";
    "show_linkedlist_recursive_dispatch_deep.forge";
    "typeclass_recursive_method_indirect_self_call.forge";
    "typeclass_recursive_method_shadowing.forge";
    "typeclass_constrained_helper_dispatch.forge";
    "typeclass_haskell_core.forge";
    "typeclass_operator_method_alias.forge";
    "typeclass_show_empty_list_dispatch.forge";
    "real_budget_ledger_monthly_report.forge";
    "real_customer_tiers.forge";
    "real_inventory_planner.forge";
    "real_project_scheduler.forge";
    "real_route_planner.forge";
    "real_sales_funnel.forge";
    "real_sensor_alerts.forge";
  ]

let test_one case_name =
  Filename.remove_extension case_name >:: fun _ ->
  let case_path = Filename.concat (compiler_cases_dir ()) case_name in
  let expected_stdout, source = parse_case_file case_path in
  let prelude =
    String.equal case_name "parser_expr_programs_test.forge"
    || String.equal case_name "regression_elab_scope_shadow_x.forge"
  in
  with_tmpdir @@ fun dir ->
  let src = Filename.concat dir "prog.forge" in
  let exe = Filename.concat dir "prog_out" in
  Out_channel.with_open_bin src (fun oc -> Out_channel.output_string oc source);
  let interpreter_stdout = run_interpreter_capture_stdout ~prelude src in
  let compiled_stdout = compile_and_capture_stdout ~prelude src exe in
  assert_equal
    ~printer:(fun s -> Printf.sprintf "%S" s)
    ~msg:(case_name ^ ": interpreter output should match fixture expected output")
    expected_stdout interpreter_stdout;
  assert_equal
    ~printer:(fun s -> Printf.sprintf "%S" s)
    ~msg:(case_name ^ ": compiler output should match interpreter output")
    interpreter_stdout compiled_stdout

let suite =
  "compiler_interpreter_parity"
  >::: List.map test_one parity_case_files

let () = run_test_tt_main suite
