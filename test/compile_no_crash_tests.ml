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
  let path = Filename.temp_file "ls_compile_no_crash_" "" in
  Unix.unlink path;
  Unix.mkdir path 0o700;
  Fun.protect ~finally:(fun () -> rm_rf path) (fun () -> f path)

let repo_root_for_paths () : string =
  let has_prelude dir =
    Sys.file_exists (Filename.concat dir "prelude/prelude.ls")
  in
  let rec search_up dir =
    if has_prelude dir then Some dir
    else
      let parent = Filename.dirname dir in
      if String.equal parent dir then None else search_up parent
  in
  match Sys.getenv_opt "DUNE_SOURCEROOT" with
  | Some root when has_prelude root -> root
  | _ -> (
      match search_up (Sys.getcwd ()) with
      | Some root -> root
      | None -> Sys.getcwd ())

let programs_test_path () : string =
  Filename.concat (repo_root_for_paths ()) "programs/test.ls"

let interpreter_exe_path () : string =
  let root = repo_root_for_paths () in
  let candidates =
    [
      Filename.concat root "_build/default/bin/interpreter.exe";
      Filename.concat (Sys.getcwd ()) "_build/default/bin/interpreter.exe";
      Filename.concat (Filename.dirname Sys.executable_name) "../bin/interpreter.exe";
      Filename.concat (Filename.dirname Sys.executable_name) "interpreter.exe";
    ]
  in
  match List.find_opt Sys.file_exists candidates with
  | Some p -> p
  | None ->
      failwith
        "Cannot find interpreter.exe; build artifacts missing at \
         _build/default/bin/interpreter.exe"

let run_interpreter_capture_stdout (src_path : string) : string =
  let exe = interpreter_exe_path () in
  run_process_capture_stdout exe [| exe; src_path |]

let compile_programs_test_never_raises _ =
  with_tmpdir @@ fun dir ->
  let out = Filename.concat dir "prog_out" in
  let src = programs_test_path () in
  match
    try Ok (Language.Compile_pipeline.compile ~quiet:true src out)
    with exn -> Error (Printexc.to_string exn)
  with
  | Error exn_str ->
      assert_failure ("compile raised an exception instead of returning Error: " ^ exn_str)
  | Ok _ -> ()

let compile_programs_test_matches_interpreter _ =
  with_tmpdir @@ fun dir ->
  let out = Filename.concat dir "prog_out" in
  let src = programs_test_path () in
  let interpreter_out = run_interpreter_capture_stdout src in
  let compiled_out =
    match Language.Compile_pipeline.compile ~quiet:true src out with
    | Error msg -> assert_failure ("compile failed: " ^ msg)
    | Ok () -> run_exe_capture_stdout out
  in
  assert_equal ~printer:(fun s -> Printf.sprintf "%S" s) interpreter_out
    compiled_out

let suite =
  "compile_no_crash"
  >::: [
         "compile_programs_test_never_raises"
         >:: compile_programs_test_never_raises;
         "compile_programs_test_matches_interpreter"
         >:: compile_programs_test_matches_interpreter;
       ]

let () = run_test_tt_main suite
