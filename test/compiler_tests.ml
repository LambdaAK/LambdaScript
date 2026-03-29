open OUnit2

let run_exe_capture_stdout exe =
  let ic = Unix.open_process_args_in exe [| exe |] in
  let contents = In_channel.input_all ic in
  match Unix.close_process_in ic with
  | Unix.WEXITED 0 -> contents
  | Unix.WEXITED n ->
      assert_failure
        (Printf.sprintf "executable %S exited %d; stdout: %S" exe n contents)
  | Unix.WSIGNALED s ->
      assert_failure
        (Printf.sprintf "executable %S signal %d; stdout: %S" exe s contents)
  | Unix.WSTOPPED s ->
      assert_failure
        (Printf.sprintf "executable %S stopped %d; stdout: %S" exe s contents)

let rm_rf dir =
  ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir)) : int)

let with_tmpdir f =
  (* Avoid [Unix.mkdtemp] for compatibility with older OCaml Unix bindings. *)
  let path = Filename.temp_file "ls_compile_" "" in
  Unix.unlink path;
  Unix.mkdir path 0o700;
  Fun.protect ~finally:(fun () -> rm_rf path) (fun () -> f path)

(** Programs paired with the exact bytes expected on stdout (including any
    final newline from [println] / [puts]). *)
let compiler_cases : (string * string * string) list =
  [
    ( "factorial",
      {|
let fact n =
  let rec fact_helper n acc =
    if n == 0 then acc
    else fact_helper (n - 1) (n * acc)
  in
  fact_helper n 1

let () = println (int_to_str (fact 5))
|},
      "120\n" );
    ( "println string",
      {|let () = println "hello"|},
      "hello\n" );
  ]

let test_one (name, program, expected_stdout) =
  name >:: fun _ ->
    with_tmpdir @@ fun dir ->
    let src = Filename.concat dir "prog.ls" in
    let exe = Filename.concat dir "prog_out" in
    let oc = open_out src in
    output_string oc program;
    close_out oc;
    match Language.Compile_pipeline.compile ~quiet:true src exe with
    | Error msg -> assert_failure ("compile failed: " ^ msg)
    | Ok () ->
        let actual = run_exe_capture_stdout exe in
        assert_equal ~printer:(fun s -> Printf.sprintf "%S" s) expected_stdout
          actual

let suite =
  "compiler_integration"
  >::: List.map test_one compiler_cases

let () = run_test_tt_main suite
