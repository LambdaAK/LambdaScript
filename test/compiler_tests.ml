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

(** Drop carriage returns so fixture files can be edited on Windows. *)
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

(** Format: first line [Expected:], then expected stdout bytes until a line
    containing only [Source:], then the Forge source. *)
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

let repo_root_for_prelude () : string =
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

let list_case_files () : string list =
  let dir = compiler_cases_dir () in
  let entries = Sys.readdir dir |> Array.to_list in
  let cases =
    List.filter (fun name -> Filename.check_suffix name ".ls") entries
    |> List.map (fun name -> Filename.concat dir name)
    |> List.sort String.compare
  in
  if cases = [] then failwith ("No .ls compiler case files under " ^ dir);
  cases

let test_one case_path =
  let name = Filename.remove_extension (Filename.basename case_path) in
  name >:: fun _ ->
  let expected_stdout, program = parse_case_file case_path in
  let prelude = String.equal name "parser_expr_programs_test" in
  let repo_root = repo_root_for_prelude () in
  with_tmpdir @@ fun dir ->
  let src = Filename.concat dir "prog.ls" in
  let exe = Filename.concat dir "prog_out" in
  Out_channel.with_open_bin src (fun oc -> Out_channel.output_string oc program);
  let compile_result =
    if prelude then
      let old_cwd = Sys.getcwd () in
      Fun.protect
        ~finally:(fun () -> Sys.chdir old_cwd)
        (fun () ->
          Sys.chdir repo_root;
          Language.Compile_pipeline.compile ~quiet:true ~prelude src exe)
    else Language.Compile_pipeline.compile ~quiet:true ~prelude src exe
  in
  match compile_result with
  | Error msg -> assert_failure ("compile failed: " ^ msg)
  | Ok () ->
      let actual = run_exe_capture_stdout exe in
      assert_equal
        ~printer:(fun s -> Printf.sprintf "%S" s)
        expected_stdout actual

let suite = "compiler_integration" >::: List.map test_one (list_case_files ())
let () = run_test_tt_main suite
