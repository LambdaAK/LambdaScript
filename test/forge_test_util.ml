(** Shared helpers for the end-to-end test suites.

    Programs are run through the real executables ([interpreter.exe],
    [compile_forge.exe], [repl.exe], [forge_hover.exe]) as subprocesses with a
    timeout, so a crash, a hang or an uncaught OCaml exception in any stage
    shows up as a test failure instead of taking the test runner down. *)

open OUnit2

(* -------------------------------------------------------------------------- *)
(* Paths                                                                       *)
(* -------------------------------------------------------------------------- *)

let has_prelude dir = Sys.file_exists (Filename.concat dir "prelude/prelude.forge")

(** The source checkout (never the [_build/default] mirror). *)
let repo_root : string Lazy.t =
  lazy
    (let rec search_up dir =
       if has_prelude dir && not (Filename.basename dir = "default") then Some dir
       else
         let parent = Filename.dirname dir in
         if String.equal parent dir then None else search_up parent
     in
     match Sys.getenv_opt "DUNE_SOURCEROOT" with
     | Some root when has_prelude root -> root
     | _ -> (
         match search_up (Sys.getcwd ()) with
         | Some root -> root
         | None -> Sys.getcwd ()))

let repo () = Lazy.force repo_root

(** Path of a built executable from [bin/], e.g. [bin_exe "interpreter"]. *)
let bin_exe (name : string) : string =
  let file = name ^ ".exe" in
  let candidates =
    [
      Filename.concat (repo ()) ("_build/default/bin/" ^ file);
      Filename.concat (Sys.getcwd ()) ("_build/default/bin/" ^ file);
      Filename.concat (Sys.getcwd ()) ("bin/" ^ file);
      Filename.concat (Filename.dirname Sys.executable_name) ("../bin/" ^ file);
    ]
  in
  match List.find_opt Sys.file_exists candidates with
  | Some p -> p
  | None -> failwith ("cannot find built executable " ^ file)

(* -------------------------------------------------------------------------- *)
(* Files                                                                       *)
(* -------------------------------------------------------------------------- *)

let rm_rf dir =
  ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir)) : int)

let with_tmpdir ?(prefix = "forge_test_") f =
  let path = Filename.temp_file prefix "" in
  Unix.unlink path;
  Unix.mkdir path 0o700;
  Fun.protect ~finally:(fun () -> rm_rf path) (fun () -> f path)

let write_file path contents =
  Out_channel.with_open_bin path (fun oc -> Out_channel.output_string oc contents)

let read_file path = In_channel.with_open_bin path In_channel.input_all

(* -------------------------------------------------------------------------- *)
(* Processes with timeouts                                                     *)
(* -------------------------------------------------------------------------- *)

type status = Exited of int | Signaled of int | Timed_out

type result = { status : status; out : string; err : string }

let string_of_status = function
  | Exited n -> Printf.sprintf "exit %d" n
  | Signaled n -> Printf.sprintf "signal %d" n
  | Timed_out -> "timeout"

let describe (r : result) : string =
  Printf.sprintf "[%s]\n--- stdout ---\n%s\n--- stderr ---\n%s" (string_of_status r.status)
    r.out r.err

(** Run [prog args] with an optional [stdin] string, killing it after
    [timeout] seconds. Reads stdout and stderr concurrently so large outputs
    cannot deadlock the pipes. *)
let run_process ?(timeout = 20.0) ?(stdin = "") ?cwd ?(env = Unix.environment ())
    (prog : string) (args : string list) : result =
  let in_r, in_w = Unix.pipe ~cloexec:true () in
  let out_r, out_w = Unix.pipe ~cloexec:true () in
  let err_r, err_w = Unix.pipe ~cloexec:true () in
  let old_cwd = Sys.getcwd () in
  (match cwd with Some d -> Sys.chdir d | None -> ());
  let pid =
    Fun.protect
      ~finally:(fun () -> Sys.chdir old_cwd)
      (fun () ->
        Unix.create_process_env prog (Array.of_list (prog :: args)) env in_r out_w
          err_w)
  in
  Unix.close in_r;
  Unix.close out_w;
  Unix.close err_w;
  (* Feed stdin (small inputs only; tests never pipe megabytes). *)
  (try
     let b = Bytes.of_string stdin in
     let rec write_all off =
       if off < Bytes.length b then
         let n = Unix.write in_w b off (Bytes.length b - off) in
         write_all (off + n)
     in
     write_all 0
   with Unix.Unix_error _ -> ());
  Unix.close in_w;
  let out_buf = Buffer.create 4096 and err_buf = Buffer.create 1024 in
  let chunk = Bytes.create 65536 in
  let deadline = Unix.gettimeofday () +. timeout in
  let open_fds = ref [ out_r; err_r ] in
  let timed_out = ref false in
  while !open_fds <> [] && not !timed_out do
    let remaining = deadline -. Unix.gettimeofday () in
    if remaining <= 0.0 then timed_out := true
    else
      let ready, _, _ =
        try Unix.select !open_fds [] [] (min remaining 0.5)
        with Unix.Unix_error (Unix.EINTR, _, _) -> ([], [], [])
      in
      List.iter
        (fun fd ->
          let n = try Unix.read fd chunk 0 (Bytes.length chunk) with _ -> 0 in
          if n = 0 then (
            Unix.close fd;
            open_fds := List.filter (fun f -> f != fd) !open_fds)
          else
            Buffer.add_subbytes (if fd == out_r then out_buf else err_buf) chunk 0 n)
        ready
  done;
  if !timed_out then (
    (try Unix.kill pid Sys.sigkill with _ -> ());
    List.iter (fun fd -> try Unix.close fd with _ -> ()) !open_fds);
  let _, st = Unix.waitpid [] pid in
  let status =
    if !timed_out then Timed_out
    else
      match st with
      | Unix.WEXITED n -> Exited n
      | Unix.WSIGNALED n -> Signaled n
      | Unix.WSTOPPED n -> Signaled n
  in
  { status; out = Buffer.contents out_buf; err = Buffer.contents err_buf }

(* -------------------------------------------------------------------------- *)
(* Running Forge programs                                                      *)
(* -------------------------------------------------------------------------- *)

(** Run [source] through the interpreter executable (with the prelude, which
    is how users run programs). *)
let interp ?(timeout = 20.0) ?(file_name = "prog.forge") (source : string) : result =
  with_tmpdir @@ fun dir ->
  let src = Filename.concat dir file_name in
  write_file src source;
  run_process ~timeout ~cwd:(repo ()) (bin_exe "interpreter") [ src ]

(** Compile [source] with [compile_forge.exe] and run the executable.
    Returns [Error compile_result] if compilation fails. *)
let native ?(timeout = 20.0) (source : string) : (result, result) Stdlib.result =
  with_tmpdir @@ fun dir ->
  let src = Filename.concat dir "prog.forge" in
  let exe = Filename.concat dir "prog.out" in
  write_file src source;
  let c = run_process ~timeout:120.0 ~cwd:(repo ()) (bin_exe "compile_forge") [ src; exe ] in
  match c.status with
  | Exited 0 when Sys.file_exists exe -> Ok (run_process ~timeout ~cwd:dir exe [])
  | _ -> Error c

(* -------------------------------------------------------------------------- *)
(* Assertions                                                                  *)
(* -------------------------------------------------------------------------- *)

let contains (s : string) (sub : string) : bool =
  let ls = String.length s and lsub = String.length sub in
  let rec go i = i + lsub <= ls && (String.sub s i lsub = sub || go (i + 1)) in
  lsub = 0 || go 0

let lowercase_contains s sub = contains (String.lowercase_ascii s) (String.lowercase_ascii sub)

let show_src src = "\n--- source ---\n" ^ src

(** Program must exit 0 and print exactly [expected] on stdout. *)
let assert_runs ?(msg = "") ~(expected : string) (label : string) (src : string)
    (r : result) : unit =
  match r.status with
  | Exited 0 ->
      if r.out <> expected then
        assert_failure
          (Printf.sprintf "%s%s: wrong output\nexpected: %S\n     got: %S%s" msg label
             expected r.out (show_src src))
  | _ -> assert_failure (Printf.sprintf "%s%s: expected success, got %s%s" msg label (describe r) (show_src src))

(** Messages that leak implementation exceptions instead of a diagnostic. *)
let internal_markers =
  [
    "Failure(";
    "Invalid_argument";
    "Not_found";
    "Stack_overflow";
    "Out_of_memory";
    "Fatal error";
    "exception";
    "Assert_failure";
    "Match_failure";
    "impossible";
    "internal:";
  ]

(** Program must fail with a non-zero exit code (not a signal, not a hang),
    print a non-empty diagnostic that does not leak an OCaml exception, and —
    if [mentions] is given — the diagnostic must contain each of those
    substrings (case-insensitive). *)
let assert_clean_error ?(mentions = []) (label : string) (src : string) (r : result) : unit =
  let text = r.out ^ r.err in
  (match r.status with
  | Exited n when n <> 0 && n < 128 -> ()
  | _ ->
      assert_failure
        (Printf.sprintf "%s: expected a clean error exit, got %s%s" label (describe r)
           (show_src src)));
  if String.trim text = "" then
    assert_failure (Printf.sprintf "%s: error exit but no diagnostic printed%s" label (show_src src));
  List.iter
    (fun m ->
      if contains text m then
        assert_failure
          (Printf.sprintf "%s: diagnostic leaks internal error %S:\n%s%s" label m text
             (show_src src)))
    internal_markers;
  List.iter
    (fun m ->
      if not (lowercase_contains text m) then
        assert_failure
          (Printf.sprintf "%s: diagnostic should mention %S:\n%s%s" label m text
             (show_src src)))
    mentions

(** Interpreter and native executable must agree exactly on stdout and exit
    status, and print [expected] (when given). *)
let assert_both ?expected (label : string) (src : string) : unit =
  let ri = interp src in
  (match expected with
  | Some e -> assert_runs ~msg:"[interpreter] " ~expected:e label src ri
  | None -> ());
  match native src with
  | Error c ->
      assert_failure
        (Printf.sprintf "%s: native compilation failed (interpreter: %s)\n%s%s" label
           (string_of_status ri.status) (describe c) (show_src src))
  | Ok rn ->
      if rn.status <> ri.status || rn.out <> ri.out then
        assert_failure
          (Printf.sprintf
             "%s: interpreter and native disagree\n\
              interpreter [%s]: %S\n\
             \     native [%s]: %S%s"
             label (string_of_status ri.status) ri.out (string_of_status rn.status) rn.out
             (show_src src))

(** Both back ends must fail at run time: a non-zero exit code (not a signal
    or a hang), after printing exactly [stdout] (the output produced before
    the error; diagnostics belong on stderr). *)
let assert_both_runtime_error ?(stdout = "") (label : string) (src : string) : unit =
  let check which (r : result) =
    (match r.status with
    | Exited n when n <> 0 -> ()
    | _ ->
        assert_failure
          (Printf.sprintf "%s [%s]: expected a run-time error exit, got %s%s" label which
             (describe r) (show_src src)));
    if r.out <> stdout then
      assert_failure
        (Printf.sprintf "%s [%s]: expected stdout %S before the error, got %S%s" label which
           stdout r.out (show_src src));
    if String.trim r.err = "" then
      assert_failure
        (Printf.sprintf "%s [%s]: run-time error without a diagnostic on stderr%s" label which
           (show_src src))
  in
  check "interpreter" (interp src);
  match native src with
  | Error c ->
      assert_failure
        (Printf.sprintf "%s: native compilation failed\n%s%s" label (describe c)
           (show_src src))
  | Ok rn -> check "native" rn

(* -------------------------------------------------------------------------- *)
(* In-process REPL kernel (fast type queries)                                  *)
(* -------------------------------------------------------------------------- *)

type repl_state =
  Language.Cexpr.static_env * Language.Cexpr.env * Language.Typecheck.type_env

let prelude_state : repl_state Lazy.t =
  lazy
    (Language.Repl_kernel.clear_prelude_condense_cache ();
     let se = Language.Build_env.build_full_static_env () in
     let de = Language.Ceval.initial_env () |> Language.Ceval.unwrap_eval_result in
     let old = Sys.getcwd () in
     Sys.chdir (repo ());
     let r =
       Fun.protect ~finally:(fun () -> Sys.chdir old) (fun () ->
           Language.Repl_kernel.merge_prelude se de [])
     in
     match r with Ok st -> st | Error m -> failwith ("prelude: " ^ m))

(** Evaluate a sequence of REPL inputs starting from a fresh prelude state;
    returns the outcome of the last one. *)
let repl_eval (inputs : string list) : Language.Repl_kernel.eval_outcome =
  ignore (Lazy.force prelude_state);
  (* Each query starts from the prelude: drop user definitions remembered by
     a previous query. *)
  Language.Repl_kernel.user_defns_for_condense := [];
  Language.Repl_kernel.user_condensed_defn_count := 0;
  let st = Lazy.force prelude_state in
  let rec go (se, de, te) = function
    | [] -> failwith "repl_eval: no input"
    | [ last ] ->
        let o, _, _, _ = Language.Repl_kernel.eval_user_input se de te last in
        o
    | x :: rest ->
        let o, se', de', te' = Language.Repl_kernel.eval_user_input se de te x in
        (match o with
        | Language.Repl_kernel.Ev_error m ->
            failwith (Printf.sprintf "setup input %S failed: %s" x m)
        | _ -> ());
        go (se', de', te') rest
  in
  go st inputs
