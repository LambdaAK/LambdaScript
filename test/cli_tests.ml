(** Command-line entry points: interpreter, compile_forge, repl, forge_hover,
    dump_min_ir — usage errors, bad paths, empty files, exit codes. *)

open OUnit2
open Forge_test_util

let exe = bin_exe

let expect_usage name prog args =
  name >:: fun _ ->
  let r = run_process ~cwd:(repo ()) (exe prog) args in
  (match r.status with
  | Exited n when n <> 0 && n < 128 -> ()
  | _ -> assert_failure ("expected a usage error exit: " ^ describe r));
  if not (lowercase_contains (r.out ^ r.err) "usage") then
    assert_failure ("expected a usage message: " ^ describe r)

let clean_error ?(mentions = []) name prog args =
  name >:: fun _ ->
  let r = run_process ~cwd:(repo ()) (exe prog) args in
  assert_clean_error ~mentions name (String.concat " " (prog :: args)) r

let interpreter_tests =
  [
    expect_usage "interpreter: no arguments" "interpreter" [];
    expect_usage "interpreter: too many arguments" "interpreter" [ "a.forge"; "b.forge" ];
    clean_error ~mentions:[ "no_such_file.forge" ] "interpreter: missing file" "interpreter"
      [ "/nonexistent/no_such_file.forge" ];
    clean_error "interpreter: directory instead of file" "interpreter" [ "/tmp" ];
    ( "interpreter: empty file runs and prints nothing" >:: fun _ ->
      let r = interp "" in
      assert_runs ~expected:"" "empty" "" r;
      assert_equal ~printer:(Printf.sprintf "%S") "" r.err );
    ( "interpreter: comments-only file" >:: fun _ ->
      let src = "// just a comment\n/* and a block */\n" in
      assert_runs ~expected:"" "comments" src (interp src) );
    ( "interpreter: whitespace-only file" >:: fun _ -> assert_runs ~expected:"" "ws" "  \n\n\t" (interp "  \n\n\t") );
    ( "interpreter: errors go to stderr, not stdout" >:: fun _ ->
      let r = interp "let x = 1 + true" in
      assert_equal ~printer:(Printf.sprintf "%S") ~msg:"stdout should be empty" "" r.out;
      assert_bool "stderr should carry the error" (String.trim r.err <> "") );
    ( "interpreter: type error exit code is 1" >:: fun _ ->
      let r = interp "let x = 1 + true" in
      assert_equal ~printer:string_of_status (Exited 1) r.status );
    ( "interpreter: parse error exit code is 1" >:: fun _ ->
      let r = interp "let x = (" in
      assert_equal ~printer:string_of_status (Exited 1) r.status );
    ( "interpreter: runtime error exit code is 1" >:: fun _ ->
      let r = interp "let x = 1 / 0" in
      assert_equal ~printer:string_of_status (Exited 1) r.status );
    ( "interpreter: success exit code is 0" >:: fun _ ->
      let r = interp "let () = print_string \"hi\"" in
      assert_equal ~printer:string_of_status (Exited 0) r.status );
    ( "interpreter: path with spaces" >:: fun _ ->
      let r = interp ~file_name:"my program.forge" "let () = print_string \"spaces ok\"" in
      assert_runs ~expected:"spaces ok\n" "spaces" "" r );
    ( "interpreter: runs from another working directory" >:: fun _ ->
      with_tmpdir @@ fun dir ->
      let src = Filename.concat dir "p.forge" in
      write_file src "let () = println [1, 2]";
      let r = run_process ~cwd:dir (exe "interpreter") [ src ] in
      assert_runs ~expected:"[1, 2]\n" "other cwd" "" r );
    ( "interpreter: relative path" >:: fun _ ->
      with_tmpdir @@ fun dir ->
      write_file (Filename.concat dir "p.forge") "let () = print_string \"rel\"";
      let r = run_process ~cwd:dir (exe "interpreter") [ "p.forge" ] in
      assert_runs ~expected:"rel\n" "relative" "" r );
    ( "interpreter: file without trailing newline" >:: fun _ ->
      let src = "let () = print_string \"x\"" in
      assert_runs ~expected:"x\n" "no newline" src (interp src) );
    ( "interpreter: output is flushed before a runtime error" >:: fun _ ->
      let src = "let () = print \"partial\"\nlet x = 1 / 0" in
      let r = interp src in
      assert_equal ~printer:(Printf.sprintf "%S") "partial" r.out );
    ( "interpreter: large output" >:: fun _ ->
      let src = "let rec go n = if n == 0 then () else { print_string \"line\"; go (n - 1) }\nlet () = go 20000" in
      let r = interp ~timeout:30.0 src in
      assert_equal ~printer:string_of_int (20000 * 5) (String.length r.out) );
  ]

let compiler_tests =
  [
    expect_usage "compile_forge: no arguments" "compile_forge" [];
    expect_usage "compile_forge: too many arguments" "compile_forge" [ "a"; "b"; "c" ];
    clean_error ~mentions:[ "no_such_file.forge" ] "compile_forge: missing file" "compile_forge"
      [ "/nonexistent/no_such_file.forge"; "/tmp/forge_never_written" ];
    ( "compile_forge: parse error is a clean error" >:: fun _ ->
      with_tmpdir @@ fun dir ->
      let src = Filename.concat dir "bad.forge" in
      write_file src "let x = (";
      let r = run_process ~cwd:(repo ()) (exe "compile_forge") [ src; Filename.concat dir "out" ] in
      assert_clean_error "compile parse error" "let x = (" r );
    ( "compile_forge: type error is a clean error" >:: fun _ ->
      with_tmpdir @@ fun dir ->
      let src = Filename.concat dir "bad.forge" in
      write_file src "let x = 1 + true";
      let r = run_process ~cwd:(repo ()) (exe "compile_forge") [ src; Filename.concat dir "out" ] in
      assert_clean_error "compile type error" "let x = 1 + true" r );
    ( "compile_forge: unwritable output directory" >:: fun _ ->
      with_tmpdir @@ fun dir ->
      let src = Filename.concat dir "ok.forge" in
      write_file src "let () = print_string \"x\"";
      let r =
        run_process ~cwd:(repo ()) (exe "compile_forge") [ src; "/nonexistent_dir_forge/out" ]
      in
      assert_clean_error "unwritable output" "" r );
    ( "compile_forge: paths with spaces" >:: fun _ ->
      with_tmpdir @@ fun dir ->
      let sub = Filename.concat dir "has space" in
      Unix.mkdir sub 0o755;
      let src = Filename.concat sub "my prog.forge" in
      write_file src "let () = print_string \"spaced\"";
      let out = Filename.concat sub "my exe" in
      let r = run_process ~timeout:120.0 ~cwd:(repo ()) (exe "compile_forge") [ src; out ] in
      (match r.status with Exited 0 -> () | _ -> assert_failure ("compile failed: " ^ describe r));
      let rr = run_process out [] in
      assert_runs ~expected:"spaced\n" "spaces" "" rr );
    ( "compile_forge: default output name is a.out in the cwd" >:: fun _ ->
      with_tmpdir @@ fun dir ->
      let src = Filename.concat dir "p.forge" in
      write_file src "let () = print_string \"default\"";
      let r =
        run_process ~timeout:120.0 ~cwd:dir
          ~env:(Array.append [| "FORGE_ROOT=" ^ repo () |] (Unix.environment ()))
          (exe "compile_forge") [ src ]
      in
      (match r.status with Exited 0 -> () | _ -> assert_failure ("compile failed: " ^ describe r));
      assert_bool "a.out exists" (Sys.file_exists (Filename.concat dir "a.out")) );
    ( "compile_forge: works outside the repo with FORGE_ROOT" >:: fun _ ->
      with_tmpdir @@ fun dir ->
      let src = Filename.concat dir "p.forge" in
      write_file src "let () = println (1 + 1)";
      let out = Filename.concat dir "p.out" in
      let r =
        run_process ~timeout:120.0 ~cwd:dir
          ~env:(Array.append [| "FORGE_ROOT=" ^ repo () |] (Unix.environment ()))
          (exe "compile_forge") [ src; out ]
      in
      (match r.status with Exited 0 -> () | _ -> assert_failure ("compile failed: " ^ describe r));
      assert_runs ~expected:"2\n" "forge_root" "" (run_process out []) );
    ( "compile_forge: empty program" >:: fun _ ->
      match native "" with
      | Ok r -> assert_runs ~expected:"" "empty native" "" r
      | Error c -> assert_failure ("empty program should compile: " ^ describe c) );
  ]

let repl_run input = run_process ~stdin:input ~cwd:(repo ()) (exe "repl") []

let strip_ansi s =
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let i = ref 0 in
  while !i < n do
    if s.[!i] = '\027' then (
      while !i < n && not (s.[!i] >= 'a' && s.[!i] <= 'z' || s.[!i] >= 'A' && s.[!i] <= 'Z') do
        incr i
      done;
      incr i)
    else (
      Buffer.add_char b s.[!i];
      incr i)
  done;
  Buffer.contents b

let repl_tests =
  [
    ( "repl: evaluates and exits on EOF" >:: fun _ ->
      let r = repl_run "1 + 2;;\n" in
      (match r.status with Exited 0 -> () | _ -> assert_failure (describe r));
      assert_bool ("shows the value and type: " ^ r.out) (contains (strip_ansi r.out) "3 : Int") );
    ( "repl: definitions persist" >:: fun _ ->
      let r = repl_run "let x = 41;;\nx + 1;;\n" in
      assert_bool ("uses x: " ^ r.out) (contains (strip_ansi r.out) "42 : Int") );
    ( "repl: :type command" >:: fun _ ->
      let r = repl_run ":type fn x -> x;;\n" in
      assert_bool ("prints type: " ^ r.out) (contains (strip_ansi r.out) "a -> a") );
    ( "repl: :quit exits 0" >:: fun _ ->
      let r = repl_run ":quit\n1 + 1;;\n" in
      assert_equal ~printer:string_of_status (Exited 0) r.status;
      assert_bool "does not evaluate after quit" (not (contains (strip_ansi r.out) "2 : Int")) );
    ( "repl: error then recovery" >:: fun _ ->
      let r = repl_run "1 + true;;\n2 + 2;;\n" in
      let out = strip_ansi r.out in
      assert_bool ("reports error: " ^ out) (lowercase_contains out "error");
      assert_bool ("keeps going: " ^ out) (contains out "4 : Int") );
    ( "repl: runtime error then recovery" >:: fun _ ->
      let r = repl_run "1 / 0;;\n5;;\n" in
      let out = strip_ansi r.out in
      assert_bool ("keeps going: " ^ out) (contains out "5 : Int");
      assert_bool ("no doubled 'Error: Error:' prefix: " ^ out) (not (contains out "Error: Error:")) );
    ( "repl: multi-line input" >:: fun _ ->
      let r = repl_run "let f x =\n  x * 2\n;;\nf 21;;\n" in
      assert_bool ("multi-line works: " ^ r.out) (contains (strip_ansi r.out) "42 : Int") );
    ( "repl: trailing garbage after a definition is an error" >:: fun _ ->
      let r = repl_run "let y = 5 garbage;;\n" in
      let out = strip_ansi r.out in
      assert_bool ("should report an error: " ^ out) (lowercase_contains out "error") );
    ( "repl: unknown command" >:: fun _ ->
      let r = repl_run ":frobnicate;;\n" in
      let out = strip_ansi r.out in
      assert_bool ("should say unknown command: " ^ out)
        (lowercase_contains out "unknown" || lowercase_contains out "error") );
    ( "repl: preload file" >:: fun _ ->
      with_tmpdir @@ fun dir ->
      let f = Filename.concat dir "pre.forge" in
      write_file f "let preloaded = 99";
      let r = run_process ~stdin:"preloaded;;\n" ~cwd:(repo ()) (exe "repl") [ f ] in
      assert_bool ("preloaded binding visible: " ^ r.out) (contains (strip_ansi r.out) "99 : Int") );
    ( "repl: preload missing file is reported" >:: fun _ ->
      let r = run_process ~stdin:"" ~cwd:(repo ()) (exe "repl") [ "/nonexistent/x.forge" ] in
      let text = strip_ansi (r.out ^ r.err) in
      assert_bool ("should report the missing file: " ^ text) (lowercase_contains text "x.forge") );
    ( "repl: shows lists like the prelude" >:: fun _ ->
      let r = repl_run "show [1, 2];;\n" in
      assert_bool ("show list: " ^ r.out) (contains (strip_ansi r.out) "\"[1, 2]\"") );
  ]

let hover src line ch = run_process ~stdin:src ~cwd:(repo ()) (exe "forge_hover") [ "x.forge"; "1"; string_of_int line; string_of_int ch ]

let hover_tests =
  [
    ( "hover: type of a top-level binding" >:: fun _ ->
      let r = hover "let answer = 42\n" 0 5 in
      assert_equal ~printer:(Printf.sprintf "%S") "TYPE\tInt" r.out );
    ( "hover: type of a function" >:: fun _ ->
      let r = hover "let inc x = x + 1\n" 0 4 in
      assert_equal ~printer:(Printf.sprintf "%S") "TYPE\tInt -> Int" r.out );
    ( "hover: second line" >:: fun _ ->
      let r = hover "let a = 1\nlet b = \"s\"\n" 1 4 in
      assert_equal ~printer:(Printf.sprintf "%S") "TYPE\tString" r.out );
    ( "hover: usage error" >:: fun _ ->
      let r = run_process ~cwd:(repo ()) (exe "forge_hover") [] in
      assert_equal ~printer:string_of_status (Exited 2) r.status );
    ( "hover: non-numeric line" >:: fun _ ->
      let r = run_process ~stdin:"let a = 1" ~cwd:(repo ()) (exe "forge_hover") [ "x.forge"; "1"; "x"; "0" ] in
      assert_equal ~printer:string_of_status (Exited 2) r.status );
    ( "hover: out of range position fails cleanly" >:: fun _ ->
      let r = hover "let a = 1\n" 50 3 in
      (match r.status with Exited 1 -> () | _ -> assert_failure (describe r));
      assert_bool "ERROR prefix" (String.starts_with ~prefix:"ERROR" r.out) );
    ( "hover: negative position fails cleanly" >:: fun _ ->
      let r = hover "let a = 1\n" (-1) (-1) in
      (match r.status with Exited 1 -> () | _ -> assert_failure (describe r));
      assert_bool "ERROR prefix" (String.starts_with ~prefix:"ERROR" r.out) );
    ( "hover: empty buffer fails cleanly" >:: fun _ ->
      let r = hover "" 0 0 in
      match r.status with Exited 1 -> () | _ -> assert_failure (describe r) );
    ( "hover: buffer with a type error still answers for good names" >:: fun _ ->
      let r = hover "let good = 1\nlet bad = 1 + true\n" 0 5 in
      assert_equal ~printer:(Printf.sprintf "%S") "TYPE\tInt" r.out );
    ( "hover: buffer with a parse error does not crash" >:: fun _ ->
      let r = hover "let good = 1\nlet bad = (\n" 0 5 in
      match r.status with Exited (0 | 1) -> () | _ -> assert_failure (describe r) );
  ]

let dump_ir_tests =
  [
    expect_usage "dump_min_ir: no arguments" "dump_min_ir" [];
    ( "dump_min_ir: prints IR for a program" >:: fun _ ->
      with_tmpdir @@ fun dir ->
      let src = Filename.concat dir "p.forge" in
      write_file src "let () = println (1 + 2)";
      let r = run_process ~cwd:(repo ()) (exe "dump_min_ir") [ src ] in
      (match r.status with Exited 0 -> () | _ -> assert_failure (describe r));
      assert_bool "mentions a function" (contains r.out "func" || contains r.out "define" || contains r.out "main") );
    clean_error "dump_min_ir: missing file" "dump_min_ir" [ "/nonexistent/p.forge" ];
  ]

let suite =
  "cli"
  >::: [
         "interpreter" >::: interpreter_tests;
         "compile_forge" >::: compiler_tests;
         "repl" >::: repl_tests;
         "forge_hover" >::: hover_tests;
         "dump_min_ir" >::: dump_ir_tests;
       ]

let () = run_test_tt_main suite
