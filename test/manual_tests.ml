(** The examples of the reference manual (documentation/examples), run
    through both back ends.

    For [name.forge]:
    - [name.out] is the expected standard output (of the interpreter, and of
      the native executable);
    - [name.err], if present, lists lines that must appear on standard error
      (warnings, or the error that stops the program); when one of them is an
      [error:], the program must fail with status 1, otherwise succeed. For a
      native compilation error, the lines must appear in the compiler's
      output. File paths in messages are compared without their directory.

    The manual includes these files verbatim, so what it shows is what is
    tested. *)

open OUnit2
open Forge_test_util

let examples_dir () = Filename.concat (repo ()) "documentation/examples"

let lines_of (s : string) : string list =
  List.filter (fun l -> String.trim l <> "") (String.split_on_char '\n' s)

(** [strip_dir dir s] removes the directory [dir] from the file paths in [s],
    so that messages read [file.forge:2: ...] whichever copy was run. *)
let strip_dir (dir : string) (s : string) : string =
  let prefix = dir ^ "/" in
  let n = String.length prefix in
  let b = Buffer.create (String.length s) in
  let i = ref 0 in
  while !i < String.length s do
    if !i + n <= String.length s && String.sub s !i n = prefix then i := !i + n
    else (
      Buffer.add_char b s.[!i];
      incr i)
  done;
  Buffer.contents b

let check_stderr name (expected : string list) (stderr : string) =
  List.iter
    (fun l ->
      if not (contains stderr l) then
        assert_failure (Printf.sprintf "%s: stderr should contain %S, got:\n%s" name l stderr))
    expected

let test_example (file : string) : test =
  let base = Filename.chop_suffix file ".forge" in
  let path = Filename.concat (examples_dir ()) file in
  let side ext =
    let p = Filename.concat (examples_dir ()) (base ^ ext) in
    if Sys.file_exists p then Some (read_file p) else None
  in
  base >:: fun _ ->
  let expected_out = match side ".out" with Some s -> s | None -> assert_failure (base ^ ".out is missing") in
  let expected_err = match side ".err" with Some s -> lines_of s | None -> [] in
  let fails = List.exists (fun l -> contains l "error:") expected_err in
  let expected_status = if fails then Exited 1 else Exited 0 in
  (* Interpreter. *)
  let r = run_process ~cwd:(repo ()) (bin_exe "interpreter") [ path ] in
  assert_equal ~printer:string_of_status ~msg:(base ^ ": interpreter exit status") expected_status r.status;
  assert_equal ~printer:(fun s -> s) ~msg:(base ^ ": interpreter output") expected_out r.out;
  check_stderr (base ^ " (interpreter)") expected_err (strip_dir (examples_dir ()) r.err);
  (* Native compiler, on a copy (it writes its intermediate files next to the
     source; the copies of the other examples serve imports). *)
  with_tmpdir @@ fun dir ->
  Array.iter
    (fun f ->
      if Filename.check_suffix f ".forge" then
        write_file (Filename.concat dir f) (read_file (Filename.concat (examples_dir ()) f)))
    (Sys.readdir (examples_dir ()));
  let exe = Filename.concat dir "a.out" in
  let c =
    run_process ~timeout:60.0 ~cwd:(repo ()) (bin_exe "compile_forge") [ Filename.concat dir file; exe ]
  in
  match c.status with
  | Exited 0 ->
      let n = run_process exe [] in
      assert_equal ~printer:string_of_status ~msg:(base ^ ": native exit status") expected_status n.status;
      assert_equal ~printer:(fun s -> s) ~msg:(base ^ ": native output") expected_out n.out;
      check_stderr (base ^ " (native)") expected_err (strip_dir dir (c.out ^ c.err ^ n.err))
  | _ ->
      if not fails then assert_failure (base ^ ": native compilation failed:\n" ^ describe c);
      assert_equal ~printer:(fun s -> s) ~msg:(base ^ ": output before a compile error") "" expected_out;
      check_stderr (base ^ " (native compiler)") expected_err (strip_dir dir (c.out ^ c.err))

let suite =
  let files =
    Sys.readdir (examples_dir ())
    |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".forge")
    |> List.sort compare
  in
  "manual" >::: List.map test_example files

let () = run_test_tt_main suite
