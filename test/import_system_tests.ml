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
  let path = Filename.temp_file "forge_import_tests_" "" in
  Unix.unlink path;
  Unix.mkdir path 0o700;
  Fun.protect ~finally:(fun () -> rm_rf path) (fun () -> f path)

let mkdir_p (dir : string) : unit =
  let rec go d =
    if d = "" || d = "." || d = "/" then ()
    else if Sys.file_exists d then ()
    else (
      go (Filename.dirname d);
      Unix.mkdir d 0o755)
  in
  go dir

let write_file (path : string) (content : string) : unit =
  mkdir_p (Filename.dirname path);
  Out_channel.with_open_bin path (fun oc -> Out_channel.output_string oc content)

let compile_and_run (src : string) (out : string) : string =
  match Language.Compile_pipeline.compile ~quiet:true src out with
  | Error msg -> assert_failure ("compile failed: " ^ msg)
  | Ok () -> run_exe_capture_stdout out

let contains_substring (s : string) (sub : string) : bool =
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

let compile_expect_error ~src ~contains =
  match Language.Compile_pipeline.compile ~quiet:true src (src ^ ".out") with
  | Ok () ->
      assert_failure
        (Printf.sprintf "expected compile error containing %S but compile succeeded"
           contains)
  | Error msg ->
      assert_bool
        (Printf.sprintf "expected error containing %S, got %S" contains msg)
        (contains_substring msg contains)

let suite =
  "import_system"
  >::: [
         ( "basic_relative_import_module_value" >:: fun _ ->
           with_tmpdir (fun dir ->
               let lib = Filename.concat dir "lib_a.ls" in
               let main = Filename.concat dir "main.ls" in
               let exe = Filename.concat dir "out" in
               write_file lib
                 {|
mod A where
  let x = 41
end
|};
               write_file main
                 {|
import "lib_a.ls"
use A
let () = print_string (int_to_str (x + 1))
|};
               let out = compile_and_run main exe in
               assert_equal ~printer:Fun.id "42\n" out) );
         ( "import_without_extension_works" >:: fun _ ->
           with_tmpdir (fun dir ->
               let lib = Filename.concat dir "lib_b.ls" in
               let main = Filename.concat dir "main.ls" in
               let exe = Filename.concat dir "out" in
               write_file lib
                 {|
mod B where
  let y = 42
end
|};
               write_file main
                 {|
import "./lib_b"
let () = print_string (int_to_str B.y)
|};
               let out = compile_and_run main exe in
               assert_equal ~printer:Fun.id "42\n" out) );
         ( "nested_import_chain_works" >:: fun _ ->
           with_tmpdir (fun dir ->
               let c = Filename.concat dir "c.ls" in
               let b = Filename.concat dir "b.ls" in
               let main = Filename.concat dir "main.ls" in
               let exe = Filename.concat dir "out" in
               write_file c
                 {|
mod C where
  let x = 40
end
|};
               write_file b
                 {|
import "c.ls"
mod B where
  let y = C.x + 1
end
|};
               write_file main
                 {|
import "b.ls"
let () = print_string (int_to_str (B.y + 1))
|};
               let out = compile_and_run main exe in
               assert_equal ~printer:Fun.id "42\n" out) );
         ( "shared_dependency_imported_once" >:: fun _ ->
           with_tmpdir (fun dir ->
               let common = Filename.concat dir "common.ls" in
               let left = Filename.concat dir "left.ls" in
               let right = Filename.concat dir "right.ls" in
               let main = Filename.concat dir "main.ls" in
               let exe = Filename.concat dir "out" in
               write_file common
                 {|
mod Common where
  let v = 41
end
|};
               write_file left
                 {|
import "common.ls"
mod Left where
  let x = Common.v
end
|};
               write_file right
                 {|
import "common.ls"
mod Right where
  let y = Common.v + 1
end
|};
               write_file main
                 {|
import "left.ls"
import "right.ls"
let () = print_string (int_to_str Left.x)
let () = print_string (int_to_str Right.y)
|};
               let out = compile_and_run main exe in
               assert_equal ~printer:Fun.id "41\n42\n" out) );
         ( "import_cycle_reports_error" >:: fun _ ->
           with_tmpdir (fun dir ->
               let a = Filename.concat dir "a.ls" in
               let b = Filename.concat dir "b.ls" in
               let main = Filename.concat dir "main.ls" in
               write_file a {|import "b.ls"|};
               write_file b {|import "a.ls"|};
               write_file main {|import "a.ls"|};
               compile_expect_error ~src:main ~contains:"import cycle detected")
           );
         ( "missing_import_reports_error" >:: fun _ ->
           with_tmpdir (fun dir ->
               let main = Filename.concat dir "main.ls" in
               write_file main {|import "does_not_exist.ls"|};
               compile_expect_error ~src:main ~contains:"import not found") );
         ( "import_with_modules_traits_and_impls" >:: fun _ ->
           with_tmpdir (fun dir ->
               let lib = Filename.concat dir "render.ls" in
               let main = Filename.concat dir "main.ls" in
               let exe = Filename.concat dir "out" in
               write_file lib
                 {|
mod M where
  trait Render<a> where
    val render : a -> String
  end

  impl Render for Int where
    render x = int_to_str x
  end

  let out <Render Int> x = render x
end
|};
               write_file main
                 {|
import "render.ls"
let () = print_string (M.out 42)
|};
               let out = compile_and_run main exe in
               assert_equal ~printer:Fun.id "42\n" out) );
         ( "relative_parent_path_import_works" >:: fun _ ->
           with_tmpdir (fun dir ->
               let lib = Filename.concat dir "pkg/lib.ls" in
               let main = Filename.concat dir "pkg/sub/main.ls" in
               let exe = Filename.concat dir "out" in
               write_file lib
                 {|
mod L where
  let x = 42
end
|};
               write_file main
                 {|
import "../lib.ls"
let () = print_string (int_to_str L.x)
|};
               let out = compile_and_run main exe in
               assert_equal ~printer:Fun.id "42\n" out) );
       ]

let () = run_test_tt_main suite
