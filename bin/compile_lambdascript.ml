open Language.Lex
open Language.Parser.ProgramParser
open Language.Condense
open Language.Typecheck
open Language.Build_env
open Language.Min_ir
open Language.Llvm_emit

let read_file filename =
  let ch = open_in filename in
  let rec read acc =
    try read (acc ^ input_line ch ^ "\n")
    with End_of_file ->
      close_in ch;
      acc
  in
  read ""

let runtime_c_path () =
  match Sys.getenv_opt "LAMBDASCRIPT_ROOT" with
  | Some root -> Filename.concat root "runtime/ls_runtime.c"
  | None ->
      let here = Filename.concat (Sys.getcwd ()) "runtime/ls_runtime.c" in
      if Sys.file_exists here then here
      else
        failwith
          "Cannot find runtime/ls_runtime.c — run from the LambdaScript repo root or set LAMBDASCRIPT_ROOT"

let compile (src_path : string) (out_path : string) =
  let file_contents = read_file src_path in
  let tokens =
    lex (file_contents |> String.to_seq |> List.of_seq)
    |> List.map (fun t -> t.token_type)
  in
  match program_parser tokens with
  | None ->
      print_endline "Parsing failed";
      exit 1
  | Some (program, remaining) ->
      if remaining <> [] then (
        print_endline "Parsing failed: extra tokens after program";
        exit 1);
      let condensed_program = List.map condense_defn program in
      let static_env = build_full_static_env () in
      let type_env : type_env = [] in
      let static_env, type_env =
        List.fold_left
          (fun (static_env, type_env) defn ->
            match generate_defn static_env type_env defn with
            | Ok (nb, nte) -> (nb @ static_env, nte @ type_env)
            | Error e ->
                print_endline (string_of_type_check_error e);
                exit 1)
          (static_env, type_env) condensed_program
      in
      let min_ir_prog =
        match
          Language.Lower_min_ir.lower_c_program condensed_program static_env
            type_env
        with
        | Ok p -> p
        | Error msg ->
            print_endline ("Lowering failed: " ^ msg);
            exit 1
      in
      let base = Filename.remove_extension src_path in
      let mir_path = base ^ ".mir" in
      let oc_mir = open_out mir_path in
      output_string oc_mir (string_of_prog min_ir_prog);
      close_out oc_mir;
      let ll = emit_prog min_ir_prog in
      let ll_path = base ^ ".ll" in
      let asm_path = base ^ ".s" in
      let oc = open_out ll_path in
      output_string oc ll;
      close_out oc;
      (* Assembly for the generated module (externals call into runtime). *)
      let cmd_asm =
        Printf.sprintf "clang -S -O1 -g -Wno-override-module -o %s %s"
          asm_path ll_path
      in
      let code_asm = Sys.command cmd_asm in
      if code_asm <> 0 then (
        print_endline
          ("clang -S (IR → assembly) failed (exit " ^ string_of_int code_asm ^ ")");
        exit code_asm);
      let rt = runtime_c_path () in
      let cmd_exe =
        Printf.sprintf "clang -O1 -g -Wno-override-module -o %s %s %s" out_path
          ll_path rt
      in
      let code_exe = Sys.command cmd_exe in
      if code_exe <> 0 then (
        print_endline ("clang (link executable) failed (exit " ^ string_of_int code_exe ^ ")");
        exit code_exe);
      Printf.printf
        "Wrote Min IR:     %s\n\
         Wrote LLVM IR:  %s\n\
         Wrote assembly: %s\n\
         Wrote executable: %s\n"
        mir_path ll_path asm_path out_path

let () =
  if Array.length Sys.argv < 2 || Array.length Sys.argv > 3 then (
    print_endline "Usage: compile_lambdascript <source.ls> [output_executable]";
    print_endline "Default output: ./a.out";
    exit 1);
  let src = Sys.argv.(1) in
  let out =
    if Array.length Sys.argv = 3 then Sys.argv.(2) else "a.out"
  in
  try compile src out
  with Failure msg ->
    print_endline msg;
    exit 1
