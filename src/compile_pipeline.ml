open Lex
open Parser.ProgramParser
open Condense
open Typecheck
open Build_env
open Min_ir
open Llvm_emit

type typechecked_envs =
  Cexpr.static_env * Typecheck.type_env * Typecheck.constructor_env

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
  let candidate_in dir = Filename.concat dir "runtime/ls_runtime.c" in
  let rec search_up dir =
    let c = candidate_in dir in
    if Sys.file_exists c then Some c
    else
      let parent = Filename.dirname dir in
      if String.equal parent dir then None else search_up parent
  in
  match
    match Sys.getenv_opt "FORGE_ROOT" with
    | Some _ as r -> r
    | None -> Sys.getenv_opt "LAMBDASCRIPT_ROOT"
  with
  | Some root -> Filename.concat root "runtime/ls_runtime.c"
  | None -> (
      let cwd = Sys.getcwd () in
      let here = candidate_in cwd in
      if Sys.file_exists here then here
      else
        match search_up cwd with
        | Some p -> p
        | None ->
            failwith
              "Cannot find runtime/ls_runtime.c — run from the Forge repo root \
               or set FORGE_ROOT (legacy: LAMBDASCRIPT_ROOT)")

let rec typecheck_defns static_env type_env ctor_env defns
    : (typechecked_envs, string) result =
  match defns with
  | [] -> Ok (static_env, type_env, ctor_env)
  | defn :: rest -> (
      match generate_defn static_env type_env defn with
      | Ok (nb, nte, nce) ->
          typecheck_defns (nb @ static_env) (nte @ type_env) (nce @ ctor_env) rest
      | Error e -> Error (string_of_type_check_error e))

let compile ?(quiet = false) (src_path : string) (out_path : string) :
    (unit, string) result =
  let file_contents = read_file src_path in
  let tokens =
    lex (file_contents |> String.to_seq |> List.of_seq)
    |> List.map (fun t -> t.token_type)
  in
  match program_parser tokens with
  | None -> Error "Parsing failed"
  | Some (_, remaining) when remaining <> [] ->
      Error "Parsing failed: extra tokens after program"
  | Some (program, _) -> (
      let condensed_program = List.map condense_defn program in
      let static_env = build_full_static_env () in
      let type_env : type_env = [] in
      let ctor_env : Typecheck.constructor_env = [] in
      match typecheck_defns static_env type_env ctor_env condensed_program with
      | Error _ as e -> e
      | Ok (static_env, type_env, ctor_env) -> (
          match
            Lower_min_ir.lower_c_program condensed_program static_env type_env
              ctor_env
          with
          | Error msg ->
              if not quiet then print_endline ("Lowering failed: " ^ msg);
              Error ("Lowering failed: " ^ msg)
          | Ok min_ir_prog ->
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
              let cmd_asm =
                Printf.sprintf "clang -S -O1 -g -Wno-override-module -o %s %s"
                  asm_path ll_path
              in
              let code_asm = Sys.command cmd_asm in
              if code_asm <> 0 then
                Error
                  (Printf.sprintf "clang -S (IR → assembly) failed (exit %d)"
                     code_asm)
              else
                let rt = runtime_c_path () in
                let cmd_exe =
                  Printf.sprintf "clang -O1 -g -Wno-override-module -o %s %s %s"
                    out_path ll_path rt
                in
                let code_exe = Sys.command cmd_exe in
                if code_exe <> 0 then
                  Error
                    (Printf.sprintf "clang (link executable) failed (exit %d)"
                       code_exe)
                else (
                  if not quiet then
                    Printf.printf
                      "Wrote Min IR:     %s\n\
                       Wrote LLVM IR:  %s\n\
                       Wrote assembly: %s\n\
                       Wrote executable: %s\n"
                      mir_path ll_path asm_path out_path;
                  Ok ())))
