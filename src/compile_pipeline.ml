open Lex
open Condense
open Import_resolve
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

let read_program_source ?(prelude = true) (src_path : string) : string =
  let raw = read_file src_path in
  Prelude.prepend_to_source ~enabled:prelude ~src_path raw

let runtime_c_path () =
  let candidate_in dir = Filename.concat dir "runtime/ls_runtime.c" in
  let rec search_up dir =
    let c = candidate_in dir in
    if Sys.file_exists c then Some c
    else
      let parent = Filename.dirname dir in
      if String.equal parent dir then None else search_up parent
  in
  match Sys.getenv_opt "FORGE_ROOT" with
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
               or set FORGE_ROOT")

let rec typecheck_and_elaborate_defns static_env type_env ctor_env
    (acc : Cexpr.c_defn list) (defns : Cexpr.c_defn list) :
    (Cexpr.c_defn list * typechecked_envs, string) result =
  match defns with
  | [] -> Ok (List.rev acc, (static_env, type_env, ctor_env))
  | defn :: rest -> (
      match
        Exhaustiveness.generate_defn_checked ~warn:Diag.warn static_env type_env ctor_env defn
      with
      | Ok (new_bindings, new_type_env, new_ctor_env) ->
          let static_env' = new_bindings @ static_env in
          let type_env' = new_type_env @ type_env in
          let ctor_env' = new_ctor_env @ ctor_env in
          let elaborated_defn =
            Typecheck.elaborate_defn ~rewrite_constrained_calls:true static_env' type_env'
              defn
          in
          typecheck_and_elaborate_defns static_env' type_env' ctor_env'
            (elaborated_defn :: acc) rest
      | Error e -> Error (Diag.type_error_message e))

(** Front end + lowering: parse, resolve imports, condense, typecheck,
    elaborate and lower [src_path] (with the prelude unless [~prelude:false])
    to Min IR. Errors are user-facing messages. *)
let rec min_ir_of_file ?(prelude = true) (src_path : string) :
    (Min_ir.prog, string) result =
  try min_ir_of_file_exn ~prelude src_path with
  | e -> (
      Condense.clear_id_queue ();
      match Diag.message_of_exn ~file:src_path e with
      | Some msg -> Error msg
      | None -> Error ("internal compiler error: " ^ Printexc.to_string e))

and min_ir_of_file_exn ~prelude (src_path : string) : (Min_ir.prog, string) result =
  let user_raw =
    try read_file src_path with Sys_error msg -> failwith ("cannot read " ^ msg)
  in
  let file_contents = Prelude.prepend_to_source ~enabled:prelude ~src_path user_raw in
  let delta = String.length file_contents - String.length user_raw in
  let prefix_lines = Diag.lines_in_prefix file_contents delta in
  let full_tokens =
    Diag.with_source_lines ~file:src_path ~prefix_lines (fun () ->
        lex (file_contents |> String.to_seq |> List.of_seq))
  in
  Condense.set_id_queue_from_tokens full_tokens;
  let program =
    Diag.with_source_lines ~file:src_path ~prefix_lines (fun () ->
        Parser.parse_program_tokens ~boundary:delta full_tokens)
  in
  let program =
    resolve_program ~root_file:src_path ~base_dir:(Filename.dirname src_path) program
  in
  let prelude_n =
    if delta <= 0 then 0
    else
      let frag = String.sub file_contents 0 delta in
      let n = Prelude.defn_count_for_source_fragment frag in
      if n > 0 then n else Prelude.defn_count_when_parsed ()
  in
  let condensed_program =
    condense_program
      ?user_id_byte_min_after_prelude:(if delta = 0 then None else Some (prelude_n, delta))
      program
  in
  Condense.clear_id_queue ();
  let static_env = build_full_static_env () in
  match typecheck_and_elaborate_defns static_env [] [] [] condensed_program with
  | Error _ as e -> e
  | Ok (elaborated, _) -> (
      (* Dictionaries are resolved statically, every polymorphic definition is
         copied per type, and the typed monomorphic program is lowered. *)
      match Mono.program (Specialize.program (Mono.builtin_defns () @ elaborated)) with
      | exception (Specialize.Unsupported msg | Mono.Unsupported msg) ->
          Error ("the native compiler does not support this program yet: " ^ msg)
      | typed -> (
          match Lower_typed.program typed with
          | exception Lower_typed.Unsupported msg ->
              Error ("the native compiler does not support this program yet: " ^ msg)
          | prog -> Ok prog))

(** Compiler and linker flags for the Boehm garbage collector used by the
    runtime: [FORGE_GC_FLAGS] if set, else [pkg-config bdw-gc], else the
    Homebrew locations, else plain [-lgc]. *)
let gc_flags : string Lazy.t =
  lazy
    (match Sys.getenv_opt "FORGE_GC_FLAGS" with
    | Some f -> f
    | None -> (
        let from_pkg_config =
          try
            let ic = Unix.open_process_in "pkg-config --cflags --libs bdw-gc 2>/dev/null" in
            let line = try String.trim (input_line ic) with End_of_file -> "" in
            match Unix.close_process_in ic with Unix.WEXITED 0 when line <> "" -> Some line | _ -> None
          with _ -> None
        in
        match from_pkg_config with
        | Some f -> f
        | None -> (
            let prefixes = [ "/opt/homebrew/opt/bdw-gc"; "/usr/local/opt/bdw-gc" ] in
            match List.find_opt (fun d -> Sys.file_exists (Filename.concat d "include/gc.h")) prefixes with
            | Some d -> Printf.sprintf "-I%s/include -L%s/lib -lgc" d d
            | None -> "-lgc")))

(** Compile [src_path] to the executable [out_path]: writes [.mir], [.ll] and
    [.s] next to the source, then links with the C runtime using clang. *)
let compile ?(quiet = false) ?(prelude = true) (src_path : string) (out_path : string) :
    (unit, string) result =
  match min_ir_of_file ~prelude src_path with
  | Error _ as e -> e
  | Ok min_ir_prog -> (
      try
        let base = Filename.remove_extension src_path in
        let mir_path = base ^ ".mir" in
        Out_channel.with_open_bin mir_path (fun oc ->
            output_string oc (string_of_prog min_ir_prog));
        let ll = emit_prog min_ir_prog in
        let ll_path = base ^ ".ll" in
        let asm_path = base ^ ".s" in
        Out_channel.with_open_bin ll_path (fun oc -> output_string oc ll);
        let cmd_asm =
          Printf.sprintf "clang -S -O1 -g -Wno-override-module -o %s %s"
            (Filename.quote asm_path) (Filename.quote ll_path)
        in
        let code_asm = Sys.command cmd_asm in
        if code_asm <> 0 then
          Error
            (Printf.sprintf
               "internal compiler error: clang rejected the generated LLVM IR %s (exit %d)"
               ll_path code_asm)
        else
          let rt = runtime_c_path () in
          let cmd_exe =
            Printf.sprintf "clang -O1 -g -Wno-override-module -o %s %s %s %s -lm"
              (Filename.quote out_path) (Filename.quote ll_path) (Filename.quote rt)
              (Lazy.force gc_flags)
          in
          let code_exe = Sys.command cmd_exe in
          if code_exe <> 0 then
            Error (Printf.sprintf "linking %s with clang failed (exit %d)" out_path code_exe)
          else (
            if not quiet then
              Printf.printf
                "Wrote Min IR:     %s\n\
                 Wrote LLVM IR:    %s\n\
                 Wrote assembly:   %s\n\
                 Wrote executable: %s\n"
                mir_path ll_path asm_path out_path;
            Ok ())
      with
      | Sys_error msg -> Error msg
      | Failure msg when String.starts_with ~prefix:"llvm_emit" msg ->
          Error ("internal compiler error: " ^ msg)
      | Failure msg -> Error (Diag.clean msg))
