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

(** A program after the front end: its condensed definitions, and what is
    needed to say where each one is ([source] is the user's file with the
    prelude prepended, [delta] bytes / [prefix_lines] lines of it). *)
type front = {
  file : string;
  source : string;
  delta : int;
  prefix_lines : int;
  condensed : Cexpr.c_defn list;
}

(** The line, in the user's file, of the name [d] defines (from the spans
    recorded while condensing); [None] for definitions of the prelude, of
    imported files, or made up by the front end. *)
let defn_line (fr : front) (d : Cexpr.c_defn) : int option =
  let rec first_id (p : Cexpr.c_pat) : Cexpr.c_pat option =
    match p with
    | Cexpr.CIdPat _ -> Some p
    | Cexpr.CVectorPat ps -> List.find_map first_id ps
    | Cexpr.CConsPat (a, b) -> ( match first_id a with Some x -> Some x | None -> first_id b)
    | Cexpr.CRecordPat fs -> List.find_map (fun (_, q) -> first_id q) fs
    | Cexpr.CVariantPat (_, Some q) -> first_id q
    | _ -> None
  in
  let pat =
    match d with
    | Cexpr.CDefn (p, _, _, _, _, _) | Cexpr.CDefnRec (p, _, _, _, _, _) -> Some p
    | Cexpr.CDefnMutRec ((p, _, _, _, _, _) :: _) -> Some p
    | _ -> None
  in
  (* An instance dictionary has no name in the source: its [impl Cls for]
     line, if there is only one. *)
  let impl_line () =
    match pat with
    | Some (Cexpr.CIdPat x) when String.starts_with ~prefix:"__forge_dict_" x -> (
        match String.split_on_char '_' (Diag.strip_prefix ~prefix:"__forge_dict_" x) with
        | cls :: _ -> (
            let user = String.sub fr.source fr.delta (String.length fr.source - fr.delta) in
            let lines = String.split_on_char '\n' user in
            let prefix = "impl " ^ cls ^ " for " in
            match
              List.filteri (fun _ l -> String.starts_with ~prefix (String.trim l)) lines
            with
            | [ l ] ->
                let rec index i = function [] -> None | m :: rest -> if m == l then Some (i + 1) else index (i + 1) rest in
                index 0 lines
            | _ -> None)
        | [] -> None)
    | _ -> None
  in
  (* Else the first identifier of the body (for [let () = ...]). *)
  let rec first_pos (e : Cexpr.c_expr) : int option =
    let open Cexpr in
    let first l = List.find_map first_pos l in
    match e with
    | EId (_, Some (lo, _)) -> Some lo
    | EFunction (_, _, b) -> first_pos b
    | EBind (_, _, a, b, _) | EBindRec (_, _, a, b, _) | EApp (a, b) | EBop (_, a, b)
    | EListEnumeration (a, b) ->
        first [ a; b ]
    | ETernary (a, b, c) -> first [ a; b; c ]
    | ESwitch (a, brs) -> first (a :: List.map snd brs)
    | EVector es -> first es
    | ERecordLit fs -> first (List.map snd fs)
    | EFieldAccess (a, _) -> first_pos a
    | EBlock parts -> List.find_map (function Expr x -> first_pos x | Defn _ -> None) parts
    | _ -> None
  in
  let body_line () =
    match d with
    | Cexpr.CDefn (_, _, _, body, _, _) | Cexpr.CDefnRec (_, _, _, body, _, _) -> (
        match first_pos body with
        | Some lo when lo >= fr.delta && lo <= String.length fr.source ->
            Some (Diag.lines_in_prefix fr.source lo + 1 - fr.prefix_lines)
        | _ -> None)
    | _ -> None
  in
  let impl_line () = match impl_line () with Some l -> Some l | None -> body_line () in
  match Stdlib.Option.bind pat first_id with
  | None -> impl_line ()
  | Some p -> (
      match Condense.PatTbl.find_opt Condense.pat_spans p with
      | Some (lo, _) when lo >= fr.delta && lo <= String.length fr.source ->
          Some (Diag.lines_in_prefix fr.source lo + 1 - fr.prefix_lines)
      | _ -> impl_line ())

(** A type error in [d]: [file:line: type error in `f`: <what clashed>],
    without the echo of the whole (desugared) definition. *)
let defn_type_error (fr : front) (d : Cexpr.c_defn) (e : Typecheck.type_error) : string =
  let msg = Diag.clean (Typecheck.string_of_type_check_error e) in
  let msg =
    Diag.strip_prefix ~prefix:"Type inference failed while checking definition: " msg
  in
  let msg =
    let cut = "\n  in: " in
    let n = String.length cut in
    let rec find i =
      if i + n > String.length msg then None
      else if String.sub msg i n = cut then Some i
      else find (i + 1)
    in
    match find 0 with Some i -> String.sub msg 0 i | None -> msg
  in
  let name =
    match d with
    | Cexpr.CDefn (Cexpr.CIdPat x, _, _, _, _, _) | Cexpr.CDefnRec (Cexpr.CIdPat x, _, _, _, _, _)
    | Cexpr.CDefnMutRec ((Cexpr.CIdPat x, _, _, _, _, _) :: _) ->
        if String.starts_with ~prefix:"__forge_dict_" x then
          match String.split_on_char '_' (Diag.strip_prefix ~prefix:"__forge_dict_" x) with
          | cls :: _ -> Some ("an impl of " ^ cls)
          | [] -> None
        else if String.starts_with ~prefix:"__" x || x = "()" then None
        else Some ("`" ^ x ^ "`")
    | _ -> None
  in
  let what = match name with Some n -> "type error in " ^ n ^ ": " ^ msg | None -> "type error: " ^ msg in
  match defn_line fr d with Some l -> Printf.sprintf "%s:%d: %s" fr.file l what | None -> what

let rec typecheck_and_elaborate_defns ?front static_env type_env ctor_env
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
          typecheck_and_elaborate_defns ?front static_env' type_env' ctor_env'
            (elaborated_defn :: acc) rest
      | Error e -> (
          match front with
          | Some fr -> Error (defn_type_error fr defn e)
          | None -> Error (Diag.type_error_message e)))

(** Lex, parse, resolve imports and condense [src_path] (with the prelude
    unless [~prelude:false]). *)
let front_end ?(prelude = true) (src_path : string) : front =
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
  { file = src_path; source = file_contents; delta; prefix_lines; condensed = condensed_program }

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
  let fr = front_end ~prelude src_path in
  let condensed_program = fr.condensed in
  let static_env = build_full_static_env () in
  match typecheck_and_elaborate_defns ~front:fr static_env [] [] [] condensed_program with
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
            Printf.sprintf "clang -O1 -g -Wno-override-module -pthread -o %s %s %s %s -lm"
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
