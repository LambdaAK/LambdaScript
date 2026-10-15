(* Forge interpreter: run a .forge file (with the standard prelude).

   Errors are reported on stderr as [error: <message>] and exit with status
   1; the program's own output stays on stdout. *)

open Language

let interpret (filename : string) =
  let user_source =
    try Compile_pipeline.read_file filename
    with Sys_error msg -> Diag.die ("cannot read " ^ msg)
  in
  let full_source = Prelude.prepend_to_source ~src_path:filename user_source in
  let prefix_lines =
    Diag.lines_in_prefix full_source (String.length full_source - String.length user_source)
  in
  let program =
    Diag.with_source_lines ~file:filename ~prefix_lines (fun () ->
        Parser.parse_program_tokens
          ~boundary:(String.length full_source - String.length user_source)
          (Lex.lex (full_source |> String.to_seq |> List.of_seq)))
  in
  let program =
    Import_resolve.resolve_program ~root_file:filename
      ~base_dir:(Filename.dirname filename) program
  in
  let condensed_program = Condense.condense_program program in
  let static_env = Build_env.build_full_static_env () in
  let dynamic_env = Ceval.initial_env () |> Ceval.unwrap_eval_result in
  let type_env : Typecheck.type_env = [] in
  ignore
    (List.fold_left
       (fun (static_env, dynamic_env, type_env, ctor_env) defn ->
         match
           Exhaustiveness.generate_defn_checked ~warn:Diag.warn static_env type_env ctor_env defn
         with
         | Typecheck.Ok (new_bindings, new_type_env, new_ctor_env) ->
             let defn =
               Typecheck.elaborate_defn ~rewrite_constrained_calls:true
                 (new_bindings @ static_env) (new_type_env @ type_env) defn
             in
             let new_dynamic_bindings =
               match Ceval.eval_defn defn dynamic_env with
               | Ceval.Ok v -> v
               | Ceval.Error e -> Diag.die (Diag.runtime_error_message e)
             in
             ( new_bindings @ static_env,
               new_dynamic_bindings @ dynamic_env,
               new_type_env @ type_env,
               new_ctor_env @ ctor_env )
         | Typecheck.Error e -> Diag.die (Diag.type_error_message e))
       (static_env, dynamic_env, type_env, [])
       condensed_program)

let () =
  if Array.length Sys.argv <> 2 then (
    prerr_endline "usage: interpreter <file.forge>";
    exit 2)
  else
    let filename = Sys.argv.(1) in
    Ceval.tune_gc ();
    try
      interpret filename;
      flush stdout
    with e -> (
      match Diag.message_of_exn ~file:filename e with
      | Some msg -> Diag.die msg
      | None -> Diag.die ("internal error: " ^ Printexc.to_string e))
