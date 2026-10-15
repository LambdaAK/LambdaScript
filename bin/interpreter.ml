(* Forge interpreter: run a .forge file (with the standard prelude).

   Errors are reported on stderr as [error: <message>] and exit with status
   1; the program's own output stays on stdout. *)

open Language

let interpret (filename : string) =
  let fr = Compile_pipeline.front_end filename in
  let static_env = Build_env.build_full_static_env () in
  let dynamic_env = Ceval.initial_env () |> Ceval.unwrap_eval_result in
  let type_env : Typecheck.type_env = [] in
  (* The whole program is type-checked (and elaborated) before any of it
     runs, as in the native compiler. *)
  let _, _, _, elaborated =
    List.fold_left
      (fun (static_env, type_env, ctor_env, acc) defn ->
        match
          Exhaustiveness.generate_defn_checked ~warn:Diag.warn static_env type_env ctor_env defn
        with
        | Typecheck.Ok (new_bindings, new_type_env, new_ctor_env) ->
            let defn =
              Typecheck.elaborate_defn ~rewrite_constrained_calls:true
                (new_bindings @ static_env) (new_type_env @ type_env) defn
            in
            (new_bindings @ static_env, new_type_env @ type_env, new_ctor_env @ ctor_env, defn :: acc)
        | Typecheck.Error e -> Diag.die (Diag.type_error_message e))
      (static_env, type_env, [], [])
      condensed_program
  in
  ignore
    (List.fold_left
       (fun dynamic_env defn ->
         match Ceval.eval_defn defn dynamic_env with
         | Ceval.Ok bindings -> bindings @ dynamic_env
         | Ceval.Error e -> Diag.die (Diag.runtime_error_message e))
       dynamic_env (List.rev elaborated))

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
