open Language.Lex
open Language.Parser.ProgramParser
open Language.Condense
open Language.Typecheck
open Language.Build_env
open Language.Min_ir

let dump_ir (filename : string) =
  let file_contents =
    try Language.Compile_pipeline.read_program_source filename
    with Sys_error msg ->
      print_endline ("Error opening file: " ^ msg);
      exit 1
  in

  let tokens =
    lex (file_contents |> String.to_seq |> List.of_seq)
    |> List.map (fun t -> t.token_type)
  in

  match program_parser tokens with
  | None ->
      print_endline "Parsing failed";
      exit 1
  | Some (program, remaining) ->
      (match remaining with
      | [] -> ()
      | _ ->
          print_endline
          ( "Warning: "
            ^ string_of_int (List.length remaining)
            ^ " tokens remaining after parsing" );
          exit 1);
      let program =
        try
          Language.Import_resolve.resolve_program ~root_file:filename
            ~base_dir:(Filename.dirname filename)
            program
        with Failure msg ->
          print_endline msg;
          exit 1
      in
      let condensed_program = condense_program program in
      let static_env = build_full_static_env () in
      let type_env : type_env = [] in
      let ctor_env : Language.Typecheck.constructor_env = [] in
      let static_env, type_env, ctor_env =
        List.fold_left
          (fun (static_env, type_env, ctor_env) defn ->
            match generate_defn static_env type_env defn with
            | Ok (new_bindings, new_type_env, new_ctor_env) ->
                ( new_bindings @ static_env,
                  new_type_env @ type_env,
                  new_ctor_env @ ctor_env )
            | Error e ->
                print_endline (string_of_type_check_error e);
                exit 1)
          (static_env, type_env, ctor_env) condensed_program
      in
      match
        Language.Lower_min_ir.lower_c_program condensed_program static_env
          type_env ctor_env
      with
      | Ok prog -> print_endline (string_of_prog prog)
      | Error msg ->
          print_endline ("Lowering failed: " ^ msg);
          exit 1

let () =
  if Array.length Sys.argv <> 2 then (
    print_endline "Usage: dump_min_ir <filename.ls>";
    exit 1)
  else
    try dump_ir Sys.argv.(1)
    with e ->
      print_endline (Printexc.to_string e);
      exit 1
