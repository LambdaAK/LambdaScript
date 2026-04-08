open Language.Lex
open Language.Parser.ProgramParser
open Language.Condense
open Language.Typecheck
open Language.Cexpr
open Language.Ceval
open Language.Build_env

let interpret (filename : string) =
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
      (* Check if there are unparsed tokens remaining *)
      (match remaining with
      | [] -> () (* All tokens consumed, good! *)
      | _ ->
          print_endline ("Warning: " ^ string_of_int (List.length remaining) ^ " tokens remaining after parsing");
          print_endline "The entire file was not parsed successfully.";
          exit 1);
      let condensed_program = condense_program program in

      let static_env : static_env = build_full_static_env () in
      let dynamic_env : env = initial_env () |> unwrap_eval_result in
      let type_env : type_env = [] in

      (* use fold_left to iterate through the definitions and evaluate them *)
      let static_env, dynamic_env, type_env =
        List.fold_left
          (fun (static_env, dynamic_env, type_env) defn ->
            match generate_defn static_env type_env defn with
            | Ok (new_bindings, new_type_env, _new_ctor_env) ->
                (* TODO: propagate the monadic errors *)
                let defn =
                  elaborate_defn ~rewrite_constrained_calls:true
                    (new_bindings @ static_env) (new_type_env @ type_env) defn
                in
                let new_dynamic_bindings =
                  match eval_defn defn dynamic_env with
                  | Ok v -> v
                  | Error e -> failwith ("Evaluation failed: " ^ string_of_eval_error e)
                in
                (new_bindings @ static_env, new_dynamic_bindings @ dynamic_env, new_type_env @ type_env)
            | Error e ->
                print_endline (string_of_type_check_error e);
                exit 1)
          (static_env, dynamic_env, type_env) condensed_program
      in

      (* As of now, we don't really need to do anything with the resulting
         environments *)
      ignore static_env;
      ignore dynamic_env;
      ignore type_env

let () =
  if Array.length Sys.argv <> 2 then (
    print_endline "Usage: interpreter <filename>";
    exit 1)
  else
    let filename = Sys.argv.(1) in
    try interpret filename
    with e ->
      print_endline (Printexc.to_string e);
      exit 1
