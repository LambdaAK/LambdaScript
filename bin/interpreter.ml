open Language.Lex
open Language.Parser.ProgramParser
open Language.Condense
open Language.Typecheck
open Language.Cexpr
open Language.Ceval
open Language.Env

let interpret (filename : string) =
  let channel =
    try open_in filename
    with Sys_error msg ->
      print_endline ("Error opening file: " ^ msg);
      exit 1
  in
  let file_contents =
    let rec read_all acc =
      try
        let line = input_line channel in
        read_all (acc ^ line ^ "\n")
      with End_of_file -> acc
    in
    read_all ""
  in
  close_in channel;

  let tokens =
    lex (file_contents |> String.to_seq |> List.of_seq)
    |> List.map (fun t -> t.token_type)
  in

  match program_parser tokens with
  | None ->
      print_endline "Parsing failed";
      exit 1
  | Some (program, _) ->
      let condensed_program = List.map condense_defn program in

      let static_env : static_env = built_ins_types in
      let dynamic_env : env = initial_env () in

      (* use fold_left to iterate through the definitions and evaluate them *)
      let static_env, dynamic_env =
        List.fold_left
          (fun (static_env, dynamic_env) defn ->
            match generate_defn static_env defn with
            | Ok new_bindings ->
                (* TODO: propagate the monadic errors *)
                let new_dynamic_bindings =
                  match eval_defn defn dynamic_env with
                  | Ok v -> v
                  | Error _ -> failwith "Evaluation failed"
                in
                (new_bindings @ static_env, new_dynamic_bindings @ dynamic_env)
            | Error e ->
                print_endline (string_of_type_check_error e);
                exit 1)
          (static_env, dynamic_env) condensed_program
      in

      (* As of now, we don't really need to do anything with the resulting
         environments *)
      ignore static_env;
      ignore dynamic_env

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
