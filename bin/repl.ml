open Language.Lex
open Language.Parser.ExprOrDefnParser
open Language.Condense
open Language.C_to_string
open Language.Typecheck
open Language.Cexpr

(* ANSI color codes *)
let color_reset = "\027[0m"
let color_red = "\027[31m"
let color_green = "\027[32m"
let color_yellow = "\027[33m"
let color_blue = "\027[34m"
let color_magenta = "\027[35m"
let color_cyan = "\027[36m"
let color_bold = "\027[1m"
let color_dim = "\027[2m"
let color_white = "\027[37m"

(* Color printing functions *)
let print_colored color text = print_string (color ^ text ^ color_reset)
let print_colored_line color text = print_endline (color ^ text ^ color_reset)

let print_separator () =
  print_colored_line color_dim "-----------------------------"

let print_error msg = print_colored_line color_red ("Error: " ^ msg)

let print_name_info name =
  print_colored color_blue "Name : ";
  print_colored_line color_magenta name

type repl_result =
  | NoChange
  | StepEnv of static_env * env * type_env
  | Quit

(** [read_multiline ()] reads input from the user until a line containing only
    ";;" is encountered. The ;; can be on the first line or any subsequent line.
    Handles Ctrl+C gracefully and provides helpful prompts.
    @return The concatenated input lines *)
let rec read_multiline ?(prompt = "") () =
  try
    let line =
      if prompt = "" then read_line ()
      else (
        print_string prompt;
        flush_all ();
        read_line ())
    in
    if String.trim line = ";;" then ""
    else if String.ends_with ~suffix:";;" (String.trim line) then
      String.sub line 0 (String.length line - 2)
    else
      line ^ "\n" ^ read_multiline ~prompt:(color_dim ^ "  > " ^ color_reset) ()
  with
  | Sys.Break ->
      print_newline ();
      print_colored_line color_yellow "Input cancelled. Use :quit to exit.";
      ""
  | End_of_file ->
      print_newline ();
      print_colored_line (color_bold ^ color_cyan) "Goodbye! 👋";
      exit 0

let print_help () =
  print_colored_line (color_bold ^ color_cyan) "Forge REPL Commands:";
  print_colored_line color_green "  :help, :h        Show this help message";
  print_colored_line color_green "  :quit, :q        Exit the REPL";
  print_colored_line color_green "  :env             Show current environment";
  print_colored_line color_green "  :type <expr>     Show type of expression";
  print_colored_line color_dim
    "  (preload a file: make repl FILE=path or: dune exec ./bin/repl.exe path)";
  print_colored_line color_green "  :history         Show command history";
  print_colored_line color_green "  :clear           Clear the screen";
  print_newline ();
  print_colored_line color_yellow "Language Usage:";
  print_colored_line color_cyan "  - Enter expressions or definitions";
  print_colored_line color_cyan "  - End input with ;; on a new line";
  print_colored_line color_cyan "  - Example: let x = 5;;";
  print_newline ()

let handle_command cmd static_env _dynamic_env type_env history =
  let trimmed = String.trim cmd in
  if trimmed = ":help" || trimmed = ":h" then (
    print_help ();
    NoChange)
  else if trimmed = ":quit" || trimmed = ":q" then Quit
  else if trimmed = ":clear" then (
    (* ANSI escape sequence to clear screen *)
    print_string "\027[2J\027[H";
    flush_all ();
    print_colored_line (color_bold ^ color_cyan) "💻 Forge REPL";
    print_colored_line color_dim "Type :help for commands, :quit to exit";
    print_newline ();
    NoChange)
  else if trimmed = ":history" then (
    print_colored_line (color_bold ^ color_cyan) "Command History:";
    if List.length history = 0 then print_colored_line color_dim "  (empty)"
    else
      List.rev history
      |> List.mapi (fun i cmd -> (i + 1, cmd))
      |> List.iter (fun (i, cmd) ->
             print_colored color_dim (Printf.sprintf "%3d  " i);
             print_colored_line color_white cmd);
    print_newline ();
    NoChange)
  else if trimmed = ":env" then (
    print_colored_line (color_bold ^ color_cyan) "Current Environment:";
    if static_env = [] then print_colored_line color_dim "  (empty)"
    else
      List.iter
        (fun (name, typ) ->
          print_colored color_magenta ("  " ^ name ^ " : ");
          print_colored_line color_cyan (string_of_c_type typ))
        (Language.Repl_kernel.filter_repl_bindings static_env);
    print_newline ();
    NoChange)
  else if String.starts_with ~prefix:":type " trimmed then (
    let expr_str =
      String.sub trimmed 6 (String.length trimmed - 6) |> String.trim
    in
    if expr_str = "" then (
      print_error "Usage: :type <expression>";
      NoChange)
    else
      try
        let input = expr_str |> String.to_seq |> List.of_seq in
        let tokens = lex input |> List.map (fun t -> t.token_type) in
        let expr_or_defn = expr_or_defn_parser tokens in
        match expr_or_defn with
        | None ->
            print_error "Failed to parse expression";
            NoChange
        | Some (Expr expr, _) ->
            let c_expr = condense_expr expr in
            (match type_of_c_expr static_env type_env c_expr with
            | Ok t ->
                print_colored color_blue "Type: ";
                print_colored_line color_cyan (string_of_c_type t)
            | Error e -> print_error (string_of_type_check_error e));
            NoChange
        | Some (Definition _, _) ->
            print_error "Use :type with expressions, not definitions";
            NoChange
      with _ ->
        print_error "Failed to parse expression";
        NoChange)
  else if String.starts_with ~prefix:":load " trimmed then (
    print_error
      ":load is disabled; start the REPL with a file instead: make repl \
       FILE=path/to/file.ls  (or: dune exec ./bin/repl.exe path)";
    NoChange)
  else (
    print_error ("Unknown command: " ^ trimmed);
    print_colored_line color_dim "Type :help for available commands";
    NoChange)

let repl (static_env : static_env) (dynamic_env : env) (type_env : type_env)
    (history : string list) : repl_result * string list =
  (* Print prompt *)
  print_colored (color_bold ^ color_green) "λ> ";
  flush_all ();

  (* get multiline input from the user *)
  let input = read_multiline () |> String.to_seq |> List.of_seq in
  let input_str = String.of_seq (List.to_seq input) in

  (* Handle empty input *)
  if String.trim input_str = "" then (NoChange, history)
    (* Check if input is a command *)
  else if String.starts_with ~prefix:":" (String.trim input_str) then
    (handle_command input_str static_env dynamic_env type_env history, history)
  else
    (* Limit history to 100 entries *)
    let rec take n lst =
      if n <= 0 then []
      else
        match lst with
        | [] -> []
        | h :: t -> h :: take (n - 1) t
    in
    let new_history =
      let hist = input_str :: history in
      if List.length hist > 100 then take 100 hist else hist
    in
    let outcome, se', de', te' =
      Language.Repl_kernel.eval_user_input static_env dynamic_env type_env input_str
    in
    match outcome with
    | Language.Repl_kernel.Ev_error msg ->
        print_error msg;
        (NoChange, new_history)
    | Language.Repl_kernel.Ev_expr { typ; value } ->
        print_separator ();
        print_colored color_yellow value;
        print_colored color_blue " : ";
        print_colored_line color_cyan typ;
        print_separator ();
        (NoChange, new_history)
    | Language.Repl_kernel.Ev_defs { bindings } ->
        List.iter
          (fun (name, value, typ) ->
            print_separator ();
            print_name_info name;
            print_colored color_yellow value;
            print_colored color_blue " : ";
            print_colored_line color_cyan typ;
            print_separator ())
          bindings;
        (StepEnv (se', de', te'), new_history)

let rec run_repl_loop static_env dynamic_env type_env history =
  match repl static_env dynamic_env type_env history with
  | NoChange, new_history ->
      run_repl_loop static_env dynamic_env type_env new_history
  | StepEnv (se, de, te), new_history ->
      run_repl_loop se de te new_history
  | Quit, _ ->
      print_colored_line (color_bold ^ color_cyan) "Goodbye! 👋";
      exit 0

let load_file_into_env filename static_env dynamic_env type_env =
  try
    let content = Language.Compile_pipeline.read_program_source filename in

    let input = content |> String.to_seq |> List.of_seq in
    let tokens = lex input |> List.map (fun t -> t.token_type) in

    (* Try to parse as a program (multiple definitions) *)
    match Language.Parser.ProgramParser.program_parser tokens with
    | Some (program, []) ->
        (* Successfully parsed entire file as a program *)
        let condensed_program = condense_program program in

        let new_static_env, new_dynamic_env, new_type_env =
          match
            Language.Repl_kernel.process_condensed_defns static_env dynamic_env
              type_env condensed_program
          with
          | Language.Typecheck.Ok (ms, md, mt, _, _, _) ->
              Language.Repl_kernel.remember_user_defns_for_condense program
                condensed_program;
              (ms, md, mt)
          | Language.Typecheck.Error e ->
              print_error (string_of_type_check_error e);
              (static_env, dynamic_env, type_env)
        in

        print_colored_line color_green ("Loaded " ^ filename);
        (new_static_env, new_dynamic_env, new_type_env)
    | Some (_, remaining) ->
        print_error ("Warning: " ^ string_of_int (List.length remaining) ^
                    " tokens remaining after parsing");
        (static_env, dynamic_env, type_env)
    | None ->
        print_error "Failed to parse file";
        (static_env, dynamic_env, type_env)
  with
  | Sys_error msg ->
      print_error ("File error: " ^ msg);
      (static_env, dynamic_env, type_env)
  | e ->
      print_error ("Error loading file: " ^ Printexc.to_string e);
      (static_env, dynamic_env, type_env)

let merge_prelude static_env dynamic_env type_env =
  match Language.Repl_kernel.merge_prelude static_env dynamic_env type_env with
  | Ok (se, de, te) -> (se, de, te)
  | Error msg ->
      print_error msg;
      (static_env, dynamic_env, type_env)

let run_repl ?preload_file () =
  (* Print welcome message *)
  print_colored_line (color_bold ^ color_cyan) "💻 Forge REPL";
  print_colored_line color_dim "Type :help for commands, :quit to exit";
  print_newline ();

  let static_env = Language.Build_env.build_full_static_env () in
  let dynamic_env =
    Language.Ceval.initial_env () |> Language.Ceval.unwrap_eval_result
  in
  let type_env = [] in

  let static_env, dynamic_env, type_env =
    merge_prelude static_env dynamic_env type_env
  in

  (* Load preload file if provided *)
  let static_env, dynamic_env, type_env =
    match preload_file with
    | Some filename ->
        print_colored_line color_dim ("Preloading " ^ filename ^ "...");
        load_file_into_env filename static_env dynamic_env type_env
    | None -> (static_env, dynamic_env, type_env)
  in

  let history = [] in
  run_repl_loop static_env dynamic_env type_env history

let () =
  match Array.length Sys.argv with
  | 1 ->
      (* No arguments - start REPL normally *)
      run_repl ()
  | 2 ->
      (* One argument - preload the file *)
      let filename = Sys.argv.(1) in
      run_repl ~preload_file:filename ()
  | _ ->
      print_endline "Usage: repl [file_to_preload]";
      print_endline "  Start the REPL, optionally preloading definitions from a file";
      exit 1
