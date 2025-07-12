open Language.Lex
open Language.Parser.ExprOrDefnParser
open Language.Condense
open Language.C_to_string
open Language.Typecheck
open Language.Ceval
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

let print_value_and_type value typ =
  print_colored color_yellow (string_of_value value);
  print_colored color_blue " : ";
  print_colored_line color_cyan (string_of_c_type typ)

let print_name_info name =
  print_colored color_blue "Name : ";
  print_colored_line color_magenta name

type repl_result =
  | NoChange
  | NewBindings of static_env * type_env
  | Quit

(** [read_multiline ()] reads input from the user until a line containing only
    ";;" is encountered. The ;; can be on the first line or any subsequent line.
    Handles Ctrl+C gracefully and provides helpful prompts.
    @return The concatenated input lines *)
let rec read_multiline ?(prompt="") () =
  try
    let line = 
      if prompt = "" then read_line ()
      else (print_string prompt; flush_all (); read_line ())
    in
    if String.trim line = ";;" then ""
    else if String.ends_with ~suffix:";;" (String.trim line) then
      String.sub line 0 (String.length line - 2)
    else line ^ "\n" ^ read_multiline ~prompt:(color_dim ^ "  > " ^ color_reset) ()
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
  print_colored_line (color_bold ^ color_cyan) "LambdaScript REPL Commands:";
  print_colored_line color_green "  :help, :h        Show this help message";
  print_colored_line color_green "  :quit, :q        Exit the REPL";
  print_colored_line color_green "  :env             Show current environment";
  print_colored_line color_green "  :type <expr>     Show type of expression";
  print_colored_line color_green "  :load <file>     Load definitions from file";
  print_colored_line color_green "  :history         Show command history";
  print_colored_line color_green "  :clear           Clear the screen";
  print_newline ();
  print_colored_line color_yellow "Language Usage:";
  print_colored_line color_cyan "  - Enter expressions or definitions";
  print_colored_line color_cyan "  - End input with ;; on a new line";
  print_colored_line color_cyan "  - Example: let x = 5;;";
  print_newline ()

let handle_command cmd static_env type_env history =
  let trimmed = String.trim cmd in
  if trimmed = ":help" || trimmed = ":h" then (
    print_help ();
    NoChange
  ) else if trimmed = ":quit" || trimmed = ":q" then
    Quit
  else if trimmed = ":clear" then (
    (* ANSI escape sequence to clear screen *)
    print_string "\027[2J\027[H";
    flush_all ();
    print_colored_line (color_bold ^ color_cyan) "💻 LambdaScript REPL";
    print_colored_line color_dim "Type :help for commands, :quit to exit";
    print_newline ();
    NoChange
  ) else if trimmed = ":history" then (
    print_colored_line (color_bold ^ color_cyan) "Command History:";
    if List.length history = 0 then
      print_colored_line color_dim "  (empty)"
    else (
      List.rev history 
      |> List.mapi (fun i cmd -> (i + 1, cmd))
      |> List.iter (fun (i, cmd) ->
          print_colored color_dim (Printf.sprintf "%3d  " i);
          print_colored_line color_white cmd
        )
    );
    print_newline ();
    NoChange
  ) else if trimmed = ":env" then (
    print_colored_line (color_bold ^ color_cyan) "Current Environment:";
    if static_env = [] then
      print_colored_line color_dim "  (empty)"
    else
      List.iter (fun (name, typ) ->
        print_colored color_magenta ("  " ^ name ^ " : ");
        print_colored_line color_cyan (string_of_c_type typ)
      ) static_env;
    print_newline ();
    NoChange
  ) else if String.starts_with ~prefix:":type " trimmed then (
    let expr_str = String.sub trimmed 6 (String.length trimmed - 6) |> String.trim in
    if expr_str = "" then (
      print_error "Usage: :type <expression>";
      NoChange
    ) else (
      try
        let input = expr_str |> String.to_seq |> List.of_seq in
        let tokens = lex input |> List.map (fun t -> t.token_type) in
        let expr_or_defn = expr_or_defn_parser tokens in
        match expr_or_defn with
        | None -> print_error "Failed to parse expression"; NoChange
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
      with
      | _ -> print_error "Failed to parse expression"; NoChange
    )
  ) else if String.starts_with ~prefix:":load " trimmed then (
    let filename = String.sub trimmed 6 (String.length trimmed - 6) |> String.trim in
    if filename = "" then (
      print_error "Usage: :load <filename>";
      NoChange
    ) else (
      try
        let ic = open_in filename in
        let content = really_input_string ic (in_channel_length ic) in
        close_in ic;
        print_colored_line color_green ("Loading " ^ filename ^ "...");
        
        let input = content |> String.to_seq |> List.of_seq in
        let tokens = lex input |> List.map (fun t -> t.token_type) in
        let expr_or_defn = expr_or_defn_parser tokens in
        
        match expr_or_defn with
        | None -> print_error "Failed to parse file content"; NoChange
        | Some (Expr _, _) -> 
            print_error "File should contain definitions, not expressions";
            NoChange
        | Some (Definition defn, _) ->
            let c_defn = condense_defn defn in
            (match generate_defn static_env type_env c_defn with
            | Error e -> print_error (string_of_type_check_error e); NoChange
            | Ok (new_static_bindings, new_type_env) ->
                print_colored_line color_green "File loaded successfully!";
                List.iter (fun (name, typ) ->
                  print_colored color_magenta ("  " ^ name ^ " : ");
                  print_colored_line color_cyan (string_of_c_type typ)
                ) new_static_bindings;
                NewBindings (new_static_bindings, new_type_env))
      with
      | Sys_error msg -> print_error ("File error: " ^ msg); NoChange
      | _ -> print_error "Failed to load file"; NoChange
    )
  ) else (
    print_error ("Unknown command: " ^ trimmed);
    print_colored_line color_dim "Type :help for available commands";
    NoChange
  )

let repl (static_env : static_env) (dynamic_env : env) (type_env : type_env) (history : string list) :
    repl_result * string list =
  (* Print prompt *)
  print_colored (color_bold ^ color_green) "λ> ";
  flush_all ();

  (* get multiline input from the user *)
  let input = read_multiline () |> String.to_seq |> List.of_seq in
  let input_str = String.of_seq (List.to_seq input) in
  
  (* Handle empty input *)
  if String.trim input_str = "" then
    (NoChange, history)
  (* Check if input is a command *)
  else if String.starts_with ~prefix:":" (String.trim input_str) then
    (handle_command input_str static_env type_env history, history)
  else (
    (* Limit history to 100 entries *)
    let rec take n lst = 
      if n <= 0 then [] 
      else match lst with [] -> [] | h :: t -> h :: take (n-1) t
    in
    let new_history = 
      let hist = input_str :: history in
      if List.length hist > 100 then take 100 hist else hist
    in
  (* lex tokens *)
  let tokens = lex input |> List.map (fun t -> t.token_type) in

  let expr_or_defn = expr_or_defn_parser tokens in

  match expr_or_defn with
  | None ->
      print_error "Parsing failed";
      (NoChange, new_history)
  | Some (Expr expr, _) -> (
      let c_expr = condense_expr expr in
      let result = type_of_c_expr static_env type_env c_expr in
      match result with
      | Ok t ->
          (match eval_c_expr c_expr dynamic_env with
          | Ok value ->
              print_separator ();
              print_value_and_type value t;
              print_separator ()
          | Error e -> print_error (string_of_eval_error e));
          (NoChange, new_history)
      | Error e ->
          print_error (string_of_type_check_error e);
          (NoChange, new_history))
  | Some (Definition defn, _) -> (
      let c_defn = condense_defn defn in
      let result = generate_defn static_env type_env c_defn in
      match result with
      | Error e ->
          print_error (string_of_type_check_error e);
          (NoChange, new_history)
      | Ok (new_static_bindings, new_type_env) ->
          let new_dynamic_bindings =
            unwrap_eval_result (eval_defn c_defn dynamic_env)
          in
          List.iter
            (fun (name, typ) ->
              let value =
                match List.assoc_opt name new_dynamic_bindings with
                | Some v -> v
                | None ->
                    failwith ("Internal error: value for " ^ name ^ " not found")
              in
              print_separator ();
              print_name_info name;
              print_value_and_type value typ;
              print_separator ())
            new_static_bindings;
          (NewBindings (new_static_bindings, new_type_env), new_history))
  )

let rec run_repl_loop static_env dynamic_env type_env history =
  match repl static_env dynamic_env type_env history with
  | (NoChange, new_history) -> run_repl_loop static_env dynamic_env type_env new_history
  | (NewBindings (new_static_bindings, new_type_env), new_history) ->
      run_repl_loop
        (new_static_bindings @ static_env)
        dynamic_env (new_type_env @ type_env) new_history
  | (Quit, _) ->
      print_colored_line (color_bold ^ color_cyan) "Goodbye! 👋";
      exit 0

let run_repl () =
  (* Print welcome message *)
  print_colored_line (color_bold ^ color_cyan) "💻 LambdaScript REPL";
  print_colored_line color_dim "Type :help for commands, :quit to exit";
  print_newline ();

  let static_env = [] in
  let dynamic_env = [] in
  let type_env = [] in
  let history = [] in
  run_repl_loop static_env dynamic_env type_env history

let () = run_repl ()
