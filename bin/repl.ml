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

(* Color printing functions *)
let print_colored color text = print_string (color ^ text ^ color_reset)
let print_colored_line color text = print_endline (color ^ text ^ color_reset)

let print_separator () =
  print_colored_line color_dim "-----------------------------"

let print_error msg = print_colored_line color_red ("Error: " ^ msg)

let print_type_info typ =
  print_colored color_blue "Type : ";
  print_colored_line color_cyan (string_of_c_type typ)

let print_value_info value =
  print_colored color_blue "Value: ";
  print_colored_line color_yellow (string_of_value value)

let print_name_info name =
  print_colored color_blue "Name : ";
  print_colored_line color_magenta name

type repl_result =
  | NoChange
  | NewBindings of static_env * env

(** [read_multiline ()] reads input from the user until a line containing only
    ";;" is encountered. The ;; can be on the first line or any subsequent line.
    @return The concatenated input lines *)
let rec read_multiline () =
  let line = read_line () in
  if String.trim line = ";;" then ""
  else if String.ends_with ~suffix:";;" (String.trim line) then
    String.sub line 0 (String.length line - 2)
  else line ^ "\n" ^ read_multiline ()

let repl (static_env : static_env) (dynamic_env : env) : repl_result =
  (* Print prompt *)
  print_colored (color_bold ^ color_green) "λ> ";
  flush_all ();

  (* get multiline input from the user *)
  let input = read_multiline () |> String.to_seq |> List.of_seq in
  (* lex tokens *)
  let tokens = lex input |> List.map (fun t -> t.token_type) in

  let expr_or_defn = expr_or_defn_parser tokens in

  match expr_or_defn with
  | None ->
      print_error "Parsing failed";
      NoChange
  | Some (Expr expr, _) ->
      ((* parsing succeeded *)
       (* condense the expression *)
       let c_expr = condense_expr expr in
       (* type check the expression *)
       match type_of_c_expr static_env c_expr with
       | Ok t -> begin
           (* it typechecked properly *)
           (* evaluate the expression *)
           begin
             match eval_c_expr c_expr dynamic_env with
             | Ok value ->
                 (* pretty print the type and value *)
                 print_separator ();
                 print_type_info t;
                 print_value_info value;
                 print_separator ()
             | Error e -> print_error (string_of_eval_error e)
           end
         end
       | Error e -> print_error (string_of_type_check_error e));
      NoChange
  | Some (Definition defn, _) -> (
      (* condense the definition *)
      let c_defn = condense_defn defn in

      match generate_defn static_env c_defn with
      | Error e ->
          print_error (string_of_type_check_error e);
          NoChange
      | Ok new_static_bindings ->
          (*evaluate the definition, since it typechcked*)
          let new_dynamic_bindings =
            unwrap_eval_result (eval_defn c_defn dynamic_env)
          in
          (* print all of the new bindings *)
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
              print_type_info typ;
              print_value_info value;
              print_separator ())
            new_static_bindings;
          NewBindings (new_static_bindings, new_dynamic_bindings))

(* parse defn or expr *)

let rec run_repl_loop static_env dynamic_env =
  match repl static_env dynamic_env with
  | NoChange -> run_repl_loop static_env dynamic_env
  | NewBindings (new_static_bindings, new_dynamic_bindings) ->
      run_repl_loop
        (new_static_bindings @ static_env)
        (new_dynamic_bindings @ dynamic_env)

let run_repl () =
  (* Print welcome message *)
  print_colored_line (color_bold ^ color_cyan) "💻 LambdaScript REPL";
  print_newline ();

  let static_env = [] in
  let dynamic_env = [] in
  run_repl_loop static_env dynamic_env

let () = run_repl ()
