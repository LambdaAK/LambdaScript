open Language.Lex
open Language.Parser.ExprOrDefnParser
open Language.Condense
open Language.C_to_string
open Language.Typecheck
open Language.Ceval
open Language.Cexpr

type repl_result =
  | NoChange
  | NewBindings of static_env * env

let repl (static_env : static_env) (dynamic_env : env) : repl_result =
  (* get input from the user . convert to list of chars*)
  let input = read_line () |> String.to_seq |> List.of_seq in
  (* lex tokens *)

  let tokens = lex input |> List.map (fun t -> t.token_type) in

  let expr_or_defn = expr_or_defn_parser tokens in

  match expr_or_defn with
  | None ->
      print_endline "Parsing failed";
      NoChange
  | Some (Expr expr, _) ->
      ((* parsing succeeded *)
       (* condense the expression *)
       let c_expr = condense_expr expr in
       (* type check the expression *)
       match type_of_c_expr static_env c_expr with
       | Ok t ->
           (* it typechecked properly *)
           (* evaluate the expression *)
           let value = eval_c_expr c_expr dynamic_env in
           (* pretty print the type and value *)
           print_endline "-----------------------------";
           print_endline ("Type : " ^ string_of_c_type t);
           print_endline ("Value: " ^ string_of_value value);
           print_endline "-----------------------------"
       | Error e -> print_endline (string_of_type_check_error e));
      NoChange
  | Some (Definition defn, _) -> (
      (* condense the definition *)
      let c_defn = condense_defn defn in

      match generate_defn static_env c_defn with
      | Error e ->
          print_endline (string_of_type_check_error e);
          NoChange
      | Ok new_static_bindings ->
          (*evaluate the definition, since it typechcked*)
          let new_dynamic_bindings = eval_defn c_defn dynamic_env in
          (* print all of the new bindings *)
          List.iter
            (fun (name, typ) ->
              let value =
                match List.assoc_opt name new_dynamic_bindings with
                | Some v -> v
                | None ->
                    failwith ("Internal error: value for " ^ name ^ " not found")
              in
              print_endline "-----------------------------";
              print_endline ("Name : " ^ name);
              print_endline ("Type : " ^ string_of_c_type typ);
              print_endline ("Value: " ^ string_of_value value);
              print_endline "-----------------------------")
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
  let static_env = [] in
  let dynamic_env = [] in
  run_repl_loop static_env dynamic_env

let () = run_repl ()
