open Language.Parse
open Language.Lex
open Language.Reader
open Language.Condense
open Language.Ceval_defn
open Language.Typecheck
open Language.Env
open Language.Ceval
open Language.Ctostringtree.CToStringTree
open Repl

let () = ignore string_of_c_defn

let get_dir () : string =
  if Array.length Sys.argv = 1 then
    let () = print_endline "A command line argument must be provided" in
    exit 1
  else Sys.argv.(1)

let execute_definitions env static_env type_env static_type_env =
  (* if there's a type error *)
  List.fold_left
    (fun (env, static_env, type_env, static_type_env) defn ->
      (* type check the definition *)
      let new_env, new_static_env, new_type_env, new_static_type_env, _, _ =
        eval_defn defn env static_env type_env static_type_env
      in
      (new_env, new_static_env, new_type_env, new_static_type_env))
    (env, static_env, type_env, static_type_env)

let run_run (dir : string) : unit =
  let contents : string = read dir in

  let tokens = contents |> list_of_string |> lex in
  let program = parse_program tokens |> condense_program in

  (* print the program *)

  (* get the code mapping *)
  let env =
    List.map
      (fun (id, code) ->
        let v = eval_c_empty_env code in
        (id, v))
      code_mapping
    @ built_ins_values
  in
  let static_env =
    List.map
      (fun (id, code) ->
        (* get the type of the value *)
        let tokens : token list = code |> list_of_string |> lex in
        let e, _ = parse_expr tokens in
        let c_e = condense_expr e in
        let t = type_of_c_expr c_e [] [] in
        (id, t))
      code_mapping
    @ built_ins_types
  in

  (* the type env is initially empty *)
  let env, static_env, type_env, static_type_env =
    execute_definitions env static_env [] [] program
  in

  repl_loop env static_env type_env static_type_env

let () =
  (* try run_run (get_dir ()) with | _ -> print_endline "Error"; exit 1 *)
  if Array.length Sys.argv = 1 then run_repl () else run_run (get_dir ())

(* 

   Add an argument where an expression can be passed in to be evaluated as a
   command line argument arg 0: the program arg 1: mode (repl, run, eval) arg 2:
   the expression to be evaluated (if mode is eval), or the file to be run (if
   mode is run) *)
