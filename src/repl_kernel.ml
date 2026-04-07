(** Shared REPL / playground evaluation (no terminal I/O). *)

open Lex
open Parser.ExprOrDefnParser
open Parser.ProgramParser
open Condense
open C_to_string
open Cexpr
module TC = Typecheck

open Ceval

(** Prelude [defn list] and [List.length (condense_program prelude)] after a
    successful [merge_prelude]. User chunks are condensed as [prelude @ user] so
    [impl] sees [inter] from the prelude, then we drop the prelude prefix. *)
let prelude_defns_for_condense : Expr.defn list option ref = ref None

let prelude_condensed_defn_count : int ref = ref 0

let clear_prelude_condense_cache () =
  prelude_defns_for_condense := None;
  prelude_condensed_defn_count := 0

let list_drop n xs =
  let rec go n xs =
    if n <= 0 then xs else match xs with [] -> [] | _ :: t -> go (n - 1) t
  in
  go n xs

let condense_user_defns (user_defns : Expr.defn list) : c_defn list =
  match !prelude_defns_for_condense with
  | None -> condense_program user_defns
  | Some prel ->
      let all = condense_program (prel @ user_defns) in
      list_drop !prelude_condensed_defn_count all

let is_internal_repl_binding name =
  String.starts_with ~prefix:"__forge_dict_" name
  || String.starts_with ~prefix:"__dict_" name

let filter_repl_bindings bindings =
  List.filter (fun (name, _) -> not (is_internal_repl_binding name)) bindings

let process_condensed_defns ?after_step (static_env : static_env)
    (dynamic_env : env) (type_env : TC.type_env) (c_defns : c_defn list) :
    (static_env * env * TC.type_env * static_env * env * TC.type_env) TC.type_check_result
    =
  let rec go se te de acc_s acc_d acc_te = function
    | [] -> TC.Ok (se, de, te, acc_s, acc_d, acc_te)
    | cd :: rest ->
        match TC.generate_defn se te cd with
        | TC.Error e -> TC.Error e
        | TC.Ok (nb, nte, _) ->
            let cd = TC.elaborate_defn (nb @ se) (nte @ te) cd in
            let nd = unwrap_eval_result (eval_defn cd de) in
            let de' = nd @ de in
            (match after_step with Some f -> f nb de' | None -> ());
            go (nb @ se) (nte @ te) de' (acc_s @ nb) (acc_d @ nd) (acc_te @ nte)
            rest
  in
  go static_env type_env dynamic_env [] [] [] c_defns

let merge_prelude (static_env : static_env) (dynamic_env : env) (type_env : TC.type_env) :
    (static_env * env * TC.type_env, string) result =
  let pre = Prelude.contents () in
  if String.trim pre = "" then (
    clear_prelude_condense_cache ();
    Ok (static_env, dynamic_env, type_env))
  else
    let input = pre |> String.to_seq |> List.of_seq in
    let tokens = Lex.lex input |> List.map (fun t -> t.token_type) in
    match program_parser tokens with
    | Some (program, []) ->
        if program = [] then (
          clear_prelude_condense_cache ();
          Ok (static_env, dynamic_env, type_env))
        else
          let c_defns = condense_program program in
          prelude_defns_for_condense := Some program;
          prelude_condensed_defn_count := List.length c_defns;
          (match process_condensed_defns static_env dynamic_env type_env c_defns with
          | TC.Ok (se, de, te, _, _, _) -> Ok (se, de, te)
          | TC.Error e ->
              clear_prelude_condense_cache ();
              Error ("Prelude: " ^ TC.string_of_type_check_error e))
    | Some (_, rem) ->
        clear_prelude_condense_cache ();
        Error
          (Printf.sprintf "Prelude: %d token(s) left after parse" (List.length rem))
    | None ->
        clear_prelude_condense_cache ();
        Error "Prelude: parse failed"

type eval_outcome =
  | Ev_error of string
  | Ev_expr of { typ : string; value : string }
  | Ev_defs of { bindings : (string * string * string) list }
      (** (name, value string, type string) *)

let eval_user_input (static_env : static_env) (dynamic_env : env) (type_env : TC.type_env)
    (source : string) : eval_outcome * static_env * env * TC.type_env =
  let input = source |> String.to_seq |> List.of_seq in
  let tokens = lex input |> List.map (fun t -> t.token_type) in

  let collect_bindings new_static new_dynamic =
    filter_repl_bindings new_static
    |> List.map (fun (name, typ) ->
           let value = List.assoc name new_dynamic in
           (name, string_of_value value, string_of_c_type typ))
  in

  match program_parser tokens with
  | Some (program, []) when program <> [] ->
      let c_defns = condense_user_defns program in
      (match process_condensed_defns static_env dynamic_env type_env c_defns with
      | TC.Error e -> (Ev_error (TC.string_of_type_check_error e), static_env, dynamic_env, type_env)
      | TC.Ok (se, de, te, new_s, new_d, _new_te) ->
          let bindings = collect_bindings new_s new_d in
          ( Ev_defs { bindings },
            se,
            de,
            te ))
  | _ -> (
      match expr_or_defn_parser tokens with
      | None -> (Ev_error "Parsing failed", static_env, dynamic_env, type_env)
      | Some (Expr expr, _) -> (
          let c_expr = condense_expr expr in
          match TC.type_of_c_expr static_env type_env c_expr with
          | TC.Error e -> (Ev_error (TC.string_of_type_check_error e), static_env, dynamic_env, type_env)
          | TC.Ok t ->
              let c_expr = TC.elaborate_expr static_env type_env c_expr in
              (match eval_c_expr c_expr dynamic_env with
              | Ok value ->
                  ( Ev_expr { typ = string_of_c_type t; value = string_of_value value },
                    static_env,
                    dynamic_env,
                    type_env )
              | Error e -> (Ev_error (string_of_eval_error e), static_env, dynamic_env, type_env)))
      | Some (Definition defn, _) -> (
          let c_defns = condense_user_defns [ defn ] in
          match process_condensed_defns static_env dynamic_env type_env c_defns with
          | TC.Error e -> (Ev_error (TC.string_of_type_check_error e), static_env, dynamic_env, type_env)
          | TC.Ok (se, de, te, new_s, new_d, _new_te) ->
              let bindings = collect_bindings new_s new_d in
              (Ev_defs { bindings }, se, de, te)))
