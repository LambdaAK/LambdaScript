open Lex
open Parser.ExprParser
open Condense
open Typecheck
open Cexpr
open Env

(** [build_full_static_env ()] builds the complete static environment including
    types for all code_mapping entries (map, filter, list_length, etc.) and
    built_ins. This is needed for type-checking programs that use these
    functions. *)
let build_full_static_env () : static_env =
  let code_mapping_types =
    List.map
      (fun (id, code) ->
        let tokens = code |> list_of_string |> lex in
        let token_types = List.map (fun t -> t.token_type) tokens in
        match expr_parser token_types with
        | None -> failwith ("Failed to parse code_mapping for " ^ id)
        | Some (e, _) -> (
            let c_e = condense_expr e in
            match type_of_c_expr built_ins_types [] c_e with
            | Ok t -> (id, t)
            | Error e -> failwith (string_of_type_check_error e)))
      code_mapping
  in
  code_mapping_types @ built_ins_types
