open Cexpr
open Ceval
open Typecheck
open Lex
open Parse
open Condense

(* definitions of initial dynamic and static environments *)

let built_ins : (string * value * c_type) list =
  [ ("println", BuiltInFunction Println, StringType => UnitType) ]

let built_ins_values : (string * value) list =
  List.map (fun (id, v, _) -> (id, v)) built_ins

let built_ins_types : (string * c_type) list =
  List.map (fun (id, _, t) -> (id, t)) built_ins

let code_mapping : (string * string) list =
  [ ("not", {|
\a -> if a then false else true
|}) ]

let initial_env : (string * value) list =
  List.map
    (fun (id, code) ->
      let v : value = eval_c_empty_env code in
      (id, v))
    code_mapping
  @ built_ins_values

let initial_static_env : (string * c_type) list =
  List.map
    (fun (id, code) ->
      (* get the type of the value *)
      let tokens : token list = code |> list_of_string |> lex in
      let e, _ = parse_expr tokens in
      let c_e = condense_expr e in
      let t = type_of_c_expr c_e [] in
      (id, t))
    code_mapping
  @ built_ins_types

(* definitions of the REPL *)
