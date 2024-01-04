open Option.Option
open Cexpr
open Ceval
open Typecheck

type type_env = (string * c_type) list
type new_value_bindings_ids = string list

let rec eval_defn (d : c_defn) (env : env) (static_env : static_env)
    (type_env : type_env) :
    env * static_env * type_env * new_value_bindings_ids * string list =
  match d with
  | CDefn (pattern, _, body_expression) -> (
      let v : value = eval_c_expr body_expression env in
      let o =
        bind_pat pattern v >>= fun new_bindings ->
        (* generalize the types in new_bindings *)
        let new_env : env = new_bindings @ env in
        let t : c_type = type_of_c_expr body_expression static_env type_env in
        bind_static pattern t >>= fun new_static_bindings ->
        let generalized_static_bindings =
          List.map
            (fun (s, v) -> (s, generalize [] [] type_env v))
            new_static_bindings
        in

        let new_static_env = generalized_static_bindings @ static_env in
        (new_env, new_static_env, List.map fst new_bindings) |> return
      in

      match o with
      | None -> failwith "eval_defn: no pattern matched"
      | Some (new_env, new_static_env, new_bindings_ids) ->
          (new_env, new_static_env, type_env, new_bindings_ids, []))
  | CDefnRec (pattern, _, body_expression) ->
      let let_defn : c_defn =
        CDefn
          ( pattern,
            None,
            EBindRec (pattern, None, body_expression, expr_of_pat pattern) )
      in

      eval_defn let_defn env static_env type_env
  | CTypeDefn (type_name, t, type_vars) ->
      (* for each type_var, create a corresponding universal type

         the left is the old type var, and the right is the new universal type
         var *)
      let args_mapping : (c_type * c_type) list =
        List.map
          (fun t -> (TypeVarWritten t, fresh_universal_type ()))
          type_vars
      in

      (* replace the old types in the constructors with the new types *)
      let args : c_type list = List.map snd args_mapping in

      let new_t = replace_types t args_mapping in

      (* add the new type to the type environment *)
      let new_type = wrap args new_t in

      let new_type_env = (type_name, new_type) :: type_env in

      (env, static_env, new_type_env, [], [ type_name ])
  | CUnionDefn (type_name, constructors, type_vars) ->
      (* 

         for each constructor, add the type of that constructor to the static
         environment

         add the type to the type environment *)

      (* for each type_var, create a corresponding universal type

         the left is the old type var, and the right is the new universal type
         var *)
      let args_mapping : (c_type * c_type) list =
        List.map
          (fun t -> (TypeVarWritten t, fresh_universal_type ()))
          type_vars
      in

      (* replace the old types in the constructors with the new types *)
      let args : c_type list = List.map snd args_mapping in

      (* replace the old types in the constructors with the new types *)
      let new_constructors : c_constructor list =
        List.map
          (function
            | CNullaryConstructor name -> CNullaryConstructor name
            | CUnaryConstructor (name, input_type) ->
                let new_input_type = replace_types input_type args_mapping in
                CUnaryConstructor (name, new_input_type))
          constructors
      in

      (* print the new constructors *)

      (* add the new constructors to the static environment *)
      let new_static_bindings =
        List.map
          (function
            | CNullaryConstructor name -> (name, apply args (TypeName type_name))
            | CUnaryConstructor (name, input_type) ->
                ( name,
                  FunctionType (input_type, apply args (TypeName type_name)) ))
          new_constructors
      in
      let new_static_env = new_static_bindings @ static_env in

      let new_type = wrap args (UnionType new_constructors) in

      let new_type_env = (type_name, new_type) :: type_env in

      (env, new_static_env, new_type_env, [], [ type_name ])

and wrap type_vars t =
  match type_vars with
  | [] -> t
  | arg :: rest -> PolymorphicType (arg, wrap rest t)

and apply type_vars t =
  (* Given list of type_vars a b c d e .... and type t, return the type t a b c
     d e .... *)
  match type_vars with
  | [] -> (* no application *) t
  | arg :: rest ->
      (* t is applied to arg, then that is applied to the rest of the type
         vars *)
      apply rest (AppType (t, arg))
