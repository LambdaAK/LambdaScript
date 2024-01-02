open Option.Option
open Cexpr
open Ceval
open Typecheck

type type_env = (string * c_type) list
type new_value_bindings_ids = string list
type new_type_bindings_ids = string list

let rec eval_defn (d : c_defn) (env : env) (static_env : static_env)
    (type_env : type_env) :
    env * static_env * type_env * new_value_bindings_ids * new_type_bindings_ids
    =
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
  | CTypeDefn (type_name, t) ->
      let new_type_env = (type_name, t) :: type_env in
      (env, static_env, new_type_env, [], [ type_name ])
  | CUnionDefn (type_name, constructors, type_vars) ->
      ignore type_name;
      ignore constructors;
      ignore type_vars;

      (* 

         for each constructor, add the type of that constructor to the static
         environment

         add the type to the type environment *)
      let new_static_bindings =
        List.map
          (function
            | CNullaryConstructor name -> (name, TypeName type_name)
            | CUnaryConstructor (name, input_type) ->
                (name, FunctionType (input_type, TypeName type_name)))
          constructors
      in
      let new_static_env = new_static_bindings @ static_env in

      let new_type = wrap type_vars (UnionType constructors) in

      let new_type_env = (type_name, new_type) :: type_env in

      (env, new_static_env, new_type_env, [], [ type_name ])

and wrap type_vars t =
  match type_vars with
  | [] -> t
  | arg :: rest -> PolymorphicType (arg, wrap rest t)
