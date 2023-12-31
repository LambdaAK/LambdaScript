open Cexpr
open Typefixer
open Ctostringtree.CToStringTree

type type_equation = c_type * c_type
type type_equations = type_equation list
type substitutions = type_equations

exception TypeFailure

let split3 (lst : ('a * 'b * 'c) list) : 'a list * 'b list * 'c list =
  let rec split3_helper (lst : ('a * 'b * 'c) list) (a : 'a list) (b : 'b list)
      (c : 'c list) : 'a list * 'b list * 'c list =
    match lst with
    | [] -> (a, b, c)
    | (a', b', c') :: tail -> split3_helper tail (a' :: a) (b' :: b) (c' :: c)
  in
  let a, b, c = split3_helper lst [] [] [] in
  (List.rev a, List.rev b, List.rev c)

let string_of_type_equation ((t1, t2) : type_equation) : string =
  string_of_c_type t1 ^ " = " ^ string_of_c_type t2

let rec string_of_type_equations (c : type_equations) : string =
  match c with
  | [] -> ""
  | (t1, t2) :: c' ->
      string_of_type_equation (t1, t2) ^ "\n" ^ string_of_type_equations c'

let rec string_of_static_env (env : static_env) : string =
  match env with
  | [] -> ""
  | (id, t) :: env' ->
      id ^ " : " ^ string_of_c_type t ^ "\n" ^ string_of_static_env env'

let rec generate (env : static_env) (e : c_expr) : c_type * type_equations =
  match e with
  | EInt _ -> (IntType, [])
  | EFloat _ -> (FloatType, [])
  | EBool _ -> (BoolType, [])
  | EId x ->
      let uninstantiated : c_type = List.assoc x env in
      let t = instantiate uninstantiated in
      (t, [])
  | EString _ -> (StringType, [])
  | EUnit -> (UnitType, [])
  | ENil -> (CListType (fresh_type_var ()), [])
  | EBop (op, e1, e2) -> (
      let t1, c1 = generate env e1 in
      let t2, c2 = generate env e2 in
      (* let type_of_expression: c_type = fresh_type_var () in *)
      match op with
      | CCons ->
          (* Type inference relation for cons env |- e1 :: e2 : [t] -| C1, C2
             env |- e1 : t -| C1 env |- e2 : [t] -| C2 *)
          (t2, ((CListType t1, t2) :: c1) @ c2)
          (* here, t2 should be [t] for some t t1 should be t for some t the
             constraint is that [t1] = t2 *)
      | CPlus | CMinus | CMul | CDiv | CMod ->
          (IntType, ((t1, IntType) :: (t2, IntType) :: c1) @ c2)
      | CGE | CGT | CLE | CLT ->
          (BoolType, ((t1, IntType) :: (t2, IntType) :: c1) @ c2)
      | CEQ | CNE -> (BoolType, ((t1, t2) :: c1) @ c2)
      | CAnd | COr -> (BoolType, ((t1, BoolType) :: (t2, BoolType) :: c1) @ c2))
  | EFunction (pat, cto, body) ->
      let input_type, new_env_bindings, constraints_from_pattern =
        type_of_pat pat
      in
      let constraints_from_type_annotation : type_equations =
        match cto with
        | Some t ->
            [ (input_type, t) ]
            (* the input type must equal the annotated type *)
        | None -> []
      in
      let output_type, c_output = generate (new_env_bindings @ env) body in
      ( input_type => output_type,
        constraints_from_pattern @ constraints_from_type_annotation @ c_output
      )
  | ETernary (e1, e2, e3) ->
      let t1, c1 = generate env e1 in
      let t2, c2 = generate env e2 in
      let t3, c3 = generate env e3 in
      let type_of_expression : c_type = fresh_type_var () in

      ( type_of_expression,
        (t1, BoolType) :: (t2, type_of_expression) :: (t3, type_of_expression)
        :: c1
        @ c2 @ c3 )
  | EApp (first, second) -> (
      match first with
      | EFunction (CIdPat _, _, _) ->
          generate_e_app_function_pat_is_id env first second
      | _ ->
          let t1, c1 = generate env first in
          let t2, c2 = generate env second in
          let type_of_expression : c_type = fresh_type_var () in
          let new_constraint =
            (t1 |> instantiate, t2 |> instantiate => type_of_expression)
          in

          (* print the new constraint *)
          (type_of_expression, (new_constraint :: c1) @ c2))
  | EBindRec (pattern, _, e1, e2) ->
      (* perform the type inference as if it is a function application *)
      let function_id : string =
        match pattern with
        | CIdPat id -> id
        | _ -> failwith "not a valid pattern in typecheck.ml"
      in

      let function_type : c_type = fresh_type_var () in

      let new_env : static_env = (function_id, function_type) :: env in
      let t1, c1 = generate new_env e1 in
      let t2, c2 = generate new_env e2 in
      let new_constraint = (function_type, t1) in

      (t2, (new_constraint :: c1) @ c2)
  | EVector expressions ->
      (* type inference relation for vectors

         env |- (e1, e2, ..., en) : (t1, t2, ..., tn) -| C1, C2, ..., Cn
         ----------------------------------------------------------------- env
         |- e1 : t1 -| C1 env |- e2 : t2 -| C2 ... env |- en : tn -| Cn *)
      let list_of_types, list_of_lists_of_constraints =
        List.split (List.map (generate env) expressions)
      in
      let list_of_types : c_type list = List.map instantiate list_of_types in

      (VectorType list_of_types, List.flatten list_of_lists_of_constraints)
  | ESwitch (e1, branches) ->
      (* get the type of the switching expression *)
      let t1, c1 = generate env e1 in
      let type_that_all_branch_expressions_must_be : c_type =
        fresh_type_var ()
      in

      (* use this in the constraints *)

      (* get the type of each branch *)
      let branch_constraints : type_equation list =
        List.map
          (fun (bp, be) ->
            (* the pattern has to be of the same type as t1 *)
            let type_of_pattern, pattern_env, const = type_of_pat bp in

            let type_of_branch_expression, branch_expression_constraints =
              generate (pattern_env @ env) be
            in
            (* use the pattern env here *)
            let type_of_branch_expression =
              instantiate type_of_branch_expression
            in

            (* instantiate here *)

            (* the type of the branch expression must be the same as the type
               that all branch expressions must be *)
            (type_of_pattern, t1)
            :: ( type_of_branch_expression,
                 type_that_all_branch_expressions_must_be )
            :: const
            @ branch_expression_constraints)
          branches
        |> List.flatten
        (* since each iteration through map returns multiple constraints, the
           list is flattened *)
      in

      (type_that_all_branch_expressions_must_be, c1 @ branch_constraints)
  | EListEnumeration (e1, e2) ->
      let t1, c1 = generate env e1 in
      let t2, c2 = generate env e2 in
      (CListType IntType, ((t1, IntType) :: (t2, IntType) :: c1) @ c2)
  | EListComprehension (e, generators) ->
      let env, generator_constraints =
        List.fold_left
          (fun (env, constraints) (p, e) ->
            let type_of_pattern, pattern_env, const = type_of_pat p in
            let type_of_expression, expression_constraints =
              generate (pattern_env @ env) e
            in
            let type_of_expression = instantiate type_of_expression in
            let new_constraint =
              (type_of_expression, CListType type_of_pattern)
            in
            ( pattern_env @ env,
              (new_constraint :: const) @ expression_constraints @ constraints
            ))
          (env, []) generators
      in

      (* generate the type and constraints of the expression *)
      let type_of_expression, expression_constraints = generate env e in

      ( CListType type_of_expression,
        expression_constraints @ generator_constraints )

and generate_type_env_constraints type_env =
  List.map (fun (id, t) -> (TypeName id, t)) type_env

and generate_e_app_function_pat_is_id (env : static_env) (first : c_expr)
    (second : c_expr) : c_type * type_equations =
  (* start by checking whether first is a function *)
  match first with
  | EFunction (pattern, cto, body) ->
      (* fn pattern -> body *)
      (* 

         since first is a function, and first is being applied to second, the
         type of second must be generalized in the body of first

         for example, if we have (fn f -> f 1 < 2 || f true) (fn x -> x), it is
         crucial that that the type of (fn x -> x), which is initially t1 -> t1
         after unification, is generalized to a type scheme t1. t1 -> t1, so
         that it can be applied to both an int and a bool *)
      let function_id : string =
        match pattern with
        | CIdPat id -> id
        | _ -> failwith "not a valid pattern in typecheck.ml"
      in

      let input_type, new_env_bindings, constraints_from_pattern =
        type_of_pat pattern
      in
      let constraints_from_type_annotation : type_equations =
        match cto with
        | Some t ->
            [ (input_type, t) ]
            (* the input type must equal the annotated type *)
        | None -> []
      in

      let t1, c1 = generate (new_env_bindings @ env) second in
      let generalized_type : c_type =
        generalize c1 (new_env_bindings @ env) t1
      in
      ignore generalized_type;
      let t2, c2 = generate ((function_id, generalized_type) :: env) body in
      (* here ^, (function_id, generlized_type) binds the generalized function
         in the static env inside of body *)
      let output_type = fresh_type_var () in
      ( output_type,
        ((t2, output_type) :: constraints_from_pattern)
        @ constraints_from_type_annotation @ c1 @ c2 )
  | _ ->
      print_endline "not a function";
      (* first is not a function, so generalization is not necessary *)
      let e1 = first in
      let e2 = second in
      let t1, c1 = generate env e1 in
      let t2, c2 = generate env e2 in
      let type_of_expression : c_type = fresh_type_var () in
      (type_of_expression, ((t1, t2 => type_of_expression) :: c1) @ c2)

and type_of_pat (p : c_pat) : c_type * static_env * type_equations =
  match p with
  | CIdPat id ->
      let new_var : c_type = fresh_type_var () in
      (new_var, [ (id, new_var) ], [])
  | CUnitPat -> (UnitType, [], [])
  | CWildcardPat -> (fresh_type_var (), [], [])
  | CVectorPat patterns ->
      let list_of_types, list_of_lists_of_envs, lists_of_equations =
        split3 (List.map type_of_pat patterns)
      in
      ( VectorType list_of_types,
        List.flatten list_of_lists_of_envs,
        List.flatten lists_of_equations )
  | CIntPat _ -> (IntType, [], [])
  | CBoolPat _ -> (BoolType, [], [])
  | CStringPat _ -> (StringType, [], [])
  | CNilPat -> (CListType (fresh_type_var ()), [], [])
  | CConsPat (p1, p2) ->
      let t1, env1, c1 = type_of_pat p1 in
      let t2, env2, c2 = type_of_pat p2 in

      (* since h :: t is a list, and h : t1 and t : t2, it must hold that [t1] =
         t2 return this constraint as well *)
      (CListType t1, env1 @ env2, (CListType t1, t2) :: (c1 @ c2))
(* this might be wrong. check it later *)

and reduce_eq (c : type_equations) : substitutions =
  match c with
  | [] -> []
  | (t1, t2) :: c' -> (
      if t1 = t2 then reduce_eq c'
      else if inside t1 t2 || inside t2 t1 then (
        print_endline "TYPE FAILURE";
        raise TypeFailure)
      else
        match (t1, t2) with
        | TypeVar id, _ when not (inside t1 t2) ->
            (t1, t2) :: reduce_eq (substitute id t2 c')
        | _, TypeVar _ ->
            reduce_eq ((t2, t1) :: c')
            (* this switches the order of the types, then the branch before will
               be hit on the next iteration *)
        | TypeName name1, TypeName name2 ->
            (* Replace the lexographically larger one with the smaller one *)
            let smaller, larger =
              if name1 < name2 then (t1, t2) else (t2, t1)
            in

            let new_equations =
              List.map
                (fun (t1, t2) ->
                  ( replace_type smaller larger t1,
                    replace_type smaller larger t2 ))
                c'
            in
            (TypeName name1, TypeName name2) :: reduce_eq new_equations
        | TypeName name, t ->
            (* replace the type name with the type *)
            let new_equations =
              List.map
                (fun (t1, t2) ->
                  ( replace_type (TypeName name) t t1,
                    replace_type (TypeName name) t t2 ))
                c'
            in

            print_endline "new equations";
            print_endline (string_of_type_equations new_equations);
            (TypeName name, t) :: reduce_eq new_equations
        | _, TypeName _ ->
            (* use the previous branch *) reduce_eq ((t2, t1) :: c')
        | FunctionType (i1, o1), FunctionType (i2, o2) ->
            reduce_eq ((i1, i2) :: (o1, o2) :: c')
        | CListType et1, CListType et2 -> reduce_eq ((et1, et2) :: c')
        | VectorType types1, VectorType types2 -> (
            match (types1, types2) with
            | type1 :: tail1, type2 :: tail2 ->
                reduce_eq
                  ((type1, type2) :: (VectorType tail1, VectorType tail2) :: c')
            | _ ->
                (* one of them is empty, so the vectors are not the same size *)
                raise TypeFailure)
        | _ ->
            print_endline "TYPE FAILURE";
            raise TypeFailure)

and get_type (var : c_type) (subs : substitutions) : c_type =
  match var with
  | TypeVar _ -> (
      let looked_up_type = get_type_of_type_var_if_possible var subs in
      match looked_up_type with
      | FunctionType (i, o) -> FunctionType (get_type i subs, get_type o subs)
      | CListType et -> CListType (get_type et subs)
      | VectorType types ->
          VectorType (List.map (fun t -> get_type t subs) types)
      | _ -> looked_up_type)
  | FunctionType (i, o) -> FunctionType (get_type i subs, get_type o subs)
  | VectorType types -> VectorType (List.map (fun t -> get_type t subs) types)
  | IntType -> IntType
  | FloatType -> FloatType
  | BoolType -> BoolType
  | StringType -> StringType
  | UnitType -> UnitType
  | CListType et -> CListType (get_type et subs)
  | UniversalType _ -> var
  | TypeName _ -> var
  | _ -> failwith "not a type var2"

and get_type_of_type_var_if_possible (var : c_type) (subs : substitutions) :
    c_type =
  match var with
  | TypeVar _ | TypeVarWritten _ -> (
      try
        let looked_up = List.assoc var subs in
        match looked_up with
        | TypeVar _ -> get_type_of_type_var_if_possible looked_up subs
        | _ -> looked_up
      with Not_found -> var)
  | _ -> failwith "not a type var3"

and type_of_c_expr (e : c_expr) (static_env : static_env)
    (type_env : (string * c_type) list) : c_type =
  ignore type_env;
  let t, generated_constraints = generate static_env e in

  (* type *)
  print_endline "the type is";
  print_endline (string_of_c_type t);

  (* constraints *)
  let type_env_constraints = generate_type_env_constraints type_env in

  let constraints = generated_constraints @ type_env_constraints in

  (* print the constraints *)
  constraints
  |> List.iter (fun (t1, t2) ->
         print_endline (string_of_c_type t1 ^ " = " ^ string_of_c_type t2));

  let constraints_without_written_type_vars =
    replace_written_types constraints
  in
  let solution : substitutions =
    reduce_eq constraints_without_written_type_vars
  in
  get_type t solution |> fix

and inside (inside_type : c_type) (outside_type : c_type) : bool =
  match outside_type with
  | _ when inside_type = outside_type -> true
  | FunctionType (i, o) -> inside inside_type i || inside inside_type o
  (* there needs to be a case here for vectors *)
  | VectorType types -> List.exists (fun t -> inside inside_type t) types
  | CListType et -> inside inside_type et
  | _ -> false

and is_basic_type (t : c_type) : bool =
  match t with
  | IntType | FloatType | BoolType | StringType | UnitType -> true
  | TypeVar _ | UniversalType _ -> false
  | TypeVarWritten _ -> false (* fix this later if necessary *)
  | FunctionType (i, o) -> is_basic_type i && is_basic_type o
  | VectorType types -> List.for_all is_basic_type types
  | CListType et -> is_basic_type et
  | TypeName _ -> true
  | UnionType _ -> failwith "union type found in is_basic_type"

and substitute (var_id : int) (t : c_type) (equations : type_equations) :
    type_equations =
  match equations with
  | [] -> []
  | (t1, t2) :: equations' ->
      (substitute_in_type t1 var_id t, substitute_in_type t2 var_id t)
      :: substitute var_id t equations'

and substitute_in_type (type_subbing_in : c_type)
    (type_var_id_subbing_for : int) (substitute_with : c_type) : c_type =
  match type_subbing_in with
  | IntType -> IntType
  | BoolType -> BoolType
  | StringType -> StringType
  | UnitType -> UnitType
  | TypeVar id ->
      if id = type_var_id_subbing_for then substitute_with else TypeVar id
  | FunctionType (t1, t2) ->
      FunctionType
        ( substitute_in_type t1 type_var_id_subbing_for substitute_with,
          substitute_in_type t2 type_var_id_subbing_for substitute_with )
  | VectorType types ->
      VectorType
        (List.map
           (fun t ->
             substitute_in_type t type_var_id_subbing_for substitute_with)
           types)
  | CListType et ->
      CListType (substitute_in_type et type_var_id_subbing_for substitute_with)
  | TypeName n -> TypeName n
  | _ -> failwith "not a type var"

and replace_type (replace_with : c_type) (get_rid_of : c_type) (t : c_type) :
    c_type =
  if t = get_rid_of then replace_with
  else
    match t with
    | FunctionType (i, o) ->
        FunctionType
          ( replace_type replace_with get_rid_of i,
            replace_type replace_with get_rid_of o )
    | VectorType types ->
        VectorType
          (List.map (fun t -> replace_type replace_with get_rid_of t) types)
    | CListType et -> CListType (replace_type replace_with get_rid_of et)
    | _ -> t

and replace_type_in_equations (replace_with : c_type) (get_rid_of : c_type)
    (equations : type_equations) : type_equations =
  match equations with
  | [] -> []
  | (t1, t2) :: equations' ->
      ( replace_type replace_with get_rid_of t1,
        replace_type replace_with get_rid_of t2 )
      :: replace_type_in_equations replace_with get_rid_of equations'

and substitute_written_var_with_type_var (var_id : int)
    (written_var_id : string) (t : c_type) : c_type =
  match t with
  | TypeVarWritten id when id = written_var_id -> TypeVar var_id
  | FunctionType (i, o) ->
      FunctionType
        ( substitute_written_var_with_type_var var_id written_var_id i,
          substitute_written_var_with_type_var var_id written_var_id o )
  | VectorType types ->
      VectorType
        (List.map
           (fun t ->
             substitute_written_var_with_type_var var_id written_var_id t)
           types)
  | _ -> t

and substitute_written_vars_in_equations (var_id : int)
    (written_var_id : string) (equations : type_equations) : type_equations =
  match equations with
  | [] -> []
  | (t1, t2) :: equations' ->
      ( substitute_written_var_with_type_var var_id written_var_id t1,
        substitute_written_var_with_type_var var_id written_var_id t2 )
      :: substitute_written_vars_in_equations var_id written_var_id equations'

and replace_written_types (equations : type_equations) : type_equations =
  let first, second = equations |> List.split in
  let all_types : c_type list = first @ second in

  let rec find_written_type (types : c_type list) : string option =
    match types with
    | [] -> None
    | TypeVarWritten id :: _ -> Some id
    (* check for function and vector types *)
    | FunctionType (i, o) :: tail ->
        (* put them in from and call the function again *)
        let new_types : c_type list = i :: o :: tail in
        find_written_type new_types
    | VectorType types :: tail ->
        let new_types : c_type list = types @ tail in
        find_written_type new_types
    | _ :: tail -> find_written_type tail
  in

  match find_written_type all_types with
  | None -> equations
  | Some id ->
      (* generate a fresh type variable and replace the written type var
         everywhere with it *)
      let fresh_type_var : c_type = fresh_type_var () in
      let fresh_type_var_int : int =
        match fresh_type_var with
        | TypeVar n -> n
        | _ -> failwith "type var generated incorrectly"
      in
      let equations_with_subbed_type_var : type_equations =
        substitute_written_vars_in_equations fresh_type_var_int id equations
      in
      replace_written_types equations_with_subbed_type_var

and instantiate (t : c_type) : c_type =
  (* find all universal type variables in the expression *)
  let universal_type_vars : c_type list = get_universal_type_vars t in
  (* generate a fresh type variable for each universal type variable *)
  let fresh_type_vars_assoc : (c_type * c_type) list =
    List.map (fun u -> (u, fresh_type_var ())) universal_type_vars
  in
  (* replace them *)
  replace_types t fresh_type_vars_assoc

and get_universal_type_vars (t : c_type) : c_type list =
  match t with
  | UniversalType _ -> [ t ]
  | FunctionType (i, o) ->
      get_universal_type_vars i @ get_universal_type_vars o
      |> List.sort_uniq compare
  | VectorType types ->
      List.flatten (List.map get_universal_type_vars types)
      |> List.sort_uniq compare
  | CListType et -> get_universal_type_vars et |> List.sort_uniq compare
  | _ -> []

and replace_types t replacements =
  match t with
  | TypeVar _ | UniversalType _ -> (
      (* find the replacement if it exists *)
      try List.assoc t replacements
      with Not_found ->
        t (* if there is no replacement, simply return the variable as it is *))
  | FunctionType (i, o) ->
      FunctionType (replace_types i replacements, replace_types o replacements)
  | VectorType types ->
      VectorType (List.map (fun t -> replace_types t replacements) types)
  | CListType et -> CListType (replace_types et replacements)
  | _ -> t (* otherwise, just return the type *)

and generalize (constraints : type_equations) (env : static_env) (t : c_type) :
    c_type =
  (* remove written type variables from the constraints *)
  let constraints : type_equations = replace_written_types constraints in
  (* fully finish inference of the binding expression *)
  let unified : substitutions = reduce_eq constraints in
  (* apply the resulting subtitutoin to env and t1, yielding env1 and u1 *)
  (* apply the substitution to t as well *)
  let u1 : c_type = get_type t unified in

  (* generalize u1 with respect to env1, yielding a type scheme *)

  (* make replacements in env *)
  let env : static_env =
    List.map (fun (id, t) -> (id, get_type t unified)) env
  in
  (* get the list of type variables in t *)
  let type_vars : c_type list = get_type_vars u1 in
  (* get the list of types in env1 *)
  let env_types : c_type list = env |> List.map snd |> flatten_env_types in
  (* filter out the env_types from_type vars, which yields a list of free
     variables *)
  let free_vars : c_type list =
    List.filter (fun t -> not (List.mem t env_types)) type_vars
    |> List.sort_uniq compare
  in
  (* get rid of duplicates *)
  (* create a universal type variable for each type in free_vars *)
  let variable_replacements : (c_type * c_type) list =
    List.map (fun t -> (t, fresh_universal_type ())) free_vars
  in
  (* replace the free variables in u1 with the universal type variables *)
  replace_types u1 variable_replacements

and get_type_vars (t : c_type) : c_type list =
  match t with
  | TypeVar _ -> [ t ]
  | FunctionType (i, o) -> get_type_vars i @ get_type_vars o
  | VectorType types -> List.flatten (List.map get_type_vars types)
  | CListType et -> get_type_vars et
  | _ -> []

(**
  we have a list of types, this function splits each type into the smallest type possible
  for example, [flatten_env_types [t1, t2, t3 -> t4, (int, t5)]] is [[t1, t2, t3, t4, int, t5]]]
*)
and flatten_env_types (types : c_type list) : c_type list =
  match types with
  | [] -> []
  | t :: tail -> (
      match t with
      | FunctionType (i, o) -> flatten_env_types (i :: o :: tail)
      | VectorType types -> flatten_env_types (types @ tail)
      | CListType et -> flatten_env_types (et :: tail)
      | _ -> t :: flatten_env_types tail)

let rec type_of_value x =
  print_endline "a";
  x |> function
  | IntegerValue _ -> IntType
  | BooleanValue _ -> BoolType
  | StringValue _ -> StringType
  | UnitValue -> UnitType
  | VectorValue values ->
      let types : c_type list = List.map type_of_value values in
      VectorType types
  | ListValue [] ->
      (* the empty list is of type [a] where a is fresh *)
      CListType (fresh_type_var ())
  | ListValue (h :: _) ->
      let h_type = type_of_value h in
      CListType h_type
  | FunctionClosure (env, pat, _, body) ->
      print_endline "function closure";
      let input_type, _, _ = type_of_pat pat in
      print_endline "a";
      let static_env = static_env_from_dynamic_env env in
      print_endline "b";
      let output_type = type_of_c_expr body static_env [] in
      FunctionType (input_type, output_type)
  | _ -> failwith "not a value"

and static_env_from_dynamic_env (env : env) : static_env =
  List.map (fun (id, v) -> (id, type_of_value v)) env
