open Cexpr
open Typefixer
open Ctostringcode.CToStringCode

type type_equation = c_type * c_type
type type_equations = type_equation list
type substitutions = type_equations

type static_type_env =
  (int * c_kind) list (* stores the kind of a type variable *)

type kind_equations = (c_kind * c_kind) list

exception TypeFailure

let rec split3 (lst : ('a * 'b * 'c) list) : 'a list * 'b list * 'c list =
  let rec split3_helper (lst : ('a * 'b * 'c) list) (a : 'a list) (b : 'b list)
      (c : 'c list) : 'a list * 'b list * 'c list =
    match lst with
    | [] -> (a, b, c)
    | (a', b', c') :: tail -> split3_helper tail (a' :: a) (b' :: b) (c' :: c)
  in
  let a, b, c = split3_helper lst [] [] [] in
  (List.rev a, List.rev b, List.rev c)

and string_of_type_equation ((t1, t2) : type_equation) : string =
  string_of_c_type t1 ^ " = " ^ string_of_c_type t2

and string_of_type_equations (c : type_equations) : string =
  match c with
  | [] -> ""
  | (t1, t2) :: c' ->
      string_of_type_equation (t1, t2) ^ "\n" ^ string_of_type_equations c'

and string_of_static_env (env : static_env) : string =
  match env with
  | [] -> ""
  | (id, t) :: env' ->
      id ^ " : " ^ string_of_c_type t ^ "\n" ^ string_of_static_env env'

and generate (env : static_env) (type_env : (string * c_type) list) (e : c_expr)
    : c_type * type_equations =
  match e with
  | EConstructor n ->
      (* To get the type of the constructor, look it up in the static env No
         constraints are generated *)
      let type_of_constructor : c_type = List.assoc n env |> instantiate in

      (type_of_constructor, [])
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
      let t1, c1 = generate env type_env e1 in
      let t2, c2 = generate env type_env e2 in
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
        type_of_pat pat env
      in
      let constraints_from_type_annotation : type_equations =
        match cto with
        | Some t ->
            [ (input_type, t) ]
            (* the input type must equal the annotated type *)
        | None -> []
      in
      let output_type, c_output =
        generate (new_env_bindings @ env) type_env body
      in
      ( input_type => output_type,
        constraints_from_pattern @ constraints_from_type_annotation @ c_output
      )
  | ETernary (e1, e2, e3) ->
      let t1, c1 = generate env type_env e1 in
      let t2, c2 = generate env type_env e2 in
      let t3, c3 = generate env type_env e3 in
      let type_of_expression : c_type = fresh_type_var () in

      ( type_of_expression,
        (t1, BoolType) :: (t2, type_of_expression) :: (t3, type_of_expression)
        :: c1
        @ c2 @ c3 )
  | EApp (first, second) -> (
      match first with
      | EFunction (CIdPat _, _, _) ->
          generate_e_app_function_pat_is_id env type_env first second
      | _ ->
          let t1, c1 = generate env type_env first in
          let t2, c2 = generate env type_env second in

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
      let t1, c1 = generate new_env type_env e1 in
      let t2, c2 = generate new_env type_env e2 in
      let new_constraint = (function_type, t1) in

      (t2, (new_constraint :: c1) @ c2)
  | EVector expressions ->
      (* type inference relation for vectors

         env |- (e1, e2, ..., en) : (t1, t2, ..., tn) -| C1, C2, ..., Cn
         ----------------------------------------------------------------- env
         |- e1 : t1 -| C1 env |- e2 : t2 -| C2 ... env |- en : tn -| Cn *)
      let list_of_types, list_of_lists_of_constraints =
        List.split (List.map (generate env type_env) expressions)
      in
      let list_of_types : c_type list = List.map instantiate list_of_types in

      (VectorType list_of_types, List.flatten list_of_lists_of_constraints)
  | ESwitch (e1, branches) ->
      (* get the type of the switching expression *)
      let t1, c1 = generate env type_env e1 in
      let type_that_all_branch_expressions_must_be : c_type =
        fresh_type_var ()
      in

      (* use this in the constraints *)

      (* get the type of each branch *)
      let branch_constraints : type_equation list =
        List.map
          (fun (bp, be) ->
            (* the pattern has to be of the same type as t1 *)
            let type_of_pattern, pattern_env, const = type_of_pat bp env in

            let type_of_branch_expression, branch_expression_constraints =
              generate (pattern_env @ env) type_env be
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
      let t1, c1 = generate env type_env e1 in
      let t2, c2 = generate env type_env e2 in
      (CListType IntType, ((t1, IntType) :: (t2, IntType) :: c1) @ c2)
  | EListComprehension (e, generators) ->
      let env, generator_constraints =
        List.fold_left
          (fun (env, constraints) (p, e) ->
            let type_of_pattern, pattern_env, const = type_of_pat p env in
            let type_of_expression, expression_constraints =
              generate (pattern_env @ env) type_env e
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
      let type_of_expression, expression_constraints =
        generate env type_env e
      in

      ( CListType type_of_expression,
        expression_constraints @ generator_constraints )

and generate_type_env_constraints type_env =
  List.map (fun (id, t) -> (TypeName id, t)) type_env

and generate_e_app_function_pat_is_id (env : static_env) type_env
    (first : c_expr) (second : c_expr) : c_type * type_equations =
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
        type_of_pat pattern env
      in
      let constraints_from_type_annotation : type_equations =
        match cto with
        | Some t ->
            [ (input_type, t) ]
            (* the input type must equal the annotated type *)
        | None -> []
      in

      let t1, c1 = generate (new_env_bindings @ env) type_env second in
      let generalized_type : c_type =
        generalize c1 (new_env_bindings @ env) type_env t1
      in

      let t2, c2 =
        generate ((function_id, generalized_type) :: env) type_env body
      in
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
      let t1, c1 = generate env type_env e1 in
      let t2, c2 = generate env type_env e2 in
      let type_of_expression : c_type = fresh_type_var () in
      (type_of_expression, ((t1, t2 => type_of_expression) :: c1) @ c2)

and type_of_pat (p : c_pat) (static_env : static_env) :
    c_type * static_env * type_equations =
  match p with
  | CIdPat id ->
      let new_var : c_type = fresh_type_var () in
      (new_var, [ (id, new_var) ], [])
  | CConstructorPat n ->
      (* this is a pattern that represents a constructor *)
      (List.assoc n static_env |> instantiate, [], [])
  | CUnitPat -> (UnitType, [], [])
  | CWildcardPat -> (fresh_type_var (), [], [])
  | CVectorPat patterns ->
      let list_of_types, list_of_lists_of_envs, lists_of_equations =
        split3 (List.map (fun p -> type_of_pat p static_env) patterns)
      in
      ( VectorType list_of_types,
        List.flatten list_of_lists_of_envs,
        List.flatten lists_of_equations )
  | CIntPat _ -> (IntType, [], [])
  | CBoolPat _ -> (BoolType, [], [])
  | CStringPat _ -> (StringType, [], [])
  | CNilPat -> (CListType (fresh_type_var ()), [], [])
  | CConsPat (p1, p2) ->
      let t1, env1, c1 = type_of_pat p1 static_env in
      let t2, env2, c2 = type_of_pat p2 static_env in

      (* since h :: t is a list, and h : t1 and t : t2, it must hold that [t1] =
         t2 return this constraint as well *)
      (CListType t1, env1 @ env2, (CListType t1, t2) :: (c1 @ c2))
  | CAppPat (p1, p2) ->
      let t1, env1, c1 = type_of_pat p1 static_env in
      let t2, env2, c2 = type_of_pat p2 static_env in
      let type_of_expression : c_type = fresh_type_var () in
      ( type_of_expression,
        env1 @ env2,
        (t1, t2 => type_of_expression) :: (c1 @ c2) )
(* this might be wrong. check it later *)

and reduce_eq (c : type_equations) (type_env : (string * c_type) list) :
    substitutions =
  match c with
  | [] -> []
  | (t1, t2) :: c' -> (
      if t1 = t2 then reduce_eq c' type_env
      else if inside t1 t2 || inside t2 t1 then (
        print_endline "TYPE FAILURE";
        raise TypeFailure)
      else
        match (t1, t2) with
        (* if one of the types is a union type, just ignore the equation for
           now *)
        | AppType (t1, t2), AppType (t3, t4) ->
            (* t1 t2 = t3 t4 ==> t1 = t3 and t2 = t4 *)
            reduce_eq ((t1, t3) :: (t2, t4) :: c') type_env
        | UnionType _, _ | _, UnionType _ -> reduce_eq c' type_env
        | TypeVar id, _ when not (inside t1 t2) ->
            (t1, t2) :: reduce_eq (substitute id t2 c') type_env
        | _, TypeVar _ ->
            reduce_eq ((t2, t1) :: c') type_env
            (* this switches the order of the types, then the branch before will
               be hit on the next iteration *)
        | TypeName name1, TypeName name2 -> (
            (* check if they are equal

               if one of them is a variant type, then they are not the same *)
            let type1_impl = List.assoc name1 type_env in
            let type2_impl = List.assoc name2 type_env in
            match (type1_impl, type2_impl) with
            | UnionType _, _ | _, UnionType _ ->
                (* since one of them is a union type, they are not equal*)
                print_endline "type failure from union types";
                raise TypeFailure
            | _ ->
                (* Replace the lexographically larger one with the smaller
                   one *)
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
                (TypeName name1, TypeName name2)
                :: reduce_eq new_equations type_env)
        | TypeName name, t -> (
            (* 

               On the left is a type name, and on the right is a type

               We want to replace the type everywhere in the equations with the
               type name *)
            let type_impl = List.assoc name type_env |> instantiate in

            match type_impl with
            | UnionType _ ->
                print_endline "type failure from union types";
                raise TypeFailure
            | _ ->
                let new_equations =
                  List.map
                    (fun (t1, t2) ->
                      ( replace_type t (TypeName name) t1,
                        replace_type t (TypeName name) t2 ))
                    c'
                in

                reduce_eq ((type_impl, t) :: new_equations) type_env)
        | _, TypeName _ ->
            (* use the previous branch *) reduce_eq ((t2, t1) :: c') type_env
        | FunctionType (i1, o1), FunctionType (i2, o2) ->
            reduce_eq ((i1, i2) :: (o1, o2) :: c') type_env
        | CListType et1, CListType et2 -> reduce_eq ((et1, et2) :: c') type_env
        | VectorType types1, VectorType types2 -> (
            match (types1, types2) with
            | type1 :: tail1, type2 :: tail2 ->
                reduce_eq
                  ((type1, type2) :: (VectorType tail1, VectorType tail2) :: c')
                  type_env
            | _ ->
                (* one of them is empty, so the vectors are not the same size *)
                print_endline "type failure from differently-sized vectors";
                raise TypeFailure)
        | _ ->
            (* print t1 and t2 *)
            print_endline "type error 1";
            print_endline (string_of_c_type t1);
            print_endline (string_of_c_type t2);
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
      | AppType (t1, t2) -> AppType (get_type t1 subs, get_type t2 subs)
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
  | TypeVarWritten _ -> var
  | UnionType _ -> failwith "union type found in get_type"
  | PolymorphicType (arg, body) ->
      PolymorphicType (get_type arg subs, get_type body subs)
  | AppType (t1, t2) -> AppType (get_type t1 subs, get_type t2 subs)

and kind_of_type t type_env =
  let k, eq = generate_kind_equations t type_env [] in

  let solution = reduce_kind_equations eq in

  (* now, get the kind *)
  let kind = get_kind k solution in
  replace_kind_vars_with_stars kind

and generate_kind_equations t type_env (static_type_env : static_type_env) :
    c_kind * kind_equations =
  match t with
  | IntType | FloatType | StringType | UnitType | BoolType -> (Star, [])
  | CListType elm_type ->
      let kind_of_elm_type, equations =
        generate_kind_equations elm_type type_env static_type_env
      in
      (kind_of_elm_type, equations)
  | FunctionType _ -> (Star, [])
  | VectorType _ -> (Star, [])
  | PolymorphicType (i, o) ->
      (* get the "name" of the argument type *)
      let i_type_id =
        match i with
        | UniversalType id -> id
        | x ->
            x |> string_of_c_type |> print_endline;
            failwith ""
      in

      (* since i is an argument, give it a fresh kind var *)
      let kind_of_i = fresh_kind_var () in

      (* add it to the static type env *)
      let new_static_type_env =
        (i_type_id, kind_of_i) :: static_type_env
        (* generate the kind and kind equations for the body *)
      in

      let kind_of_o, equations =
        generate_kind_equations o type_env new_static_type_env
      in

      (Arrow (kind_of_i, kind_of_o), equations)
  | AppType (t1, t2) ->
      let kind_of_t1, equations_t1 =
        generate_kind_equations t1 type_env static_type_env
      in
      let kind_of_t2, equations_t2 =
        generate_kind_equations t2 type_env static_type_env
      in
      let k = fresh_kind_var () in
      let new_eq = (kind_of_t1, Arrow (kind_of_t2, k)) in
      (k, (new_eq :: equations_t1) @ equations_t2)
  | TypeVar type_var_id ->
      (* I'm pretty sure this branch will never get used, since all type
         variables are universal type variables *)
      print_endline "generating kind var I DON'T THINK THIS SHOULD BE USED!";

      let kind_of_type_var = List.assoc type_var_id static_type_env in

      (kind_of_type_var, [])
  | UniversalType type_var_id ->
      print_endline "generating universal kind var";

      let kind_of_type_var =
        try List.assoc type_var_id static_type_env
        with Not_found -> failwith "not found in generate_kind_equations"
      in
      (kind_of_type_var, [])
  | TypeName n ->
      let type_impl = List.assoc n type_env in

      generate_kind_equations type_impl type_env static_type_env
  | TypeVarWritten _ ->
      failwith "type var written found in generate_kind_equations"
  | UnionType _ ->
      print_endline "union type found in generate_kind_equations";
      (Star, [])

and reduce_kind_equations equations =
  match equations with
  | [] -> []
  | (k1, k2) :: c' -> (
      if k1 = k2 then reduce_kind_equations c'
      else
        match (k1, k2) with
        | Arrow (k1, k2), Arrow (k3, k4) ->
            reduce_kind_equations ((k1, k3) :: (k2, k4) :: c')
        | KindVar kv1, KindVar kv2 ->
            (* replace kv2 with kv1 everywhere in the equations *)
            ignore (kv1, kv2);
            []
        | KindVar kv, k ->
            (* replace kv with k everywhere in the equations *)
            let new_equations = replace_kind_var_in_kind_equations c' kv k in
            (KindVar kv, k) :: reduce_kind_equations new_equations
        | k, KindVar kv ->
            (* swap the order and use the previous branch *)
            reduce_kind_equations ((KindVar kv, k) :: c')
        | _ ->
            print_endline "type error 2";
            raise TypeFailure)

and replace_kind k to_replace replace_with =
  match k with
  | Arrow (k1, k2) ->
      let new_k1 = replace_kind k1 to_replace replace_with in
      let new_k2 = replace_kind k2 to_replace replace_with in
      Arrow (new_k1, new_k2)
  | Star -> Star
  | KindVar kv when kv = to_replace -> replace_with
  | KindVar kv -> KindVar kv

and replace_kind_var_in_kind_equations eq to_replace replace_with =
  List.map
    (fun (k1, k2) ->
      let new_k1 = replace_kind k1 to_replace replace_with in
      let new_k2 = replace_kind k2 to_replace replace_with in
      (new_k1, new_k2))
    eq

and get_kind k subs =
  match k with
  | Star -> Star
  | Arrow (k1, k2) ->
      let new_k1 = get_kind k1 subs in
      let new_k2 = get_kind k2 subs in
      Arrow (new_k1, new_k2)
  | KindVar kv -> (
      let looked_up = get_kind_of_kind_var_if_possible (KindVar kv) subs in
      match looked_up with
      | Star -> Star
      | Arrow (kk1, kk2) ->
          let new_kk1 = get_kind kk1 subs in
          let new_kk2 = get_kind kk2 subs in
          Arrow (new_kk1, new_kk2)
      | _ -> looked_up)

and get_kind_of_kind_var_if_possible var subs =
  match var with
  | KindVar _ -> (
      try
        let looked_up = List.assoc var subs in
        match looked_up with
        | KindVar _ -> get_kind_of_kind_var_if_possible looked_up subs
        | _ -> looked_up
      with Not_found -> var)
  | _ -> failwith "not a kind var in get_kind_of_kind_var_if_possible"

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

and replace_kind_vars_with_stars : c_kind -> c_kind = function
  | Star -> Star
  | Arrow (k1, k2) ->
      let new_k1 = replace_kind_vars_with_stars k1 in
      let new_k2 = replace_kind_vars_with_stars k2 in
      Arrow (new_k1, new_k2)
  | KindVar _ -> Star

and type_of_c_expr (e : c_expr) (static_env : static_env)
    (type_env : (string * c_type) list) : c_type =
  let t, generated_constraints = generate static_env type_env e in

  (* print the constraints *)
  let constraints = generated_constraints in
  print_endline "expression";
  print_endline (string_of_c_expr e);
  print_endline "TYPE: ";
  print_endline (string_of_c_type t);
  print_endline "CONSTRAINTS ";
  print_endline (string_of_type_equations constraints);
  print_endline "end constraints";

  (* solve the constraints *)

  (* solve the constraints *)
  let constraints_without_written_type_vars =
    replace_written_types constraints
  in

  (* evaluate each type *)
  let constraints_evaluated =
    List.map
      (fun (t1, t2) ->
        let t1_eval = eval_type type_env t1 in
        let t2_eval = eval_type type_env t2 in
        (t1_eval, t2_eval))
      constraints_without_written_type_vars
  in

  (* print the constraints *)

  (* solve the constraints *)
  let solution : substitutions = reduce_eq constraints_evaluated type_env in

  (* print the solution *)
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
  | AppType (t1, t2) -> is_basic_type t1 && is_basic_type t2
  | PolymorphicType _ -> false

and substitute (var_id : int) (t : c_type) (equations : type_equations) :
    type_equations =
  match equations with
  | [] -> []
  | (t1, t2) :: equations' ->
      (substitute_in_type t1 var_id t, substitute_in_type t2 var_id t)
      :: substitute var_id t equations'

(* This substitutes for a TypeVar, not a UniversalTypeVar *)
and substitute_in_type (type_subbing_in : c_type)
    (type_var_id_subbing_for : int) (substitute_with : c_type) : c_type =
  match type_subbing_in with
  | IntType -> IntType
  | BoolType -> BoolType
  | StringType -> StringType
  | UnitType -> UnitType
  | FloatType -> FloatType
  | UniversalType id -> UniversalType id
  | TypeVar id ->
      (*TODO: fix this*)
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
  | UnionType a -> UnionType a
  | TypeVarWritten n -> TypeVarWritten n
  | AppType (t1, t2) ->
      AppType
        ( substitute_in_type t1 type_var_id_subbing_for substitute_with,
          substitute_in_type t2 type_var_id_subbing_for substitute_with )
  | PolymorphicType (arg, body) ->
      PolymorphicType
        (arg, substitute_in_type body type_var_id_subbing_for substitute_with)

and eval_type type_env = function
  | TypeName name ->
      (* to evaluate a name, look it up in the type env *)
      let evaled_type =
        eval_type type_env (List.assoc name type_env |> instantiate)
      in

      (* we only actually want to replace it if it is a not a union type *)
      if is_variant_type evaled_type then TypeName name else evaled_type
  | AppType (t1, t2) -> (
      (* apply t1 to t2 *)
      let t1_eval = eval_type type_env t1 in
      let t2_eval = eval_type type_env t2 in

      match t1_eval with
      | PolymorphicType (i, o) ->
          (* replace i with t2 in o *)
          let o = replace_type t2_eval i o in
          eval_type type_env o
      | _ ->
          (* just return the application *)
          AppType (t1_eval, t2_eval))
  | t -> t

and is_variant_type = function
  | UnionType _ -> true
  | PolymorphicType (_, o) -> is_variant_type o
  | _ -> false

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
    | AppType (t1, t2) ->
        AppType
          ( replace_type replace_with get_rid_of t1,
            replace_type replace_with get_rid_of t2 )
    | PolymorphicType (arg, body) ->
        (* need to replace get_rid_of with replace_with in body *)
        let new_body = replace_type replace_with get_rid_of body in
        PolymorphicType (arg, new_body)
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
      let new_i =
        substitute_written_var_with_type_var var_id written_var_id i
      in
      let new_o =
        substitute_written_var_with_type_var var_id written_var_id o
      in
      FunctionType (new_i, new_o)
  | VectorType types ->
      VectorType
        (List.map
           (fun t ->
             substitute_written_var_with_type_var var_id written_var_id t)
           types)
  | CListType et ->
      CListType (substitute_written_var_with_type_var var_id written_var_id et)
  | AppType (t1, t2) ->
      let new_t1 =
        substitute_written_var_with_type_var var_id written_var_id t1
      in
      let new_t2 =
        substitute_written_var_with_type_var var_id written_var_id t2
      in
      AppType (new_t1, new_t2)
  | PolymorphicType (arg, body) ->
      let new_arg =
        substitute_written_var_with_type_var var_id written_var_id arg
      in
      let new_body =
        substitute_written_var_with_type_var var_id written_var_id body
      in
      PolymorphicType (new_arg, new_body)
  | _ -> t

and substitute_written_vars_in_equations (var_id : int)
    (written_var_id : string) (equations : type_equations) : type_equations =
  match equations with
  | [] -> []
  | (t1, t2) :: equations' ->
      ( substitute_written_var_with_type_var var_id written_var_id t1,
        substitute_written_var_with_type_var var_id written_var_id t2 )
      :: substitute_written_vars_in_equations var_id written_var_id equations'

and instantiate (t : c_type) : c_type =
  (* find all universal type variables in the expression *)
  let universal_type_vars : c_type list = get_universal_type_vars t in

  (* generate a fresh type variable for each universal type variable *)
  let fresh_type_vars_assoc : (c_type * c_type) list =
    List.map (fun u -> (u, fresh_type_var ())) universal_type_vars
  in
  (* replace them *)
  replace_types t fresh_type_vars_assoc

and replace_written_types (equations : type_equations) : type_equations =
  let first, second = equations |> List.split in
  let all_types : c_type list = first @ second in

  (* find some written type in the equations, it doesn't matter which one it
     is *)
  let rec find_written_type (types : c_type list) : string option =
    match types with
    | [] -> None
    | TypeVarWritten id :: _ -> Some id
    (* check for function and vector types *)
    | FunctionType (t1, t2) :: tail
    | AppType (t1, t2) :: tail
    | PolymorphicType (t1, t2) :: tail ->
        (* put them in from and call the function again *)
        let new_types : c_type list = t1 :: t2 :: tail in
        find_written_type new_types
    | VectorType types :: tail ->
        let new_types : c_type list = types @ tail in
        find_written_type new_types
    | CListType t :: tail -> find_written_type (t :: tail)
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
  | PolymorphicType (i, o) ->
      get_universal_type_vars i @ get_universal_type_vars o
      |> List.sort_uniq compare
  | AppType (t1, t2) ->
      get_universal_type_vars t1 @ get_universal_type_vars t2
      |> List.sort_uniq compare
  | _ -> []

and replace_types t replacements =
  match t with
  | TypeVar _ | UniversalType _ | TypeVarWritten _ -> (
      (* find the replacement if it exists *)
      try List.assoc t replacements
      with Not_found ->
        t (* if there is no replacement, simply return the variable as it is *))
  | FunctionType (i, o) ->
      FunctionType (replace_types i replacements, replace_types o replacements)
  | VectorType types ->
      VectorType (List.map (fun t -> replace_types t replacements) types)
  | CListType et -> CListType (replace_types et replacements)
  | PolymorphicType (n, body) ->
      PolymorphicType
        (replace_types n replacements, replace_types body replacements)
  | AppType (t1, t2) ->
      AppType (replace_types t1 replacements, replace_types t2 replacements)
  | _ -> t

and generalize (constraints : type_equations) (env : static_env)
    (type_env : (string * c_type) list) (t : c_type) : c_type =
  (* remove written type variables from the constraints *)
  let constraints : type_equations = replace_written_types constraints in

  (* fully finish inference of the binding expression *)
  let unified : substitutions = reduce_eq constraints type_env in

  (* apply the resulting subtitutoin to env and t1, yielding env1 and u1 *)
  (* apply the substitution to t as well *)

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
  print_endline "using type_of_value. this function is deprecated.";
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
      let input_type, _, _ = type_of_pat pat [] in
      let static_env = static_env_from_dynamic_env env in
      let output_type = type_of_c_expr body static_env [] in
      FunctionType (input_type, output_type)
  | _ -> failwith "not a value"

and static_env_from_dynamic_env (env : env) : static_env =
  List.map (fun (id, v) -> (id, type_of_value v)) env

(*let t1 = TypeVarWritten "a" let t2 = TypeVarWritten "b" let t =
  PolymorphicType ("a", PolymorphicType ("b", VectorType [ t1; t2 ])) let a =
  AppType (AppType (t, IntType), BoolType) let () = a |> eval_type [] |>
  string_of_c_type |> print_endline *)
