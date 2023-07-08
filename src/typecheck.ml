open Cexpr
open Expr
open Typefixer
open Ctostring

type static_env = (string * c_type) list

type type_equation = c_type * c_type

type type_equations = type_equation list

type substitutions = type_equations

exception TypeFailure

let initial_env = [("not", BoolType => BoolType)]


let string_of_type_equation ((t1, t2): type_equation): string = 
  (string_of_c_type t1) ^ " = " ^ (string_of_c_type t2)


let rec string_of_type_equations (c: type_equations): string =
  match c with
  | [] -> ""
  | (t1, t2) :: c' -> (string_of_type_equation (t1, t2)) ^ "\n" ^ (string_of_type_equations c')


let rec string_of_static_env (env: static_env): string =
  match env with
  | [] -> ""
  | (id, t) :: env' -> id ^ " : " ^ (string_of_c_type t) ^ "\n" ^ (string_of_static_env env')


let rec generate (env: static_env) (e: c_expr): c_type * type_equations =
  match e with
  | EInt _ -> IntType, []
  | EBool _ -> BoolType, []
  | EId x -> 
    List.assoc x env, []
  | EString _ -> StringType, []
  | ENothing -> NothingType, []
  | EBop (op, e1, e2) ->
    let t1, c1 = generate env e1 in
    let t2, c2 = generate env e2 in
    (* let type_of_expression: c_type = fresh_type_var () in *)
    (
    match op with
    | CPlus 
    | CMinus
    | CMul
    | CDiv
    | CMod
    ->
      IntType, (t1, IntType) :: (t2, IntType)  :: c1 @ c2
    | CGE
    | CGT
    | CLE
    | CLT
    ->
      BoolType, (t1, IntType) :: (t2, IntType)  :: c1 @ c2

    | CEQ
    | CNE
    ->
      BoolType, c1 @ c2
      (* the only constraint here is that the entire expression must be boolean *)
    
    | CAnd
    | COr
    ->
      BoolType, (t1, BoolType) :: (t2, BoolType) :: c1 @ c2
    
    )
  | EFunction (pat, cto, body) ->

    let input_type, new_env_bindings = type_of_pat pat in
    let constraints_from_type_annotation: type_equations = 
    (
      match cto with
      | Some t -> [(input_type, t)] (* the input type must equal the annotated type *)
      | None -> []

    ) in
    let output_type, c_output = generate (new_env_bindings @ env) body in
    input_type => output_type, constraints_from_type_annotation @ c_output

  | ETernary (e1, e2, e3) ->
    let t1, c1 = generate env e1 in
    let t2, c2 = generate env e2 in
    let t3, c3 = generate env e3 in
    let type_of_expression: c_type = fresh_type_var () in
    
    type_of_expression, (t1, BoolType) :: (t2, type_of_expression) :: (t3, type_of_expression) :: c1 @ c2 @ c3


  | EApp (e1, e2) ->
    let t1, c1 = generate env e1 in
    let t2, c2 = generate env e2 in
    let type_of_expression: c_type = fresh_type_var () in
    type_of_expression, (t1, t2 => type_of_expression) :: c1 @ c2

  | EBindRec (pattern, cto, e1, e2) ->
    (* perform the type inference as if it is a function application *)
    let function_id: string = (
      match pattern with
      | IdPat id -> id
      | _ -> failwith "not a valid pattern in typecheck.ml"
    ) in

    let a = EApp (EFunction (pattern, cto, e2), e1) in

    let function_type: c_type = fresh_type_var () in

    let new_env: static_env = (function_id, function_type) :: env in

    generate new_env a

  | EVector expressions ->
    (*
        type inference relation for vectors

        env |- <|e1, e2, ..., en|> : <|t1, t2, ..., tn|> -| C1, C2, ..., Cn
        -----------------------------------------------------------------
          env |- e1 : t1 -| C1
          env |- e2 : t2 -| C2
          ...
          env |- en : tn -| Cn
    *)

      let list_of_types, list_of_lists_of_constraints = List.split (List.map (generate env) expressions) in

      VectorType list_of_types, List.flatten list_of_lists_of_constraints

  
and type_of_pat (p: pat): c_type * static_env =
  match p with
  | IdPat id -> 
    let new_var: c_type = fresh_type_var () in
    new_var, [(id, new_var)]
  | NothingPat -> NothingType, []
  | WildcardPat -> fresh_type_var (), []
  | VectorPat patterns ->
    let list_of_types, list_of_lists_of_envs = List.split (List.map type_of_pat patterns) in
    VectorType list_of_types, List.flatten list_of_lists_of_envs

let rec reduce_eq (c: type_equations): substitutions =

  match c with
  | [] -> []
  | (t1, t2) :: c' ->
    if t1 = t2 then reduce_eq c'
    else
    (
      match t1, t2 with
      
      | TypeVar id, _ when not (inside t1 t2) ->
        (t1, t2) :: reduce_eq (substitute id t2 c')

      | _, TypeVar _ -> reduce_eq ((t2, t1) :: c') (* this switches the order of the types, then the branch before will be hit on the next iteration *)

      | FunctionType (i1, o1), FunctionType (i2, o2) ->
        reduce_eq ((i1, i2) :: (o1, o2) :: c')
      
      | VectorType types1, VectorType types2 ->
        (
          match types1, types2 with
          | type1 :: tail1, type2 :: tail2 ->
            reduce_eq ((type1, type2) :: (VectorType tail1, VectorType tail2) :: c')
          | _ -> (* one of them is empty, so the vectors are not the same size *)
            raise TypeFailure
        )

      | _ -> raise TypeFailure
    )

and get_type (var: c_type) (subs: substitutions): c_type =
 
  match var with
  | TypeVar _ ->
    (
    let looked_up_type = get_type_of_type_var_if_possible var subs in
    match looked_up_type with
    | FunctionType (i, o) -> FunctionType (get_type i subs, get_type o subs)
    | _ -> looked_up_type
    )
  | FunctionType (i, o) -> FunctionType (get_type i subs, get_type o subs)
  | VectorType types -> VectorType (List.map (fun t -> get_type t subs) types)
  | IntType -> IntType
  | BoolType -> BoolType
  | StringType -> StringType
  | NothingType -> NothingType
  | _ -> failwith "not a type var2"


and get_type_of_type_var_if_possible (var: c_type) (subs: substitutions): c_type =
  match var with
  | TypeVar _ | TypeVarWritten _ ->
    (
    try
      let looked_up = List.assoc var subs in (
        match looked_up with
        | TypeVar _ -> get_type_of_type_var_if_possible looked_up subs
        | _ -> looked_up
      )
    with
    | Not_found -> var
    )
  | _ -> failwith "not a type var3"

and type_of_c_expr (e: c_expr) (static_env: static_env): c_type =
  let t, constraints = generate static_env e in
 
  let constraints_without_written_type_vars = replace_written_types constraints in
  let solution: substitutions = reduce_eq constraints_without_written_type_vars in
  get_type t solution |> fix

and inside (inside_type: c_type) (outside_type: c_type): bool =
  match outside_type with
  | _ when inside_type = outside_type -> true
  | FunctionType (i, o) -> inside inside_type i || inside inside_type o
  (* there needs to be a case here for vectors *)
  | _ -> false 


and is_basic_type (t: c_type): bool =
  match t with
  | IntType
  | BoolType
  | StringType
  | NothingType -> true
  | TypeVar _ -> false
  | TypeVarWritten _ -> false (* fix this later if necessary *)
  | FunctionType (i, o) -> is_basic_type i && is_basic_type o
  | VectorType types -> List.for_all is_basic_type types


and substitute (var_id: int) (t: c_type) (equations: type_equations): type_equations =
  match equations with
  | [] -> []
  | (t1, t2) :: equations' ->
    (substitute_in_type t1 var_id t, substitute_in_type t2 var_id t) :: substitute var_id t equations'


and substitute_in_type (type_subbing_in: c_type) (type_var_id_subbing_for: int) (substitute_with: c_type): c_type =
  match type_subbing_in with
  | IntType -> IntType
  | BoolType -> BoolType
  | StringType -> StringType
  | NothingType -> NothingType
  | TypeVar id ->
    if id = type_var_id_subbing_for then substitute_with
    else TypeVar id
  | FunctionType (t1, t2) ->
    FunctionType (substitute_in_type t1 type_var_id_subbing_for substitute_with, substitute_in_type t2 type_var_id_subbing_for substitute_with)
  | VectorType types ->
    VectorType (
      List.map (fun t -> substitute_in_type t type_var_id_subbing_for substitute_with) types
    )
  | _ ->
    failwith "not a type var"

and substitute_written_var_with_type_var (var_id: int) (written_var_id: string) (t: c_type): c_type =
  match t with
  | TypeVarWritten id when id = written_var_id -> TypeVar var_id
  | FunctionType (i, o) -> FunctionType (substitute_written_var_with_type_var var_id written_var_id i, substitute_written_var_with_type_var var_id written_var_id o)
  | VectorType types -> VectorType (List.map (fun t -> substitute_written_var_with_type_var var_id written_var_id t) types)
  | _ -> t

and substitute_written_vars_in_equations (var_id: int) (written_var_id: string) (equations: type_equations): type_equations =
  match equations with
  | [] -> []
  | (t1, t2) :: equations' ->
    (substitute_written_var_with_type_var var_id written_var_id t1, substitute_written_var_with_type_var var_id written_var_id t2) :: substitute_written_vars_in_equations var_id written_var_id equations'

and replace_written_types (equations: type_equations): type_equations =
  let first, second = equations |> List.split in
  let all_types: c_type list = first @ second in
 
  
  let rec find_written_type (types: c_type list): string option =
    match types with
    | [] -> None
    | TypeVarWritten id :: _ -> Some id
    (* check for function and vector types *)
    | FunctionType (i, o) :: tail ->
      (* put them in from and call the function again *)
      let new_types: c_type list = i :: o :: tail in
      find_written_type new_types
    | VectorType types :: tail ->
      let new_types: c_type list = types @ tail in
      find_written_type new_types
    | _ :: tail -> find_written_type tail

    in

    match find_written_type all_types with
    | None -> equations
    | Some id ->
      (* generate a fresh type variable and replace the written type var everywhere with it *)
      let fresh_type_var: c_type = fresh_type_var () in
      let fresh_type_var_int: int = (
        match fresh_type_var with
        | TypeVar n -> n
        | _ -> failwith "type var generated incorrectly"
      ) in
      let equations_with_subbed_type_var: type_equations = substitute_written_vars_in_equations fresh_type_var_int id equations in
      replace_written_types equations_with_subbed_type_var
