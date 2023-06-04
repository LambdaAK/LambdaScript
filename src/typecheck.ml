open Cexpr
open Expr
open Typefixer

type static_env = (string * c_type) list

type type_equation = c_type * c_type

type type_equations = type_equation list

type substitutions = type_equations



exception TypeFailure

let initial_env = [("not", BoolType => BoolType)]

let rec generate (env: static_env) (e: c_expr): c_type * type_equations =
  match e with
  | EInt _ -> IntType, []
  | EBool _ -> BoolType, []
  | EId x -> List.assoc x env, []
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
    ignore cto;

    let input_type, new_env_bindings = type_of_pat pat in
    let constraints_from_type_annotation: type_equations = 
    (
      match cto with
      | Some t -> [(input_type, t)]
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



and type_of_pat (p: pat): c_type * static_env =
  match p with
  | IdPat id -> 
    let new_var: c_type = fresh_type_var () in
    new_var, [(id, new_var)]
  | NothingPat -> NothingType, []



let rec reduce_eq (c: type_equations): substitutions =

  match c with
  [] -> []
  | (t1, t2) :: c' ->
    if t1 = t2 then reduce_eq c'
    else
    (
      match t1, t2 with
      | TypeVar id, _ when not (inside t1 t2) ->
        (t1, t2) :: reduce_eq (substitute id t2 c')

      | _, TypeVar _ -> reduce_eq ((t2, t1) :: c')

      | FunctionType (i1, o1), FunctionType (i2, o2) ->
        reduce_eq ((i1, i2) :: (o1, o2) :: c')

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
  | _ -> var




and get_type_of_type_var_if_possible (var: c_type) (subs: substitutions): c_type =
  match var with
  | TypeVar _ ->
    (
    try
      List.assoc var subs
    with
    | Not_found -> var
    )
  | _ -> failwith "not a type var"

and type_of_c_expr (e: c_expr): c_type =
  let t, constraints = generate initial_env e in
  let solution = reduce_eq constraints in
  get_type t solution |> fix


and inside (inside_type: c_type) (outside_type: c_type): bool =
  match outside_type with
  | _ when inside_type = outside_type -> true
  | FunctionType (i, o) -> inside inside_type i || inside inside_type o
  | _ -> false 



and is_basic_type (t: c_type): bool =
  match t with
  | IntType
  | BoolType
  | StringType
  | NothingType -> true
  | TypeVar _ -> false
  | FunctionType (i, o) -> is_basic_type i && is_basic_type o



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






