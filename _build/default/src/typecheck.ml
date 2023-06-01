open Cexpr
open Ctostring
open Expr

type static_env = (string * c_type) list

type type_constraint = c_type * c_type

type constraints = type_constraint list

type substitutions = constraints

let rec string_of_constraints (c: constraints): string =
  match c with
  | [] -> ""
  | (t1, t2) :: c' -> Printf.sprintf "%s = %s\n" (string_of_c_type t1) (string_of_c_type t2) ^ string_of_constraints c'

exception TypeFailure

let rec generate (env: static_env) (e: c_expr): c_type * constraints =
  match e with
  | EInt _ -> IntType, []
  | EBool _ -> BoolType, []
  | EId x -> List.assoc x env, []
  | EString _ -> StringType, []
  | ENothing -> NothingType, []
  | EBop (op, e1, e2) ->
    let t1, c1 = generate env e1 in
    let t2, c2 = generate env e2 in
    let type_of_expression: c_type = fresh_type_var () in
    (
    match op with
    | CPlus 
    | CMinus
    | CMul
    | CDiv
    | CMod
    ->
      type_of_expression, (t1, IntType) :: (t2, IntType) :: (type_of_expression, IntType) :: c1 @ c2
    | CGE
    | CGT
    | CLE
    | CLT
    ->
      type_of_expression, (t1, IntType) :: (t2, IntType) :: (type_of_expression, BoolType) :: c1 @ c2

    | CEQ
    | CNE
    ->
      type_of_expression, (type_of_expression, BoolType) :: c1 @ c2
      (* the only constraint here is that the entire expression must be boolean *)
    
    | CAnd
    | COr
    ->
      type_of_expression, (t1, BoolType) :: (t2, BoolType) :: (type_of_expression, BoolType) :: c1 @ c2
    
    )
  | EFunction (pat, cto, body) ->
    ignore cto;

    let input_type, new_env_bindings = type_of_pat pat in
    let output_type, c_output = generate (new_env_bindings @ env) body in
    input_type => output_type,c_output

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




let rec unify (c: constraints): substitutions =
  match c with
  [] -> []
  | (t1, t2) :: c' ->
    if t1 = t2 then unify c'
    else
      failwith "unimplemented"


let initial_env = [("not", BoolType => BoolType)]


