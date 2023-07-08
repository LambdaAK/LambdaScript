open Expr

type c_bop =
  | CPlus
  | CMinus
  | CMul
  | CDiv
  | CMod
  | CEQ
  | CNE
  | CLT
  | CGT
  | CLE
  | CGE
  | CAnd
  | COr


and c_defn =
  | CDefn of pat * c_type option * c_expr


and c_expr =
  | EFunction of pat * c_type option * c_expr
  | EBindRec of pat * c_type option * c_expr * c_expr
  | ETernary of c_expr * c_expr * c_expr
  | EBool of bool
  | EString of string
  | ENothing
  | EInt of int
  | EId of string
  | EApp of c_expr * c_expr
  | EBop of c_bop * c_expr * c_expr
  | EVector of c_expr list

and c_type =
  | IntType
  | BoolType
  | StringType
  | NothingType
  | FunctionType of c_type * c_type
  | VectorType of c_type list
  | TypeVar of int

let ( => ) (t1: c_type) (t2: c_type): c_type =
  FunctionType (t1, t2)


let counter: int ref = ref 0
let fresh_type_var: unit -> c_type =
fun () ->
  counter := !counter + 1;
  TypeVar !counter
  

(* condense expression *)
