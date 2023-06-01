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

and c_expr =
  | EFunction of pat * c_type option * c_expr
  | ETernary of c_expr * c_expr * c_expr
  | EBool of bool
  | EString of string
  | ENothing
  | EInt of int
  | EId of string
  | EApp of c_expr * c_expr
  | EBop of c_bop * c_expr * c_expr

and c_type =
  | IntType
  | BoolType
  | StringType
  | NothingType
  | FunctionType of c_type * c_type

(* condense expression *)
