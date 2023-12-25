type c_pat =
  | CIntPat of int
  | CBoolPat of bool
  | CNilPat
  | CConsPat of c_pat * c_pat
  | CWildcardPat
  | CVectorPat of c_pat list
  | CStringPat of string
  | CIdPat of string
  | CUnitPat

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
  | CCons

and c_defn =
  | CDefn of c_pat * c_type option * c_expr
  | CDefnRec of c_pat * c_type option * c_expr

and c_switch_branch = c_pat * c_expr

and c_expr =
  | EFunction of c_pat * c_type option * c_expr
  | EBindRec of c_pat * c_type option * c_expr * c_expr
  | ETernary of c_expr * c_expr * c_expr
  | ESwitch of c_expr * c_switch_branch list
  | EBool of bool
  | EString of string
  | EUnit
  | EInt of int
  | EId of string
  | EApp of c_expr * c_expr
  | EBop of c_bop * c_expr * c_expr
  | EVector of c_expr list
  | ENil
  | EListEnumeration of c_expr * c_expr
  | EListComprehension of c_expr * (c_pat * c_expr) list

and c_type =
  | IntType
  | BoolType
  | StringType
  | UnitType
  | TypeVarWritten of string
  | FunctionType of c_type * c_type
  | VectorType of c_type list
  | TypeVar of int
  | CListType of c_type
  | UniversalType of int

let ( => ) (t1 : c_type) (t2 : c_type) : c_type = FunctionType (t1, t2)
let counter : int ref = ref 0

let fresh_type_var : unit -> c_type =
 fun () ->
  counter := !counter + 1;
  TypeVar !counter

let counter_2 : int ref = ref 0

let fresh_universal_type : unit -> c_type =
 fun () ->
  counter_2 := !counter_2 + 1;
  UniversalType !counter_2
