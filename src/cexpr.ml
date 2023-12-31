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
  | CTypeDefn of string * c_type
  | CUnionDefn of string * c_constructor list

and c_constructor =
  | CNullaryConstructor of string
  | CUnaryConstructor of
      string * c_type (* represents a variant type constructor *)

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
  | EFloat of float
  | EId of string
  | EApp of c_expr * c_expr
  | EBop of c_bop * c_expr * c_expr
  | EVector of c_expr list
  | ENil
  | EListEnumeration of c_expr * c_expr
  | EListComprehension of c_expr * (c_pat * c_expr) list
  | EConstructor of string

and c_type =
  | IntType
  | FloatType
  | BoolType
  | StringType
  | UnitType
  | TypeName of string
  | TypeVarWritten of string
  | FunctionType of c_type * c_type
  | VectorType of c_type list
  | TypeVar of int
  | CListType of c_type
  | UniversalType of int
  | UnionType of c_constructor list

and value =
  | IntegerValue of int
  | FloatValue of float
  | StringValue of string
  | BooleanValue of bool
  | UnitValue
  | FunctionClosure of env * c_pat * c_type option * c_expr
  | RecursiveFunctionClosure of env ref * c_pat * c_type option * c_expr
  | VectorValue of value list
  | ListValue of value list
  | BuiltInFunction of builtin_function
  | ConstructorValue of string * value option
(* string is the name of the constructor if it is applied, the value is Some
   value, where value is the value contained in the constructor *)

and builtin_function =
  | Println
  | Print
  | IntToString
  | StringToInt
  | IntToFloat
  | FloatToInt
  | Map
  | Filter
  | ReduceLeft
  | ReduceRight

and env = (string * value) list

type static_env = (string * c_type) list
type c_program = c_defn list

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

let reset_type_counters : unit -> unit =
 fun () ->
  counter := 0;
  counter_2 := 0
