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
  | EFloat of float
  | EId of string
  | EApp of c_expr * c_expr
  | EBop of c_bop * c_expr * c_expr
  | EVector of c_expr list
  | ENil
  | EListEnumeration of c_expr * c_expr
  | EListComprehension of c_expr * (c_pat * c_expr) list

and c_type =
  | IntType
  | FloatType
  | BoolType
  | StringType
  | UnitType
  | TypeVarWritten of string
  | FunctionType of c_type * c_type
  | VectorType of c_type list
  | TypeVar of int
  | CListType of c_type
  | UniversalType of int

and body_type =
  | IntTypeBody
  | FloatTypeBody
  | BoolTypeBody
  | StringTypeBody
  | UnitTypeBody
  (* Invariant: TypeVarBody conrains a string of a number only if it is
     generated in condense. Otherwise, it will only contain letters. *)
  | TypeVarBody of string (* the string is the name of the type variable*)
  | FunctionTypeBody of body_type * body_type
  | VectorTypeBody of body_type list
  | ListType of body_type

(* We only want foralls to be on the outer layer of the type, which is why we
   have two levels for the type AST

   Additionally, in certain points of the software, we want to be sure that we
   have a non polymophic type. In this case, we encode that in the type
   information by using the body_type type.

   If a type can be polymorphic, we instead use hte polymorphic_type type. *)
and polymorphic_type =
  | PolymorphicType of string * polymorphic_type
  | BodyType of body_type

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

let counter_3 : int ref = ref 0

let fresh_type_var_new : unit -> body_type =
 fun () ->
  counter_3 := !counter_3 + 1;
  TypeVarBody (string_of_int !counter_3)

let rec replace_in_polymorphic_type (t : polymorphic_type) (to_replace : string)
    (replacement : body_type) =
  match t with
  | PolymorphicType (s, t) ->
      PolymorphicType (s, replace_in_polymorphic_type t to_replace replacement)
  | BodyType bt -> BodyType (replace_in_body_type bt to_replace replacement)

and replace_in_body_type (t : body_type) (to_replace : string)
    (replacement : body_type) =
  match t with
  | IntTypeBody -> IntTypeBody
  | FloatTypeBody -> FloatTypeBody
  | BoolTypeBody -> BoolTypeBody
  | StringTypeBody -> StringTypeBody
  | UnitTypeBody -> UnitTypeBody
  | TypeVarBody s -> if s = to_replace then replacement else TypeVarBody s
  | FunctionTypeBody (i, o) ->
      FunctionTypeBody
        ( replace_in_body_type i to_replace replacement,
          replace_in_body_type o to_replace replacement )
  | VectorTypeBody ts ->
      VectorTypeBody
        (List.map (fun t -> replace_in_body_type t to_replace replacement) ts)
  | ListType t -> ListType (replace_in_body_type t to_replace replacement)

(* Given a polymorphic type and a non-polymorphic type, apply the polymorphic
   type to the non-polymorphic type. Return the resulting type, which could
   still have foralls, so it is still a polymorphic type. *)
let apply_type (func : polymorphic_type) (inp : body_type) : polymorphic_type =
  match func with
  | PolymorphicType (s, t) ->
      (* s is the name of the argument t is the body of the type

         We will replace any occurence of TypeVar s with inp *)
      let new_t = replace_in_polymorphic_type t s inp in
      new_t
  | BodyType _ ->
      (* In this case we have a non-polymorphic type, so we shouldn't be
         applying it to anything.

         Throw an exception. *)
      failwith "Cannot apply a non-polymorphic type to anything"

let rec instantiate_type (t : polymorphic_type) : body_type =
  (* Apply t several times until we have a non-polymorphic type. *)
  match t with
  | PolymorphicType (_, t) ->
      let new_type_variable = fresh_type_var_new () in
      let applied = apply_type t new_type_variable in
      instantiate_type
        applied (* continue applying until there are no more type arguments *)
  | BodyType bt -> bt

let rec string_of_body_type : body_type -> string = function
  | IntTypeBody -> "int"
  | FloatTypeBody -> "float"
  | BoolTypeBody -> "bool"
  | StringTypeBody -> "string"
  | UnitTypeBody -> "unit"
  | TypeVarBody s -> s
  | FunctionTypeBody (i, o) ->
      let i_string = string_of_body_type i in
      let o_string = string_of_body_type o in
      i_string ^ " -> " ^ o_string
  | VectorTypeBody ts ->
      let ts_string = List.map string_of_body_type ts in
      "[" ^ String.concat ", " ts_string ^ "]"
  | ListType t -> "[" ^ string_of_body_type t ^ "]"

and string_of_polymorphic_type : polymorphic_type -> string = function
  | PolymorphicType (s, t) ->
      "forall " ^ s ^ ". " ^ string_of_polymorphic_type t
  | BodyType bt -> string_of_body_type bt
