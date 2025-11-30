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
  | CVariantPat of string * c_pat option
(* Constructor name and optional payload pattern *)

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

(* Type variables are represented as strings *)
type type_var = string

(* Monomorphic types - no universal quantifiers *)
type mono_type =
  | IntType
  | FloatType
  | BoolType
  | StringType
  | UnitType
  | TypeVar of type_var
  | TypeName of string
  | FunctionType of mono_type * mono_type
  | VectorType of mono_type list
  | CListType of mono_type
  | CTypeApp of string * mono_type list
  | FixedPoint of string * mono_type
(* the string is the name of the type constructor, and mono_type list is the
   list of arguments*)
(* FixedPoint(type_name, body) represents μtype_name.body, where body contains
   recursive references to type_name via TypeName or CTypeApp *)

(* Polymorphic types - universal quantifiers only at the top level *)
type c_type =
  | Mono of mono_type
  | PolyType of type_var * c_type (* Represents ∀x.τ *)

and c_defn =
  | CDefn of c_pat * c_type option * c_expr
  | CDefnRec of c_pat * c_type option * c_expr
  | CTypeAlias of string * string list * mono_type
  | CSumType of string * string list * (string * c_type option) list
  | CSumTypeRec of string * string list * (string * c_type option) list

and c_switch_branch = c_pat * c_expr

and c_expr_or_c_defn =
  | Expr of c_expr
  | Defn of c_defn

and c_expr =
  | EFunction of c_pat * c_type option * c_expr
  | EBind of c_pat * c_type option * c_expr * c_expr
  | EBindRec of c_pat * c_type option * c_expr * c_expr
  | EBlock of c_expr_or_c_defn list
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
  | VariantValue of string * value option
(* Constructor name and optional payload value *)

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

let ( => ) (t1 : mono_type) (t2 : mono_type) : mono_type = FunctionType (t1, t2)
let counter : int ref = ref 0

let fresh_type_var : unit -> mono_type =
 fun () ->
  counter := !counter + 1;
  TypeVar ("t" ^ string_of_int !counter)

(* Type application: applies a polymorphic type to a monomorphic type *)
let rec apply_type (func : c_type) (arg : mono_type) : c_type =
  match func with
  | PolyType (var, body) -> substitute_type body var arg
  | Mono _ -> failwith "Cannot apply monomorphic type"

(* Type substitution: replaces type variables with types *)
and substitute_type (t : c_type) (var : type_var) (replacement : mono_type) :
    c_type =
  match t with
  | Mono (TypeVar v) -> if v = var then Mono replacement else t
  | Mono (FunctionType (t1, t2)) ->
      Mono
        (FunctionType
           ( substitute_mono t1 var replacement,
             substitute_mono t2 var replacement ))
  | Mono (VectorType ts) ->
      Mono
        (VectorType (List.map (fun t -> substitute_mono t var replacement) ts))
  | Mono (CListType t) -> Mono (CListType (substitute_mono t var replacement))
  | Mono (CTypeApp (name, args)) ->
      Mono (CTypeApp (name, List.map (fun arg -> substitute_mono arg var replacement) args))
  | Mono (FixedPoint (name, body)) ->
      Mono (FixedPoint (name, substitute_mono body var replacement))
  | Mono t -> Mono t
  | PolyType (v, body) ->
      if v = var then t else PolyType (v, substitute_type body var replacement)

and substitute_mono (t : mono_type) (var : type_var) (replacement : mono_type) :
    mono_type =
  match t with
  | TypeVar v -> if v = var then replacement else t
  | FunctionType (t1, t2) ->
      FunctionType
        (substitute_mono t1 var replacement, substitute_mono t2 var replacement)
  | VectorType ts ->
      VectorType (List.map (fun t -> substitute_mono t var replacement) ts)
  | CListType t -> CListType (substitute_mono t var replacement)
  | CTypeApp (name, args) ->
      CTypeApp (name, List.map (fun arg -> substitute_mono arg var replacement) args)
  | FixedPoint (name, body) ->
      FixedPoint (name, substitute_mono body var replacement)
  | _ -> t

let rec string_of_mono_type : mono_type -> string = function
  | IntType -> "int"
  | FloatType -> "float"
  | BoolType -> "bool"
  | StringType -> "string"
  | UnitType -> "unit"
  | TypeVar v -> v
  | FunctionType (t1, t2) ->
      let t1_str = string_of_mono_type t1 in
      let t2_str = string_of_mono_type t2 in
      t1_str ^ " -> " ^ t2_str
  | VectorType ts ->
      let ts_str = List.map string_of_mono_type ts in
      "[" ^ String.concat ", " ts_str ^ "]"
  | CListType t -> "[" ^ string_of_mono_type t ^ "]"
  | TypeName v -> v
  | CTypeApp (name, args) ->
      let args_str = List.map string_of_mono_type args in
      name ^ "<" ^ String.concat ", " args_str ^ ">"
  | FixedPoint (name, body) ->
      "μ" ^ name ^ ". " ^ string_of_mono_type body

let rec string_of_type : c_type -> string = function
  | Mono t -> string_of_mono_type t
  | PolyType (var, body) -> "∀" ^ var ^ ". " ^ string_of_type body

(* Types form a lambda calculus. Here are functions that help us manipulate
   types in this lambda calculus. *)

(** [get_mono_type_vars t] returns a list of all unique type variable names
    appearing in the monomorphic type [t].

    This function recursively traverses the type structure and collects all
    [TypeVar v] occurrences, removing duplicates.

    @param t The monomorphic type to extract variables from
    @return A list of unique type variable names (strings) appearing in [t] *)
let get_mono_type_vars (t : mono_type) : string list =
  let rec aux t acc =
    match t with
    | IntType | FloatType | BoolType | StringType | UnitType -> acc
    | TypeVar v -> if List.mem v acc then acc else v :: acc
    | FunctionType (t1, t2) -> aux t1 (aux t2 acc)
    | VectorType ts -> List.fold_left (fun a t -> aux t a) acc ts
    | CListType t' -> aux t' acc
    | TypeName _ -> acc
    | CTypeApp (_, args) -> List.fold_left (fun a t -> aux t a) acc args
    | FixedPoint (_, body) -> aux body acc
  in
  aux t [] |> List.rev
