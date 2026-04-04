(** Shared naming for typeclass dictionaries (must stay in sync with [condense]). *)

open Cexpr

let rec mono_type_slug (t : mono_type) : string =
  match t with
  | IntType -> "int"
  | BoolType -> "bool"
  | StringType -> "string"
  | CharType -> "char"
  | UnitType -> "unit"
  | FloatType -> "float"
  | TypeVar v ->
      String.map
        (function
          | '(' | ')' | '$' | ' ' -> '_'
          | c -> c)
        v
  | TypeName n -> n
  | CTypeApp (name, args) ->
      name ^ "__" ^ String.concat "__" (List.map mono_type_slug args)
  | TCtorApp (w, args) ->
      mono_type_slug (TypeVar w)
      ^ "__"
      ^ String.concat "__" (List.map mono_type_slug args)
  | CListType e -> "list__" ^ mono_type_slug e
  | VectorType ts -> "vec__" ^ String.concat "__" (List.map mono_type_slug ts)
  | FunctionType (_, _) -> "fn"
  | RecordType _ -> "record"
  | FixedPoint (n, b) -> "mu_" ^ n ^ "__" ^ mono_type_slug b

let dict_for_instance ~(class_name : string) (inst_ty : mono_type) : string =
  "__forge_dict_" ^ class_name ^ "_" ^ mono_type_slug inst_ty
