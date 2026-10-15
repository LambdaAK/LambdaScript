(** Shared naming for typeclass dictionaries (must stay in sync with
    [condense]). *)

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
      mono_type_slug (TypeVar w) ^ "__"
      ^ String.concat "__" (List.map mono_type_slug args)
  | CListType e -> "list__" ^ mono_type_slug e
  | VectorType ts -> "vec__" ^ String.concat "__" (List.map mono_type_slug ts)
  | FunctionType (_, _) -> "fn"
  | RecordType _ -> "record"
  | FixedPoint (n, b) -> "mu_" ^ n ^ "__" ^ mono_type_slug b

let dict_for_instance ~(class_name : string) (inst_ty : mono_type) : string =
  "__forge_dict_" ^ class_name ^ "_" ^ mono_type_slug inst_ty

(** {1 Instance registry}

    Every [impl] seen by [Condense.condense_program] is recorded with its head
    type (instance type variables are [$written(a)] type variables), its
    dictionary name and the predicates it requires, so the elaborator can
    resolve a predicate [C τ] to a dictionary expression. *)

type instance = {
  inst_class : string;
  inst_head : mono_type;
  inst_dict : string;
  inst_requires : (string * mono_type) list;
}

let instances : instance list ref = ref []

let register_instance (i : instance) : unit =
  instances :=
    i :: List.filter (fun j -> not (String.equal j.inst_dict i.inst_dict)) !instances

(** Methods dispatched through each class's dictionary (from [trait]
    declarations), in declaration order. *)
let class_methods : (string * string list) list ref = ref []

let register_class_methods (cls : string) (methods : string list) : unit =
  class_methods := (cls, methods) :: List.remove_assoc cls !class_methods

(** Name under which the interpreter also binds a class method, so that
    elaborated code can refer to it even where a local variable shadows the
    method's own name. *)
let method_alias (cls : string) (meth : string) : string =
  "__forge_method_" ^ cls ^ "_" ^ meth
