(** Arity of type constructors for validation (HKT / implicit kinds). *)

open Cexpr

let written_param_var (p : string) : string = "$written(" ^ p ^ ")"

let merge_param_arity (acc : (string * int) list) (param : string) (n : int) :
    (string * int) list =
  match List.assoc_opt param acc with
  | Some k when k <> n ->
      failwith
        ("Inconsistent arity for type constructor " ^ param ^ " (expected "
       ^ string_of_int k ^ ", got " ^ string_of_int n ^ ")")
  | Some _ -> acc
  | None -> (param, n) :: acc

let rec collect_ctypeapp_heads_for_params (params : string list)
    (acc : (string * int) list) (t : mono_type) : (string * int) list =
  match t with
  | FunctionType (a, b) ->
      collect_ctypeapp_heads_for_params params
        (collect_ctypeapp_heads_for_params params acc a)
        b
  | VectorType ts ->
      List.fold_left (collect_ctypeapp_heads_for_params params) acc ts
  | CListType e -> collect_ctypeapp_heads_for_params params acc e
  | CTypeApp (name, args) ->
      let acc' =
        if List.mem name params then
          merge_param_arity acc name (List.length args)
        else acc
      in
      List.fold_left (collect_ctypeapp_heads_for_params params) acc' args
  | FixedPoint (_, body) -> collect_ctypeapp_heads_for_params params acc body
  | RecordType fields ->
      List.fold_left
        (fun a (_, t) -> collect_ctypeapp_heads_for_params params a t)
        acc fields
  | TCtorApp (_, args) ->
      List.fold_left (collect_ctypeapp_heads_for_params params) acc args
  | IntType
  | FloatType
  | BoolType
  | StringType
  | CharType
  | UnitType
  | TypeVar _
  | TypeName _ -> acc

(** Per inter parameter: inferred arity from [p<...>] uses, or [0] if none. *)
let infer_inter_param_arities (params : string list)
    (method_monos : mono_type list) : (string * int) list =
  let acc =
    List.fold_left
      (fun a mt -> collect_ctypeapp_heads_for_params params a mt)
      [] method_monos
  in
  List.map
    (fun p ->
      ( p,
        match List.assoc_opt p acc with
        | Some n -> n
        | None -> 0 ))
    params

let rec replace_inter_heads_with_tctor ~(params : string list) (t : mono_type) :
    mono_type =
  match t with
  | FunctionType (a, b) ->
      FunctionType
        ( replace_inter_heads_with_tctor ~params a,
          replace_inter_heads_with_tctor ~params b )
  | VectorType ts ->
      VectorType (List.map (replace_inter_heads_with_tctor ~params) ts)
  | CListType e -> CListType (replace_inter_heads_with_tctor ~params e)
  | CTypeApp (name, args) when List.mem name params ->
      TCtorApp
        ( written_param_var name,
          List.map (replace_inter_heads_with_tctor ~params) args )
  | CTypeApp (name, args) ->
      CTypeApp (name, List.map (replace_inter_heads_with_tctor ~params) args)
  | FixedPoint (n, body) ->
      FixedPoint (n, replace_inter_heads_with_tctor ~params body)
  | RecordType fields ->
      RecordType
        (List.map
           (fun (nm, t') -> (nm, replace_inter_heads_with_tctor ~params t'))
           fields)
  | ( IntType
    | FloatType
    | BoolType
    | StringType
    | CharType
    | UnitType
    | TypeVar _
    | TypeName _ ) as leaf -> leaf
  | TCtorApp (w, args) ->
      TCtorApp (w, List.map (replace_inter_heads_with_tctor ~params) args)

(** Apply impl head [inst] to type arguments (after inner substitution). *)
let expand_instance_head (inst : mono_type) (args : mono_type list) : mono_type
    =
  match (inst, args) with
  | TypeName n, _ -> CTypeApp (n, args)
  | CListType _, [ elem ] -> CListType elem
  | CListType _, _ ->
      failwith
        "forge: internal list instance head expects exactly one type argument"
  | CTypeApp _, _ ->
      failwith
        "forge: partial application is not allowed as an impl head (type \
         constructor must be bare, e.g. Option not Option<int>)"
  | _ ->
      failwith
        "forge: invalid instance type for a higher-kind inter (expected a bare \
         type constructor such as Option or [u])"

let rec substitute_instance_in_mono ~(written_var : string) ~(inst : mono_type)
    (arity : int) (t : mono_type) : mono_type =
  match t with
  | TypeVar v when v = written_var && arity = 0 -> inst
  | TypeVar v -> TypeVar v
  | TCtorApp (w, args) when w = written_var && arity > 0 ->
      let args' =
        List.map (substitute_instance_in_mono ~written_var ~inst arity) args
      in
      expand_instance_head inst args'
  | TCtorApp (w, args) ->
      TCtorApp
        (w, List.map (substitute_instance_in_mono ~written_var ~inst arity) args)
  | FunctionType (a, b) ->
      FunctionType
        ( substitute_instance_in_mono ~written_var ~inst arity a,
          substitute_instance_in_mono ~written_var ~inst arity b )
  | VectorType ts ->
      VectorType
        (List.map (substitute_instance_in_mono ~written_var ~inst arity) ts)
  | CListType e ->
      CListType (substitute_instance_in_mono ~written_var ~inst arity e)
  | CTypeApp (name, args) ->
      CTypeApp
        ( name,
          List.map (substitute_instance_in_mono ~written_var ~inst arity) args
        )
  | FixedPoint (n, body) ->
      FixedPoint (n, substitute_instance_in_mono ~written_var ~inst arity body)
  | RecordType fields ->
      RecordType
        (List.map
           (fun (nm, t') ->
             (nm, substitute_instance_in_mono ~written_var ~inst arity t'))
           fields)
  | ( IntType
    | FloatType
    | BoolType
    | StringType
    | CharType
    | UnitType
    | TypeName _ ) as leaf -> leaf

let arity_of_name_in_seen (seen : (string * int) list) (n : string) : int option
    =
  List.assoc_opt n seen

let validate_impl_head ~(class_name : string) ~(required_arity : int)
    ~(seen_ctors : (string * int) list) (inst : mono_type) : unit =
  if required_arity = 0 then ()
  else
    match inst with
    | CTypeApp _ ->
        failwith
          "forge: impl head must not be a partial type application (use a bare \
           constructor, e.g. Option not Option<int>)"
    | TypeName n -> (
        match arity_of_name_in_seen seen_ctors n with
        | Some k when k <> required_arity ->
            failwith
              ("Cannot use " ^ n ^ " as " ^ class_name ^ " (expected arity "
              ^ string_of_int required_arity
              ^ ", got " ^ string_of_int k ^ ")")
        | _ -> ())
    | CListType _ when required_arity = 1 -> ()
    | CListType _ ->
        failwith
          ("Cannot use [] as " ^ class_name ^ " (expected arity "
          ^ string_of_int required_arity
          ^ ", got 1)")
    | _ ->
        failwith
          ("forge: impl for " ^ class_name
         ^ " requires a bare type constructor (type name or [u])")

(** Which curried argument position carries the functor / instance key type. *)
let rec peel_dom (acc : mono_type list) (t : mono_type) :
    mono_type list * mono_type =
  match t with
  | FunctionType (a, r) -> peel_dom (a :: acc) r
  | ret -> (List.rev acc, ret)

let rec contains_tctor_for_var (written_var : string) (t : mono_type) : bool =
  match t with
  | TCtorApp (w, _) -> w = written_var
  | FunctionType (a, b) ->
      contains_tctor_for_var written_var a
      || contains_tctor_for_var written_var b
  | VectorType ts -> List.exists (contains_tctor_for_var written_var) ts
  | CListType e -> contains_tctor_for_var written_var e
  | CTypeApp (_, args) -> List.exists (contains_tctor_for_var written_var) args
  | FixedPoint (_, body) -> contains_tctor_for_var written_var body
  | RecordType fields ->
      List.exists (fun (_, t') -> contains_tctor_for_var written_var t') fields
  | IntType
  | FloatType
  | BoolType
  | StringType
  | CharType
  | UnitType
  | TypeVar _
  | TypeName _ -> false

(** [0] if the class parameter appears only as a type variable; otherwise the
    arity of [TCtorApp(w, ...)] uses for [w]. *)
let rec ctor_arity_for_written_var (written_var : string) (t : mono_type) : int
    =
  match t with
  | FunctionType (a, b) -> (
      match ctor_arity_for_written_var written_var a with
      | 0 -> ctor_arity_for_written_var written_var b
      | n -> n)
  | VectorType ts ->
      List.fold_left
        (fun acc u -> max acc (ctor_arity_for_written_var written_var u))
        0 ts
  | CListType e -> ctor_arity_for_written_var written_var e
  | CTypeApp (_, args) ->
      List.fold_left
        (fun acc u -> max acc (ctor_arity_for_written_var written_var u))
        0 args
  | FixedPoint (_, body) -> ctor_arity_for_written_var written_var body
  | RecordType fields ->
      List.fold_left
        (fun acc (_, u) -> max acc (ctor_arity_for_written_var written_var u))
        0 fields
  | TCtorApp (w, args) ->
      let k = if w = written_var then List.length args else 0 in
      List.fold_left
        (fun acc u -> max acc (ctor_arity_for_written_var written_var u))
        k args
  | IntType
  | FloatType
  | BoolType
  | StringType
  | CharType
  | UnitType
  | TypeVar _
  | TypeName _ -> 0

let dict_resolution_arg_index (mty : mono_type) (written_class_var : string) :
    int =
  let dom, _ = peel_dom [] mty in
  let rec scan i = function
    | [] -> 0
    | arg :: rest ->
        if contains_tctor_for_var written_class_var arg then i
        else scan (i + 1) rest
  in
  scan 0 dom
