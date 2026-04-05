open Cexpr
open C_to_string
open Ceval
open Typefixer
open Forge_class_util

type type_equation = mono_type * mono_type
type type_equations = type_equation list

type class_equations = (string * mono_type) list
(** Class constraints collected while inferring a single expression (see
    [generate_e_id] / [generate_class_method_app]). Flushed in [type_of_c_expr].
*)

let pending_expression_class_preds : class_equations ref = ref []
let forge_written_param (p : string) : string = "$written(" ^ p ^ ")"

let rec scheme_has_class_constraint (t : c_type) : bool =
  match t with
  | Constrained _ -> true
  | PolyType (_, inner) -> scheme_has_class_constraint inner
  | Mono _ -> false

(** Solver metavariable heads [[t123]] from [fresh_type_var] — safe to rename
    per use site. Rigid heads like [[$written(List)]] must stay. *)
let is_solver_tctor_head_name (w : string) : bool =
  String.length w >= 2
  && w.[0] = 't'
  &&
  let rest = String.sub w 1 (String.length w - 1) in
  rest <> "" && String.for_all (fun c -> c >= '0' && c <= '9') rest

(** Names that appear as [TypeVar] in the method type or preds (before freshen),
    plus [TCtorApp] heads that are solver metavariables (see
    {!is_solver_tctor_head_name}). Rigid heads like [$written(List)] stay
    unchanged. *)
let flex_tyvar_names_in_method (m : mono_type) (preds : class_equations) :
    string list =
  let acc = ref [] in
  let add v = if not (List.mem v !acc) then acc := v :: !acc in
  let rec walk_mono (t : mono_type) : unit =
    match t with
    | TypeVar v -> add v
    | FunctionType (a, b) ->
        walk_mono a;
        walk_mono b
    | VectorType ts -> List.iter walk_mono ts
    | CListType e -> walk_mono e
    | CTypeApp (_, args) -> List.iter walk_mono args
    | TCtorApp (w, args) ->
        if is_solver_tctor_head_name w then add w;
        List.iter walk_mono args
    | FixedPoint (_, body) -> walk_mono body
    | RecordType fs -> List.iter (fun (_, t) -> walk_mono t) fs
    | IntType
    | FloatType
    | BoolType
    | StringType
    | CharType
    | UnitType
    | TypeName _ -> ()
  in
  List.iter (fun (_, ty) -> walk_mono ty) preds;
  walk_mono m;
  !acc

(** After peeling [Poly]/[Constrained], copy the method [mono] and class preds
    so every source type variable is renamed to a fresh name **for this use
    site**. Otherwise method parameters from the [inter] (e.g. [a] in
    [a -> m<a>]) are shared across all applications of [return] / [>>=], and
    nested calls like [return (return 10)] wrongly force the same [a] to be both
    [Int] and [m Int]. *)
let freshen_method_mono_and_preds (m : mono_type) (preds : class_equations) :
    mono_type * class_equations =
  let flex = flex_tyvar_names_in_method m preds in
  let is_flex_head w = List.mem w flex || is_solver_tctor_head_name w in
  let map : (string * string) list ref = ref [] in
  let fresh_name () =
    match fresh_type_var () with
    | TypeVar s -> s
    | _ -> assert false
  in
  let map_var (v : string) : string =
    match List.assoc_opt v !map with
    | Some v' -> v'
    | None ->
        let v' = fresh_name () in
        map := (v, v') :: !map;
        v'
  in
  let rec freshen (t : mono_type) : mono_type =
    match t with
    | TypeVar v -> TypeVar (map_var v)
    | FunctionType (a, b) -> FunctionType (freshen a, freshen b)
    | VectorType ts -> VectorType (List.map freshen ts)
    | CListType e -> CListType (freshen e)
    | CTypeApp (name, args) -> CTypeApp (name, List.map freshen args)
    | TCtorApp (w, args) ->
        let w' = if is_flex_head w then map_var w else w in
        TCtorApp (w', List.map freshen args)
    | FixedPoint (n, body) -> FixedPoint (n, freshen body)
    | RecordType fields ->
        RecordType (List.map (fun (name, t) -> (name, freshen t)) fields)
    | ( IntType
      | FloatType
      | BoolType
      | StringType
      | CharType
      | UnitType
      | TypeName _ ) as prim -> prim
  in
  let preds' = List.map (fun (c, ty) -> (c, freshen ty)) preds in
  let m' = freshen m in
  (m', preds')

let rec instantiate_infer_scheme_inner (t : c_type) :
    mono_type * class_equations =
  match t with
  | Mono m -> (m, [])
  | Constrained (ps, inner) ->
      let m, ps2 = instantiate_infer_scheme_inner inner in
      (m, ps @ ps2)
  | PolyType (v, body) ->
      let arg = fresh_type_var () in
      let body' = substitute_type body v arg in
      instantiate_infer_scheme_inner body'

let instantiate_infer_scheme (t : c_type) : mono_type * class_equations =
  let m, preds = instantiate_infer_scheme_inner t in
  freshen_method_mono_and_preds m preds

type type_error =
  | UnboundVariable of string
  | TypeMismatch of mono_type * mono_type
  | PatternMismatch of c_pat * mono_type
  | OtherError of string

type 'a type_check_result =
  | Ok of 'a
  | Error of type_error

let string_of_type_check_error (e : type_error) : string =
  match e with
  | UnboundVariable s -> "Unbound variable: " ^ s
  | TypeMismatch (t1, t2) ->
      "Type mismatch: " ^ string_of_mono_type t1 ^ " != "
      ^ string_of_mono_type t2
  | PatternMismatch (p, t) ->
      "Pattern mismatch: " ^ string_of_pat p ^ " != " ^ string_of_mono_type t
  | OtherError s -> "Other error: " ^ s

let return (x : 'a) : 'a type_check_result = Ok x

let ( >>= ) (x : 'a type_check_result) (f : 'a -> 'b type_check_result) :
    'b type_check_result =
  match x with
  | Ok x -> f x
  | Error e -> Error e

let ( let- ) = ( >>= )

let unwrap_type_check_result (x : 'a type_check_result) : 'a =
  match x with
  | Ok x -> x
  | Error e -> failwith (string_of_type_check_error e)

exception TypeFailure

let assert_distinct_record_field_names (names : string list) : unit =
  let sorted = List.sort String.compare names in
  let rec nodups = function
    | a :: b :: _ when String.compare a b = 0 -> raise TypeFailure
    | _ :: r -> nodups r
    | [] -> ()
  in
  nodups sorted

(* maps type names to their types *)
type type_env = (string * string list * mono_type) list

type sum_type_decl = string * string list * (string * c_type option) list
(** Declared sum types: type name, type parameters, ordered constructors. *)

type constructor_env = sum_type_decl list
(** All sum type declarations in scope (for native lowering / exhaustiveness).
*)

let string_of_type_env (env : type_env) : string =
  let rec aux acc = function
    | [] -> acc
    | (id, params, t) :: env' ->
        let params_str =
          if params = [] then "" else "<" ^ String.concat ", " params ^ ">"
        in
        aux (id ^ params_str ^ " : " ^ string_of_mono_type t ^ "\n" ^ acc) env'
  in
  aux "" env

(* Algorithm for performing type inference:

   Given an expression e, we want to infer the most general type of e.

   We start with an empty static environment. We generate constraints for e.

   Then, we solve the constraints to get a most general monomorphic type for e.

   Finally, we generalize the monomorphic type to a polymorphic type. *)

(** [split3 lst] splits a list of triples into three lists.

    For example, [split3 [(1,2,3); (4,5,6)]] returns ([1;4], [2;5], [3;6])

    @param lst The list of triples to split
    @return
      A triple containing three lists - one for each component of the input
      triples *)
let split3 (lst : ('a * 'b * 'c) list) : 'a list * 'b list * 'c list =
  let rec split3_helper (lst : ('a * 'b * 'c) list) (a : 'a list) (b : 'b list)
      (c : 'c list) : 'a list * 'b list * 'c list =
    match lst with
    | [] -> (a, b, c)
    | (a', b', c') :: tail -> split3_helper tail (a' :: a) (b' :: b) (c' :: c)
  in
  let a, b, c = split3_helper lst [] [] [] in
  (List.rev a, List.rev b, List.rev c)

(** [string_of_type_equation eq] converts a type equation to a string
    representation.

    For example, [string_of_type_equation (IntType, BoolType)] returns "int =
    bool"

    @param eq A type equation consisting of two monomorphic types
    @return
      A string representation of the equation with the types separated by " = "
*)
let string_of_type_equation ((t1, t2) : type_equation) : string =
  string_of_mono_type t1 ^ " = " ^ string_of_mono_type t2

(** [string_of_type_equations equations] converts a list of type equations to a
    string representation.

    Each equation is printed on a new line using [string_of_type_equation].

    For example,
    [string_of_type_equations [(IntType, BoolType); (StringType, IntType)]]
    returns: "int = bool string = int"

    @param equations A list of type equations
    @return
      A string representation of the equations with each equation on a new line
*)
let rec string_of_type_equations (c : type_equations) : string =
  match c with
  | [] -> ""
  | (t1, t2) :: c' ->
      string_of_type_equation (t1, t2) ^ "\n" ^ string_of_type_equations c'

(** [string_of_static_env env] converts a static environment to a string
    representation.

    Each binding in the environment is printed on a new line in the format "id :
    type".

    For example, [string_of_static_env [("x", IntType); ("y", BoolType)]]
    returns: "x : int y : bool"

    @param env A static environment mapping identifiers to types
    @return
      A string representation of the environment with each binding on a new line
*)
let rec string_of_static_env (env : static_env) : string =
  match env with
  | [] -> ""
  | (id, t) :: env' ->
      id ^ " : " ^ string_of_c_type t ^ "\n" ^ string_of_static_env env'

and string_of_mono_type (t : mono_type) : string =
  match t with
  | IntType -> "Int"
  | FloatType -> "Float"
  | BoolType -> "Bool"
  | StringType -> "String"
  | CharType -> "Char"
  | UnitType -> "Unit"
  | TypeVar v -> v
  | FunctionType (t1, t2) ->
      let t1_str = string_of_mono_type t1 in
      let t2_str = string_of_mono_type t2 in
      "(" ^ t1_str ^ " -> " ^ t2_str ^ ")"
  | VectorType types ->
      let types_str = List.map string_of_mono_type types in
      "(" ^ String.concat ", " types_str ^ ")"
  | CListType et -> "[" ^ string_of_mono_type et ^ "]"
  | TypeName v -> v
  | CTypeApp (name, args) ->
      let args_str = List.map string_of_mono_type args in
      name ^ "<" ^ String.concat ", " args_str ^ ">"
  | TCtorApp (w, args) -> Cexpr.string_of_mono_type (TCtorApp (w, args))
  | FixedPoint (name, body) -> "μ" ^ name ^ ". " ^ string_of_mono_type body
  | RecordType fields ->
      let field_strs =
        List.map (fun (name, t) -> name ^ ": " ^ string_of_mono_type t) fields
      in
      "{" ^ String.concat ", " field_strs ^ "}"

(** Substitute a type variable inside a monomorphic type (used for forge dicts).
*)
let rec replace_typevar_in_mono ~(var_id : string) ~(with_ty : mono_type)
    (t : mono_type) : mono_type =
  match t with
  | IntType | FloatType | BoolType | StringType | CharType | UnitType -> t
  | TypeVar id -> if id = var_id then with_ty else TypeVar id
  | TypeName n -> TypeName n
  | FunctionType (i, o) ->
      FunctionType
        ( replace_typevar_in_mono ~var_id ~with_ty i,
          replace_typevar_in_mono ~var_id ~with_ty o )
  | VectorType ts ->
      VectorType (List.map (replace_typevar_in_mono ~var_id ~with_ty) ts)
  | CListType et -> CListType (replace_typevar_in_mono ~var_id ~with_ty et)
  | CTypeApp (name, args) ->
      CTypeApp (name, List.map (replace_typevar_in_mono ~var_id ~with_ty) args)
  | TCtorApp (w, args) ->
      TCtorApp (w, List.map (replace_typevar_in_mono ~var_id ~with_ty) args)
  | FixedPoint (n, body) ->
      FixedPoint (n, replace_typevar_in_mono ~var_id ~with_ty body)
  | RecordType fields ->
      RecordType
        (List.map
           (fun (name, t) -> (name, replace_typevar_in_mono ~var_id ~with_ty t))
           fields)

let instantiate_dict_scheme_to_record ~(sch : c_type) ~(tau : mono_type) :
    mono_type option =
  let rec collect_poly_vars acc t =
    match t with
    | PolyType (v, inner) -> collect_poly_vars (v :: acc) inner
    | Constrained (_, inner) -> collect_poly_vars acc inner
    | Mono _ -> acc
  in
  let poly_vars = collect_poly_vars [] sch in
  let rec strip_to_mono t =
    match t with
    | Mono m -> Some m
    | Constrained (_, inner) -> strip_to_mono inner
    | PolyType (_, inner) -> strip_to_mono inner
  in
  match strip_to_mono sch with
  | None -> None
  | Some mono_body -> (
      let substituted =
        List.fold_left
          (fun acc var_id -> replace_typevar_in_mono ~var_id ~with_ty:tau acc)
          mono_body poly_vars
      in
      match substituted with
      | RecordType _ -> Some substituted
      | _ -> None)

(** Slug segment of [dict_name] after [__forge_dict_<class>_], if any. *)
let forge_dict_slug ~(class_name : string) (dict_name : string) : string option
    =
  let p = "__forge_dict_" ^ class_name ^ "_" in
  if String.starts_with ~prefix:p dict_name then
    Some
      (String.sub dict_name (String.length p)
         (String.length dict_name - String.length p))
  else None

(** Reject dictionary candidates whose instance head (encoded in the dict name)
    cannot apply to [tau]. Without this, [instantiate_dict_scheme_to_record]
    substitutes every top-level [Poly] of the dict scheme with [tau], so e.g. a
    [Functor Option] dict spuriously "matches" [[Int]] and steals dispatch from
    [Functor [u]]. *)
let dict_candidate_matches_tau ~(dict_name : string) ~(class_name : string)
    (tau : mono_type) : bool =
  match forge_dict_slug ~class_name dict_name with
  | None -> false
  | Some sfx -> (
      let sfx_starts (pre : string) = String.starts_with ~prefix:pre sfx in
      match tau with
      | TypeVar _ -> true
      | CListType _ -> sfx_starts "list"
      | CTypeApp (n, _) -> sfx_starts n || sfx_starts (n ^ "__")
      | FixedPoint (n, _) -> sfx_starts ("mu_" ^ n) || sfx_starts n
      | _ -> true)

let find_compatible_dict_name ~(static_env : static_env) ~(class_name : string)
    ~(method_name : string) ~(tau : mono_type) : string option =
  let prefix = "__forge_dict_" ^ class_name ^ "_" in
  let exact = dict_for_instance ~class_name tau in
  let exact_ok =
    match List.assoc_opt exact static_env with
    | Some sch -> (
        match instantiate_dict_scheme_to_record ~sch ~tau with
        | Some (RecordType fields) -> List.mem_assoc method_name fields
        | _ -> false)
    | None -> false
  in
  if exact_ok then Some exact
  else
    let rec scan env =
      match env with
      | [] -> None
      | (name, sch) :: rest ->
          if String.starts_with ~prefix name then
            if not (dict_candidate_matches_tau ~dict_name:name ~class_name tau)
            then scan rest
            else
              match instantiate_dict_scheme_to_record ~sch ~tau with
              | Some (RecordType fields) when List.mem_assoc method_name fields
                -> Some name
              | _ -> scan rest
          else scan rest
    in
    scan static_env

(** Like [find_compatible_dict_name] but does not require the method name to be
    a field of the dictionary.  Used when dispatching a non-method function that
    carries a class constraint (dictionary-passing style). *)
let find_dict_for_class ~(static_env : static_env) ~(class_name : string)
    ~(tau : mono_type) : string option =
  let prefix = "__forge_dict_" ^ class_name ^ "_" in
  let exact = dict_for_instance ~class_name tau in
  if List.mem_assoc exact static_env then Some exact
  else
    let rec scan = function
      | [] -> None
      | (name, sch) :: rest ->
          if String.starts_with ~prefix name
             && dict_candidate_matches_tau ~dict_name:name ~class_name tau
          then
            match instantiate_dict_scheme_to_record ~sch ~tau with
            | Some (RecordType _) -> Some name
            | _ -> scan rest
          else scan rest
    in
    scan static_env

(** Return the method names declared in [class_name] by inspecting existing
    dictionary records in the static environment. *)
let class_method_names ~(static_env : static_env) ~(class_name : string) :
    string list =
  let prefix = "__forge_dict_" ^ class_name ^ "_" in
  let rec peel_mono (t : c_type) : mono_type =
    match t with
    | Mono t -> t
    | PolyType (_, t) | Constrained (_, t) -> peel_mono t
  in
  List.fold_left
    (fun acc (name, sch) ->
      if String.starts_with ~prefix name then
        match peel_mono sch with
        | RecordType fields ->
            List.fold_left
              (fun a (f, _) -> if List.mem f a then a else f :: a)
              acc fields
        | _ -> acc
      else acc)
    [] static_env

(** Polymorphic instance heads (e.g. [impl Monoid for [a]]) use dictionary names
    that do not match the exact [dict_for_instance] slug of a concrete [tau];
    [generalize] still needs to accept those predicates. *)
let forge_dict_resolves_for_class ~(static_env : static_env)
    ~(class_name : string) (tau : mono_type) : bool =
  let exact = dict_for_instance ~class_name tau in
  if List.mem_assoc exact static_env then true
  else
    let prefix = "__forge_dict_" ^ class_name ^ "_" in
    List.exists
      (fun (name, sch) ->
        String.starts_with ~prefix name
        && dict_candidate_matches_tau ~dict_name:name ~class_name tau
        &&
        match instantiate_dict_scheme_to_record ~sch ~tau with
        | Some (RecordType _) -> true
        | _ -> false)
      static_env

(** [generate env e] performs type inference on the expression [e] in the static
    environment [env].

    It returns a pair [(t, constraints)], where [t] is the inferred monomorphic
    type of [e], and [constraints] is a list of type equations (constraints)
    that must be satisfied for the typing to be valid.

    The function recursively traverses the structure of [e], generating fresh
    type variables as needed, and accumulates constraints based on the typing
    rules for each kind of expression.

    @param env The static environment mapping variable names to their types
    @param e The expression to typecheck
    @return
      A pair [(t, constraints)] where [t] is the inferred type and [constraints]
      is the list of type equations *)
let rec generate (env : static_env) (type_env : type_env) (e : c_expr) :
    (mono_type * type_equations * type_env) type_check_result =
  match e with
  | EInt _ -> generate_e_int
  | EFloat _ -> generate_e_float
  | EBool _ -> generate_e_bool
  | EString _ -> generate_e_string
  | EChar _ -> generate_e_char
  | EUnit -> generate_e_unit
  | ENil -> generate_e_nil ()
  | EId x -> generate_e_id env x
  | EBop (op, e1, e2) -> generate_e_bop env type_env op e1 e2
  | EFunction (pat, cto, body) -> generate_e_function env type_env pat cto body
  | EApp (e1, e2) -> generate_e_app env type_env e1 e2
  | EBind (pat, cto, e1, e2, return_type) ->
      generate_e_bind env type_env pat cto e1 e2 return_type
  | EBindRec (pat, _, e1, e2, return_type) ->
      generate_e_bind_rec env type_env pat e1 e2 return_type
  | EBindMutRec (bindings, body) ->
      generate_e_bind_mut_rec env type_env bindings body
  | ETernary (e1, e2, e3) -> generate_e_ternary env type_env e1 e2 e3
  | EVector expressions -> generate_e_vector env type_env expressions
  | EListEnumeration (e1, e2) -> generate_e_list_enumeration env type_env e1 e2
  | EListComprehension (e, generators) ->
      generate_e_list_comprehension env type_env e generators
  | ESwitch (e1, branches) -> generate_e_switch env type_env e1 branches
  | ERecordLit fields ->
      let () = assert_distinct_record_field_names (List.map fst fields) in
      (* Generate constraints for each field expression *)
      let rec process_fields acc_types acc_equations = function
        | [] -> return (List.rev acc_types, acc_equations)
        | (field_name, field_expr) :: rest ->
            let- field_type, field_equations, _ =
              generate env type_env field_expr
            in
            process_fields
              ((field_name, field_type) :: acc_types)
              (acc_equations @ field_equations)
              rest
      in
      let- field_types, equations = process_fields [] [] fields in
      return (RecordType field_types, equations, [])
  | ERecordUpdate (record_expr, updates) ->
      (* { expr with field1 = val1, ... } - type is same as record_expr *)
      let- record_type, record_equations, _ =
        generate env type_env record_expr
      in
      (* For each update, add constraint that the field type matches *)
      let rec process_updates acc_equations = function
        | [] -> return (record_type, acc_equations, [])
        | (field_name, field_expr) :: rest ->
            let- field_type, field_equations, _ =
              generate env type_env field_expr
            in
            let minimal_record = RecordType [ (field_name, field_type) ] in
            let equation = (record_type, minimal_record) in
            process_updates ((equation :: field_equations) @ acc_equations) rest
      in
      process_updates record_equations updates
  | EFieldAccess (record_expr, field_name) -> (
      let- record_type, record_equations, _ =
        generate env type_env record_expr
      in
      (* Known record shape (e.g. typeclass dictionary): use the field's type
         and freshen its metavariables so each access site is independent — same
         issue as [instantiate_infer_scheme] for [inter] methods. *)
      match record_type with
      | RecordType fields -> (
          match List.assoc_opt field_name fields with
          | None ->
              let field_type = fresh_type_var () in
              let minimal_record = RecordType [ (field_name, field_type) ] in
              return
                ( field_type,
                  (record_type, minimal_record) :: record_equations,
                  [] )
          | Some ft ->
              let ft', _ = freshen_method_mono_and_preds ft [] in
              return (ft', record_equations, []))
      | _ ->
          let field_type = fresh_type_var () in
          let minimal_record = RecordType [ (field_name, field_type) ] in
          let equation = (record_type, minimal_record) in
          return (field_type, equation :: record_equations, []))
  | EBlock [] -> return (UnitType, [], [])
  | EBlock parts -> (
      (* if the last part is a definition, then the entire thing evaluates to
         unit *)
      let last_part = List.hd (List.rev parts) in
      let other_parts = List.rev (List.tl (List.rev parts)) in

      (* if last part is a definition, then the type of the block is unit *)
      match last_part with
      | Defn _ -> return (UnitType, [], [])
      | Expr e ->
          (* generate each definition in the block *)
          let- new_env, defn_equations, _ =
            let rec process_defns acc_env acc_equations acc_type_env = function
              | [] -> return (acc_env, acc_equations, acc_type_env)
              | Defn d :: rest ->
                  let- new_bindings, new_type_env, _new_ctor_env =
                    generate_defn acc_env acc_type_env d
                  in
                  process_defns (new_bindings @ acc_env) acc_equations
                    (new_type_env @ acc_type_env)
                    rest
              | Expr _ :: rest ->
                  process_defns acc_env acc_equations acc_type_env rest
            in
            process_defns env [] type_env other_parts
          in

          (* generate type for the last expression in the new environment *)
          let- last_type, last_equations, _ = generate new_env type_env e in

          (* combine all equations *)
          return (last_type, defn_equations @ last_equations, []))

(* use List.fold_left to generate the type constraints for each definition*)

(* use fold_left to generate the type constraints for each definition *)

(* otherwise, we need to generate type constraints for the entire block *)

(** [generate_e_int] generates type constraints for integer literals.
    @return A pair containing IntType and an empty list of constraints *)
and generate_e_int = return (IntType, [], [])

(** [generate_e_float] generates type constraints for float literals.
    @return A pair containing FloatType and an empty list of constraints *)
and generate_e_float = return (FloatType, [], [])

(** [generate_e_bool] generates type constraints for boolean literals.
    @return A pair containing BoolType and an empty list of constraints *)
and generate_e_bool = return (BoolType, [], [])

(** [generate_e_char] generates type constraints for char literals.
    @return A pair containing CharType and an empty list of constraints *)
and generate_e_char = return (CharType, [], [])

(** [generate_e_string] generates type constraints for string literals.
    @return A pair containing StringType and an empty list of constraints *)
and generate_e_string = return (StringType, [], [])

(** [generate_e_unit] generates type constraints for unit literals.
    @return A pair containing UnitType and an empty list of constraints *)
and generate_e_unit = return (UnitType, [], [])

(** [generate_e_nil] generates type constraints for nil literals.
    @return
      A pair containing a list type with a fresh type variable and an empty list
      of constraints *)
and generate_e_nil () = return (CListType (fresh_type_var ()), [], [])

(** [generate_e_id env x] generates type constraints for identifier expressions.
    @param env The static environment mapping identifiers to their types
    @param x The identifier to look up
    @return
      A pair containing the instantiated type of the identifier and an empty
      list of constraints *)
and generate_e_id (env : static_env) (x : string) :
    (mono_type * type_equations * type_env) type_check_result =
  match List.assoc_opt x env with
  | Some sch when scheme_has_class_constraint sch ->
      let m, preds = instantiate_infer_scheme sch in
      pending_expression_class_preds := preds @ !pending_expression_class_preds;
      return (m, [], [])
  | Some uninstantiated ->
      let t = instantiate uninstantiated in
      return (t, [], [])
  | None -> Error (UnboundVariable x)

(** [generate_e_bop env op e1 e2] generates type constraints for binary
    operations.
    @param env The static environment
    @param op The binary operator
    @param e1 The left operand expression
    @param e2 The right operand expression
    @return A pair containing the result type and constraints for the operation
*)
and generate_e_bop (env : static_env) (type_env : type_env) (op : c_bop)
    (e1 : c_expr) (e2 : c_expr) :
    (mono_type * type_equations * type_env) type_check_result =
  let- t1, c1, _ = generate env type_env e1 in
  let- t2, c2, _ = generate env type_env e2 in
  match op with
  | CCons ->
      (* e1 :: e2, e2 must be a list of the type of e1 *)
      return (t2, ((CListType t1, t2) :: c1) @ c2, [])
  | CPlus | CMinus | CMul | CDiv | CMod ->
      (* arithmetic: both operands must be int, result is int *)
      return (IntType, ((t1, IntType) :: (t2, IntType) :: c1) @ c2, [])
  | CConcat ->
      (* string concatenation: both operands must be string, result is string *)
      return (StringType, ((t1, StringType) :: (t2, StringType) :: c1) @ c2, [])
  | CGE | CGT | CLE | CLT ->
      (* comparisons: both operands must be int, result is bool *)
      return (BoolType, ((t1, IntType) :: (t2, IntType) :: c1) @ c2, [])
  | CEQ | CNE ->
      (* equality/inequality: operands must be same type, result is bool *)
      return (BoolType, ((t1, t2) :: c1) @ c2, [])
  | CAnd | COr ->
      (* logical: both operands must be bool, result is bool *)
      return (BoolType, ((t1, BoolType) :: (t2, BoolType) :: c1) @ c2, [])

(** [generate_e_function env pat cto body] generates type constraints for
    function expressions.
    @param env The static environment
    @param pat The function parameter pattern
    @param cto Optional type annotation for the function
    @param body The function body expression
    @return A pair containing the function type and constraints for the function
*)
and generate_e_function (env : static_env) (type_env : type_env) (pat : c_pat)
    (cto : c_type option) (body : c_expr) :
    (mono_type * type_equations * type_env) type_check_result =
  let input_type, new_env_bindings, constraints_from_pattern =
    type_of_pat env type_env pat
  in
  let- constraints_from_type_annotation =
    match cto with
    | Some t ->
        let- simplified_t = instantiate_and_simplify t type_env in
        return [ (input_type, simplified_t) ]
    | None -> return []
  in
  let- output_type, c_output, _ =
    generate (new_env_bindings @ env) type_env body
  in

  return
    ( input_type => output_type,
      constraints_from_pattern @ constraints_from_type_annotation @ c_output,
      [] )

(** [generate_e_app env e1 e2] generates type constraints for function
    application.
    @param env The static environment
    @param e1 The function expression
    @param e2 The argument expression
    @return
      A pair containing the result type and constraints for the application *)
and generate_class_method_app (env : static_env) (type_env : type_env)
    (_f : string) (sch : c_type) (e2 : c_expr) :
    (mono_type * type_equations * type_env) type_check_result =
  let t_fun, preds = instantiate_infer_scheme sch in
  pending_expression_class_preds := preds @ !pending_expression_class_preds;
  let- t2, c2, _ = generate env type_env e2 in
  let result_type = fresh_type_var () in
  let app_constraint = (t_fun, FunctionType (t2, result_type)) in
  return (result_type, app_constraint :: c2, [])

and generate_e_app_plain (env : static_env) (type_env : type_env) (e1 : c_expr)
    (e2 : c_expr) : (mono_type * type_equations * type_env) type_check_result =
  let- t1, c1, _ = generate env type_env e1 in
  let- t2, c2, _ = generate env type_env e2 in
  let result_type = fresh_type_var () in
  let app_constraint = (t1, FunctionType (t2, result_type)) in
  return (result_type, (app_constraint :: c1) @ c2, [])

and generate_e_app (env : static_env) (type_env : type_env) (e1 : c_expr)
    (e2 : c_expr) : (mono_type * type_equations * type_env) type_check_result =
  match e1 with
  | EId f -> (
      match List.assoc_opt f env with
      | Some sch when scheme_has_class_constraint sch ->
          generate_class_method_app env type_env f sch e2
      | _ -> generate_e_app_plain env type_env e1 e2)
  | _ -> generate_e_app_plain env type_env e1 e2

(** [generate_e_bind env pat cto e1 e2] generates type constraints for let
    bindings.
    @param env The static environment
    @param pat The pattern to bind to
    @param cto Optional type annotation for the binding
    @param e1 The expression to bind
    @param e2 The expression in the scope of the binding
    @return A pair containing the type of e2 and constraints for the binding *)
and generate_e_bind (env : static_env) (type_env : type_env) (pat : c_pat)
    (cto : c_type option) (e1 : c_expr) (e2 : c_expr)
    (return_type : c_type option) :
    (mono_type * type_equations * type_env) type_check_result =
  let t_pat, pat_env, pat_constraints = type_of_pat env type_env pat in
  let- t1, c1, _ = generate env type_env e1 in
  let p1 = !pending_expression_class_preds in
  pending_expression_class_preds := [];
  let- annotation_constraints =
    match cto with
    | Some t ->
        let- simplified_t = instantiate_and_simplify t type_env in
        return [ (t_pat, simplified_t) ]
    | None -> return []
  in
  let- return_type_constraints =
    match return_type with
    | Some t ->
        let- simplified_t = instantiate_and_simplify t type_env in
        (* Extract the return type from t1 if it's a function *)
        let rec extract_return_type ty =
          match ty with
          | FunctionType (_, ret) -> extract_return_type ret
          | other -> other
        in
        let actual_return_type = extract_return_type t1 in
        return [ (actual_return_type, simplified_t) ]
    | None -> return []
  in
  let new_constraint = (t_pat, t1) in
  (* Check if we're inside a recursive function (indicated by a function with a
     TypeVar in the environment, which means it's being defined) *)
  let is_inside_rec_function =
    List.exists
      (fun (_, t) ->
        match t with
        | Mono (TypeVar _) -> true
        | _ -> false)
      env
  in
  (* If inside a recursive function, don't generalize - just use the type
     directly to preserve type variable unification *)
  (* TODO: check if this logic is sound Make sure that we should be not
     generalizing here in this case *)
  if is_inside_rec_function then
    (* Don't generalize - use the type directly *)
    let- t2, c2, _ =
      match pat_env with
      | [] ->
          (* e.g. [let () = e1 in e2]: no identifiers bound; do not call
             [List.hd] *)
          generate env type_env e2
      | _ -> generate ((fst (List.hd pat_env), Mono t1) :: env) type_env e2
    in
    return
      ( t2,
        pat_constraints @ annotation_constraints @ return_type_constraints
        @ (new_constraint :: c1) @ c2,
        [] )
  else
    (* Generalize the type of e1 before using it in e2 *)
    let- generalized_type =
      generalize ~class_preds:p1
        (return_type_constraints @ (new_constraint :: c1))
        env type_env t1
    in
    let- t2, c2, _ =
      match pat_env with
      | [] -> generate env type_env e2
      | _ ->
          generate
            ((fst (List.hd pat_env), generalized_type) :: env)
            type_env e2
    in
    return
      ( t2,
        pat_constraints @ annotation_constraints @ return_type_constraints
        @ (new_constraint :: c1) @ c2,
        [] )

(** [generate_e_bind_rec env pat e1 e2] generates type constraints for recursive
    let bindings.
    @param env The static environment
    @param pat The pattern to bind to (must be an identifier)
    @param e1 The expression to bind
    @param e2 The expression in the scope of the binding
    @return
      A pair containing the type of e2 and constraints for the recursive binding
*)
and generate_e_bind_rec (env : static_env) (type_env : type_env) (pat : c_pat)
    (e1 : c_expr) (e2 : c_expr) (return_type : c_type option) :
    (mono_type * type_equations * type_env) type_check_result =
  let- function_id =
    match pat with
    | CIdPat id -> return id
    | _ ->
        Error
          (OtherError
             ("Invalid pattern in recursive let binding: expected an \
               identifier, got: " ^ string_of_pat pat))
  in
  let function_type = fresh_type_var () in
  let new_env = (function_id, Mono function_type) :: env in
  let- t1, c1, _ = generate new_env type_env e1 in
  let p1 = !pending_expression_class_preds in
  pending_expression_class_preds := [];
  (* Add constraint that function_type must equal t1 *)
  let new_constraint = (function_type, t1) in
  let- return_type_constraints =
    match return_type with
    | Some t ->
        let- simplified_t = instantiate_and_simplify t type_env in
        (* Extract the return type from t1 if it's a function *)
        let rec extract_return_type ty =
          match ty with
          | FunctionType (_, ret) -> extract_return_type ret
          | other -> other
        in
        let actual_return_type = extract_return_type t1 in
        return [ (actual_return_type, simplified_t) ]
    | None -> return []
  in
  (* Generalize the function type to make it polymorphic *)
  (* Use env (not new_env) so that the function's type variable can be generalized *)
  let- generalized_type =
    generalize ~class_preds:p1
      (return_type_constraints @ (new_constraint :: c1))
      env type_env t1
  in
  let- t2, c2, _ =
    generate ((function_id, generalized_type) :: env) type_env e2
  in
  return (t2, return_type_constraints @ (new_constraint :: c1) @ c2, [])

(** [generate_e_bind_mut_rec env type_env bindings body] generates type
    constraints for mutually recursive let bindings.
    @param env The static environment
    @param type_env The type environment
    @param bindings
      List of (pat, type_annotation, expr, return_type, num_explicit_params)
    @param body The expression in the scope of the bindings
    @return Type and constraints for the mutually recursive bindings *)
and generate_e_bind_mut_rec (env : static_env) (type_env : type_env)
    (bindings : (c_pat * c_type option * c_expr * c_type option * int) list)
    (body : c_expr) : (mono_type * type_equations * type_env) type_check_result
    =
  (* Extract function IDs from patterns *)
  let- function_ids =
    let rec extract_ids acc = function
      | [] -> return (List.rev acc)
      | (pat, _, _, _, _) :: rest ->
          let- id =
            match pat with
            | CIdPat id -> return id
            | _ ->
                Error
                  (OtherError
                     ("Invalid pattern in mutually recursive let binding: \
                       expected an identifier, got: " ^ string_of_pat pat))
          in
          extract_ids (id :: acc) rest
    in
    extract_ids [] bindings
  in

  (* Create fresh type variables for each function *)
  let fresh_types = List.map (fun _ -> fresh_type_var ()) function_ids in

  (* Create the mutual recursive environment *)
  let mut_rec_env =
    List.fold_left2
      (fun acc_env id fresh_type -> (id, Mono fresh_type) :: acc_env)
      env function_ids fresh_types
  in

  (* Typecheck all bodies in the mutual recursive environment *)
  let- body_results =
    let rec typecheck_bodies acc = function
      | [] -> return (List.rev acc)
      | (_, _, expr, _, _) :: rest ->
          let- t, constraints, _ = generate mut_rec_env type_env expr in
          typecheck_bodies ((t, constraints) :: acc) rest
    in
    typecheck_bodies [] bindings
  in

  (* Generate constraints for each binding *)
  let all_constraints =
    List.fold_left2
      (fun acc fresh_type (body_type, body_constraints) ->
        (* Constraint: fresh type must equal body type *)
        let type_constraint = (fresh_type, body_type) in
        (type_constraint :: body_constraints) @ acc)
      [] fresh_types body_results
  in

  (* Generalize all function types *)
  let- generalized_types =
    let rec generalize_all acc = function
      | [] -> return (List.rev acc)
      | (body_type, _) :: rest ->
          let- gen_type = generalize all_constraints env type_env body_type in
          generalize_all (gen_type :: acc) rest
    in
    generalize_all [] body_results
  in

  (* Create environment for the body with generalized types *)
  let body_env =
    List.fold_left2
      (fun acc_env id gen_type -> (id, gen_type) :: acc_env)
      env function_ids generalized_types
  in

  (* Typecheck the body *)
  let- t_body, c_body, _ = generate body_env type_env body in
  return (t_body, all_constraints @ c_body, [])

(** [generate_e_ternary env e1 e2 e3] generates type constraints for ternary
    expressions.
    @param env The static environment
    @param e1 The condition expression
    @param e2 The then expression
    @param e3 The else expression
    @return A pair containing the result type and constraints for the ternary *)
and generate_e_ternary (env : static_env) (type_env : type_env) (e1 : c_expr)
    (e2 : c_expr) (e3 : c_expr) :
    (mono_type * type_equations * type_env) type_check_result =
  let- t1, c1, _ = generate env type_env e1 in
  let- t2, c2, _ = generate env type_env e2 in
  let- t3, c3, _ = generate env type_env e3 in
  let type_of_expression = fresh_type_var () in
  return
    ( type_of_expression,
      (t1, BoolType) :: (t2, type_of_expression) :: (t3, type_of_expression)
      :: c1
      @ c2 @ c3,
      [] )

(** [generate_e_vector env expressions] generates type constraints for vector
    expressions.
    @param env The static environment
    @param expressions The list of expressions in the vector
    @return A pair containing the vector type and constraints for the vector *)
and generate_e_vector (env : static_env) (type_env : type_env)
    (expressions : c_expr list) :
    (mono_type * type_equations * type_env) type_check_result =
  let- results =
    let rec aux acc_types acc_constraints = function
      | [] -> return (List.rev acc_types, List.rev acc_constraints)
      | e :: es ->
          let- t, c, _ = generate env type_env e in
          aux (t :: acc_types) (c :: acc_constraints) es
    in
    aux [] [] expressions
  in
  let list_of_types, list_of_lists_of_constraints = results in
  return
    (VectorType list_of_types, List.flatten list_of_lists_of_constraints, [])

(** [generate_e_list_enumeration env e1 e2] generates type constraints for list
    enumeration.
    @param env The static environment
    @param e1 The start expression
    @param e2 The end expression
    @return A pair containing the list type and constraints for the enumeration
*)
and generate_e_list_enumeration (env : static_env) (type_env : type_env)
    (e1 : c_expr) (e2 : c_expr) :
    (mono_type * type_equations * type_env) type_check_result =
  let- t1, c1, _ = generate env type_env e1 in
  let- t2, c2, _ = generate env type_env e2 in
  (* Enumerations can only be done with integers *)
  return (CListType IntType, ((t1, IntType) :: (t2, IntType) :: c1) @ c2, [])

(** [generate_e_list_comprehension env e generators] generates type constraints
    for list comprehensions.
    @param env The static environment
    @param e The expression to generate list elements from
    @param generators The list of pattern-expression pairs for generators
    @return
      A pair containing the list type and constraints for the comprehension *)
and generate_e_list_comprehension (env : static_env) (type_env : type_env)
    (e : c_expr) (generators : (c_pat * c_expr) list) :
    (mono_type * type_equations * type_env) type_check_result =
  let- env, generator_constraints =
    let rec aux acc_env acc_constraints = function
      | [] -> return (acc_env, acc_constraints)
      | (p, e) :: rest ->
          let type_of_pattern, pattern_env, const =
            type_of_pat acc_env type_env p
          in
          let- type_of_expression, expression_constraints, _ =
            generate (pattern_env @ acc_env) type_env e
          in
          let new_constraint =
            (type_of_expression, CListType type_of_pattern)
          in
          let- new_env, new_constraints =
            aux (pattern_env @ acc_env)
              ((new_constraint :: const) @ expression_constraints
             @ acc_constraints)
              rest
          in
          return (new_env, new_constraints)
    in
    aux env [] generators
  in

  let- type_of_expression, expression_constraints, _ =
    generate env type_env e
  in

  return
    ( CListType type_of_expression,
      expression_constraints @ generator_constraints,
      [] )

(** [generate_e_switch env e1 branches] generates type constraints for switch
    expressions.
    @param env The static environment
    @param e1 The expression to switch on
    @param branches The list of pattern-expression pairs for each branch
    @return A pair containing the result type and constraints for the switch *)
and generate_e_switch (env : static_env) (type_env : type_env) (e1 : c_expr)
    (branches : (c_pat * c_expr) list) :
    (mono_type * type_equations * type_env) type_check_result =
  let- t1, c1, _ = generate env type_env e1 in
  let type_that_all_branch_expressions_must_be = fresh_type_var () in
  let- branch_constraints =
    let rec aux acc_constraints = function
      | [] -> return acc_constraints
      | (pat, expr) :: rest ->
          let type_of_pattern, pattern_env, const =
            type_of_pat env type_env pat
          in
          let- type_of_branch_expression, branch_expression_constraints, _ =
            generate (pattern_env @ env) type_env expr
          in
          let new_constraint =
            (type_of_pattern, t1)
            :: ( type_of_branch_expression,
                 type_that_all_branch_expressions_must_be )
            :: const
            @ branch_expression_constraints
          in
          let- new_constraints = aux (new_constraint :: acc_constraints) rest in
          return new_constraints
    in
    aux [] branches
  in
  return
    ( type_that_all_branch_expressions_must_be,
      c1 @ List.flatten branch_constraints,
      [] )

and type_of_pat (env : static_env) (type_env : type_env) (pat : c_pat) :
    mono_type * static_env * type_equations =
  match pat with
  | CIdPat id ->
      let new_var = fresh_type_var () in
      (new_var, [ (id, Mono new_var) ], [])
  | CUnitPat -> (UnitType, [], [])
  | CWildcardPat -> (fresh_type_var (), [], [])
  | CVectorPat patterns ->
      let types, envs, eqs =
        split3 (List.map (type_of_pat env type_env) patterns)
      in
      (VectorType types, List.flatten envs, List.flatten eqs)
  | CRecordPat field_pats ->
      let names = List.map fst field_pats in
      let () = assert_distinct_record_field_names names in
      let types, envs, eqs =
        split3 (List.map (fun (_, p) -> type_of_pat env type_env p) field_pats)
      in
      ( RecordType (List.map2 (fun nm ty -> (nm, ty)) names types),
        List.flatten envs,
        List.flatten eqs )
  | CIntPat _ -> (IntType, [], [])
  | CBoolPat _ -> (BoolType, [], [])
  | CCharPat _ -> (CharType, [], [])
  | CStringPat _ -> (StringType, [], [])
  | CNilPat -> (CListType (fresh_type_var ()), [], [])
  | CConsPat (p1, p2) ->
      let t1, env1, c1 = type_of_pat env type_env p1 in
      let t2, env2, c2 = type_of_pat env type_env p2 in
      (* [t1] = t2 *)
      (CListType t1, env1 @ env2, (CListType t1, t2) :: (c1 @ c2))
  | CVariantPat (cons_name, payload_pat_opt) -> (
      (* Look up the constructor in the static environment *)
      match List.assoc_opt cons_name env with
      | None ->
          (* Constructor not found - this should be a type error but for now
             return fresh var *)
          let sum_type = fresh_type_var () in
          (sum_type, [], [])
      | Some cons_type -> (
          (* Instantiate the constructor type *)
          match instantiate cons_type with
          | FunctionType (payload_type, sum_type) -> (
              (* Constructor with payload *)
              match payload_pat_opt with
              | None ->
                  (* Pattern has no payload but constructor expects one - type error *)
                  (* For now, return the sum type *)
                  (sum_type, [], [])
              | Some payload_pat ->
                  let payload_pat_type, payload_env, payload_constraints =
                    type_of_pat env type_env payload_pat
                  in
                  (* Add constraint that payload pattern type matches
                     constructor payload type *)
                  ( sum_type,
                    payload_env,
                    (payload_pat_type, payload_type) :: payload_constraints ))
          | sum_type -> (
              (* Nullary constructor *)
              match payload_pat_opt with
              | None -> (sum_type, [], [])
              | Some _ ->
                  (* Pattern has payload but constructor is nullary - type error *)
                  (* For now, just ignore the payload *)
                  (sum_type, [], []))))

and reduce_eq (c : type_equations) (_type_env : type_env) : type_equations =
  (* Optimized version: instead of substituting through all accumulated
     equations on every step, we just accumulate the equations and defer
     substitution. This changes O(n²) behavior to O(n). *)
  let rec reduce_eq_acc (acc : type_equations) (c : type_equations) :
      type_equations =
    match c with
    | [] -> List.rev acc
    | (t1, t2) :: c' -> (
        if t1 = t2 then reduce_eq_acc acc c'
        else
          match (t1, t2) with
          | TypeVar id, RecordType fields2 when not (inside t1 t2) ->
              (* Special handling for type variables unified with records *)
              let rec collect_record_constraints acc_records remaining =
                match remaining with
                | [] -> (List.rev acc_records, [])
                | (TypeVar id2, RecordType fields) :: rest when id = id2 ->
                    collect_record_constraints (fields :: acc_records) rest
                | other :: rest ->
                    let records, others =
                      collect_record_constraints acc_records rest
                    in
                    (records, other :: others)
              in
              let other_records, other_constraints =
                collect_record_constraints [ fields2 ] c'
              in
              let all_fields = List.flatten other_records in
              let rec merge_fields acc_fields extra_eqs = function
                | [] -> (List.rev acc_fields, extra_eqs)
                | (name, typ) :: rest -> (
                    match List.assoc_opt name acc_fields with
                    | Some existing_typ ->
                        let representative, other =
                          match (existing_typ, typ) with
                          | TypeVar _, _ -> (existing_typ, typ)
                          | _, TypeVar _ -> (typ, existing_typ)
                          | _ -> (existing_typ, typ)
                        in
                        let acc_fields_updated =
                          if representative = existing_typ then acc_fields
                          else
                            List.map
                              (fun (n, t) ->
                                if n = name then (n, representative) else (n, t))
                              acc_fields
                        in
                        merge_fields acc_fields_updated
                          ((representative, other) :: extra_eqs)
                          rest
                    | None ->
                        merge_fields ((name, typ) :: acc_fields) extra_eqs rest)
              in
              let unique_fields, field_equations =
                merge_fields [] [] all_fields
              in
              let merged_record = RecordType unique_fields in
              (* OPTIMIZATION: Only substitute in remaining constraints, not in
                 acc *)
              let new_remaining =
                field_equations @ substitute id merged_record other_constraints
              in
              reduce_eq_acc ((t1, merged_record) :: acc) new_remaining
          | TypeVar id, _ when not (inside t1 t2) ->
              (* OPTIMIZATION: Only substitute in remaining constraints, not in
                 acc. The accumulated equations will be processed by get_type
                 which follows chains. *)
              let new_remaining = substitute id t2 c' in
              reduce_eq_acc ((t1, t2) :: acc) new_remaining
          | _, TypeVar _ -> reduce_eq_acc acc ((t2, t1) :: c')
          | FunctionType (i1, o1), FunctionType (i2, o2) ->
              reduce_eq_acc acc ((i1, i2) :: (o1, o2) :: c')
          | CListType et1, CListType et2 -> reduce_eq_acc acc ((et1, et2) :: c')
          (* Native lists are [CListType elem]; HKT signatures use
             [TCtorApp(w,[elem])] for the class parameter (e.g. [impl Functor
             for [u]]). *)
          | TCtorApp (_, as1), CListType et2 | CListType et2, TCtorApp (_, as1)
            ->
              if List.length as1 = 1 then
                reduce_eq_acc acc ((List.hd as1, et2) :: c')
              else raise TypeFailure
          | CTypeApp (name1, args1), CTypeApp (name2, args2) ->
              if name1 = name2 && List.length args1 = List.length args2 then
                let arg_equations = List.combine args1 args2 in
                reduce_eq_acc acc (arg_equations @ c')
              else raise TypeFailure
          | TCtorApp (w1, as1), TCtorApp (w2, as2) ->
              if List.length as1 = List.length as2 then
                let arg_equations = List.combine as1 as2 in
                if w1 = w2 then reduce_eq_acc acc (arg_equations @ c')
                else if
                  is_solver_tctor_head_name w1 || is_solver_tctor_head_name w2
                then
                  reduce_eq_acc acc
                    (((TypeVar w1, TypeVar w2) :: arg_equations) @ c')
                else raise TypeFailure
              else raise TypeFailure
          | TCtorApp (w, as1), CTypeApp (n, as2)
          | CTypeApp (n, as2), TCtorApp (w, as1) ->
              if List.length as1 = List.length as2 then
                let arg_equations = List.combine as1 as2 in
                reduce_eq_acc acc
                  (((TypeVar w, TypeName n) :: arg_equations) @ c')
              else raise TypeFailure
          (* Recursive sum types like [type rec List<a> = ...] are μ-types
             (FixedPoint) in the environment, while class method signatures use
             TCtorApp for the type-class parameter applied to [a]. Unify the
             same way as [TCtorApp] vs [CTypeApp]: bind the head to the type
             name and relate the μ-body to a fully applied [CTypeApp]. *)
          | TCtorApp (w, as1), FixedPoint (name, body)
          | FixedPoint (name, body), TCtorApp (w, as1) ->
              reduce_eq_acc acc
                ((TypeVar w, TypeName name)
                :: (body, CTypeApp (name, as1))
                :: c')
          | VectorType types1, VectorType types2 -> (
              match (types1, types2) with
              | type1 :: tail1, type2 :: tail2 ->
                  reduce_eq_acc acc
                    ((type1, type2)
                    :: (VectorType tail1, VectorType tail2)
                    :: c')
              | _ -> raise TypeFailure)
          | FixedPoint (name1, body1), FixedPoint (name2, body2) ->
              if name1 = name2 then reduce_eq_acc acc ((body1, body2) :: c')
              else raise TypeFailure
          | FixedPoint (name, body), CTypeApp (app_name, args) ->
              if name = app_name then
                reduce_eq_acc acc ((body, CTypeApp (app_name, args)) :: c')
              else raise TypeFailure
          | CTypeApp (app_name, args), FixedPoint (name, body) ->
              if name = app_name then
                reduce_eq_acc acc ((CTypeApp (app_name, args), body) :: c')
              else raise TypeFailure
          | TypeName name1, CTypeApp (name2, _) ->
              if name1 = name2 then reduce_eq_acc acc c' else raise TypeFailure
          | CTypeApp (name1, _), TypeName name2 ->
              if name1 = name2 then reduce_eq_acc acc c' else raise TypeFailure
          | RecordType fields1, RecordType fields2 ->
              let rec unify_common_fields acc_fields remaining1 remaining2 =
                match remaining1 with
                | [] -> (List.rev acc_fields, [], remaining2)
                | (name1, type1) :: rest1 -> (
                    match List.assoc_opt name1 remaining2 with
                    | Some type2 ->
                        let remaining2' =
                          List.filter (fun (n, _) -> n <> name1) remaining2
                        in
                        unify_common_fields
                          ((type1, type2) :: acc_fields)
                          rest1 remaining2'
                    | None ->
                        let eqs, extra1, extra2 =
                          unify_common_fields acc_fields rest1 remaining2
                        in
                        (eqs, (name1, type1) :: extra1, extra2))
              in
              let common_eqs, _, _ = unify_common_fields [] fields1 fields2 in
              reduce_eq_acc acc (common_eqs @ c')
          | _ -> raise TypeFailure)
  in
  reduce_eq_acc [] c

(** [get_type var subs] applies a substitution to a type variable.

    Given a type variable and a list of type equations (substitutions), returns
    the type that the variable should be substituted with. If no substitution
    exists, returns the original variable.

    @param var The type variable to look up
    @param subs The list of type equations representing substitutions
    @return The type that the variable should be substituted with *)
and get_type (var : mono_type) (subs : type_equations) (type_env : type_env) :
    mono_type type_check_result =
  match var with
  | TypeVar v -> (
      let- looked_up_type = get_type_of_type_var v subs type_env in
      match looked_up_type with
      | FunctionType (i, o) ->
          let- i_type = get_type i subs type_env in
          let- o_type = get_type o subs type_env in
          return (FunctionType (i_type, o_type))
      | CListType et ->
          let- et_type = get_type et subs type_env in
          return (CListType et_type)
      (* Recursively applies the substitution [subs] to each element of the
         vector type [types], returning a new VectorType with all elements
         substituted. *)
      | VectorType types ->
          let rec aux acc = function
            | [] -> return (VectorType (List.rev acc))
            | t :: ts ->
                let- t_type = get_type t subs type_env in
                aux (t_type :: acc) ts
          in
          aux [] types
      | _ -> return looked_up_type)
  | FunctionType (i, o) ->
      let- i_type = get_type i subs type_env in
      let- o_type = get_type o subs type_env in
      return (FunctionType (i_type, o_type))
  | VectorType types ->
      (* Recursively applies the substitution [subs] to each element of the
         vector type [types], returning a new VectorType with all elements
         substituted. *)
      let rec aux acc = function
        | [] -> return (VectorType (List.rev acc))
        | t :: ts ->
            let- t_type = get_type t subs type_env in
            aux (t_type :: acc) ts
      in
      aux [] types
  | IntType -> return IntType
  | FloatType -> return FloatType
  | BoolType -> return BoolType
  | StringType -> return StringType
  | CharType -> return CharType
  | UnitType -> return UnitType
  | CListType et ->
      let- et_type = get_type et subs type_env in
      return (CListType et_type)
  | TypeName v -> return (TypeName v)
  | CTypeApp (name, args) ->
      (* Recursively apply substitution to all type arguments *)
      let rec aux acc = function
        | [] -> return (CTypeApp (name, List.rev acc))
        | arg :: rest ->
            let- arg_type = get_type arg subs type_env in
            aux (arg_type :: acc) rest
      in
      aux [] args
  | TCtorApp (w, args) -> (
      let- head_resolved = get_type_of_type_var w subs type_env in
      let rec aux acc = function
        | [] -> return (List.rev acc)
        | arg :: rest ->
            let- arg_type = get_type arg subs type_env in
            aux (arg_type :: acc) rest
      in
      let- resolved_args = aux [] args in
      match head_resolved with
      | TypeName n -> return (CTypeApp (n, resolved_args))
      | CListType _ -> (
          match resolved_args with
          | [ elem ] -> return (CListType elem)
          | _ ->
              Error
                (OtherError
                   "internal: [] expects exactly one type argument in this \
                    context"))
      | TypeVar w' -> return (TCtorApp (w', resolved_args))
      | _ ->
          Error
            (OtherError
               ("Cannot apply type arguments to "
               ^ string_of_mono_type head_resolved)))
  | FixedPoint (name, body) ->
      (* Apply substitution to the body of the fixed point *)
      let- body_type = get_type body subs type_env in
      return (FixedPoint (name, body_type))
  | RecordType fields ->
      (* Apply substitution to each field type *)
      let rec aux acc = function
        | [] -> return (RecordType (List.rev acc))
        | (field_name, field_type) :: rest ->
            let- new_field_type = get_type field_type subs type_env in
            aux ((field_name, new_field_type) :: acc) rest
      in
      aux [] fields

and get_type_of_type_var (var : string) (subs : type_equations)
    (type_env : type_env) : mono_type type_check_result =
  match List.assoc_opt (TypeVar var) subs with
  | Some looked_up -> (
      match looked_up with
      | TypeVar new_var -> get_type_of_type_var new_var subs type_env
      | _ ->
          get_type looked_up subs type_env (* Recursively apply substitutions *)
      )
  | None -> return (TypeVar var)

(** [inside inside_type outside_type] checks if a type appears inside another
    type.

    For example, [inside (TypeVar "a") (FunctionType (TypeVar "a", IntType))]
    returns true.

    @param inside_type The type to look for
    @param outside_type The type to search in
    @return
      true if inside_type appears anywhere in outside_type, false otherwise *)
and inside (inside_type : mono_type) (outside_type : mono_type) : bool =
  match outside_type with
  | _ when inside_type = outside_type -> true
  | FunctionType (i, o) -> inside inside_type i || inside inside_type o
  | VectorType ts -> List.exists (inside inside_type) ts
  | CListType t -> inside inside_type t
  | CTypeApp (_, args) -> List.exists (inside inside_type) args
  | TCtorApp (w, args) -> (
      match inside_type with
      | TypeVar id when id = w -> true
      | _ -> List.exists (inside inside_type) args)
  | FixedPoint (_, body) -> inside inside_type body
  | RecordType fields -> List.exists (fun (_, t) -> inside inside_type t) fields
  | _ -> false

(** [is_basic_type t] checks if a type is a basic type (int, bool, string,
    unit).

    @param t The type to check
    @return true if t is a basic type, false otherwise *)
and is_basic_type (t : mono_type) : bool =
  match t with
  | IntType | FloatType | BoolType | StringType | CharType | UnitType -> true
  | TypeVar _ -> false
  | FunctionType (i, o) -> is_basic_type i && is_basic_type o
  | VectorType types -> List.for_all is_basic_type types
  | CListType et -> is_basic_type et
  | TypeName _ -> false
  | CTypeApp _ -> false
  | TCtorApp _ -> false
  | FixedPoint (_, body) -> is_basic_type body
  | RecordType fields -> List.for_all (fun (_, t) -> is_basic_type t) fields

(** [substitute var_id t equations] substitutes a type variable with a type
    throughout a list of type equations.

    For each equation (t1, t2) in the input list: 1. Substitute var_id with t in
    t1 2. Substitute var_id with t in t2 3. Create new equation with substituted
    types 4. Recursively substitute through remaining equations

    @param var_id The ID of the type variable to substitute
    @param t The type to substitute in place of the type variable
    @param equations The list of type equations to perform substitution on
    @return
      A new list of type equations with all occurrences of TypeVar(var_id)
      replaced with type t *)
and substitute (var_id : string) (t : mono_type) (equations : type_equations) :
    type_equations =
  let rec substitute_in_type (type_subbing_in : mono_type) : mono_type =
    match type_subbing_in with
    | IntType -> IntType
    | FloatType -> FloatType
    | BoolType -> BoolType
    | StringType -> StringType
    | CharType -> CharType
    | UnitType -> UnitType
    | TypeVar id -> if id = var_id then t else TypeVar id
    | FunctionType (t1, t2) ->
        FunctionType (substitute_in_type t1, substitute_in_type t2)
    | VectorType types -> VectorType (List.map substitute_in_type types)
    | CListType et -> CListType (substitute_in_type et)
    | TypeName v -> TypeName v
    | CTypeApp (name, args) -> CTypeApp (name, List.map substitute_in_type args)
    | TCtorApp (w, args) when w = var_id -> (
        match t with
        | TypeName n -> CTypeApp (n, List.map substitute_in_type args)
        | CTypeApp (n, prefix_args) ->
            CTypeApp (n, prefix_args @ List.map substitute_in_type args)
        | CListType _ when List.length args = 1 ->
            CListType (substitute_in_type (List.hd args))
        | _ -> TCtorApp (w, List.map substitute_in_type args))
    | TCtorApp (w, args) -> TCtorApp (w, List.map substitute_in_type args)
    | FixedPoint (name, body) -> FixedPoint (name, substitute_in_type body)
    | RecordType fields ->
        RecordType
          (List.map (fun (name, t) -> (name, substitute_in_type t)) fields)
  in
  match equations with
  | [] -> []
  | (t1, t2) :: equations' ->
      (substitute_in_type t1, substitute_in_type t2)
      :: substitute var_id t equations'

(** [instantiate t] converts a polymorphic type to a monomorphic type.

    Given a polymorphic type (c_type), returns a monomorphic type by replacing
    all type variables with fresh type variables.

    For example, instantiating (∀x.x -> x) gives (t1 -> t1) where t1 is fresh.

    @param t The polymorphic type to instantiate
    @return A monomorphic type with fresh type variables *)
and instantiate (t : c_type) : mono_type =
  match t with
  | Mono t -> t (* if we have a monomorphic type, we can just return it*)
  | Constrained (_, inner) -> instantiate inner
  | PolyType _ ->
      (* if we have a polymorphic type, reduce by one layer, then call
         instantiate again *)
      let fresh_var = fresh_type_var () in
      let applied_once = apply_type t fresh_var in
      (* call instantiate again on applied_once *)
      instantiate applied_once

(** [instantiate_and_simplify t type_env] converts a polymorphic type to a
    monomorphic type and simplifies any type names (aliases) in it.

    This is useful when handling type annotations, as we want to resolve type
    aliases like "Integer" to their underlying types like "int".

    @param t The polymorphic type to instantiate and simplify
    @param type_env The type environment containing type alias definitions
    @return A simplified monomorphic type with type names resolved *)
and instantiate_and_simplify (t : c_type) (type_env : type_env) :
    mono_type type_check_result =
  let instantiated = instantiate t in
  simplify_mono_type instantiated type_env

(** [generalize constraints env t] converts a monomorphic type to a polymorphic
    type.

    Given a monomorphic type, its constraints, and the current environment,
    returns a polymorphic type by quantifying over all type variables that are
    not free in the environment.

    For example, if t is (t1 -> t2) and t1 is free in the environment but t2 is
    not, the result would be (∀t2.t1 -> t2).

    @param constraints The type equations that must be satisfied
    @param env The current static environment
    @param t The monomorphic type to generalize
    @return A polymorphic type with appropriate universal quantifiers *)
and generalize ?(class_preds : class_equations = [])
    (constraints : type_equations) (env : static_env) (type_env : type_env)
    (t : mono_type) : c_type type_check_result =
  (* First reduce the constraints to get a solution *)
  let solution = reduce_eq constraints type_env in

  (* Apply the solution to the type *)
  let- u1 = get_type t solution type_env in

  let- preds_solved =
    let rec aux acc = function
      | [] -> return (List.rev acc)
      | (c, ty) :: rest ->
          let- ty' = get_type ty solution type_env in
          aux ((c, ty') :: acc) rest
    in
    aux [] class_preds
  in
  let preds_with_remaining_tyvars =
    List.filter (fun (_, tau) -> get_type_vars tau <> []) preds_solved
    |> List.sort_uniq compare
  in
  let pred_tyvars_for_gen =
    List.concat_map
      (fun (_, tau) -> get_type_vars tau)
      preds_with_remaining_tyvars
  in
  (* Generalize tyvars in the result and in any still-ambiguous class predicates
     (e.g. element type of [[]] under Show). *)
  let type_vars = get_type_vars u1 @ pred_tyvars_for_gen in

  (* Get all types from the environment *)
  let env_types = List.map snd env in
  let env_types = List.map instantiate env_types in

  (* Apply the solution to environment types so we can see constrained type
     variables *)
  let- env_types_with_solution =
    let rec apply_solution_to_list acc = function
      | [] -> return (List.rev acc)
      | t :: rest ->
          let- t' = get_type t solution type_env in
          apply_solution_to_list (t' :: acc) rest
    in
    apply_solution_to_list [] env_types
  in

  let env_types = flatten_env_types env_types_with_solution in

  (* Get all type variables in the environment *)
  let env_vars = List.flatten (List.map get_type_vars env_types) in

  (* Filter out type variables that appear in the environment *)
  let free_vars_result =
    type_vars
    |> List.filter (fun t -> not (List.mem t env_vars))
    |> List.fold_left
         (fun acc t ->
           match acc with
           | Error _ as e -> e
           | Ok vars -> (
               match t with
               | TypeVar v -> Ok (v :: vars)
               | _ -> Error (OtherError "not a type var")))
         (Ok [])
  in

  let- free_vars = free_vars_result in
  let free_vars = List.sort_uniq compare free_vars in

  let- () =
    List.fold_left
      (fun acc (c, tau) ->
        match acc with
        | Error _ as e -> e
        | Ok () ->
            if get_type_vars tau <> [] then Ok ()
            else if
              forge_dict_resolves_for_class ~static_env:env ~class_name:c tau
            then Ok ()
            else
              Error
                (OtherError
                   ("No instance `" ^ c ^ "` for type "
                  ^ string_of_mono_type tau)))
      (Ok ()) preds_solved
  in
  (* Keep class preds that still mention type variables (see preds_solved
     above). *)
  let base =
    if preds_with_remaining_tyvars = [] then Mono u1
    else Constrained (preds_with_remaining_tyvars, Mono u1)
  in
  let res =
    List.fold_right (fun var acc -> PolyType (var, acc)) free_vars base
  in
  return res

and flatten_env_types (types : mono_type list) : mono_type list =
  match types with
  | [] -> []
  | t :: tail -> (
      match t with
      | FunctionType (i, o) -> flatten_env_types (i :: o :: tail)
      | VectorType types -> flatten_env_types (types @ tail)
      | CListType et -> flatten_env_types (et :: tail)
      | CTypeApp (_, args) | TCtorApp (_, args) ->
          flatten_env_types (args @ tail)
      | _ -> t :: flatten_env_types tail)

(** [get_type_vars t] extracts all type variables from a type.

    @param t The type to extract variables from
    @return A list of all type variables appearing in t *)
and get_type_vars (t : mono_type) : mono_type list =
  match t with
  | TypeVar _ -> [ t ]
  | FunctionType (i, o) -> get_type_vars i @ get_type_vars o
  | VectorType types -> List.flatten (List.map get_type_vars types)
  | CListType et -> get_type_vars et
  | CTypeApp (_, args) -> List.flatten (List.map get_type_vars args)
  | TCtorApp (w, args) ->
      TypeVar w :: List.flatten (List.map get_type_vars args)
  | RecordType fields ->
      List.flatten (List.map (fun (_, ft) -> get_type_vars ft) fields)
  | FixedPoint (_, body) -> get_type_vars body
  | _ -> []

and type_of_c_expr (env : static_env) (type_env : type_env) (e : c_expr) :
    c_type type_check_result =
  match e with
  | EId x -> (
      match List.assoc_opt x env with
      | None -> Error (UnboundVariable x)
      | Some sch ->
          let- sch' = simplify_type sch type_env in
          return (fix_c_type sch'))
  | _ ->
      pending_expression_class_preds := [];
      let- t, constraints, _ = generate env type_env e in
      let extra_preds = !pending_expression_class_preds in
      pending_expression_class_preds := [];

      let- t = simplify_mono_type t type_env in
      (* simplify constraints *)
      let- simplified_constraints =
        let rec simplify_constraint_list acc = function
          | [] -> return (List.rev acc)
          | (t1, t2) :: rest ->
              let- t1_simplified = simplify_mono_type t1 type_env in
              let- t2_simplified = simplify_mono_type t2 type_env in
              simplify_constraint_list
                ((t1_simplified, t2_simplified) :: acc)
                rest
        in
        simplify_constraint_list [] constraints
      in

      let solution = reduce_eq simplified_constraints type_env in
      let- the_mono_type = get_type t solution type_env in
      (* Do not [fix_type] before [generalize]: the solver's substitution still
         uses internal names (e.g. [t214]); renaming here breaks [get_type]
         inside [generalize]. *)
      let- the_c_type =
        generalize ~class_preds:extra_preds simplified_constraints env type_env
          the_mono_type
      in
      return (fix_c_type the_c_type)

(** Solved monomorphic type of [e1] (the function position) in the binary
    application [EApp (e1, e2)], after constraint solving. Used for
    monomorphization. *)
and mono_fun_type_of_binary_app (env : static_env) (type_env : type_env)
    (e1 : c_expr) (e2 : c_expr) : mono_type type_check_result =
  let- t1, c1, _ = generate env type_env e1 in
  let- t2, c2, _ = generate env type_env e2 in
  let result_type = fresh_type_var () in
  let app_constraint = (t1, FunctionType (t2, result_type)) in
  let constraints = app_constraint :: (c1 @ c2) in
  let- simplified_constraints =
    let rec simplify_constraint_list acc = function
      | [] -> return (List.rev acc)
      | (t1, t2) :: rest ->
          let- t1_simplified = simplify_mono_type t1 type_env in
          let- t2_simplified = simplify_mono_type t2 type_env in
          simplify_constraint_list ((t1_simplified, t2_simplified) :: acc) rest
    in
    simplify_constraint_list [] constraints
  in
  let solution =
    try Ok (reduce_eq simplified_constraints type_env)
    with TypeFailure ->
      Error
        (OtherError "mono_fun_type_of_binary_app: constraint solving failed")
  in
  let- solution = solution in
  let- the_mono_type = get_type t1 solution type_env in
  let the_mono_type = fix_type the_mono_type in
  return the_mono_type

(** Monomorphic type of top-level [f_name] after applying it left-to-right to
    [args] (same spine as the surface application). Used to monomorphize curried
    calls: a binary step alone can leave free type variables (e.g. [f 1] when
    [f : 'a -> 'b -> unit]); this solves the whole spine. *)
and mono_fun_type_of_curried_app (env : static_env) (type_env : type_env)
    (f_name : string) (args : c_expr list) : mono_type type_check_result =
  match List.assoc_opt f_name env with
  | None ->
      Error
        (OtherError ("mono_fun_type_of_curried_app: unbound `" ^ f_name ^ "`"))
  | Some ct ->
      (* Use the same freshening path as [generate_e_id] so monomorphization of
         curried class methods (e.g. [fmap g xs]) does not reuse stale solver
         metavariables from [instantiate] alone. *)
      let m0 = fst (instantiate_infer_scheme ct) in
      let rec simplify_constraint_list acc = function
        | [] -> return (List.rev acc)
        | (t1, t2) :: rest ->
            let- t1_simplified = simplify_mono_type t1 type_env in
            let- t2_simplified = simplify_mono_type t2 type_env in
            simplify_constraint_list
              ((t1_simplified, t2_simplified) :: acc)
              rest
      in
      let rec loop cur_ty constraints = function
        | [] ->
            let- simplified_constraints =
              simplify_constraint_list [] constraints
            in
            let solution =
              try Ok (reduce_eq simplified_constraints type_env)
              with TypeFailure ->
                Error
                  (OtherError
                     "mono_fun_type_of_curried_app: constraint solving failed")
            in
            let- solution = solution in
            let- the_mono_type = get_type m0 solution type_env in
            return (fix_type the_mono_type)
        | arg :: rest ->
            let- ta, ca, _ = generate env type_env arg in
            let- ta = simplify_mono_type ta type_env in
            let r = fresh_type_var () in
            let app_eq = (cur_ty, FunctionType (ta, r)) in
            loop r (app_eq :: (ca @ constraints)) rest
      in
      loop m0 [] args

and mono_type_fully_concrete (m : mono_type) : bool = get_type_vars m = []

(** Replace every [TypeVar] with [IntType] (deep). Used to build a native
    monomorph key when a call spine leaves unconstrained type variables;
    distinct logical instantiations may collapse — programs relying on that
    distinction should use explicit type annotations. *)
and mono_default_type_vars_to_int (m : mono_type) : mono_type =
  let rec go = function
    | TypeVar _ -> IntType
    | IntType -> IntType
    | BoolType -> BoolType
    | StringType -> StringType
    | UnitType -> UnitType
    | FloatType -> FloatType
    | CharType -> CharType
    | TypeName _ as t -> t
    | FunctionType (a, r) -> FunctionType (go a, go r)
    | VectorType ts -> VectorType (List.map go ts)
    | CListType e -> CListType (go e)
    | CTypeApp (n, args) -> CTypeApp (n, List.map go args)
    | TCtorApp (w, args) -> TCtorApp (w, List.map go args)
    | FixedPoint (n, b) -> FixedPoint (n, go b)
    | RecordType fields ->
        RecordType (List.map (fun (nm, t) -> (nm, go t)) fields)
  in
  go m

and mono_concrete_or_int_default (m : mono_type) : mono_type =
  if mono_type_fully_concrete m then m else mono_default_type_vars_to_int m

(* swap all variables for new variables *)
and swap_all_variables_in_type (t : mono_type) : mono_type type_check_result =
  (* First generalize the type to quantify over all variables *)
  let- generalized = generalize [] [] [] t in
  (* Then instantiate it to get fresh variables *)
  let instantiated = instantiate generalized in
  return instantiated

and generate_defn (env : static_env) (type_env : type_env) (defn : c_defn) :
    (static_env * type_env * constructor_env) type_check_result =
  let class_constraint_link_equations (inferred : class_equations)
      (explicit : class_equations) : type_equations =
    List.filter_map
      (fun (cls_explicit, ty_explicit) ->
        match
          List.find_opt
            (fun (cls_inferred, _) -> cls_inferred = cls_explicit)
            inferred
        with
        | Some (_, ty_inferred) -> Some (ty_inferred, ty_explicit)
        | None -> None)
      explicit
  in
  match defn with
  | CDefn
      ( pat,
        explicit_class_constraints,
        type_annotation,
        body,
        return_type,
        num_explicit_params ) ->
      (* Generate type and equations for the body *)
      let- body_type, body_equations, _ = generate env type_env body in
      let p_body = !pending_expression_class_preds in
      pending_expression_class_preds := [];
      let class_link_equations =
        class_constraint_link_equations p_body explicit_class_constraints
      in

      (* Get pattern type and bindings *)
      let pattern_type, pattern_env, pattern_equations =
        type_of_pat env type_env pat
      in

      (* Constraint: pattern type must match body type *)
      let pattern_body_constraint = (pattern_type, body_type) in

      (* Handle type annotation if present *)
      let- annotation_equations =
        match type_annotation with
        | Some t ->
            let- simplified_t = instantiate_and_simplify t type_env in
            return [ (pattern_type, simplified_t) ]
        | None -> return []
      in

      (* Handle return type annotation if present *)
      let- return_type_equations =
        match return_type with
        | Some t ->
            let- simplified_t = instantiate_and_simplify t type_env in
            (* Extract the return type by skipping the explicit parameter
               layers *)
            let rec extract_return_type n ty =
              match (n, ty) with
              | 0, _ -> ty
              | n, FunctionType (_, ret) when n > 0 ->
                  extract_return_type (n - 1) ret
              | _ -> ty
            in
            let actual_return_type =
              extract_return_type num_explicit_params body_type
            in
            return [ (actual_return_type, simplified_t) ]
        | None -> return []
      in

      (* Combine all equations *)
      let all_equations =
        body_equations @ pattern_equations @ annotation_equations
        @ return_type_equations @ class_link_equations
        @ [ pattern_body_constraint ]
      in

      (* Generalize the body type *)
      let- generalized_type =
        generalize
          ~class_preds:(p_body @ explicit_class_constraints)
          all_equations env type_env body_type
      in

      (* Create new environment with pattern bindings using bind_static *)
      let new_bindings =
        match bind_static pat generalized_type with
        | Some bindings ->
            (* Use pattern_env to ensure we have all the variables *)
            let var_names = List.map fst pattern_env in
            List.filter (fun (id, _) -> List.mem id var_names) bindings
        | None -> failwith "Pattern binding failed"
      in

      (* Return value bindings in static env and empty type env *)
      return (new_bindings, [], [])
  | CDefnRec
      ( pat,
        explicit_class_constraints,
        type_annotation,
        body,
        return_type,
        num_explicit_params ) ->
      (* For recursive definitions, we need to add the binding to the
         environment before type checking the body *)
      let pattern_type, pattern_env, pattern_equations =
        type_of_pat env type_env pat
      in

      (* Create a fresh type variable for the recursive binding *)
      let rec_type = fresh_type_var () in
      let rec_env = (fst (List.hd pattern_env), Mono rec_type) :: env in

      (* Generate type and equations for the body with the recursive binding *)
      let- body_type, body_equations, _ = generate rec_env type_env body in
      let p_body = !pending_expression_class_preds in
      pending_expression_class_preds := [];
      let class_link_equations =
        class_constraint_link_equations p_body explicit_class_constraints
      in

      (* Add constraint that the recursive type must match the body type *)
      let rec_constraint = (rec_type, body_type) in

      (* Constraint: pattern type must match body type *)
      let pattern_body_constraint = (pattern_type, body_type) in

      (* Handle type annotation if present *)
      let- annotation_equations =
        match type_annotation with
        | Some t ->
            let- simplified_t = instantiate_and_simplify t type_env in
            return [ (pattern_type, simplified_t) ]
        | None -> return []
      in

      (* Handle return type annotation if present *)
      let- return_type_equations =
        match return_type with
        | Some t ->
            let- simplified_t = instantiate_and_simplify t type_env in
            (* Extract the return type by skipping the explicit parameter
               layers *)
            let rec extract_return_type n ty =
              match (n, ty) with
              | 0, _ -> ty
              | n, FunctionType (_, ret) when n > 0 ->
                  extract_return_type (n - 1) ret
              | _ -> ty
            in
            let actual_return_type =
              extract_return_type num_explicit_params body_type
            in
            return [ (actual_return_type, simplified_t) ]
        | None -> return []
      in

      (* Combine all equations *)
      let all_equations =
        body_equations @ pattern_equations @ annotation_equations
        @ return_type_equations @ class_link_equations
        @ [ rec_constraint; pattern_body_constraint ]
      in

      (* Generalize the body type *)
      let- generalized_type =
        generalize
          ~class_preds:(p_body @ explicit_class_constraints)
          all_equations env type_env body_type
      in

      (* Create new environment with pattern bindings using bind_static *)
      let new_bindings =
        match bind_static pat generalized_type with
        | Some bindings ->
            (* Use pattern_env to ensure we have all the variables *)
            let var_names = List.map fst pattern_env in
            List.filter (fun (id, _) -> List.mem id var_names) bindings
        | None -> failwith "Pattern binding failed"
      in

      (* Return value bindings in static env and empty type env *)
      return (new_bindings, [], [])
  | CDefnMutRec defns ->
      (* For mutually recursive definitions, we need to: 1. Create fresh type
         variables for each definition 2. Add all of them to the environment 3.
         Typecheck all bodies in that environment 4. Generate constraints and
         solve them 5. Generalize the types *)

      (* Extract patterns and create fresh type variables for each *)
      let patterns_and_fresh_types =
        List.map
          (fun (pat, _, _, _, _, _) ->
            let pattern_type, pattern_env, pattern_equations =
              type_of_pat env type_env pat
            in
            let fresh_type = fresh_type_var () in
            (pat, pattern_type, pattern_env, pattern_equations, fresh_type))
          defns
      in

      (* Create the recursive environment with all names *)
      let rec_env =
        List.fold_left
          (fun acc_env (_, _, pattern_env, _, fresh_type) ->
            List.map (fun (id, _) -> (id, Mono fresh_type)) pattern_env
            @ acc_env)
          env patterns_and_fresh_types
      in

      (* Typecheck all bodies and collect equations *)
      let- all_body_results =
        let rec process_bodies acc_equations = function
          | [] -> return (List.rev acc_equations)
          | (_, _, _, body, _, _) :: rest ->
              let- body_type, body_equations, _ =
                generate rec_env type_env body
              in
              process_bodies ((body_type, body_equations) :: acc_equations) rest
        in
        process_bodies [] defns
      in

      (* Generate constraints for each definition *)
      let all_equations =
        List.fold_left2
          (fun acc (_, pattern_type, _, pattern_equations, fresh_type)
               (body_type, body_equations) ->
            (* Constraint: pattern type must match body type *)
            let pattern_body_constraint = (pattern_type, body_type) in
            (* Constraint: fresh type must match body type *)
            let rec_constraint = (fresh_type, body_type) in
            pattern_equations @ body_equations
            @ [ pattern_body_constraint; rec_constraint ]
            @ acc)
          [] patterns_and_fresh_types all_body_results
      in

      (* Handle type annotations if present *)
      let- annotation_equations =
        let rec process_annotations acc = function
          | [] -> return (List.rev acc)
          | (_, pattern_type, _, _, _) :: pats_rest -> (
              match defns with
              | (_, _, type_annotation, _, _, _) :: _ ->
                  let- annot_eqs =
                    match type_annotation with
                    | Some t ->
                        let- simplified_t =
                          instantiate_and_simplify t type_env
                        in
                        return [ (pattern_type, simplified_t) ]
                    | None -> return []
                  in
                  process_annotations (annot_eqs @ acc) pats_rest
              | [] -> return acc)
        in
        process_annotations [] patterns_and_fresh_types
      in

      let all_equations = all_equations @ annotation_equations in

      (* Generalize all the types *)
      let- generalized_types =
        let rec generalize_all acc = function
          | [] -> return (List.rev acc)
          | (body_type, _) :: rest ->
              let- gen_type = generalize all_equations env type_env body_type in
              generalize_all (gen_type :: acc) rest
        in
        generalize_all [] all_body_results
      in

      (* Create bindings for all definitions *)
      let all_bindings =
        List.fold_left2
          (fun acc (pat, _, _, _, _) gen_type ->
            match bind_static pat gen_type with
            | Some bindings -> bindings @ acc
            | None -> failwith "Pattern binding failed in mutual recursion")
          [] patterns_and_fresh_types generalized_types
      in

      return (all_bindings, [], [])
  | CClassDecl (_trait_name, params, methods) -> (
      match params with
      | [ p ] ->
          let w = forge_written_param p in
          let bindings =
            List.map
              (fun (mname, mty, dispatch_cls) ->
                ( mname,
                  PolyType
                    (w, Constrained ([ (dispatch_cls, TypeVar w) ], Mono mty))
                ))
              methods
          in
          return (bindings, [], [])
      | _ -> return ([], [], []))
  | CTypeAlias (name, params, body) ->
      (* Add the type alias to the type environment *)
      return ([], [ (name, params, body) ], [])
  | CSumType (type_name, type_params, constructors) ->
      (* Add the sum type to the type environment *)
      (* Represent sum types as CTypeApp with their type parameters *)
      let sum_type_body =
        CTypeApp (type_name, List.map (fun p -> TypeVar p) type_params)
      in
      let type_env_entry = [ (type_name, type_params, sum_type_body) ] in

      (* Create constructor bindings in static_env *)
      (* Each constructor is a function: payload_type -> SumType<params> *)
      (* For nullary constructors, they're just values of type SumType<params> *)
      let constructor_bindings =
        List.map
          (fun (cons_name, payload_type_opt) ->
            let sum_type_app =
              CTypeApp (type_name, List.map (fun p -> TypeVar p) type_params)
            in
            (* Helper function to wrap a type in PolyType quantifiers for each
               type parameter *)
            let rec make_poly_type_nullary params_left sum_type =
              match params_left with
              | [] -> Mono sum_type
              | param :: rest ->
                  PolyType (param, make_poly_type_nullary rest sum_type)
            in
            match payload_type_opt with
            | None ->
                (* Nullary constructor - wrap in PolyType if there are type
                   parameters *)
                (cons_name, make_poly_type_nullary type_params sum_type_app)
            | Some payload_type ->
                (* Constructor with payload - function type *)
                let payload_mono =
                  match payload_type with
                  | Mono m -> m
                  | PolyType _ | Constrained _ ->
                      failwith "Constructor payload cannot be polymorphic"
                in
                (* First, simplify the payload type to expand type aliases *)
                let payload_simplified =
                  match simplify_mono_type payload_mono type_env with
                  | Ok t -> t
                  | Error _ ->
                      payload_mono (* If simplification fails, use original *)
                in
                (* Convert TypeName references to type parameters into
                   TypeVar *)
                let rec convert_params_to_vars t =
                  match t with
                  | TypeName v when List.mem v type_params -> TypeVar v
                  | TypeVar v ->
                      (* Extract variable name from $written(x) format *)
                      let var_name =
                        if String.length v > 9 && String.sub v 0 9 = "$written("
                        then String.sub v 9 (String.length v - 10)
                        else v
                      in
                      if List.mem var_name type_params then TypeVar var_name
                      else TypeVar v
                  | FunctionType (t1, t2) ->
                      FunctionType
                        (convert_params_to_vars t1, convert_params_to_vars t2)
                  | VectorType ts ->
                      VectorType (List.map convert_params_to_vars ts)
                  | CListType t -> CListType (convert_params_to_vars t)
                  | CTypeApp (name, args) ->
                      CTypeApp (name, List.map convert_params_to_vars args)
                  | RecordType fields ->
                      RecordType
                        (List.map
                           (fun (n, t) -> (n, convert_params_to_vars t))
                           fields)
                  | _ -> t
                in
                let payload_with_vars =
                  convert_params_to_vars payload_simplified
                in
                (* Create polymorphic type: ∀params. payload ->
                   SumType<params> *)
                let rec make_poly_type params_left payload sum_type =
                  match params_left with
                  | [] -> Mono (FunctionType (payload, sum_type))
                  | param :: rest ->
                      PolyType (param, make_poly_type rest payload sum_type)
                in
                ( cons_name,
                  make_poly_type type_params payload_with_vars sum_type_app ))
          constructors
      in
      return
        ( constructor_bindings,
          type_env_entry,
          [ (type_name, type_params, constructors) ] )
  | CSumTypeRec (type_name, type_params, constructors) ->
      (* Recursive sum types use FixedPoint (μ) to represent the recursion *)
      (* For type rec List<a> = | Nil | Cons of a * List<a> *)
      (* We represent this as: ∀a. μList. (Nil | Cons of (a, List<a>)) *)

      (* Create a FixedPoint body for the type environment *)
      (* For recursive types, we use μtype_name. CTypeApp(type_name, params) *)
      let sum_type_app =
        CTypeApp (type_name, List.map (fun p -> TypeVar p) type_params)
      in
      let fixedpoint_body = FixedPoint (type_name, sum_type_app) in
      let type_env_entry = [ (type_name, type_params, fixedpoint_body) ] in
      (* Extend type environment with the current type so it can be referenced
         in payloads *)
      let extended_type_env = type_env_entry @ type_env in

      (* Create constructor bindings *)
      (* For recursive types, the type is μtype_name.body *)
      let constructor_bindings =
        List.map
          (fun (cons_name, payload_type_opt) ->
            (* The sum type application (e.g., List<a>) *)
            let sum_type_app =
              CTypeApp (type_name, List.map (fun p -> TypeVar p) type_params)
            in
            match payload_type_opt with
            | None ->
                (* Nullary constructor - just the sum type *)
                (* Wrap in polymorphic type if there are parameters *)
                let rec make_poly params_left =
                  match params_left with
                  | [] -> Mono sum_type_app
                  | param :: rest -> PolyType (param, make_poly rest)
                in
                (cons_name, make_poly type_params)
            | Some payload_type ->
                (* Constructor with payload *)
                let payload_mono =
                  match payload_type with
                  | Mono m -> m
                  | PolyType _ | Constrained _ ->
                      failwith "Constructor payload cannot be polymorphic"
                in
                (* First, simplify the payload type to expand type aliases *)
                (* Use extended_type_env so the recursive type can be referenced *)
                let payload_simplified =
                  match simplify_mono_type payload_mono extended_type_env with
                  | Ok t -> t
                  | Error _ ->
                      payload_mono (* If simplification fails, use original *)
                in
                (* Convert TypeName references to type parameters into TypeVar,
                   and references to the recursive type itself into the proper
                   type application *)
                let rec convert_params_to_vars t =
                  match t with
                  | TypeName v when v = type_name ->
                      (* Reference to the recursive type itself *)
                      sum_type_app
                  | TypeName v when List.mem v type_params -> TypeVar v
                  | TypeVar v ->
                      (* Extract variable name from $written(x) format *)
                      let var_name =
                        if String.length v > 9 && String.sub v 0 9 = "$written("
                        then String.sub v 9 (String.length v - 10)
                        else v
                      in
                      if List.mem var_name type_params then TypeVar var_name
                      else TypeVar v
                  | FunctionType (t1, t2) ->
                      FunctionType
                        (convert_params_to_vars t1, convert_params_to_vars t2)
                  | VectorType ts ->
                      VectorType (List.map convert_params_to_vars ts)
                  | CListType t -> CListType (convert_params_to_vars t)
                  | CTypeApp (name, args) ->
                      CTypeApp (name, List.map convert_params_to_vars args)
                  | FixedPoint (name, body) ->
                      FixedPoint (name, convert_params_to_vars body)
                  | RecordType fields ->
                      RecordType
                        (List.map
                           (fun (n, t) -> (n, convert_params_to_vars t))
                           fields)
                  | _ -> t
                in
                let payload_with_vars =
                  convert_params_to_vars payload_simplified
                in
                (* Create polymorphic type: ∀params. payload ->
                   SumType<params> *)
                let rec make_poly_type params_left payload sum_type =
                  match params_left with
                  | [] -> Mono (FunctionType (payload, sum_type))
                  | param :: rest ->
                      PolyType (param, make_poly_type rest payload sum_type)
                in
                ( cons_name,
                  make_poly_type type_params payload_with_vars sum_type_app ))
          constructors
      in
      return
        ( constructor_bindings,
          type_env_entry,
          [ (type_name, type_params, constructors) ] )
  | CSumTypeRecMutRec types ->
      (* Mutually recursive sum types - similar to CSumTypeRec but for multiple types *)
      (* For type rec Even = | Zero | SuccE of Odd and Odd = | SuccO of Even *)
      (* We represent each type as: μTypeName. CTypeApp(TypeName, params) *)

      (* Step 1: Create type environment entries for all types *)
      let type_env_entries =
        List.map
          (fun (type_name, type_params, _) ->
            let sum_type_app =
              CTypeApp (type_name, List.map (fun p -> TypeVar p) type_params)
            in
            let fixedpoint_body = FixedPoint (type_name, sum_type_app) in
            (type_name, type_params, fixedpoint_body))
          types
      in

      (* Step 2: Extend type environment with all types so they can reference
         each other *)
      let extended_type_env = type_env_entries @ type_env in

      (* Step 3: Create constructor bindings for all types *)
      let all_constructor_bindings =
        List.concat_map
          (fun (type_name, type_params, constructors) ->
            let sum_type_app =
              CTypeApp (type_name, List.map (fun p -> TypeVar p) type_params)
            in
            List.map
              (fun (cons_name, payload_type_opt) ->
                match payload_type_opt with
                | None ->
                    (* Nullary constructor *)
                    let rec make_poly params_left =
                      match params_left with
                      | [] -> Mono sum_type_app
                      | param :: rest -> PolyType (param, make_poly rest)
                    in
                    (cons_name, make_poly type_params)
                | Some payload_type ->
                    (* Constructor with payload *)
                    let payload_mono =
                      match payload_type with
                      | Mono m -> m
                      | PolyType _ | Constrained _ ->
                          failwith "Constructor payload cannot be polymorphic"
                    in
                    (* Simplify the payload type using extended environment *)
                    let payload_simplified =
                      match
                        simplify_mono_type payload_mono extended_type_env
                      with
                      | Ok t -> t
                      | Error _ -> payload_mono
                    in
                    (* Convert TypeName references to the proper types *)
                    (* This function needs to handle references to ANY of the mutually recursive types *)
                    let all_type_names =
                      List.map (fun (name, _, _) -> name) types
                    in
                    let rec convert_params_to_vars t =
                      match t with
                      | TypeName v when List.mem v all_type_names ->
                          (* Reference to one of the mutually recursive types *)
                          (* Find its type parameters *)
                          let _, found_params, _ =
                            List.find
                              (fun (name, _, _) -> name = v)
                              type_env_entries
                          in
                          CTypeApp
                            (v, List.map (fun p -> TypeVar p) found_params)
                      | TypeName v when List.mem v type_params -> TypeVar v
                      | TypeVar v ->
                          (* Extract variable name from $written(x) format *)
                          let var_name =
                            if
                              String.length v > 9
                              && String.sub v 0 9 = "$written("
                            then String.sub v 9 (String.length v - 10)
                            else v
                          in
                          if List.mem var_name type_params then TypeVar var_name
                          else TypeVar v
                      | FunctionType (t1, t2) ->
                          FunctionType
                            ( convert_params_to_vars t1,
                              convert_params_to_vars t2 )
                      | VectorType ts ->
                          VectorType (List.map convert_params_to_vars ts)
                      | CListType t -> CListType (convert_params_to_vars t)
                      | CTypeApp (name, args) ->
                          CTypeApp (name, List.map convert_params_to_vars args)
                      | FixedPoint (name, body) ->
                          FixedPoint (name, convert_params_to_vars body)
                      | RecordType fields ->
                          RecordType
                            (List.map
                               (fun (n, t) -> (n, convert_params_to_vars t))
                               fields)
                      | _ -> t
                    in
                    let payload_with_vars =
                      convert_params_to_vars payload_simplified
                    in
                    (* Create polymorphic type *)
                    let rec make_poly_type params_left payload sum_type =
                      match params_left with
                      | [] -> Mono (FunctionType (payload, sum_type))
                      | param :: rest ->
                          PolyType (param, make_poly_type rest payload sum_type)
                    in
                    ( cons_name,
                      make_poly_type type_params payload_with_vars sum_type_app
                    ))
              constructors)
          types
      in
      return (all_constructor_bindings, type_env_entries, types)

(* Given a type with type names, simplify it by replacing the type names with
   the actual types

   For example, if we declare type t = int, and we have type t -> t, we it will
   get simplified to int -> int *)
and simplify_type (t : c_type) (type_env : type_env) : c_type type_check_result
    =
  match t with
  | Mono t ->
      let- mono_t = simplify_mono_type t type_env in
      return (Mono mono_t)
  | PolyType (var, t) ->
      let- t_simplified = simplify_type t type_env in
      return (PolyType (var, t_simplified))
  | Constrained (ps, body) ->
      let- ps' =
        let rec aux acc = function
          | [] -> return (List.rev acc)
          | (c, ty) :: rest ->
              let- ty' = simplify_mono_type ty type_env in
              aux ((c, ty') :: acc) rest
        in
        aux [] ps
      in
      let- body' = simplify_type body type_env in
      return (Constrained (ps', body'))

and simplify_mono_type (t : mono_type) (type_env : type_env) :
    mono_type type_check_result =
  match t with
  | IntType -> return IntType
  | FloatType -> return FloatType
  | BoolType -> return BoolType
  | StringType -> return StringType
  | CharType -> return CharType
  | UnitType -> return UnitType
  | TypeVar v -> return (TypeVar v)
  | FunctionType (t1, t2) ->
      (* Evaluate both input and output types *)
      let- t1_simplified = simplify_mono_type t1 type_env in
      let- t2_simplified = simplify_mono_type t2 type_env in
      return (FunctionType (t1_simplified, t2_simplified))
  | VectorType types ->
      (* Evaluate each type in the vector *)
      let rec aux acc = function
        | [] -> return (VectorType (List.rev acc))
        | t :: ts ->
            let- t_simplified = simplify_mono_type t type_env in
            aux (t_simplified :: acc) ts
      in
      aux [] types
  | CListType t ->
      (* Evaluate the element type *)
      let- t_simplified = simplify_mono_type t type_env in
      return (CListType t_simplified)
  | TypeName v -> (
      (* Look up and evaluate the type definition *)
      match List.find_opt (fun (n, _, _) -> n = v) type_env with
      | Some type_def -> (
          let _, _, t = type_def in
          (* Check if this is a recursive sum type (FixedPoint) *)
          match t with
          | FixedPoint (name, body) when name = v -> (
              (* For recursive sum types in annotations, return the CTypeApp
                 directly rather than the FixedPoint wrapper, as the FixedPoint
                 is just for internal representation. This allows proper
                 unification with constructor types which use CTypeApp. *)
              match body with
              | CTypeApp (app_name, _) when app_name = v ->
                  (* Return CTypeApp with no args for nullary recursive types *)
                  return (CTypeApp (v, []))
              | _ -> simplify_mono_type body type_env)
          | _ -> simplify_mono_type t type_env)
      | None -> (
          (* Surface primitive names (not in [type_env]) match lexer
             keywords. *)
          match v with
          | "String" | "string" -> return StringType
          | "Int" | "int" -> return IntType
          | "Bool" | "bool" -> return BoolType
          | "Float" | "float" -> return FloatType
          | "Char" | "char" -> return CharType
          | "Unit" | "unit" -> return UnitType
          | _ ->
              (* If not found, treat it as a type variable (could be a type
                 parameter) *)
              return (TypeVar v)))
  | TCtorApp (w, args) ->
      let rec eval_args acc = function
        | [] -> return (List.rev acc)
        | arg :: rest ->
            let- arg_simplified = simplify_mono_type arg type_env in
            eval_args (arg_simplified :: acc) rest
      in
      let- simplified_args = eval_args [] args in
      return (TCtorApp (w, simplified_args))
  | CTypeApp (name, args) -> (
      (* First evaluate all the argument types *)
      let rec eval_args acc = function
        | [] -> return (List.rev acc)
        | arg :: rest ->
            let- arg_simplified = simplify_mono_type arg type_env in
            eval_args (arg_simplified :: acc) rest
      in
      let- simplified_args = eval_args [] args in

      (* Look up the type definition *)
      match List.find_opt (fun (n, _, _) -> n = name) type_env with
      | Some type_def ->
          let _, params, body = type_def in
          if List.length params <> List.length simplified_args then
            Error
              (OtherError
                 ("Type " ^ name ^ " expects "
                 ^ string_of_int (List.length params)
                 ^ " arguments, got "
                 ^ string_of_int (List.length simplified_args)))
          else
            (* Check if this is a sum type (body is CTypeApp with same name) or
               a recursive sum type (FixedPoint) *)
            let is_sum_type, is_fixedpoint =
              match body with
              | CTypeApp (body_name, _) when body_name = name -> (true, false)
              | FixedPoint _ -> (true, true)
              | _ -> (false, false)
            in

            (* For sum types, don't simplify - just return the CTypeApp with
             simplified args *)
            (* For recursive sum types (FixedPoint), return the FixedPoint with
             simplified params substituted *)
            if is_sum_type then
              if is_fixedpoint then
                (* Substitute the type parameters in the FixedPoint body *)
                match body with
                | FixedPoint (fp_name, fp_body) ->
                    let subst = List.combine params simplified_args in
                    let rec apply_subst t =
                      match t with
                      | TypeVar v -> (
                          match List.assoc_opt v subst with
                          | Some arg -> arg
                          | None -> t)
                      | FunctionType (i, o) ->
                          FunctionType (apply_subst i, apply_subst o)
                      | VectorType types ->
                          VectorType (List.map apply_subst types)
                      | CListType et -> CListType (apply_subst et)
                      | CTypeApp (n, args) ->
                          CTypeApp (n, List.map apply_subst args)
                      | TCtorApp (w, args) ->
                          TCtorApp (w, List.map apply_subst args)
                      | FixedPoint (n, b) -> FixedPoint (n, apply_subst b)
                      | RecordType fields ->
                          RecordType
                            (List.map
                               (fun (name, t) -> (name, apply_subst t))
                               fields)
                      | _ -> t
                    in
                    return (FixedPoint (fp_name, apply_subst fp_body))
                | _ -> return (CTypeApp (name, simplified_args))
              else return (CTypeApp (name, simplified_args))
            else
              (* Create substitution mapping type parameters to their evaluated
                 arguments *)
              let subst = List.combine params simplified_args in

              (* Apply the substitution to the body type *)
              let rec apply_subst t =
                match t with
                | TypeVar v -> (
                    (* Extract the variable name from $written(v) format *)
                    let var_name =
                      if String.length v > 9 && String.sub v 0 9 = "$written("
                      then String.sub v 9 (String.length v - 10)
                      else v
                    in
                    match List.assoc_opt var_name subst with
                    | Some arg -> arg
                    | None -> t)
                | TypeName v -> (
                    (* Check if this type name is actually a type parameter *)
                    match List.assoc_opt v subst with
                    | Some arg -> arg
                    | None -> t)
                | FunctionType (i, o) ->
                    FunctionType (apply_subst i, apply_subst o)
                | VectorType types -> VectorType (List.map apply_subst types)
                | CListType et -> CListType (apply_subst et)
                | CTypeApp (n, args) -> CTypeApp (n, List.map apply_subst args)
                | TCtorApp (w, args) -> TCtorApp (w, List.map apply_subst args)
                | FixedPoint (name, body) -> FixedPoint (name, apply_subst body)
                | RecordType fields ->
                    RecordType
                      (List.map (fun (name, t) -> (name, apply_subst t)) fields)
                | _ -> t
              in

              (* Apply substitution and recursively evaluate the result *)
              let substituted = apply_subst body in
              simplify_mono_type substituted type_env
      | None -> Error (OtherError ("Unknown type constructor: " ^ name)))
  | FixedPoint (name, body) ->
      (* Simplify the body of the fixed point *)
      let- body_simplified = simplify_mono_type body type_env in
      return (FixedPoint (name, body_simplified))
  | RecordType fields ->
      (* Simplify each field type *)
      let rec aux acc = function
        | [] -> return (RecordType (List.rev acc))
        | (field_name, field_type) :: rest ->
            let- simplified_field_type =
              simplify_mono_type field_type type_env
            in
            aux ((field_name, simplified_field_type) :: acc) rest
      in
      aux [] fields

(** Inferred type of [e1] in [let rec id = e1 in ...], using [env] as the
    surrounding static environment (for lowering after whole-program typecheck).
*)
let type_rec_binding_rhs (env : static_env) (type_env : type_env) (id : string)
    (e1 : c_expr) : c_type type_check_result =
  let function_type = fresh_type_var () in
  let new_env = (id, Mono function_type) :: env in
  let- t1, c1, _ = generate new_env type_env e1 in
  let new_constraint = (function_type, t1) in
  generalize (new_constraint :: c1) env type_env t1

let rec get_mono_type (t : c_type) : mono_type =
  match t with
  | Mono t -> t
  | PolyType (_, t) | Constrained (_, t) -> get_mono_type t

let rec primary_class_constraint (t : c_type) : string option =
  match t with
  | Constrained ((c, _) :: _, _) -> Some c
  | PolyType (_, inner) -> primary_class_constraint inner
  | _ -> None

let rec extract_class_param_and_mono_template (class_name : string) (t : c_type)
    : (string * mono_type) option =
  match t with
  | PolyType (_v, inner) ->
      extract_class_param_and_mono_template class_name inner
  | Constrained (preds, inner) -> (
      let pred_match =
        List.find_opt
          (fun (c, ty) ->
            c = class_name
            &&
            match ty with
            | TypeVar _ -> true
            | _ -> false)
          preds
      in
      match (pred_match, inner) with
      | Some (_c, TypeVar v), Mono mty -> Some (v, mty)
      | _ -> None)
  | _ -> None

let instantiate_method_type_for_class ~(static_env : static_env)
    ~(class_name : string) ~(method_name : string) ~(tau : mono_type)
    ~(type_env : type_env) : mono_type option =
  match List.assoc_opt method_name static_env with
  | None -> None
  | Some sch -> (
      match extract_class_param_and_mono_template class_name sch with
      | None -> None
      | Some (var_id, mty) -> (
          let arity = Type_arity.ctor_arity_for_written_var var_id mty in
          let specialized =
            Type_arity.substitute_instance_in_mono ~written_var:var_id ~inst:tau
              arity mty
          in
          match simplify_mono_type specialized type_env with
          | Ok t -> Some t
          | Error _ -> Some specialized))

let dict_expected_record_type ~(static_env : static_env) ~(class_name : string)
    ~(tau : mono_type) ~(type_env : type_env) : mono_type option =
  let method_fields =
    List.filter_map
      (fun (name, sch) ->
        match primary_class_constraint sch with
        | Some c when c = class_name && scheme_has_class_constraint sch -> (
            match
              instantiate_method_type_for_class ~static_env ~class_name
                ~method_name:name ~tau ~type_env
            with
            | Some t -> Some (name, t)
            | None -> None)
        | _ -> None)
      static_env
  in
  if method_fields = [] then None
  else
    let fields = List.sort (fun (a, _) (b, _) -> compare a b) method_fields in
    Some (RecordType fields)

(** Substitute occurrences of class method names with dictionary field
    accesses.  Used to elaborate the body of a constrained non-method function
    so that [show x] becomes [__dict.show x]. *)
let subst_methods_with_dict_access ~(dict_param : string)
    ~(method_names : string list) (expr : c_expr) : c_expr =
  let rec go (e : c_expr) : c_expr =
    match e with
    | EId name when List.mem name method_names ->
        EFieldAccess (EId dict_param, name)
    | EApp (e1, e2) -> EApp (go e1, go e2)
    | EFunction (p, a, b) -> EFunction (p, a, go b)
    | EBind (p, a, e1, e2, r) -> EBind (p, a, go e1, go e2, r)
    | EBindRec (p, a, e1, e2, r) -> EBindRec (p, a, go e1, go e2, r)
    | EBindMutRec (bs, body) ->
        EBindMutRec
          ( List.map (fun (p, a, e, r, n) -> (p, a, go e, r, n)) bs,
            go body )
    | EBlock parts ->
        EBlock
          (List.map
             (function
               | Expr e -> Expr (go e)
               | Defn d -> (
                   match d with
                   | CDefn (p, cs, a, b, r, n) ->
                       Defn (CDefn (p, cs, a, go b, r, n))
                   | CDefnRec (p, cs, a, b, r, n) ->
                       Defn (CDefnRec (p, cs, a, go b, r, n))
                   | other -> Defn other))
             parts)
    | ETernary (e1, e2, e3) -> ETernary (go e1, go e2, go e3)
    | ESwitch (e, br) ->
        ESwitch (go e, List.map (fun (p, ee) -> (p, go ee)) br)
    | EVector es -> EVector (List.map go es)
    | EListEnumeration (e1, e2) -> EListEnumeration (go e1, go e2)
    | EListComprehension (e, gs) ->
        EListComprehension (go e, List.map (fun (p, ge) -> (p, go ge)) gs)
    | EBop (o, e1, e2) -> EBop (o, go e1, go e2)
    | ERecordLit fs -> ERecordLit (List.map (fun (n, ee) -> (n, go ee)) fs)
    | ERecordUpdate (e, fs) ->
        ERecordUpdate (go e, List.map (fun (n, ee) -> (n, go ee)) fs)
    | EFieldAccess (e, fld) -> EFieldAccess (go e, fld)
    | _ -> e
  in
  go expr

(** Rewrite [Class.method e] to record dispatch after whole-program typecheck.
*)
let rec elaborate_expr (static_env : static_env) (type_env : type_env) :
    c_expr -> c_expr =
  (* When rewriting overloaded identifiers (e.g. [mappend]) into dictionary
     dispatch, we must respect lexical shadowing: if an overloaded name is
     locally bound (e.g. by [let rec mappend = ...] inside an [impl]),
     occurrences of that name should refer to the local binding, not to the
     class method. *)
  let rec pat_bound_simple (p : c_pat) : string list =
    match p with
    | CIdPat id -> [ id ]
    | CConsPat (a, b) -> pat_bound_simple a @ pat_bound_simple b
    | CVectorPat ps -> List.concat (List.map pat_bound_simple ps)
    | CRecordPat fs ->
        List.concat (List.map (fun (_, p) -> pat_bound_simple p) fs)
    | CVariantPat (_, Some p) -> pat_bound_simple p
    | CVariantPat (_, None) -> []
    | CWildcardPat | CUnitPat | CNilPat -> []
    | CIntPat _ | CBoolPat _ | CStringPat _ | CCharPat _ -> []
  in
  let add_all shadowed new_names =
    List.fold_left
      (fun acc n -> if List.mem n acc then acc else n :: acc)
      shadowed new_names
  in
  let rec peel_apps acc e =
    match e with
    | EApp (a, b) -> peel_apps (b :: acc) a
    | e -> (e, acc)
  in
  let arg_at_index idx xs =
    let rec go i = function
      | [] -> None
      | y :: ys -> if i = 0 then Some y else go (i - 1) ys
    in
    go idx xs
  in
  let try_dict_dispatch shadowed e1_full e2_last =
    let head, prefix = peel_apps [] e1_full in
    match head with
    | EId f when not (List.mem f shadowed) -> (
        match List.assoc_opt f static_env with
        | Some sch when scheme_has_class_constraint sch -> (
            match primary_class_constraint sch with
            | None -> None
            | Some cls -> (
                match extract_class_param_and_mono_template cls sch with
                | None -> None
                | Some (var_id, mty) -> (
                    let idx = Type_arity.dict_resolution_arg_index mty var_id in
                    let all_args = prefix @ [ e2_last ] in
                    if List.length all_args <= idx then None
                    else
                      match arg_at_index idx all_args with
                      | None -> None
                      | Some tau_e -> (
                          match type_of_c_expr static_env type_env tau_e with
                          | Ok arg_ct -> (
                              let tau = get_mono_type arg_ct in
                              match
                                find_compatible_dict_name ~static_env
                                  ~class_name:cls ~method_name:f ~tau
                              with
                              | Some dict ->
                                  Some
                                    (List.fold_left
                                       (fun acc arg -> EApp (acc, arg))
                                       (EFieldAccess (EId dict, f))
                                       all_args)
                              | None ->
                                  if Hashtbl.mem dict_wrapped_fns f then
                                    match
                                      find_dict_for_class ~static_env
                                        ~class_name:cls ~tau
                                    with
                                    | Some dict ->
                                        Some
                                          (List.fold_left
                                             (fun acc arg -> EApp (acc, arg))
                                             (EApp (EId f, EId dict))
                                             all_args)
                                    | None -> None
                                  else None)
                          | Error _ -> None))))
        | _ -> None)
    | _ -> None
  in
  let rec aux shadowed (e : c_expr) : c_expr =
    match e with
    | EApp (e1, e2) -> (
        let e2' = aux shadowed e2 in
        let e1' = aux shadowed e1 in
        match try_dict_dispatch shadowed e1' e2' with
        | Some e -> e
        | None -> EApp (e1', e2'))
    | EFunction (p, a, b) ->
        let shadowed' = add_all shadowed (pat_bound_simple p) in
        EFunction (p, a, aux shadowed' b)
    | EBind (p, a, e1, e2, r) ->
        let bound = pat_bound_simple p in
        (* In [let x = e1 in e2], [x] is only in scope for [e2]. *)
        EBind (p, a, aux shadowed e1, aux (add_all shadowed bound) e2, r)
    | EBindRec (p, a, e1, e2, r) ->
        let shadowed' = add_all shadowed (pat_bound_simple p) in
        (* In [let rec x = e1 in e2], [x] is in scope for [e1] and [e2]. *)
        EBindRec (p, a, aux shadowed' e1, aux shadowed' e2, r)
    | EBindMutRec (bs, body) ->
        let bound_names =
          List.concat (List.map (fun (p, _, _, _, _) -> pat_bound_simple p) bs)
        in
        let shadowed' = add_all shadowed bound_names in
        EBindMutRec
          ( List.map (fun (p, a, e, r, n) -> (p, a, aux shadowed' e, r, n)) bs,
            aux shadowed' body )
    | EBlock parts ->
        EBlock
          (List.map
             (function
               | Expr e -> Expr (aux shadowed e)
               | Defn d -> Defn (elaborate_defn static_env type_env d))
             parts)
    | ETernary (e1, e2, e3) ->
        ETernary (aux shadowed e1, aux shadowed e2, aux shadowed e3)
    | ESwitch (e, br) ->
        ESwitch
          ( aux shadowed e,
            List.map
              (fun (p, ee) ->
                let shadowed' = add_all shadowed (pat_bound_simple p) in
                (p, aux shadowed' ee))
              br )
    | EVector es -> EVector (List.map (aux shadowed) es)
    | EListEnumeration (e1, e2) ->
        EListEnumeration (aux shadowed e1, aux shadowed e2)
    | EListComprehension (e, gs) ->
        (* Generator patterns bind names only for later generator expressions
           and the comprehension body; this is more involved than we need for
           the [impl] case, so we don't attempt to shadow through generators. *)
        EListComprehension
          (aux shadowed e, List.map (fun (p, ge) -> (p, aux shadowed ge)) gs)
    | EBop (o, e1, e2) -> EBop (o, aux shadowed e1, aux shadowed e2)
    | ERecordLit fs ->
        ERecordLit (List.map (fun (n, ee) -> (n, aux shadowed ee)) fs)
    | ERecordUpdate (e, fs) ->
        ERecordUpdate
          (aux shadowed e, List.map (fun (n, ee) -> (n, aux shadowed ee)) fs)
    | EFieldAccess (e, fld) -> EFieldAccess (aux shadowed e, fld)
    | (EInt _ | EFloat _ | EBool _ | EString _ | EChar _ | EUnit | ENil | EId _)
      as lit -> lit
  in
  aux []

and dict_wrapped_fns : (string, bool) Hashtbl.t = Hashtbl.create 16

and elaborate_constrained_body (static_env : static_env)
    (type_env : type_env) (name : string) (body : c_expr) : c_expr =
  match List.assoc_opt name static_env with
  | Some sch when scheme_has_class_constraint sch -> (
      match primary_class_constraint sch with
      | Some cls ->
          let dict_param = "__dict_" ^ cls in
          let method_names = class_method_names ~static_env ~class_name:cls in
          let body' = elaborate_expr static_env type_env body in
          let body'' =
            subst_methods_with_dict_access ~dict_param ~method_names body'
          in
          if body' = body'' then body'
          else (
            Hashtbl.replace dict_wrapped_fns name true;
            EFunction (CIdPat dict_param, None, body''))
      | None -> elaborate_expr static_env type_env body)
  | _ -> elaborate_expr static_env type_env body

and elaborate_defn (static_env : static_env) (type_env : type_env) d : c_defn =
  match d with
  | CDefn (CIdPat name as pat, cs, a, body, r, n) ->
      CDefn (pat, cs, a, elaborate_constrained_body static_env type_env name body, r, n)
  | CDefn (pat, cs, a, body, r, n) ->
      CDefn (pat, cs, a, elaborate_expr static_env type_env body, r, n)
  | CDefnRec (CIdPat name as pat, cs, a, body, r, n) ->
      CDefnRec (pat, cs, a, elaborate_constrained_body static_env type_env name body, r, n)
  | CDefnRec (pat, cs, a, body, r, n) ->
      CDefnRec (pat, cs, a, elaborate_expr static_env type_env body, r, n)
  | CDefnMutRec defs ->
      CDefnMutRec
        (List.map
           (fun (p, cs, a, body, r, n) ->
             (p, cs, a, elaborate_expr static_env type_env body, r, n))
           defs)
  | d -> d
