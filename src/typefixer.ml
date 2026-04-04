open Cexpr

(** Source names use [$written(name)] in the solver. *)
let is_forge_written_var (v : string) : bool =
  String.length v > 9 && String.sub v 0 9 = "$written("
  && String.ends_with ~suffix:")" v

(** Type variables that stand for the class dictionary slot (e.g. [f] in
    [Functor f => ...]) should keep their source name; other [$written(...)]
    vars are still canonicalized to [a], [b], ... *)
let rec dict_predicate_written_vars (acc : string list) (t : c_type) :
    string list =
  match t with
  | Constrained (ps, inner) ->
      let acc' =
        List.fold_left
          (fun acc (_cls, m) ->
            match m with
            | TypeVar v when is_forge_written_var v ->
                if List.mem v acc then acc else v :: acc
            | _ -> acc)
          acc ps
      in
      dict_predicate_written_vars acc' inner
  | PolyType (_, inner) -> dict_predicate_written_vars acc inner
  | Mono _ -> acc

let tv = ref 0

let number_to_letter n =
  let rec aux n acc =
    if n <= 0 then acc
    else aux ((n - 1) / 26) (Char.chr (97 + ((n - 1) mod 26)) :: acc)
  in
  String.of_seq (List.to_seq (aux n []))

let new_fresh_type_var () : mono_type =
  tv := !tv + 1;
  TypeVar (number_to_letter !tv)

(* given a type, creates a mapping from type variables to type variables,
   represented as an association list *)
let rec create_substitution (t : mono_type) (seen : string list) :
    (string * string) list =
  match t with
  | TypeVar v ->
      if List.mem v seen then []
      else (
        tv := !tv + 1;
        [ (v, number_to_letter !tv) ])
  | FunctionType (t1, t2) ->
      let subs1 = create_substitution t1 seen in
      let seen' = seen @ List.map fst subs1 in
      subs1 @ create_substitution t2 seen'
  | VectorType types ->
      List.fold_left
        (fun (acc, seen) t ->
          let subs = create_substitution t seen in
          (acc @ subs, seen @ List.map fst subs))
        ([], seen) types
      |> fst
  | CListType t -> create_substitution t seen
  | IntType -> []
  | FloatType -> []
  | BoolType -> []
  | StringType -> []
  | CharType -> []
  | UnitType -> []
  | TypeName _ -> []
  | CTypeApp (_, args) | TCtorApp (_, args) ->
      List.fold_left
        (fun (acc, seen) t ->
          let subs = create_substitution t seen in
          (acc @ subs, seen @ List.map fst subs))
        ([], seen) args
      |> fst
  | FixedPoint (_, body) -> create_substitution body seen
  | RecordType fields ->
      List.fold_left
        (fun (acc, seen) (_, t) ->
          let subs = create_substitution t seen in
          (acc @ subs, seen @ List.map fst subs))
        ([], seen) fields
      |> fst

(* There may be type variables in t. We need to replace them with variables 1,
   2, ....

   They should be replaced in the order they appear in t. *)
let fix_type (t : mono_type) : mono_type =
  tv := 0;
  (* Reset counter at start *)
  let rec apply_substitution (t : mono_type) (subs : (string * string) list) :
      mono_type =
    match t with
    | TypeVar v -> ( try TypeVar (List.assoc v subs) with Not_found -> t)
    | FunctionType (t1, t2) ->
        FunctionType (apply_substitution t1 subs, apply_substitution t2 subs)
    | VectorType types ->
        VectorType (List.map (fun t -> apply_substitution t subs) types)
    | CListType t -> CListType (apply_substitution t subs)
    | IntType -> IntType
    | FloatType -> FloatType
    | BoolType -> BoolType
    | StringType -> StringType
    | CharType -> CharType
    | UnitType -> UnitType
    | TypeName v -> TypeName v
    | CTypeApp (name, args) ->
        CTypeApp (name, List.map (fun t -> apply_substitution t subs) args)
    | TCtorApp (w, args) ->
        TCtorApp (w, List.map (fun t -> apply_substitution t subs) args)
    | FixedPoint (name, body) ->
        FixedPoint (name, apply_substitution body subs)
    | RecordType fields ->
        RecordType (List.map (fun (name, t) -> (name, apply_substitution t subs)) fields)
  in
  let subs = create_substitution t [] in
  apply_substitution t subs

(** Rename type variables throughout a [c_type] (quantifiers, preds, mono) in
    one pass so names stay consistent — e.g. after [generalize]. *)
let fix_c_type (ct : c_type) : c_type =
  let dict_keep = dict_predicate_written_vars [] ct in
  let names : string list ref = ref [] in
  let add (v : string) =
    if (not (List.mem v !names)) && not (List.mem v dict_keep) then
      names := !names @ [ v ]
  in
  let rec walk_mono (t : mono_type) : unit =
    match t with
    | TypeVar v -> add v
    | FunctionType (t1, t2) ->
        walk_mono t1;
        walk_mono t2
    | VectorType ts -> List.iter walk_mono ts
    | CListType e -> walk_mono e
    | CTypeApp (_, args) | TCtorApp (_, args) -> List.iter walk_mono args
    | FixedPoint (_, body) -> walk_mono body
    | RecordType fields -> List.iter (fun (_, t) -> walk_mono t) fields
    | IntType | FloatType | BoolType | StringType | CharType | UnitType
    | TypeName _ ->
        ()
  in
  let rec walk_c (t : c_type) : unit =
    match t with
    | Mono m -> walk_mono m
    | PolyType (v, inner) ->
        add v;
        walk_c inner
    | Constrained (ps, inner) ->
        List.iter (fun (_, m) -> walk_mono m) ps;
        walk_c inner
  in
  walk_c ct;
  tv := 0;
  let subs : (string * string) list =
    List.map
      (fun v ->
        tv := !tv + 1;
        (v, number_to_letter !tv))
      !names
  in
  let rec apply_mono (m : mono_type) : mono_type =
    match m with
    | TypeVar v -> (
        try TypeVar (List.assoc v subs) with Not_found -> m)
    | FunctionType (t1, t2) ->
        FunctionType (apply_mono t1, apply_mono t2)
    | VectorType ts -> VectorType (List.map apply_mono ts)
    | CListType e -> CListType (apply_mono e)
    | TypeName v -> TypeName v
    | CTypeApp (name, args) ->
        CTypeApp (name, List.map apply_mono args)
    | TCtorApp (w, args) ->
        TCtorApp (w, List.map apply_mono args)
    | FixedPoint (name, body) ->
        FixedPoint (name, apply_mono body)
    | RecordType fields ->
        RecordType
          (List.map (fun (name, t) -> (name, apply_mono t)) fields)
    | (IntType | FloatType | BoolType | StringType | CharType | UnitType) as
        prim ->
        prim
  in
  let rec apply_c (t : c_type) : c_type =
    match t with
    | Mono m -> Mono (apply_mono m)
    | PolyType (v, inner) ->
        let v' =
          try List.assoc v subs with Not_found -> v
        in
        PolyType (v', apply_c inner)
    | Constrained (ps, inner) ->
        Constrained
          ( List.map (fun (c, m) -> (c, apply_mono m)) ps,
            apply_c inner )
  in
  apply_c ct
