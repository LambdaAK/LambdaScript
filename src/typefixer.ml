open Cexpr

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
  | UnitType -> []

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
    | UnitType -> UnitType
  in
  let subs = create_substitution t [] in
  apply_substitution t subs
