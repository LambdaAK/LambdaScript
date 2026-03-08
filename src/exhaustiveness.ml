open Cexpr

(* Pattern matrix type: a list of pattern rows *)
type pattern_matrix = c_pat list list

(* Constructor information from type environment *)
type constructor_info = {
  name: string;
  arity: int;
  type_name: string;
}

(* Result of exhaustiveness checking *)
type exhaustiveness_result =
  | Exhaustive
  | NonExhaustive of string  (* Missing pattern example *)

(* Get all constructors for a given type *)
let get_constructors_for_type (type_name : string) (constructor_env : (string * string * string list * c_type option) list) : constructor_info list =
  (* Filter constructors that belong to this type *)
  let type_constructors = List.filter (fun (_, tname, _, _) -> tname = type_name) constructor_env in

  (* Convert to constructor_info *)
  List.map (fun (cons_name, tname, _, payload_opt) ->
    let arity = match payload_opt with
      | None -> 0
      | Some _ -> 1  (* For simplicity, we treat payloads as single arguments *)
    in
    { name = cons_name; arity; type_name = tname }
  ) type_constructors

(* Check if a pattern is a wildcard (matches everything) *)
let is_wildcard (pat : c_pat) : bool =
  match pat with
  | CWildcardPat -> true
  | CIdPat _ -> true  (* Variable bindings are wildcards *)
  | _ -> false

(* Check if a pattern is a constructor pattern *)
let is_constructor (pat : c_pat) : bool =
  match pat with
  | CVariantPat _ -> true
  | CNilPat -> true
  | CConsPat _ -> true
  | CIntPat _ -> true
  | CBoolPat _ -> true
  | CCharPat _ -> true
  | CStringPat _ -> true
  | CUnitPat -> true
  | CVectorPat _ -> true
  | _ -> false

(* Get the constructor name from a pattern *)
let get_constructor_name (pat : c_pat) : string option =
  match pat with
  | CVariantPat (name, _) -> Some name
  | CNilPat -> Some "Nil"
  | CConsPat _ -> Some "Cons"
  | CIntPat i -> Some (string_of_int i)
  | CBoolPat b -> Some (string_of_bool b)
  | CCharPat c -> Some (String.make 1 c)
  | CStringPat s -> Some s
  | CUnitPat -> Some "()"
  | CVectorPat _ -> Some "Vector"
  | _ -> None

(* Get the arity of a pattern constructor *)
let get_pattern_arity (pat : c_pat) : int =
  match pat with
  | CVariantPat (_, None) -> 0
  | CVariantPat (_, Some _) -> 1
  | CNilPat -> 0
  | CConsPat _ -> 2
  | CIntPat _ -> 0
  | CBoolPat _ -> 0
  | CCharPat _ -> 0
  | CStringPat _ -> 0
  | CUnitPat -> 0
  | CVectorPat pats -> List.length pats
  | _ -> 0

(* Expand a pattern into a list of subpatterns *)
let expand_pattern (pat : c_pat) : c_pat list =
  match pat with
  | CVariantPat (_, None) -> []
  | CVariantPat (_, Some p) -> [p]
  | CConsPat (p1, p2) -> [p1; p2]
  | CVectorPat pats -> pats
  | _ -> []

(* Create n wildcard patterns *)
let rec wildcards (n : int) : c_pat list =
  if n <= 0 then []
  else CWildcardPat :: wildcards (n - 1)

(* Specialize a pattern row for a given constructor *)
let specialize_row (constructor_name : string) (arity : int) (row : c_pat list) : c_pat list option =
  match row with
  | [] -> None
  | first_pat :: rest ->
      if is_wildcard first_pat then
        (* Wildcard matches any constructor - expand to n wildcards *)
        Some (wildcards arity @ rest)
      else
        match get_constructor_name first_pat with
        | Some name when name = constructor_name ->
            (* Constructor matches - expand the pattern *)
            let subpats = expand_pattern first_pat in
            Some (subpats @ rest)
        | _ ->
            (* Different constructor - remove this row *)
            None

(* Specialize a pattern matrix for a given constructor *)
let specialize_matrix (constructor_name : string) (arity : int) (matrix : pattern_matrix) : pattern_matrix =
  List.filter_map (specialize_row constructor_name arity) matrix

(* Get the default matrix - rows that start with wildcards *)
let default_matrix (matrix : pattern_matrix) : pattern_matrix =
  List.filter_map (fun row ->
    match row with
    | [] -> None
    | first_pat :: rest ->
        if is_wildcard first_pat then Some rest
        else None
  ) matrix

(* Check if matrix is empty *)
let is_empty_matrix (matrix : pattern_matrix) : bool =
  matrix = []

(* Check if first row is all wildcards/variables *)
let first_row_all_wildcards (matrix : pattern_matrix) : bool =
  match matrix with
  | [] -> false
  | row :: _ -> List.for_all is_wildcard row

(* Check if all rows are empty (all positions matched) *)
let all_rows_empty (matrix : pattern_matrix) : bool =
  match matrix with
  | [] -> false
  | _ -> List.for_all (fun row -> row = []) matrix

(* Get the type of a pattern (simplified version) *)
let rec get_pattern_type (pat : c_pat) (static_env : (string * c_type) list) : mono_type option =
  match pat with
  | CIntPat _ -> Some IntType
  | CBoolPat _ -> Some BoolType
  | CCharPat _ -> Some CharType
  | CStringPat _ -> Some StringType
  | CUnitPat -> Some UnitType
  | CNilPat -> Some (CListType (TypeVar "a"))  (* Generic list *)
  | CConsPat _ -> Some (CListType (TypeVar "a"))
  | CVariantPat (cons_name, _) ->
      (* Look up constructor in environment to get its return type *)
      (match List.assoc_opt cons_name static_env with
       | Some c_type ->
          (* Extract the return type from constructor type *)
          let mono = match c_type with
            | Mono m -> m
            | PolyType _ ->
                (* Instantiate to get mono type *)
                let rec get_return_type = function
                  | Mono (FunctionType (_, ret)) -> ret
                  | Mono t -> t
                  | PolyType (_, body) -> get_return_type body
                in
                get_return_type c_type
           in
           (match mono with
            | FunctionType (_, ret) -> Some ret
            | _ -> Some mono)
       | None -> None)
  | CVectorPat pats ->
      (* Get types of all patterns in vector *)
      let types = List.filter_map (fun p -> get_pattern_type p static_env) pats in
      Some (VectorType types)
  | CWildcardPat -> None  (* Cannot determine type from wildcard *)
  | CIdPat _ -> None  (* Cannot determine type from variable *)

(* Main exhaustiveness checking function with column type tracking *)
let rec check_exhaustiveness
    (matrix : pattern_matrix)
    (column_types : mono_type list)
    (constructor_env : (string * string * string list * c_type option) list)
    (static_env : (string * c_type) list) : exhaustiveness_result =

  (* Base case 1: Empty matrix means no patterns match *)
  if is_empty_matrix matrix then
    NonExhaustive "_"

  (* Base case 2: All rows are empty means all positions matched *)
  else if all_rows_empty matrix then
    Exhaustive

  (* Base case 3: First row is all wildcards - catches everything *)
  else if first_row_all_wildcards matrix then
    Exhaustive

  (* Recursive case: split by constructors *)
  else
    (* Get the type of the first column *)
    let first_column_type = match column_types with
      | [] -> None  (* No type info *)
      | t :: _ -> Some t
    in

    (* If we don't have type info, try to infer from pattern *)
    let match_type = match first_column_type with
      | Some t -> Some t
      | None ->
          let first_pat = match matrix with
            | (p :: _) :: _ -> p
            | _ -> CWildcardPat
          in
          get_pattern_type first_pat static_env
    in

    (* Get all possible constructors for this type *)
    match match_type with
    | Some (CTypeApp (type_name, _)) | Some (TypeName type_name) | Some (FixedPoint (type_name, _)) ->
        let constructors = get_constructors_for_type type_name constructor_env in
        check_all_constructors matrix column_types constructors constructor_env static_env

    | Some (CListType _elem_type) ->
        (* List type has two constructors: Nil and Cons *)
        let constructors = [
          { name = "Nil"; arity = 0; type_name = "List" };
          { name = "Cons"; arity = 2; type_name = "List" }
        ] in
        check_all_constructors matrix column_types constructors constructor_env static_env

    | Some BoolType ->
        (* Bool type has two constructors: true and false *)
        let constructors = [
          { name = "true"; arity = 0; type_name = "bool" };
          { name = "false"; arity = 0; type_name = "bool" }
        ] in
        check_all_constructors matrix column_types constructors constructor_env static_env

    | Some UnitType ->
        (* Unit type has one constructor: () *)
        let constructors = [
          { name = "()"; arity = 0; type_name = "unit" }
        ] in
        check_all_constructors matrix column_types constructors constructor_env static_env

    | Some (VectorType types) ->
        (* Vector has one constructor with arity equal to the number of elements *)
        let constructors = [
          { name = "Vector"; arity = List.length types; type_name = "Vector" }
        ] in
        check_all_constructors matrix column_types constructors constructor_env static_env

    | Some IntType | Some CharType | Some StringType ->
        (* Infinite types - check if there's a wildcard in default matrix *)
        let default = default_matrix matrix in
        let rest_types = match column_types with _ :: rest -> rest | [] -> [] in
        if is_empty_matrix default then
          NonExhaustive "_"
        else
          check_exhaustiveness default rest_types constructor_env static_env

    | _ ->
        (* Unknown type or wildcard - assume exhaustive if there's a wildcard row *)
        let default = default_matrix matrix in
        let rest_types = match column_types with _ :: rest -> rest | [] -> [] in
        if is_empty_matrix default then
          NonExhaustive "_"
        else
          check_exhaustiveness default rest_types constructor_env static_env

(* Generate a witness example for a type *)
and generate_witness_for_type (t : mono_type) (constructor_env : (string * string * string list * c_type option) list) (depth : int) : string =
  if depth <= 0 then "_"
  else
    match t with
    | IntType -> "_"
    | BoolType -> "_"
    | CharType -> "_"
    | StringType -> "_"
    | UnitType -> "()"
    | FloatType -> "_"
    | TypeVar _ -> "_"
    | CListType _ ->
        (* For list types, use Nil as the base case *)
        "Nil"
    | VectorType types ->
        (* Generate witness for each component *)
        let witnesses = List.map (fun ty -> generate_witness_for_type ty constructor_env (depth - 1)) types in
        "(" ^ String.concat ", " witnesses ^ ")"
    | CTypeApp (type_name, _) | TypeName type_name | FixedPoint (type_name, _) ->
        (* Find a constructor for this type, prefer nullary ones *)
        let constructors = List.filter (fun (_, tname, _, _) -> tname = type_name) constructor_env in
        (match constructors with
         | [] -> "_"
         | (cons_name, _, _, None) :: _ ->
             (* Nullary constructor - use it *)
             cons_name
         | (cons_name, _, _, Some payload) :: _ ->
             (* Constructor with payload - generate witness for payload *)
             let payload_mono = match payload with Mono m -> m | PolyType _ -> TypeVar "a" in
             let witness = generate_witness_for_type payload_mono constructor_env (depth - 1) in
             cons_name ^ " " ^ witness)
    | FunctionType _ -> "_"
    | RecordType _ -> "_"

(* Generate a missing pattern example for a constructor *)
and generate_missing_pattern (cons_name : string) (constructor_env : (string * string * string list * c_type option) list) : string =
  (* Look up the constructor in the environment to get payload info *)
  let payload_type_opt =
    List.find_map (fun (name, _, _, payload) ->
      if name = cons_name then Some payload else None
    ) constructor_env
  in

  match payload_type_opt with
  | None | Some None ->
      (* Nullary constructor *)
      cons_name
  | Some (Some payload_ctype) ->
      (* Constructor with payload - generate a witness *)
      let payload_mono = match payload_ctype with Mono m -> m | PolyType _ -> TypeVar "a" in
      let witness = generate_witness_for_type payload_mono constructor_env 2 in
      cons_name ^ " " ^ witness

(* Get the component types when a constructor is applied *)
and get_constructor_column_types
    (cons_name : string)
    (first_column_type : mono_type option)
    (rest_column_types : mono_type list)
    (constructor_env : (string * string * string list * c_type option) list) : mono_type list =

  (* Special handling for built-in list constructors *)
  if cons_name = "Cons" then
    match first_column_type with
    | Some (CListType elem_type) ->
        (* Cons has tuple payload: (elem, list) *)
        [elem_type; CListType elem_type] @ rest_column_types
    | _ ->
        (* Fallback *)
        TypeVar "a" :: TypeVar "list" :: rest_column_types
  else if cons_name = "Nil" then
    (* Nil has no payload *)
    rest_column_types
  else
    (* Look up constructor payload type *)
    let payload_opt = List.find_map (fun (name, _, _, payload) ->
      if name = cons_name then Some payload else None
    ) constructor_env in

    match payload_opt with
    | None | Some None ->
        (* Nullary constructor - just remove first column *)
        rest_column_types
    | Some (Some (Mono (VectorType types))) ->
        (* Tuple payload - expand to multiple columns *)
        types @ rest_column_types
    | Some (Some (Mono t)) ->
        (* Single payload - one new column *)
        t :: rest_column_types
    | Some (Some (PolyType _)) ->
        (* Polymorphic payload - treat as wildcard *)
        TypeVar "a" :: rest_column_types

(* Check exhaustiveness for all constructors of a type *)
and check_all_constructors
    (matrix : pattern_matrix)
    (column_types : mono_type list)
    (constructors : constructor_info list)
    (constructor_env : (string * string * string list * c_type option) list)
    (static_env : (string * c_type) list) : exhaustiveness_result =

  match constructors with
  | [] ->
      (* No more constructors to check - all have been covered *)
      Exhaustive

  | cons :: rest ->
      (* Specialize matrix for this constructor *)
      let specialized = specialize_matrix cons.name cons.arity matrix in

      (* Compute new column types after specialization *)
      let first_column_type = match column_types with t :: _ -> Some t | [] -> None in
      let rest_column_types = match column_types with _ :: rest -> rest | [] -> [] in
      let new_column_types = get_constructor_column_types cons.name first_column_type rest_column_types constructor_env in

      (* Recursively check the specialized matrix *)
      let result = check_exhaustiveness specialized new_column_types constructor_env static_env in

      match result with
      | NonExhaustive nested_witness ->
          (* This constructor is not covered - build full pattern with context *)
          let missing_pattern =
            if cons.arity = 0 then
              (* Nullary constructor *)
              cons.name
            else if nested_witness = "_" then
              (* No specific nested witness - generate a default one *)
              generate_missing_pattern cons.name constructor_env
            else
              (* Have a nested witness - incorporate it *)
              cons.name ^ " " ^ nested_witness
          in
          NonExhaustive missing_pattern
      | Exhaustive ->
          (* This constructor is covered - check the rest *)
          check_all_constructors matrix column_types rest constructor_env static_env

(* Main entry point for checking a switch expression *)
let check_switch_exhaustiveness
    (scrutinee_type : mono_type)
    (branches : (c_pat * c_expr) list)
    (constructor_env : (string * string * string list * c_type option) list)
    (static_env : (string * c_type) list) : exhaustiveness_result =

  (* Extract just the patterns from the branches *)
  let patterns = List.map fst branches in

  (* Create pattern matrix (each pattern is a single-element row) *)
  let matrix = List.map (fun pat -> [pat]) patterns in

  (* Check exhaustiveness with single-column type *)
  check_exhaustiveness matrix [scrutinee_type] constructor_env static_env
