open Cexpr

(** Given an element [elm] and [lst], [visited elm lst] returns true if [elm] is
    in [lst] and false otherwise. *)
let visited (elm : 'a) (lst : 'a list) = List.exists (fun x -> x = elm) lst

(** Given an element [elm] and a list of replacements [lst],
    [get_replacement elm] returns the replacement of [elm] specified in [lst] *)
let rec get_replacement (elm : int) (lst : int list) : int =
  match lst with
  | [] -> failwith "failure in get_replacement"
  | h :: _ when h = elm -> 1 (* the first element gets replaced by 1 *)
  | _ :: t -> 1 + get_replacement elm t (* add 1 *)

(** Given a type [t], [fix t] is the type [t] but with type variables replaced
    in a way such that, when converted to a string, a type variable is less than
    or equal to all types to the right of it. *)
let fix (t : c_type) : c_type =
  (* replace written type vars with type vars *)
  let order : int list ref = ref [] in
  (* [search t] is a helper function that searches through [t] and adds all type
     variables to [order] in the order that they appear in [t] from left to
     right. *)
  let rec search (t : c_type) : unit =
    match t with
    | IntType -> ()
    | BoolType -> ()
    | StringType -> ()
    | UnitType -> ()
    | FloatType -> ()
    | TypeName _ -> ()
    | CListType et -> search et
    | UniversalType _ -> ()
    | TypeVarWritten _ -> () (* fix this later *)
    | TypeVar id ->
        if not (visited id !order) then order := !order @ [ id ]
          (* append the var*)
    | FunctionType (i, o) ->
        search i;
        search o
    | VectorType types -> List.iter (fun t -> search t) types
    | UnionType _ -> failwith "union type found in search in typefixer.ml"
    | AppType (t1, t2) ->
        search t1;
        search t2
    | PolymorphicType (i, o) ->
        search i;
        search o
  in

  let () = search t in

  (* we now need to replace the types *)
  let rec replace (t : c_type) (lst : int list) : c_type =
    match t with
    | IntType -> IntType
    | BoolType -> BoolType
    | StringType -> StringType
    | UnitType -> UnitType
    | FloatType -> FloatType
    | TypeName _ -> t
    | UniversalType id -> UniversalType id
    | TypeVar id -> TypeVar (get_replacement id lst)
    | TypeVarWritten _ -> t
    | FunctionType (i, o) -> FunctionType (replace i lst, replace o lst)
    | CListType et -> CListType (replace et lst)
    | VectorType types -> VectorType (List.map (fun t -> replace t lst) types)
    | AppType (t1, t2) -> AppType (replace t1 lst, replace t2 lst)
    | PolymorphicType (i, o) -> PolymorphicType (replace i lst, replace o lst)
    | UnionType _ -> failwith "union type found in replace in typefixer.ml"
  in

  replace t !order

let fix_kind (k : c_kind) : c_kind =
  let order : int list ref = ref [] in
  let rec search (k : c_kind) : unit =
    match k with
    | Star -> ()
    | Arrow (k1, k2) ->
        search k1;
        search k2
    | KindVar id -> if not (visited id !order) then order := !order @ [ id ]
  in
  search k;

  let rec replace (k : c_kind) (lst : int list) : c_kind =
    match k with
    | Star -> Star
    | Arrow (k1, k2) -> Arrow (replace k1 lst, replace k2 lst)
    | KindVar id -> KindVar (get_replacement id lst)
  in

  replace k !order
