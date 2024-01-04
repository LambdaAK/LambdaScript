open Cexpr

let rec visited (elm : 'a) (lst : 'a list) =
  match lst with
  | [] -> false
  | h :: t -> if h = elm then true else visited elm t

let rec get_replacement (elm : int) (lst : int list) : int =
  match lst with
  | [] -> failwith "failure in get_replacement"
  | h :: _ when h = elm -> 1 (* the first element gets replaced by 1 *)
  | _ :: t -> 1 + get_replacement elm t (* add 1 *)

let fix (t : c_type) : c_type =
  (* replace written type vars with type vars *)
  let order : int list ref = ref [] in
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
