open Cexpr

  let rec visited (elm: 'a) (lst: 'a list) =
    match lst with
    | [] -> false
    | h :: t ->
      if h = elm then true else visited elm t

  let rec get_replacement (elm: int) (lst: int list): int =
    match lst with
    | [] -> failwith "failure in get_replacement"
    | h :: _ when h = elm -> 1 (* the first element gets replaced by 1 *)
    | _ :: t -> 1 + get_replacement elm t (* add 1 *)

let print_int_list (lst: int list): unit =
  let rec print (lst: int list): unit =
    match lst with
    | [] -> ()
    | h :: t -> Printf.printf "%d " h; print t
  in
  print lst

let () = ignore print_int_list

  let fix (t: c_type): c_type =
    let order: (int list) ref = ref [] in
    let rec search (t: c_type): unit =
      (
      match t with
      | IntType -> ()
      | BoolType -> ()
      | StringType -> ()
      | NothingType -> ()
      | TypeVar id ->
        if not (visited id !order) then
        (
          order := (!order @ [id]) (* append the var*)
        )
      | FunctionType (i, o) ->
        search i;
        search o
      )
    in
    let () = search t in
   
    (* we now need to replace the types *)

    let rec replace(t: c_type) (lst: int list): c_type =
      match t with
      | IntType -> IntType
      | BoolType -> BoolType
      | StringType -> StringType
      | NothingType -> NothingType
      | TypeVar id ->
    
       

        TypeVar (get_replacement id lst)

      | FunctionType (i, o) ->

        FunctionType (replace i lst, replace o lst)

      in

    replace t !order

