open Cexpr
open Expr
[@@@coverage off]

let indentations (level: int) = String.make (2 * level) ' '

let indentations_with_newline (level: int) = "\n" ^ (indentations level)


let rec string_of_pat pat =
  match pat with
  | IdPat s -> s
  | WildcardPat -> "_"
  | NothingPat -> "ng"
  | PairPat (p1, p2) ->
    let p1_string: string = string_of_pat p1 in
    let p2_string: string = string_of_pat p2 in
    "<|" ^ p1_string ^ ", " ^ p2_string ^ "|>"
  | VectorPat patterns ->
    let patterns_string: string =
      List.fold_left
        (fun acc p ->
          let p_string: string = string_of_pat p in
          acc
          ^ p_string
          ^ ", "
        )
        ""
        patterns
    in
    "<|"
    ^ (String.sub patterns_string 0 ((String.length patterns_string) - 2))
    ^ "|>"


and string_of_c_defn (d: c_defn) =
  match d with
  | CDefn (pattern, cto, body_expression) -> ignore cto;
    "Definition ("
      ^ indentations_with_newline (1)
      ^ (string_of_pat pattern)
      ^ ","
      ^ (string_of_c_expr body_expression 1)
      ^ indentations_with_newline 1
      ^ ")"


and string_of_c_expr (e: c_expr) (level: int) =
  match e with
  | EInt i ->
    "Int ("
    ^ (string_of_int i)
    ^ ")"
  | EString s ->
    "String ("
    ^ s
    ^ ")"
  | EBool b ->
    "Bool ("
    ^ (string_of_bool b)
    ^ ")"
  | ETernary (e1, e2, e3) ->
      let e1_string: string = string_of_c_expr e1 (level + 1) in
      let e2_string: string = string_of_c_expr e2 (level + 1) in
      let e3_string: string = string_of_c_expr e3 (level + 1) in
    
      "Ternary ("
      ^ indentations_with_newline (level + 1)
      ^ e1_string
      ^ ","
      ^ indentations_with_newline (level + 1)
      ^ e2_string
      ^ ","
      ^ indentations_with_newline (level + 1)
      ^ e3_string
      ^ indentations_with_newline level
      ^ ")"
  | ENothing ->
    "Nothing"
  | EId s ->
    "Id ("
    ^ s
    ^ ")"
  | EFunction (pattern, cto, body) ->
      let pattern_string: string = string_of_pat pattern in
      let body_string: string = string_of_c_expr body (level + 1) in
      
      "Function ("
      ^ indentations_with_newline (level + 1)
      ^ pattern_string
      ^ ","
      ^ (
        match cto with
        | None -> ""
        | Some t ->
          (* add the type annotation *)
          let string_of_ct: string = string_of_c_type t in
          indentations_with_newline (level + 1)
          ^ string_of_ct
          ^ ","
        )
      ^ indentations_with_newline (level + 1)
      ^ body_string
      ^ indentations_with_newline level
      ^ ")"
  | EApp (e1, e2) ->
    let e1_string: string = string_of_c_expr e1 (level + 1) in
    let e2_string: string = string_of_c_expr e2 (level + 1) in

    "App ("
    ^ indentations_with_newline (level + 1)
    ^ e1_string
    ^ ","
    ^ indentations_with_newline (level + 1)
    ^ e2_string
    ^ indentations_with_newline level
    ^ ")"
  | EBop (bop, e1, e2) ->
    let e1_string: string = string_of_c_expr e1 (level + 1) in
    let e2_string: string = string_of_c_expr e2 (level + 1) in

    "Bop ("
    ^ indentations_with_newline (level + 1)
    ^ (string_of_c_bop bop)
    ^ ","
    ^ indentations_with_newline (level + 1)
    ^ e1_string
    ^ ","
    ^ indentations_with_newline (level + 1)
    ^ e2_string
    ^ indentations_with_newline level
    ^ ")"

  | EBindRec (pattern, cto, body, e) ->
    let pattern_string: string = string_of_pat pattern in
    let body_string: string = string_of_c_expr body (level + 1) in
    let e_string: string = string_of_c_expr e (level + 1) in

    "BindRec ("
    ^ indentations_with_newline (level + 1)
    ^ pattern_string
    ^ ","
    ^ (
      match cto with
      | None -> ""
      | Some t ->
        (* add the type annotation *)
        let string_of_ct: string = string_of_c_type t in
        indentations_with_newline (level + 1)
        ^ string_of_ct
        ^ ","
      )
    ^ indentations_with_newline (level + 1)
    ^ body_string
    ^ ","
    ^ indentations_with_newline (level + 1)
    ^ e_string
    ^ indentations_with_newline level
    ^ ")"
  | EPair (e1, e2) ->
    let e1_string: string = string_of_c_expr e1 (level + 1) in
    let e2_string: string = string_of_c_expr e2 (level + 1) in

    "Pair ("
    ^ indentations_with_newline (level + 1)
    ^ e1_string
    ^ ","
    ^ indentations_with_newline (level + 1)
    ^ e2_string
    ^ indentations_with_newline level
    ^ ")"
  | EVector expressions ->
    let expressions_string: string =
      List.fold_left
        (fun acc e ->
          let e_string: string = string_of_c_expr e (level + 1) in
          acc
          ^ indentations_with_newline (level + 1)
          ^ e_string
          ^ ","
        )
        ""
        expressions
    in
    "Vector ("
    ^ indentations_with_newline (level + 1)
    ^ expressions_string
    ^ indentations_with_newline level
    ^ ")"
    

and string_of_c_bop: c_bop -> string =
  function
  | CPlus -> "+"
  | CMinus -> "-"
  | CMul -> "*"
  | CDiv -> "/"
  | CMod -> "%"
  | CEQ -> "=="
  | CNE -> "!="
  | CLT -> "<"
  | CGT -> ">"
  | CLE -> "<="
  | CGE -> ">="
  | CAnd -> "&&"
  | COr -> "||"



and string_of_c_type t =
  match t with
  | BoolType -> "bool"
  | StringType -> "str"
  | NothingType -> "ng"
  | IntType -> "int"
  | FunctionType (i, o) ->
    (* if i is a function type it must be surrounded by ( ) *)
    let i_string: string =
    (
      match i with
      | FunctionType _ -> "(" ^ (string_of_c_type i) ^ ")"
      | _ -> string_of_c_type i
    ) in
    let o_string: string = string_of_c_type o in
    i_string ^ " -> " ^ o_string


  | TypeVar n -> "t" ^ (string_of_int n)
  | PairType (t1, t2) ->
    let t1_string: string = string_of_c_type t1 in
    let t2_string: string = string_of_c_type t2 in
    "<|" ^ t1_string ^ ", " ^ t2_string ^ "|>"
  | VectorType types ->
    let types_string: string =
      List.fold_left
        (fun acc t ->
          let t_string: string = string_of_c_type t in
          acc
     
          ^ t_string
          ^ ", "
        )
        ""
        types
    in
    "<|"
    ^ (String.sub types_string 0 ((String.length types_string) - 2))
    ^ "|>"


let string_of_c_expr e = string_of_c_expr e 0

let () = ignore string_of_c_defn