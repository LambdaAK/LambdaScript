open Cexpr
let indentations (level: int) = String.make (2 * level) ' '

let indentations_with_newline (level: int) = "\n" ^ (indentations level)




let rec string_of_c_expr (e: c_expr) (level: int) =
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
          let string_of_ct: string = string_of_t t in
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

and string_of_pat pat =
  match pat with
  | IdPat s -> s
  | NothingPat -> "nothing"


and string_of_t t =
  match t with
  | BoolType -> "bool"
  | StringType -> "string"
  | NothingType -> "nothing"
  | IntType -> "int"
  | FunctionType (i, o) -> Printf.sprintf "%s -> %s" (string_of_t i) (string_of_t o)