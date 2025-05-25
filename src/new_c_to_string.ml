open New_cexpr

let rec string_of_mono_type : mono_type -> string = function
  | IntType -> "int"
  | FloatType -> "float"
  | BoolType -> "bool"
  | StringType -> "string"
  | UnitType -> "unit"
  | TypeVar v -> v
  | FunctionType (t1, t2) ->
      let t1_str =
        match t1 with
        | FunctionType _ -> "(" ^ string_of_mono_type t1 ^ ")"
        | _ -> string_of_mono_type t1
      in
      let t2_str =
        match t2 with
        | FunctionType _ -> "(" ^ string_of_mono_type t2 ^ ")"
        | _ -> string_of_mono_type t2
      in
      t1_str ^ " -> " ^ t2_str
  | VectorType ts ->
      let ts_str = List.map string_of_mono_type ts in
      "(" ^ String.concat ", " ts_str ^ ")"
  | CListType t -> "[" ^ string_of_mono_type t ^ "]"

let rec string_of_c_type : c_type -> string = function
  | Mono t -> string_of_mono_type t
  | PolyType (var, body) -> "∀" ^ var ^ ". " ^ string_of_c_type body

let rec string_of_pat : c_pat -> string = function
  | CIntPat i -> string_of_int i
  | CBoolPat b -> string_of_bool b
  | CNilPat -> "[]"
  | CConsPat (p1, p2) -> string_of_pat p1 ^ " :: " ^ string_of_pat p2
  | CWildcardPat -> "_"
  | CVectorPat ps -> "(" ^ String.concat ", " (List.map string_of_pat ps) ^ ")"
  | CStringPat s -> "\"" ^ s ^ "\""
  | CIdPat id -> id
  | CUnitPat -> "()"

let rec string_of_expr : c_expr -> string = function
  | EInt i -> string_of_int i
  | EFloat f -> string_of_float f
  | EBool b -> string_of_bool b
  | EString s -> "\"" ^ s ^ "\""
  | EUnit -> "()"
  | EId id -> id
  | ENil -> "[]"
  | EFunction (pat, t_opt, body) ->
      let type_annot =
        match t_opt with
        | Some t -> " : " ^ string_of_type t
        | None -> ""
      in
      "fn " ^ string_of_pat pat ^ type_annot ^ " -> " ^ string_of_expr body
  | EBindRec (pat, t_opt, e1, e2) ->
      let type_annot =
        match t_opt with
        | Some t -> " : " ^ string_of_type t
        | None -> ""
      in
      "let rec " ^ string_of_pat pat ^ type_annot ^ " = " ^ string_of_expr e1
      ^ " in " ^ string_of_expr e2
  | ETernary (cond, t_branch, f_branch) ->
      "if " ^ string_of_expr cond ^ " then " ^ string_of_expr t_branch
      ^ " else " ^ string_of_expr f_branch
  | ESwitch (e, branches) ->
      "match " ^ string_of_expr e ^ " with\n"
      ^ String.concat "\n"
          (List.map
             (fun (p, e) -> "| " ^ string_of_pat p ^ " -> " ^ string_of_expr e)
             branches)
  | EApp (e1, e2) -> "(" ^ string_of_expr e1 ^ ") (" ^ string_of_expr e2 ^ ")"
  | EBop (op, e1, e2) ->
      let op_str =
        match op with
        | CPlus -> "+"
        | CMinus -> "-"
        | CMul -> "*"
        | CDiv -> "/"
        | CMod -> "%"
        | CEQ -> "="
        | CNE -> "<>"
        | CLT -> "<"
        | CGT -> ">"
        | CLE -> "<="
        | CGE -> ">="
        | CAnd -> "&&"
        | COr -> "||"
        | CCons -> "::"
      in
      "(" ^ string_of_expr e1 ^ ") " ^ op_str ^ " (" ^ string_of_expr e2 ^ ")"
  | EVector es -> "(" ^ String.concat ", " (List.map string_of_expr es) ^ ")"
  | EListEnumeration (e1, e2) ->
      "[" ^ string_of_expr e1 ^ ".." ^ string_of_expr e2 ^ "]"
  | EListComprehension (e, generators) ->
      "[" ^ string_of_expr e ^ " | "
      ^ String.concat ", "
          (List.map
             (fun (p, e) -> string_of_pat p ^ " <- " ^ string_of_expr e)
             generators)
      ^ "]"

let string_of_defn : c_defn -> string = function
  | CDefn (pat, t_opt, e) ->
      let type_annot =
        match t_opt with
        | Some t -> " : " ^ string_of_type t
        | None -> ""
      in
      "let " ^ string_of_pat pat ^ type_annot ^ " = " ^ string_of_expr e
  | CDefnRec (pat, t_opt, e) ->
      let type_annot =
        match t_opt with
        | Some t -> " : " ^ string_of_type t
        | None -> ""
      in
      "let rec " ^ string_of_pat pat ^ type_annot ^ " = " ^ string_of_expr e

let rec string_of_program : c_program -> string = function
  | [] -> ""
  | d :: ds -> string_of_defn d ^ "\n" ^ string_of_program ds
