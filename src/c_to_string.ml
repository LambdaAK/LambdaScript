open Cexpr

let rec string_of_mono_type : mono_type -> string = function
  | IntType -> "int"
  | FloatType -> "float"
  | BoolType -> "bool"
  | StringType -> "str"
  | CharType -> "char"
  | UnitType -> "unit"
  | TypeVar v -> "'" ^ v
  | FunctionType (t1, t2) ->
      let t1_str =
        match t1 with
        | FunctionType _ -> "(" ^ string_of_mono_type t1 ^ ")"
        | _ -> string_of_mono_type t1
      in
      t1_str ^ " -> " ^ string_of_mono_type t2
  | VectorType ts ->
      let ts_str = List.map string_of_mono_type ts in
      "(" ^ String.concat ", " ts_str ^ ")"
  | TypeName t -> t
  | CListType t -> "[" ^ string_of_mono_type t ^ "]"
  | CTypeApp (name, args) ->
      if args = [] then name
      else
        let args_str = List.map string_of_mono_type args in
        name ^ "<" ^ String.concat ", " args_str ^ ">"
  | FixedPoint (_, body) -> string_of_mono_type body
  | RecordType fields ->
      let field_strs = List.map (fun (name, t) ->
        name ^ ": " ^ string_of_mono_type t
      ) fields in
      "{" ^ String.concat ", " field_strs ^ "}"

let rec string_of_c_type (ct : c_type) : string =
  let pretty_tv (v : string) : string =
    if
      String.length v > 9 && String.sub v 0 9 = "$written("
      && String.ends_with ~suffix:")" v
    then "'" ^ String.sub v 9 (String.length v - 10)
    else if String.starts_with ~prefix:"'" v then v
    else if v = "string" then "str"
    else "'" ^ v
  in
  let rec string_of_mono_for_display : mono_type -> string = function
    | TypeVar v -> pretty_tv v
    | FunctionType (t1, t2) ->
        let t1_str =
          match t1 with
          | FunctionType _ ->
              "(" ^ string_of_mono_for_display t1 ^ ")"
          | _ -> string_of_mono_for_display t1
        in
        t1_str ^ " -> " ^ string_of_mono_for_display t2
    | VectorType ts ->
        let ts_str = List.map string_of_mono_for_display ts in
        "(" ^ String.concat ", " ts_str ^ ")"
    | CListType t -> "[" ^ string_of_mono_for_display t ^ "]"
    | CTypeApp (name, args) ->
        if args = [] then name
        else
          let args_str = List.map string_of_mono_for_display args in
          name ^ "<" ^ String.concat ", " args_str ^ ">"
    | FixedPoint (_, body) -> string_of_mono_for_display body
    | RecordType fields ->
        let field_strs =
          List.map
            (fun (name, t) ->
              name ^ ": " ^ string_of_mono_for_display t)
            fields
        in
        "{" ^ String.concat ", " field_strs ^ "}"
    | TypeName t -> if t = "string" then "str" else t
    | ( IntType | FloatType | BoolType | StringType | CharType | UnitType )
      as m ->
        string_of_mono_type m
  in
  let rec flatten_quant acc c =
    match c with
    | PolyType (v, rest) -> flatten_quant (pretty_tv v :: acc) rest
    | t -> (List.rev acc, t)
  in
  let quant, rest = flatten_quant [] ct in
  let constr, inner =
    match rest with
    | Constrained (ps, inner') ->
        ( ps,
          (match inner' with
          | Mono m -> `Mono m
          | PolyType _ | Constrained _ -> `Nested inner') )
    | Mono m -> ([], `Mono m)
    | PolyType _ -> ([], `Nested rest)
  in
  (* When there are class constraints, quantified vars appear in the preds and
     body — skip a duplicate leading [quant_str] (e.g. Show 'a => 'a -> t). *)
  let quant_str =
    if constr <> [] then ""
    else
      match quant with
      | [] -> ""
      | [ v ] -> v ^ " "
      | vs -> "(" ^ String.concat ", " vs ^ ") "
  in
  let constr_str =
    if constr = [] then ""
    else
      String.concat ", "
        (List.map
           (fun (cls, ty) -> cls ^ " " ^ string_of_mono_for_display ty)
           constr)
      ^ " => "
  in
  let inner_str =
    match inner with
    | `Mono m -> string_of_mono_for_display m
    | `Nested t -> string_of_c_type t
  in
  quant_str ^ constr_str ^ inner_str

(** Formats an optional type annotation. Returns " : type" if Some type, or
    empty string if None. *)
let format_type_annotation (t_opt : c_type option) : string =
  match t_opt with
  | Some t -> " : " ^ string_of_type t
  | None -> ""

let rec string_of_pat : c_pat -> string = function
  | CIntPat i -> string_of_int i
  | CBoolPat b -> string_of_bool b
  | CNilPat -> "[]"
  | CConsPat (p1, p2) -> string_of_pat p1 ^ " :: " ^ string_of_pat p2
  | CWildcardPat -> "_"
  | CVectorPat ps -> "(" ^ String.concat ", " (List.map string_of_pat ps) ^ ")"
  | CRecordPat fields ->
      "{"
      ^ String.concat ", "
          (List.map (fun (n, p) -> n ^ ": " ^ string_of_pat p) fields)
      ^ "}"
  | CStringPat s -> "\"" ^ s ^ "\""
  | CCharPat c -> "'" ^ String.make 1 c ^ "'"
  | CIdPat id -> id
  | CUnitPat -> "()"
  | CVariantPat (cons_name, None) -> cons_name
  | CVariantPat (cons_name, Some payload_pat) ->
      cons_name ^ " " ^ string_of_pat payload_pat

let rec string_of_expr : c_expr -> string = function
  | EInt i -> string_of_int i
  | EFloat f -> string_of_float f
  | EBool b -> string_of_bool b
  | EString s -> "\"" ^ s ^ "\""
  | EChar c -> "'" ^ String.make 1 c ^ "'"
  | EUnit -> "()"
  | EId id -> id
  | ENil -> "[]"
  | EBlock parts ->
      let defns, e =
        match parts with
        | [] -> ([], EUnit)
        | _ ->
            let rev = List.rev parts in
            let e = List.hd rev in
            let defns = List.rev (List.tl rev) in
            let defns =
              List.filter_map
                (function
                  | Defn d -> Some d
                  | Expr _ -> None)
                defns
            in
            let e =
              match e with
              | Expr e -> e
              | Defn _ -> EUnit
            in
            (defns, e)
      in
      "{\n"
      ^ String.concat "\n" (List.map string_of_defn defns)
      ^ "\n" ^ string_of_expr e ^ "\n}"
  | EFunction (pat, t_opt, body) ->
      "fn " ^ string_of_pat pat
      ^ format_type_annotation t_opt
      ^ " -> " ^ string_of_expr body
  | EBind (pat, t_opt, e1, e2, return_type_opt) ->
      "let " ^ string_of_pat pat
      ^ format_type_annotation t_opt
      ^ format_type_annotation return_type_opt
      ^ " = " ^ string_of_expr e1 ^ " in " ^ string_of_expr e2
  | EBindRec (pat, t_opt, e1, e2, return_type_opt) ->
      "let rec " ^ string_of_pat pat
      ^ format_type_annotation t_opt
      ^ format_type_annotation return_type_opt
      ^ " = " ^ string_of_expr e1 ^ " in " ^ string_of_expr e2
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
        | CConcat -> "^"
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
  | ERecordLit fields ->
      let field_strs = List.map (fun (name, e) ->
        name ^ ": " ^ string_of_expr e
      ) fields in
      "{" ^ String.concat ", " field_strs ^ "}"
  | ERecordUpdate (e, updates) ->
      let update_strs = List.map (fun (name, e) ->
        name ^ " = " ^ string_of_expr e
      ) updates in
      "{" ^ string_of_expr e ^ " with " ^ String.concat ", " update_strs ^ "}"
  | EFieldAccess (e, field) ->
      string_of_expr e ^ "." ^ field
  | EBindMutRec (bindings, body) ->
      let binding_strs = List.map (fun (pat, _, expr, _, _) ->
        string_of_pat pat ^ " = " ^ string_of_expr expr
      ) bindings in
      "let rec " ^ String.concat "\nand " binding_strs ^ " in\n" ^ string_of_expr body

and string_of_defn : c_defn -> string = function
  | CDefn (pat, t_opt, e, return_type_opt, _) ->
      let type_annot =
        match t_opt with
        | Some t -> " : " ^ string_of_type t
        | None -> ""
      in
      let return_annot =
        match return_type_opt with
        | Some t -> " : " ^ string_of_type t
        | None -> ""
      in
      "let " ^ string_of_pat pat ^ type_annot ^ return_annot ^ " = " ^ string_of_expr e
  | CDefnRec (pat, t_opt, e, return_type_opt, _) ->
      let type_annot =
        match t_opt with
        | Some t -> " : " ^ string_of_type t
        | None -> ""
      in
      let return_annot =
        match return_type_opt with
        | Some t -> " : " ^ string_of_type t
        | None -> ""
      in
      "let rec " ^ string_of_pat pat ^ type_annot ^ return_annot ^ " = " ^ string_of_expr e
  | CDefnMutRec defns ->
      let defn_strs = List.map (fun (pat, t_opt, e, _, _) ->
        let type_annot =
          match t_opt with
          | Some t -> " : " ^ string_of_type t
          | None -> ""
        in
        string_of_pat pat ^ type_annot ^ " = " ^ string_of_expr e
      ) defns in
      "let rec " ^ String.concat "\nand " defn_strs
  | CClassDecl (name, args, methods) ->
      let args_str =
        match args with
        | [] -> ""
        | _ -> "<" ^ String.concat ", " args ^ ">"
      in
      let meth_strs =
        List.map
          (fun (m, t) -> "val " ^ m ^ " : " ^ string_of_mono_type t)
          methods
      in
      "inter " ^ name ^ args_str ^ " {\n  "
      ^ String.concat ",\n  " meth_strs
      ^ "\n}"
  | CTypeAlias (name, args, body) ->
      let args_str =
        match args with
        | [] -> ""
        | _ -> "<" ^ String.concat ", " args ^ ">"
      in
      "type " ^ name ^ args_str ^ " = " ^ string_of_mono_type body
  | CSumType (name, args, constructors) ->
      let args_str =
        match args with
        | [] -> ""
        | _ -> "<" ^ String.concat ", " args ^ ">"
      in
      let constructors_str =
        List.map
          (fun (cons_name, payload_type_opt) ->
            match payload_type_opt with
            | None -> "| " ^ cons_name
            | Some payload_type ->
                "| " ^ cons_name ^ " of " ^ string_of_c_type payload_type)
          constructors
      in
      "type " ^ name ^ args_str ^ " = " ^ String.concat "\n  " constructors_str
  | CSumTypeRec (name, args, constructors) ->
      let args_str =
        match args with
        | [] -> ""
        | _ -> "<" ^ String.concat ", " args ^ ">"
      in
      let constructors_str =
        List.map
          (fun (cons_name, payload_type_opt) ->
            match payload_type_opt with
            | None -> "| " ^ cons_name
            | Some payload_type ->
                "| " ^ cons_name ^ " of " ^ string_of_c_type payload_type)
          constructors
      in
      "type rec " ^ name ^ args_str ^ " = " ^ String.concat "\n  " constructors_str
  | CSumTypeRecMutRec types ->
      let type_strs =
        List.mapi
          (fun i (name, args, constructors) ->
            let args_str =
              match args with
              | [] -> ""
              | _ -> "<" ^ String.concat ", " args ^ ">"
            in
            let constructors_str =
              List.map
                (fun (cons_name, payload_type_opt) ->
                  match payload_type_opt with
                  | None -> "| " ^ cons_name
                  | Some payload_type ->
                      "| " ^ cons_name ^ " of " ^ string_of_c_type payload_type)
                constructors
            in
            let prefix = if i = 0 then "type rec " else "and " in
            prefix ^ name ^ args_str ^ " = " ^ String.concat "\n  " constructors_str)
          types
      in
      String.concat "\n" type_strs

let rec string_of_program : c_program -> string = function
  | [] -> ""
  | d :: ds -> string_of_defn d ^ "\n" ^ string_of_program ds
