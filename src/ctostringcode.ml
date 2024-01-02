open Cexpr
open Ctostring

[@@@coverage off]

module CToStringCode : CToString = struct
  let rec string_of_c_pat = function
    | CIntPat i -> string_of_int i
    | CBoolPat b -> string_of_bool b
    | CUnitPat -> "()"
    | CIdPat s -> s
    | CConsPat (p1, p2) -> string_of_c_pat p1 ^ "::" ^ string_of_c_pat p2
    | CNilPat -> "[]"
    | CVectorPat ps ->
        "(" ^ String.concat ", " (List.map string_of_c_pat ps) ^ ")"
    | CWildcardPat -> "_"
    | CStringPat s -> "\"" ^ s ^ "\""
    | CConstructorPat n -> n
    | CAppPat (p1, p2) -> string_of_c_pat p1 ^ " " ^ string_of_c_pat p2

  let string_of_c_bop : c_bop -> string = function
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
    | CCons -> "::"

  let indentations (level : int) = String.make (2 * level) ' '
  let indentations_with_newline (level : int) = "\n" ^ indentations level

  let rec string_of_c_expr_level (e : c_expr) (level : int) =
    match e with
    | EInt i -> string_of_int i
    | EBool b -> string_of_bool b
    | EUnit -> "()"
    | EId s -> s
    | EBop (b, e1, e2) ->
        string_of_c_expr_level e1 level
        ^ " " ^ string_of_c_bop b ^ " "
        ^ string_of_c_expr_level e2 level
    | EApp (e1, e2) ->
        let e1_string =
          match e1 with
          | EId _ -> string_of_c_expr_level e1 level
          | _ -> "(" ^ string_of_c_expr_level e1 level ^ ")"
        in

        let e2_string = string_of_c_expr_level e2 level in

        e1_string ^ " " ^ e2_string
    | EBindRec (pattern, _, body, e) ->
        indentations level ^ "bind rec " ^ string_of_c_pat pattern ^ " <-"
        ^ indentations_with_newline level
        ^ string_of_c_expr_level body (level + 1)
        ^ indentations_with_newline level
        ^ "in "
        ^ string_of_c_expr_level e level
    | EVector es ->
        "( "
        ^ String.concat ", "
            (List.map (fun e -> string_of_c_expr_level e level) es)
        ^ " )"
    | ENil -> "[]"
    | EFunction (pattern, _, body) ->
        "fn " ^ string_of_c_pat pattern ^ " -> "
        ^ string_of_c_expr_level body (level + 1)
    | ETernary (e1, e2, e3) ->
        "if "
        ^ string_of_c_expr_level e1 level
        ^ " then "
        ^ string_of_c_expr_level e2 level
        ^ " else "
        ^ string_of_c_expr_level e3 level
    | ESwitch (e, cases) ->
        (* print it like this: switch e => | p1 -> e1 | p2 -> e2 | ... | pn ->
           en end *)
        indentations_with_newline level
        ^ "switch "
        ^ string_of_c_expr_level e level
        ^ " =>"
        ^ indentations_with_newline level
        ^ String.concat
            (indentations_with_newline level ^ "| ")
            (List.map
               (fun (p, e) ->
                 string_of_c_pat p ^ " -> "
                 ^ string_of_c_expr_level e (level + 1))
               cases)
        ^ indentations_with_newline level
        ^ "end"
    | EString s -> "\"" ^ s ^ "\""
    | EFloat f -> string_of_float f
    | EListEnumeration (e1, e2) ->
        string_of_c_expr_level e1 level
        ^ " .. "
        ^ string_of_c_expr_level e2 level
    | EListComprehension (e, generators) ->
        "[ "
        ^ string_of_c_expr_level e level
        ^ " | "
        ^ String.concat
            (indentations_with_newline level ^ "| ")
            (List.map
               (fun (p, e) ->
                 string_of_c_pat p ^ " <- "
                 ^ string_of_c_expr_level e (level + 1))
               generators)
        ^ indentations_with_newline level
        ^ "]"
    | EConstructor id -> id

  let string_of_c_expr e = string_of_c_expr_level e 0

  let rec string_of_c_type = function
    | BoolType -> "Bool"
    | StringType -> "Str"
    | UnitType -> "Unit"
    | IntType -> "Int"
    | FloatType -> "Float"
    | TypeName s -> s
    | CListType t ->
        let t_string : string = string_of_c_type t in
        "[" ^ t_string ^ "]"
    | FunctionType (i, o) ->
        (* if i is a function type it must be surrounded by ( ) *)
        let i_string : string =
          match i with
          | FunctionType _ -> "(" ^ string_of_c_type i ^ ")"
          | _ -> string_of_c_type i
        in
        let o_string : string = string_of_c_type o in
        i_string ^ " -> " ^ o_string
    | TypeVar n -> string_of_type_var n
    | TypeVarWritten i -> "'" ^ i
    | VectorType types ->
        let types_string : string =
          List.fold_left
            (fun acc t ->
              let t_string : string = string_of_c_type t in
              acc ^ t_string ^ ", ")
            "" types
        in
        "(" ^ String.sub types_string 0 (String.length types_string - 2) ^ ")"
    | UniversalType u ->
        let u_string : string = string_of_int u in
        "u" ^ u_string
    | AppType (t1, t2) ->
        let t1_string : string =
          match t1 with
          | FunctionType _ -> "(" ^ string_of_c_type t1 ^ ")"
          | _ -> string_of_c_type t1
        in
        let t2_string : string =
          match t2 with
          | FunctionType _ -> "(" ^ string_of_c_type t2 ^ ")"
          | _ -> string_of_c_type t2
        in
        t1_string ^ " " ^ t2_string
    | UnionType constructors ->
        ignore constructors;
        "UNION TYPE"
    | PolymorphicType (s, t) ->
        let t_string : string = string_of_c_type t in
        s ^ " -> " ^ t_string

  and string_of_type_var n =
    if n <= 26 then
      (* if n is less than 26, then it is a letter *)
      let letter = Char.chr (n + 96) in
      String.make 1 letter
    else
      (* if n is greater than 26, then it is a letter followed by a number *)
      (* get the index of the letter *)
      let index = n mod 26 in
      (* get the letter from the index *)
      let letter = Char.chr (index + 96) in

      let number = n / 26 in
      let number_string = string_of_int number in
      String.make 1 letter ^ number_string

  let string_of_c_defn (d : c_defn) =
    match d with
    | CDefn (p, _, e) ->
        "let " ^ string_of_c_pat p ^ " = " ^ string_of_c_expr e ^ "\n"
    | CDefnRec (p, _, e) ->
        "let rec " ^ string_of_c_pat p ^ " = " ^ string_of_c_expr e ^ "\n"
    | CTypeDefn (id, t) -> "type " ^ id ^ " = " ^ string_of_c_type t ^ "\n"
    | CUnionDefn (id, constructors) ->
        let constructor_strings =
          constructors
          |> List.map (function
               | CNullaryConstructor id -> id
               | CUnaryConstructor (id, t) -> id ^ " : " ^ string_of_c_type t)
        in
        "type " ^ id ^ " = " ^ String.concat " | " constructor_strings ^ "\n"
end
