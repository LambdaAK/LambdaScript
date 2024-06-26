open Cexpr
open Ctostring

[@@@coverage off]

module CToStringTree : CToString = struct
  let indentations (level : int) = String.make (2 * level) ' '
  let indentations_with_newline (level : int) = "\n" ^ indentations level

  let rec string_of_c_pat : c_pat -> string = function
    | CIntPat i -> "IntPat (" ^ string_of_int i ^ ")"
    | CBoolPat b -> "BoolPat (" ^ string_of_bool b ^ ")"
    | CUnitPat -> "UnitPat"
    | CIdPat s -> "IdPat (" ^ s ^ ")"
    | CConsPat (p1, p2) ->
        let p1_string : string = string_of_c_pat p1 in
        let p2_string : string = string_of_c_pat p2 in
        "ConsPat (" ^ p1_string ^ "," ^ p2_string ^ ")"
    | CNilPat -> "NilPat"
    | CVectorPat ps ->
        let ps_string : string =
          List.fold_left
            (fun acc p ->
              let p_string : string = string_of_c_pat p in
              acc ^ p_string ^ ",")
            "" ps
        in
        "VectorPat (" ^ ps_string ^ ")"
    | CWildcardPat -> "WildcardPat"
    | CStringPat s -> "StringPat (" ^ s ^ ")"
    | CConstructorPat n -> "ConstructorPat (" ^ n ^ ")"
    | CAppPat (p1, p2) ->
        "AppPat (" ^ string_of_c_pat p1 ^ "," ^ string_of_c_pat p2 ^ ")"

  and string_of_c_defn (d : c_defn) =
    match d with
    | CDefn (pattern, cto, body_expression) ->
        ignore cto;
        "Definition ("
        ^ indentations_with_newline 1
        ^ string_of_c_pat pattern ^ ","
        ^ string_of_c_expr body_expression 1
        ^ indentations_with_newline 1
        ^ ")"
    | CDefnRec (pattern, cto, body_expression) ->
        ignore cto;
        "DefinitionRec ("
        ^ indentations_with_newline 1
        ^ string_of_c_pat pattern ^ ","
        ^ string_of_c_expr body_expression 1
        ^ indentations_with_newline 1
        ^ ")"
    | CTypeDefn (name, t, type_vars) ->
        ignore (name, t, type_vars);
        "CTypeDefn"
    | CUnionDefn (name, constructors, type_vars) ->
        ignore (name, constructors, type_vars);
        "CUnionDefn"

  and string_of_c_expr (e : c_expr) (level : int) =
    match e with
    | EInt i -> "Int (" ^ string_of_int i ^ ")"
    | EString s -> "String (" ^ s ^ ")"
    | EBool b -> "Bool (" ^ string_of_bool b ^ ")"
    | ETernary (e1, e2, e3) ->
        let e1_string : string = string_of_c_expr e1 (level + 1) in
        let e2_string : string = string_of_c_expr e2 (level + 1) in
        let e3_string : string = string_of_c_expr e3 (level + 1) in

        "Ternary ("
        ^ indentations_with_newline (level + 1)
        ^ e1_string ^ ","
        ^ indentations_with_newline (level + 1)
        ^ e2_string ^ ","
        ^ indentations_with_newline (level + 1)
        ^ e3_string
        ^ indentations_with_newline level
        ^ ")"
    | ESwitch (e, branches) ->
        let e_string : string = string_of_c_expr e (level + 1) in
        let branches_string : string =
          List.fold_left
            (fun acc (p, e) ->
              let p_string : string = string_of_c_pat p in
              let e_string : string = string_of_c_expr e (level + 1) in
              acc
              ^ indentations_with_newline (level + 1)
              ^ p_string ^ " -> " ^ e_string ^ ",")
            "" branches
        in
        "Switch ("
        ^ indentations_with_newline (level + 1)
        ^ e_string ^ ","
        ^ indentations_with_newline (level + 1)
        ^ branches_string
        ^ indentations_with_newline level
        ^ ")"
    | EUnit -> "Unit"
    | EFloat f -> "Float (" ^ string_of_float f ^ ")"
    | EId s -> "Id (" ^ s ^ ")"
    | EConstructor s -> "Constructor (" ^ s ^ ")"
    | EFunction (pattern, cto, body) ->
        let pattern_string : string = string_of_c_pat pattern in
        let body_string : string = string_of_c_expr body (level + 1) in

        "Function ("
        ^ indentations_with_newline (level + 1)
        ^ pattern_string ^ ","
        ^ (match cto with
          | None -> ""
          | Some t ->
              (* add the type annotation *)
              let string_of_ct : string = string_of_c_type t in
              indentations_with_newline (level + 1) ^ string_of_ct ^ ",")
        ^ indentations_with_newline (level + 1)
        ^ body_string
        ^ indentations_with_newline level
        ^ ")"
    | EApp (e1, e2) ->
        let e1_string : string = string_of_c_expr e1 (level + 1) in
        let e2_string : string = string_of_c_expr e2 (level + 1) in

        "App ("
        ^ indentations_with_newline (level + 1)
        ^ e1_string ^ ","
        ^ indentations_with_newline (level + 1)
        ^ e2_string
        ^ indentations_with_newline level
        ^ ")"
    | EBop (bop, e1, e2) ->
        let e1_string : string = string_of_c_expr e1 (level + 1) in
        let e2_string : string = string_of_c_expr e2 (level + 1) in

        "Bop ("
        ^ indentations_with_newline (level + 1)
        ^ string_of_c_bop bop ^ ","
        ^ indentations_with_newline (level + 1)
        ^ e1_string ^ ","
        ^ indentations_with_newline (level + 1)
        ^ e2_string
        ^ indentations_with_newline level
        ^ ")"
    | EBindRec (pattern, cto, body, e) ->
        let pattern_string : string = string_of_c_pat pattern in
        let body_string : string = string_of_c_expr body (level + 1) in
        let e_string : string = string_of_c_expr e (level + 1) in

        "BindRec ("
        ^ indentations_with_newline (level + 1)
        ^ pattern_string ^ ","
        ^ (match cto with
          | None -> ""
          | Some t ->
              (* add the type annotation *)
              let string_of_ct : string = string_of_c_type t in
              indentations_with_newline (level + 1) ^ string_of_ct ^ ",")
        ^ indentations_with_newline (level + 1)
        ^ body_string ^ ","
        ^ indentations_with_newline (level + 1)
        ^ e_string
        ^ indentations_with_newline level
        ^ ")"
    | EVector expressions ->
        let expressions_string : string =
          List.fold_left
            (fun acc e ->
              let e_string : string = string_of_c_expr e (level + 1) in
              acc ^ indentations_with_newline (level + 1) ^ e_string ^ ",")
            "" expressions
        in
        "Vector ("
        ^ indentations_with_newline (level + 1)
        ^ expressions_string
        ^ indentations_with_newline level
        ^ ")"
    | ENil -> "[]"
    | EListEnumeration (e1, e2) ->
        let e1_string : string = string_of_c_expr e1 (level + 1) in
        let e2_string : string = string_of_c_expr e2 (level + 1) in

        "ListEnumeration ("
        ^ indentations_with_newline (level + 1)
        ^ e1_string ^ ","
        ^ indentations_with_newline (level + 1)
        ^ e2_string
        ^ indentations_with_newline level
        ^ ")"
    | EListComprehension (e, generators) ->
        let e_string : string = string_of_c_expr e (level + 1) in
        let generators_string : string =
          List.fold_left
            (fun acc (p, e) ->
              let p_string : string = string_of_c_pat p in
              let e_string : string = string_of_c_expr e (level + 1) in
              acc
              ^ indentations_with_newline (level + 1)
              ^ p_string ^ " <- " ^ e_string ^ ",")
            "" generators
        in
        "ListComprehension ("
        ^ indentations_with_newline (level + 1)
        ^ e_string ^ ","
        ^ indentations_with_newline (level + 1)
        ^ generators_string
        ^ indentations_with_newline level
        ^ ")"

  and string_of_c_bop : c_bop -> string = function
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

  and string_of_c_type_scheme (universal_types, t) =
    match universal_types with
    | [] -> string_of_c_type t
    | _ ->
        let universal_types_string : string =
          List.fold_left
            (fun acc t ->
              let t_string : string = string_of_c_type t in
              acc ^ t_string ^ ", ")
            "" universal_types
        in
        "∀ "
        ^ String.sub universal_types_string 0
            (String.length universal_types_string - 2)
        ^ ". " ^ string_of_c_type t

  and string_of_c_kind = function
    | Star -> "*"
    | Arrow (k1, k2) ->
        let k1_string =
          match k1 with
          | Star -> "*"
          | Arrow _ -> "(" ^ string_of_c_kind k1 ^ ")"
          | KindVar n -> string_of_type_var n |> String.uppercase_ascii
        in
        let k2_string = string_of_c_kind k2 in
        k1_string ^ " -> " ^ k2_string
    | KindVar n ->
        let lower_string = string_of_type_var n in
        (* conver it to upper *)
        let upper_string = String.uppercase_ascii lower_string in
        upper_string

  and string_of_c_type t =
    match t with
    | BoolType -> "Bool"
    | StringType -> "Str"
    | UnitType -> "Unit"
    | IntType -> "Int"
    | FloatType -> "Float"
    | TypeName s -> s
    | UnionType constructors ->
        let constructors_string : string =
          List.fold_left
            (fun acc c ->
              let c_string : string = string_of_c_constructor c in
              acc ^ c_string ^ " | ")
            "" constructors
        in
        String.sub constructors_string 0 (String.length constructors_string - 3)
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
    | TypeVarWritten n -> n
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
        (* t1 t2 *)
        let t1_string = string_of_c_type t1 in
        (* t2 needs parents around it iff t2 is an app type or function type *)
        let t2_string =
          match t2 with
          | AppType _ | FunctionType _ -> "(" ^ string_of_c_type t2 ^ ")"
          | _ -> string_of_c_type t2
        in
        t1_string ^ " " ^ t2_string
    | PolymorphicType (i, o) ->
        let i_string : string = string_of_c_type i in
        let o_string : string = string_of_c_type o in
        i_string ^ " -> " ^ o_string

  and string_of_c_constructor (c : c_constructor) =
    match c with
    | CNullaryConstructor name -> name
    | CUnaryConstructor (name, t) ->
        let t_string : string = string_of_c_type t in
        name ^ " " ^ t_string

  and string_of_type_var n =
    (* if n = 1, then a if n = 2, then b if n = 3, then c .... if n = 26, then z
       if n = 27, then a1 if n = 28, then b1 ....

       if it's between 1 and 26, then it's a letter if it's between 27 and 52,
       then it's a letter followed by a 1 if it's between 53 and 78, then it's a
       letter followed by a 2 if it's between 79 and 104, then it's a letter
       followed by a 3 *)
    if n <= 26 then
      (* get the letter from the index *)
      let letter = Char.chr (n + 96) in
      String.make 1 letter
    else
      (* get the index of the letter *)
      let index = n mod 26 in
      (* get the letter from the index *)
      let letter = Char.chr (index + 96) in

      let number = n / 26 in
      let number_string = string_of_int number in
      String.make 1 letter ^ number_string

  let string_of_c_expr e = string_of_c_expr e 0
  let () = ignore string_of_c_type_scheme
end
