open Expr

let indentations (level : int) = String.make (2 * level) ' '
let indentations_with_newline (level : int) = "\n" ^ indentations level

let rec string_of_pat : pat -> string = function
  | SubPat sub_pat -> string_of_sub_pat sub_pat
  | ConsPat (p1, p2) ->
      "Cons Pattern ("
      ^ indentations_with_newline 1
      ^ string_of_sub_pat p1 ^ ","
      ^ indentations_with_newline 1
      ^ string_of_pat p2
      ^ indentations_with_newline 0
      ^ ")"

and string_of_sub_pat : sub_pat -> string = function
  | UnitPat -> "Unit Pattern"
  | WildcardPat -> "Wildcard Pattern"
  | IdPat s -> "Id Pattern (" ^ s ^ ")"
  | NilPat -> "Nil Pattern"
  | VectorPat patterns ->
      "Vector Pattern ("
      ^ indentations_with_newline 1
      ^ String.concat
          (",\n" ^ indentations_with_newline 1)
          (List.map string_of_pat patterns)
      ^ indentations_with_newline 0
      ^ ")"
  | RecordPat fields ->
      "Record Pattern ({"
      ^ indentations_with_newline 1
      ^ String.concat
          (",\n" ^ indentations_with_newline 1)
          (List.map (fun (name, p) -> name ^ ": " ^ string_of_pat p) fields)
      ^ indentations_with_newline 0
      ^ "})"
  | IntPat n -> "Int Pattern (" ^ string_of_int n ^ ")"
  | BoolPat b -> "Bool Pattern (" ^ string_of_bool b ^ ")"
  | CharPat c -> "Char Pattern (" ^ String.make 1 c ^ ")"
  | StringPat s -> "String Pattern (" ^ s ^ ")"
  | Pat p -> string_of_pat p
  | InfixPat s -> "Infix Pattern (" ^ s ^ ")"
  | VariantPat (name, payload_opt) -> (
      match payload_opt with
      | None -> "Variant Pattern (" ^ name ^ ")"
      | Some p -> "Variant Pattern (" ^ name ^ ", " ^ string_of_pat p ^ ")")

let string_of_rel_op : rel_op -> string = function
  | EQ -> "EQ"
  | NE -> "NE"
  | LT -> "LT"
  | GT -> "GT"
  | LE -> "LE"
  | GE -> "GE"

let rec string_of_basic_type (ft : factor_type) (level : int) : string =
  match ft with
  | IntegerType -> "IntegerType"
  | BooleanType -> "BooleanType"
  | StringType -> "StringType"
  | CharType -> "CharType"
  | UnitType -> "UnitType"
  | ParenFactorType c -> string_of_compound_type c level
  | VectorType types ->
      "VectorType ("
      ^ indentations_with_newline (level + 1)
      ^ String.concat
          (",\n" ^ indentations_with_newline (level + 1))
          (List.map (fun t -> string_of_compound_type t (level + 1)) types)
      ^ indentations_with_newline level
      ^ ")"
  | ListType t ->
      "ListType ("
      ^ indentations_with_newline (level + 1)
      ^ string_of_compound_type t (level + 1)
      ^ indentations_with_newline level
      ^ ")"
  | FloatType -> "FloatType"
  | TypeVarWritten v -> "TypeVarWritten (" ^ v ^ ")"
  | TypeName v -> "TypeName (" ^ v ^ ")"
  | TypeApp (name, args) ->
      "TypeApp (" ^ name ^ ", ["
      ^ indentations_with_newline (level + 1)
      ^ String.concat
          (",\n" ^ indentations_with_newline (level + 1))
          (List.map (fun t -> string_of_compound_type t (level + 1)) args)
      ^ indentations_with_newline level
      ^ "] )"
  | RecordTypeWritten fields ->
      "RecordTypeWritten ({"
      ^ indentations_with_newline (level + 1)
      ^ String.concat
          (",\n" ^ indentations_with_newline (level + 1))
          (List.map (fun (name, t) -> name ^ ": " ^ string_of_compound_type t (level + 1)) fields)
      ^ indentations_with_newline level
      ^ "})"

and string_of_compound_type (ct : compound_type) (level : int) =
  match ct with
  | BasicType t -> string_of_basic_type t level
  | FunctionType (t1, t2) ->
      "FunctionType ("
      ^ indentations_with_newline (level + 1)
      ^ string_of_basic_type t1 level
      ^ ","
      ^ indentations_with_newline (level + 1)
      ^ string_of_compound_type t2 (level + 1)
      ^ indentations_with_newline level
      ^ ")"

let rec string_of_expr (e : expr) (level : int) : string =
  match e with
  | Ternary (e1, e2, e3) ->
      let e1_string : string = string_of_expr e1 (level + 1) in
      let e2_string : string = string_of_expr e2 (level + 1) in
      let e3_string : string = string_of_expr e3 (level + 1) in

      "Ternary ("
      ^ indentations_with_newline (level + 1)
      ^ e1_string ^ ","
      ^ indentations_with_newline (level + 1)
      ^ e2_string ^ ","
      ^ indentations_with_newline (level + 1)
      ^ e3_string
      ^ indentations_with_newline level
      ^ ")"
  | Block parts ->
      let parts_string =
        String.concat
          (",\n" ^ indentations_with_newline (level + 1))
          (List.map
             (function
               | Expr e -> "Expr (" ^ string_of_expr e (level + 1) ^ ")"
               | Definition d ->
                   "Definition (" ^ string_of_defn d (level + 1) ^ ")")
             parts)
      in
      "Block (\n"
      ^ indentations_with_newline (level + 1)
      ^ parts_string
      ^ indentations_with_newline level
      ^ ")"
  | Switch (e, branches) ->
      let e_string : string = string_of_expr e (level + 1) in
      let branches_string : string =
        String.concat
          (",\n" ^ indentations_with_newline (level + 1))
          (List.map
             (fun (p, e) ->
               indentations_with_newline (level + 1)
               ^ string_of_pat p ^ ",\n"
               ^ indentations_with_newline (level + 1)
               ^ string_of_expr e (level + 1))
             branches)
      in

      "Switch ("
      ^ indentations_with_newline (level + 1)
      ^ e_string ^ ","
      ^ indentations_with_newline (level + 1)
      ^ branches_string
      ^ indentations_with_newline level
      ^ ")"
  | Function (pattern, cto, body) ->
      let pattern_string : string = string_of_pat pattern in
      let body_string : string = string_of_expr body (level + 1) in

      "Function ("
      ^ indentations_with_newline (level + 1)
      ^ pattern_string ^ ","
      ^ (match cto with
        | None -> ""
        | Some ct ->
            (* add the type annotation *)
            let string_of_ct : string =
              string_of_compound_type ct (level + 1)
            in
            indentations_with_newline (level + 1) ^ string_of_ct ^ ",")
      ^ indentations_with_newline (level + 1)
      ^ body_string
      ^ indentations_with_newline level
      ^ ")"
  | ConsExpr e ->
      "ConsExpr ("
      ^ indentations_with_newline (level + 1)
      ^ string_of_cons_expr e (level + 1)
      ^ indentations_with_newline level
      ^ ")"
  | Bind (p, cto, e1, e2, return_type) ->
      let p_string : string = string_of_pat p in
      let e1_string : string = string_of_expr e1 (level + 1) in
      let e2_string : string = string_of_expr e2 (level + 1) in

      "Bind ("
      ^ indentations_with_newline (level + 1)
      ^ p_string ^ ","
      ^ (match cto with
        | None -> ""
        | Some ct ->
            (* add the type annotation *)
            let string_of_ct : string =
              string_of_compound_type ct (level + 1)
            in
            indentations_with_newline (level + 1) ^ string_of_ct ^ ",")
      ^ indentations_with_newline (level + 1)
      ^ e1_string ^ ","
      ^ indentations_with_newline (level + 1)
      ^ e2_string ^ ","
      ^ (match return_type with
        | None -> ""
        | Some ct ->
            let string_of_ct : string =
              string_of_compound_type ct (level + 1)
            in
            indentations_with_newline (level + 1) ^ string_of_ct)
      ^ indentations_with_newline level
      ^ ")"
  | BindRec (p, cto, e1, e2, return_type) ->
      let p_string : string = string_of_pat p in
      let e1_string : string = string_of_expr e1 (level + 1) in
      let e2_string : string = string_of_expr e2 (level + 1) in

      "BindRec ("
      ^ indentations_with_newline (level + 1)
      ^ p_string ^ ","
      ^ (match cto with
        | None -> ""
        | Some ct ->
            (* add the type annotation *)
            let string_of_ct : string =
              string_of_compound_type ct (level + 1)
            in
            indentations_with_newline (level + 1) ^ string_of_ct ^ ",")
      ^ indentations_with_newline (level + 1)
      ^ e1_string ^ ","
      ^ indentations_with_newline (level + 1)
      ^ e2_string ^ ","
      ^ (match return_type with
        | None -> ""
        | Some ct ->
            let string_of_ct : string =
              string_of_compound_type ct (level + 1)
            in
            indentations_with_newline (level + 1) ^ string_of_ct)
      ^ indentations_with_newline level
      ^ ")"
  | BindMutRec (bindings, body) ->
      let bindings_str = String.concat "\nand " (List.map (fun (p, _, e, _, _) ->
        string_of_pat p ^ " = " ^ string_of_expr e level
      ) bindings) in
      let body_str = string_of_expr body (level + 1) in
      "BindMutRec (" ^ indentations_with_newline (level + 1)
      ^ bindings_str
      ^ indentations_with_newline (level + 1)
      ^ body_str
      ^ indentations_with_newline level
      ^ ")"

and string_of_cons_expr (ce : cons_expr) (level : int) : string =
  match ce with
  | Cons (e1, e2) ->
      "Cons ("
      ^ indentations_with_newline (level + 1)
      ^ string_of_disjunction e1 (level + 1)
      ^ ","
      ^ indentations_with_newline (level + 1)
      ^ string_of_cons_expr e2 (level + 1)
      ^ indentations_with_newline level
      ^ ")"
  | DisjunctionUnderCons d -> string_of_disjunction d level
  | Pipeline (l, r) ->
      "Pipeline ("
      ^ indentations_with_newline (level + 1)
      ^ string_of_cons_expr l (level + 1)
      ^ ","
      ^ indentations_with_newline (level + 1)
      ^ string_of_cons_expr r (level + 1)
      ^ indentations_with_newline level
      ^ ")"

and string_of_disjunction (d : disjunction) (level : int) : string =
  match d with
  | ConjunctionUnderDisjunction c -> string_of_conjunction c level
  | Disjunction (c, d) ->
      "Disjunction ("
      ^ indentations_with_newline (level + 1)
      ^ string_of_conjunction c (level + 1)
      ^ ","
      ^ indentations_with_newline (level + 1)
      ^ string_of_disjunction d (level + 1)
      ^ indentations_with_newline level
      ^ ")"

and string_of_conjunction (c : conjunction) (level : int) : string =
  match c with
  | RelationUnderConjunction r -> string_of_rel_expr r level
  | Conjunction (r, c) ->
      "Conjunction ("
      ^ indentations_with_newline (level + 1)
      ^ string_of_rel_expr r (level + 1)
      ^ ","
      ^ indentations_with_newline (level + 1)
      ^ string_of_conjunction c (level + 1)
      ^ indentations_with_newline level
      ^ ")"

and string_of_rel_expr (re : rel_expr) (level : int) : string =
  match re with
  | Relation (op, re, ae) ->
      "Relation ("
      ^ indentations_with_newline (level + 1)
      ^ string_of_rel_op op ^ ","
      ^ indentations_with_newline (level + 1)
      ^ string_of_rel_expr re (level + 1)
      ^ ","
      ^ indentations_with_newline (level + 1)
      ^ string_of_arith_expr ae (level + 1)
      ^ indentations_with_newline level
      ^ ")"
  | CustomRelExpr (op_string, re, ae) ->
      "CustomRelExpr ("
      ^ indentations_with_newline (level + 1)
      ^ op_string ^ ","
      ^ indentations_with_newline (level + 1)
      ^ string_of_rel_expr re (level + 1)
      ^ ","
      ^ indentations_with_newline (level + 1)
      ^ string_of_arith_expr ae (level + 1)
      ^ indentations_with_newline level
      ^ ")"
  | ArithmeticUnderRelExpr ae -> string_of_arith_expr ae level

and string_of_arith_expr (ae : arith_expr) (level : int) =
  match ae with
  | Plus (e, t) ->
      "Plus ("
      ^ indentations_with_newline (level + 1)
      ^ string_of_arith_expr e (level + 1)
      ^ ","
      ^ indentations_with_newline (level + 1)
      ^ string_of_arith_term t (level + 1)
      ^ indentations_with_newline level
      ^ ")"
  | Minus (e, t) ->
      "Minus ("
      ^ indentations_with_newline (level + 1)
      ^ string_of_arith_expr e (level + 1)
      ^ ","
      ^ indentations_with_newline (level + 1)
      ^ string_of_arith_term t (level + 1)
      ^ indentations_with_newline level
      ^ ")"
  | Term t -> string_of_arith_term t (level + 1)
  | CustomArithExpr (op_string, ae, t) ->
      "CustomArithExpr ("
      ^ indentations_with_newline (level + 1)
      ^ op_string ^ ","
      ^ indentations_with_newline (level + 1)
      ^ string_of_arith_expr ae (level + 1)
      ^ ","
      ^ indentations_with_newline (level + 1)
      ^ string_of_arith_term t (level + 1)
      ^ indentations_with_newline level
      ^ ")"

and string_of_arith_term (at : term) (level : int) =
  match at with
  | Mul (f, af) ->
      "Mul ("
      ^ indentations_with_newline (level + 1)
      ^ string_of_arith_term f (level + 1)
      ^ ","
      ^ indentations_with_newline (level + 1)
      ^ string_of_app_factor af (level + 1)
      ^ indentations_with_newline level
      ^ ")"
  | Div (f, af) ->
      "Div ("
      ^ indentations_with_newline (level + 1)
      ^ string_of_arith_term f (level + 1)
      ^ ","
      ^ indentations_with_newline (level + 1)
      ^ string_of_app_factor af (level + 1)
      ^ indentations_with_newline level
      ^ ")"
  | Mod (f, af) ->
      "Mod ("
      ^ indentations_with_newline (level + 1)
      ^ string_of_arith_term f (level + 1)
      ^ ","
      ^ indentations_with_newline (level + 1)
      ^ string_of_app_factor af (level + 1)
      ^ indentations_with_newline level
      ^ ")"
  | Factor af -> string_of_app_factor af (level + 1)
  | CustomTerm (op_string, t, af) ->
      "CustomTerm ("
      ^ indentations_with_newline (level + 1)
      ^ op_string ^ ","
      ^ indentations_with_newline (level + 1)
      ^ string_of_arith_term t (level + 1)
      ^ ","
      ^ indentations_with_newline (level + 1)
      ^ string_of_app_factor af (level + 1)
      ^ indentations_with_newline level
      ^ ")"

and string_of_app_factor (af : app_factor) (level : int) =
  match af with
  | Application (ap, f) ->
      "App ("
      ^ indentations_with_newline (level + 1)
      ^ string_of_app_factor ap (level + 1)
      ^ ","
      ^ indentations_with_newline (level + 1)
      ^ string_of_factor f (level + 1)
      ^ indentations_with_newline level
      ^ ")"
  | FactorUnderApplication f -> string_of_factor f (level + 1)

and string_of_factor (factor : factor) (level : int) =
  match factor with
  | Boolean b -> "Boolean (" ^ string_of_bool b ^ ")"
  | Integer n -> "Integer (" ^ string_of_int n ^ ")"
  | String s -> "String (" ^ s ^ ")"
  | Char c -> "Char (" ^ String.make 1 c ^ ")"
  | Unit -> "Unit"
  | Vector es ->
      "Vector ("
      ^ indentations_with_newline (level + 1)
      ^ String.concat
          (",\n" ^ indentations_with_newline (level + 1))
          (List.map (fun e -> string_of_expr e (level + 1)) es)
      ^ indentations_with_newline level
      ^ ")"
  | Id s -> "Id (" ^ s ^ ")"
  | ParenFactor e ->
      "ParenFactor ("
      ^ indentations_with_newline (level + 1)
      ^ string_of_expr e (level + 1)
      ^ indentations_with_newline level
      ^ ")"
  | Opposite f ->
      "Opposite ("
      ^ indentations_with_newline (level + 1)
      ^ string_of_factor f (level + 1)
      ^ indentations_with_newline level
      ^ ")"
  | ListEnumeration (e1, e2) ->
      "ListEnumeration ("
      ^ indentations_with_newline (level + 1)
      ^ string_of_expr e1 (level + 1)
      ^ ","
      ^ indentations_with_newline (level + 1)
      ^ string_of_expr e2 (level + 1)
      ^ indentations_with_newline level
      ^ ")"
  | ListComprehension (e1, gen_list) ->
      "ListComprehension ("
      ^ indentations_with_newline (level + 1)
      ^ string_of_expr e1 (level + 1)
      ^ ","
      ^ indentations_with_newline (level + 1)
      ^ String.concat
          (",\n" ^ indentations_with_newline (level + 1))
          (List.map
             (fun (p, e) ->
               string_of_pat p ^ ",\n"
               ^ indentations_with_newline (level + 1)
               ^ string_of_expr e (level + 1))
             gen_list)
      ^ indentations_with_newline level
      ^ ")"
  | Nil -> "Nil"
  | FloatFactor f -> "FloatFactor (" ^ string_of_float f ^ ")"
  | ListSugar es ->
      "ListSugar ("
      ^ String.concat
          (",\n" ^ indentations_with_newline (level + 1))
          (List.map (fun e -> string_of_expr e (level + 1)) es)
      ^ ")"
  | RecordLit fields ->
      "RecordLit ("
      ^ String.concat
          (",\n" ^ indentations_with_newline (level + 1))
          (List.map (fun (name, e) ->
            name ^ ": " ^ string_of_expr e (level + 1)) fields)
      ^ ")"
  | RecordUpdate (record_expr, updates) ->
      "RecordUpdate ("
      ^ string_of_expr record_expr (level + 1)
      ^ " with "
      ^ String.concat
          (",\n" ^ indentations_with_newline (level + 1))
          (List.map (fun (name, e) ->
            name ^ " = " ^ string_of_expr e (level + 1)) updates)
      ^ ")"
  | FieldAccess (f, field_name) ->
      "FieldAccess ("
      ^ string_of_factor f (level + 1)
      ^ ", " ^ field_name ^ ")"
  | MacroInvoke (name, args) ->
      "MacroInvoke (" ^ name ^ ", ["
      ^ String.concat ", " (List.map string_of_macro_tt args)
      ^ "])"

and string_of_trait_item (item : trait_item) (level : int) : string =
  match item with
  | TraitVal (m, ct) ->
      "TraitVal (val " ^ m ^ " : " ^ string_of_compound_type ct (level + 1) ^ ")"
  | TraitLet (m, e) ->
      "TraitLet (" ^ m ^ ", " ^ string_of_expr e (level + 1) ^ ")"

and string_of_macro_delim : macro_delim -> string = function
  | MacroParen -> "()"
  | MacroBracket -> "[]"
  | MacroBrace -> "{}"

and string_of_macro_tt : macro_tt -> string = function
  | MacroTTToken tok -> Lex.string_of_token_type tok
  | MacroTTGroup (delim, inner) ->
      "Group"
      ^ string_of_macro_delim delim
      ^ "(["
      ^ String.concat ", " (List.map string_of_macro_tt inner)
      ^ "])"

and string_of_defn (d : defn) (level : int) =
  let cto_string cto =
    match cto with
    | None -> ""
    | Some ct ->
        indentations_with_newline (level + 1)
        ^ string_of_compound_type ct (level + 1)
        ^ ","
  in
  match d with
  | Defn (p, _cs, cto, e, return_type, _) ->
      "Defn ("
      ^ indentations_with_newline (level + 1)
      ^ string_of_pat p ^ "," ^ cto_string cto
      ^ indentations_with_newline (level + 1)
      ^ string_of_expr e (level + 1)
      ^ "," ^ cto_string return_type
      ^ ")"
  | DefnRec (p, _cs, cto, e, return_type, _) ->
      "DefnRec ("
      ^ indentations_with_newline (level + 1)
      ^ string_of_pat p ^ "," ^ cto_string cto
      ^ indentations_with_newline (level + 1)
      ^ string_of_expr e (level + 1)
      ^ "," ^ cto_string return_type
      ^ ")"
  | DefnMutRec defns ->
      let defns_str = String.concat "\nand " (List.map (fun (p, _cs, cto, e, _, _) ->
        string_of_pat p ^ cto_string cto ^ " = " ^ string_of_expr e level
      ) defns) in
      "DefnMutRec (" ^ indentations_with_newline (level + 1)
      ^ defns_str
      ^ indentations_with_newline level
      ^ ")"
  | TypeDef (name, args, ct) ->
      "TypeDef (" ^ name
      ^ (if args = [] then "" else ", [" ^ String.concat ", " args ^ "]")
      ^ ","
      ^ indentations_with_newline (level + 1)
      ^ string_of_compound_type ct (level + 1)
      ^ ")"
  | SumTypeDef (name, args, constructors) ->
      "SumTypeDef (" ^ name
      ^ (if args = [] then "" else ", [" ^ String.concat ", " args ^ "]")
      ^ ", ["
      ^ String.concat ", "
          (List.map
             (fun (cons_name, payload_type_opt) ->
               match payload_type_opt with
               | None -> cons_name
               | Some ct ->
                   cons_name ^ " of " ^ string_of_compound_type ct (level + 1))
             constructors)
      ^ "])"
  | SumTypeDefRec (name, args, constructors) ->
      "SumTypeDefRec (" ^ name
      ^ (if args = [] then "" else ", [" ^ String.concat ", " args ^ "]")
      ^ ", ["
      ^ String.concat ", "
          (List.map
             (fun (cons_name, payload_type_opt) ->
               match payload_type_opt with
               | None -> cons_name
               | Some ct ->
                   cons_name ^ " of " ^ string_of_compound_type ct (level + 1))
             constructors)
      ^ "])"
  | SumTypeDefMutRec types ->
      "SumTypeDefMutRec (["
      ^ String.concat ", "
          (List.map
             (fun (name, args, constructors) ->
               "(" ^ name
               ^ (if args = [] then "" else ", [" ^ String.concat ", " args ^ "]")
               ^ ", ["
               ^ String.concat ", "
                   (List.map
                      (fun (cons_name, payload_type_opt) ->
                        match payload_type_opt with
                        | None -> cons_name
                        | Some ct ->
                            cons_name ^ " of " ^ string_of_compound_type ct (level + 1))
                      constructors)
               ^ "])")
             types)
      ^ "])"
  | ClassDef (name, args, requires, items) ->
      "ClassDef ("
      ^ name
      ^ (if args = [] then ""
         else
           ", ["
           ^ String.concat ", "
               (List.map
                  (fun (a, k) ->
                    a ^ if k >= 0 then "(" ^ string_of_int k ^ ")" else "(infer)")
                  args)
           ^ "]")
      ^ ", requires ["
      ^ String.concat ", "
          (List.map
             (fun (c, ct) ->
               c ^ " " ^ string_of_compound_type ct (level + 1))
             requires)
      ^ "], ["
      ^ String.concat ", "
          (List.map (fun it -> string_of_trait_item it (level + 1)) items)
      ^ "])"
  | InstanceDef (cls, head_ty, requires, impls) ->
      "InstanceDef ("
      ^ cls
      ^ ", "
      ^ string_of_compound_type head_ty (level + 1)
      ^ ", requires ["
      ^ String.concat ", "
          (List.map
             (fun (c, ct) ->
               c ^ " " ^ string_of_compound_type ct (level + 1))
             requires)
      ^ "]"
      ^ ", ["
      ^ String.concat ", "
          (List.map
          (fun (m, e) -> m ^ " = " ^ string_of_expr e (level + 1))
             impls)
      ^ "])"
  | ModDef (name, defs) ->
      "ModDef (" ^ name ^ ", ["
      ^ String.concat ", " (List.map (fun d -> string_of_defn d (level + 1)) defs)
      ^ "])"
  | MacroDef (name, arms) ->
      "MacroDef (" ^ name ^ ", ["
      ^ String.concat "; "
          (List.map
             (fun (matcher, transcriber) ->
               "["
               ^ String.concat ", " (List.map string_of_macro_tt matcher)
               ^ "] => ["
               ^ String.concat ", " (List.map string_of_macro_tt transcriber)
               ^ "]")
             arms)
      ^ "])"
  | UseDef path ->
      "UseDef (" ^ String.concat "." path ^ ")"
  | ImportDef path ->
      "ImportDef (" ^ path ^ ")"

let string_of_expr (e : expr) = string_of_expr e 0
