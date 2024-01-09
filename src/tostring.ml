open Expr

let indentations (level : int) = String.make (2 * level) ' '
let indentations_with_newline (level : int) = "\n" ^ indentations level

(** * This module is used to print the AST in a readable format. *)
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
  | AppPat (sp1, sp2) ->
      let sp1_string : string = string_of_sub_pat sp1 in
      let sp2_string : string = string_of_sub_pat sp2 in
      "App Pattern ("
      ^ indentations_with_newline 1
      ^ sp1_string ^ ","
      ^ indentations_with_newline 1
      ^ sp2_string
      ^ indentations_with_newline 0
      ^ ")"

(** * This function is used to print the sub-patterns. * It is used by
    [string_of_pat] to print the sub-patterns. *)
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
  | IntPat n -> "Int Pattern (" ^ string_of_int n ^ ")"
  | BoolPat b -> "Bool Pattern (" ^ string_of_bool b ^ ")"
  | StringPat s -> "String Pattern (" ^ s ^ ")"
  | Pat p -> string_of_pat p
  | InfixPat s -> "Infix Pattern (" ^ s ^ ")"
  | ConstructorPat n -> "Constructor Pattern (" ^ n ^ ")"

(** [string_of_rel_op] is used to print the relational operators. *)
let string_of_rel_op : rel_op -> string = function
  | EQ -> "EQ"
  | NE -> "NE"
  | LT -> "LT"
  | GT -> "GT"
  | LE -> "LE"
  | GE -> "GE"

(** * This function is used to print the types. *)
let rec string_of_basic_type (ft : factor_type) (level : int) : string =
  match ft with
  | IntegerType -> "IntegerType"
  | BooleanType -> "BooleanType"
  | StringType -> "StringType"
  | UnitType -> "UnitType"
  | FloatType -> "FloatType"
  | TypeVarWritten s -> "TypeVarWritten (" ^ s ^ ")"
  | ParenFactorType c -> string_of_compound_type c level
  | VectorType types ->
      "VectorType ("
      ^ indentations_with_newline (level + 1)
      ^ String.concat
          (",\n" ^ indentations_with_newline (level + 1))
          (List.map (fun t -> string_of_compound_type t (level + 1)) types)
      ^ indentations_with_newline level
      ^ ")"
  | TypeName n -> "TypeName (" ^ n ^ ")"
  | ListType t -> "ListType (" ^ string_of_compound_type t (level + 1) ^ ")"

(** [string_of_factor_app_type] is used to print the factor application types. *)
and string_of_factor_app_type (fat : factor_app_type) (level : int) : string =
  match fat with
  | FactorType ft -> string_of_basic_type ft level
  | AppType (fat, ft) ->
      "AppType ("
      ^ indentations_with_newline (level + 1)
      ^ string_of_factor_app_type fat (level + 1)
      ^ ","
      ^ indentations_with_newline (level + 1)
      ^ string_of_basic_type ft (level + 1)
      ^ indentations_with_newline level
      ^ ")"

(** [string_of_compound_type] is used to print the compound types. *)
and string_of_compound_type (ct : compound_type) (level : int) =
  match ct with
  | BasicType t -> string_of_factor_app_type t level
  | UnionType constructors ->
      "UnionType ("
      ^ indentations_with_newline (level + 1)
      ^ String.concat
          (",\n" ^ indentations_with_newline (level + 1))
          (List.map (fun c -> string_of_constructor c) constructors)
      ^ indentations_with_newline level
      ^ ")"
  | FunctionType (fat, ct) ->
      "FunctionType ("
      ^ indentations_with_newline (level + 1)
      ^ string_of_factor_app_type fat (level + 1)
      ^ ","
      ^ indentations_with_newline (level + 1)
      ^ string_of_compound_type ct (level + 1)
      ^ indentations_with_newline level
      ^ ")"
  | PolymorphicType (s, ct) ->
      "PolymorphicType ("
      ^ indentations_with_newline (level + 1)
      ^ s ^ ","
      ^ indentations_with_newline (level + 1)
      ^ string_of_compound_type ct (level + 1)
      ^ indentations_with_newline level
      ^ ")"

(** [string_of_constructor] is used to print the constructors. *)
and string_of_constructor (c : constructor) =
  match c with
  | NullaryConstructor name -> name
  | UnaryConstructor (name, t) ->
      name ^ " (" ^ string_of_compound_type t 0 ^ ")"

(** [string_of_expr] is used to print the expressions. *)
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
  | BindRec (p, cto, e1, e2) ->
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
      ^ e2_string
      ^ indentations_with_newline level
      ^ ")"

(** [string_of_cons_expr] is used to print the cons expressions. *)
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

(** [string_of_disjunction] is used to print the disjunctions. *)
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

(** [string_of_conjunction] is used to print the conjunctions. *)
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

(** [string_of_rel_expr] is used to print the relational expressions. *)
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

(** [string_of_arith_expr] is used to print the arithmetic expressions. *)
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

(** [string_of_arith_term] is used to print the arithmetic terms. *)
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

(** [string_of_app_factor] is used to print the application factors. It is used
    by [string_of_arith_term] to print the application factors. *)
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

(** [string_of_factor] is used to print the factors. It is used by
    [string_of_app_factor] to print the factors. *)
and string_of_factor (factor : factor) (level : int) =
  match factor with
  | Boolean b -> "Boolean (" ^ string_of_bool b ^ ")"
  | Integer n -> "Integer (" ^ string_of_int n ^ ")"
  | String s -> "String (" ^ s ^ ")"
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
  | Constructor n -> "Constructor (" ^ n ^ ")"

(** [string_of_expr e] is a pretty printed string representing [e] *)
let string_of_expr (e : expr) = string_of_expr e 0
