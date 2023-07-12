open Expr

let indentations (level: int) = String.make (2 * level) ' '

let indentations_with_newline (level: int) = "\n" ^ (indentations level)


let rec string_of_pat: pat -> string =
  function
  | NothingPat -> "Nothing Pattern"
  | WildcardPat -> "Wildcard Pattern"
  | IdPat s -> "Id Pattern (" ^ s ^ ")"
  | VectorPat patterns ->
    "Vector Pattern ("
    ^ indentations_with_newline 1
    ^ (String.concat (",\n" ^ indentations_with_newline 1) (List.map string_of_pat patterns))
    ^ indentations_with_newline 0
    ^ ")"
  | IntPat n -> "Int Pattern (" ^ (string_of_int n) ^ ")"
  | BoolPat b -> "Bool Pattern (" ^ (string_of_bool b) ^ ")"
  | StringPat s -> "String Pattern (" ^ s ^ ")"


let string_of_rel_op: rel_op -> string =
  function
  | LT -> "LT"
  | GT -> "GT"
  | LE -> "LE"
  | GE -> "GE"

let string_of_eq_op: eq_op -> string =
  function
  | EQ -> "EQ"
  | NE -> "NE"


let rec string_of_basic_type (ft: factor_type) (level: int): string =
  match ft with
  | IntegerType -> "IntegerType"
  | BooleanType -> "BooleanType"
  | StringType -> "StringType"
  | NothingType -> "NothingType"
  | ParenFactorType c ->
    string_of_compound_type c level
  | VectorType types ->
    "VectorType ("
    ^ indentations_with_newline (level + 1)
    ^ (String.concat (",\n" ^ indentations_with_newline (level + 1)) (List.map (fun t -> string_of_compound_type t (level + 1)) types))
    ^ indentations_with_newline level
    ^ ")"
  | _ -> "Not implemented yet"




and string_of_compound_type (ct: compound_type) (level: int) =
  match ct with
  | BasicType t ->
    string_of_basic_type t level
  | FunctionType (t1, t2) ->
    "FunctionType ("
    ^ indentations_with_newline (level + 1)
    ^ string_of_basic_type t1 level
    ^ ","
    ^ indentations_with_newline (level + 1)
    ^ (string_of_compound_type t2 (level + 1))
    ^ indentations_with_newline level
    ^ ")"


let rec string_of_expr (e: expr) (level: int): string = match e with
| Ternary (e1, e2, e3) ->

  let e1_string: string = string_of_expr e1 (level + 1) in
  let e2_string: string = string_of_expr e2 (level + 1) in
  let e3_string: string = string_of_expr e3 (level + 1) in


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
| Switch (e, branches) ->
  let e_string: string = string_of_expr e (level + 1) in
  let branches_string: string = String.concat (",\n" ^ indentations_with_newline (level + 1)) (List.map (fun (p, e) -> indentations_with_newline (level + 1) ^ string_of_pat p ^ ",\n" ^ indentations_with_newline (level + 1) ^ string_of_expr e (level + 1)) branches) in

  "Switch ("
  ^ indentations_with_newline (level + 1)
  ^ e_string
  ^ ","
  ^ indentations_with_newline (level + 1)
  ^ branches_string
  ^ indentations_with_newline level
  ^ ")"

| Function (pattern, cto, body) ->
  let pattern_string: string = string_of_pat pattern in
  let body_string: string = string_of_expr body (level + 1) in
  
  "Function ("
  ^ indentations_with_newline (level + 1)
  ^ pattern_string
  ^ ","
  ^ (
    match cto with
    | None -> ""
    | Some ct ->
      (* add the type annotation *)
      let string_of_ct: string = string_of_compound_type ct (level + 1) in
      indentations_with_newline (level + 1)
      ^ string_of_ct
      ^ ","
    )
  ^ indentations_with_newline (level + 1)
  ^ body_string
  ^ indentations_with_newline level
  ^ ")"

| ConsExpr e ->
  "ConsExpr ("
  ^ indentations_with_newline (level + 1)
  ^ (string_of_cons_expr e (level + 1))
  ^ indentations_with_newline level
  ^ ")"

| BindRec (p, cto, e1, e2) ->
  let p_string: string = string_of_pat p in
  let e1_string: string = string_of_expr e1 (level + 1) in
  let e2_string: string = string_of_expr e2 (level + 1) in

  "BindRec ("
  ^ indentations_with_newline (level + 1)
  ^ p_string
  ^ ","
  ^ (
    match cto with
    | None -> ""
    | Some ct ->
      (* add the type annotation *)
      let string_of_ct: string = string_of_compound_type ct (level + 1) in
      indentations_with_newline (level + 1)
      ^ string_of_ct
      ^ ","
    )
  ^ indentations_with_newline (level + 1)
  ^ e1_string
  ^ ","
  ^ indentations_with_newline (level + 1)
  ^ e2_string
  ^ indentations_with_newline level
  ^ ")"


and string_of_cons_expr (ce: cons_expr) (level: int): string =
  match ce with
  | Cons (e1, e2) ->
    "Cons ("
    ^ indentations_with_newline (level + 1)
    ^ (string_of_disjunction e1 (level + 1))
    ^ ","
    ^ indentations_with_newline (level + 1)
    ^ (string_of_cons_expr e2 (level + 1))
    ^ indentations_with_newline level
    ^ ")"
  | DisjunctionUnderCons d ->
    string_of_disjunction d level


and string_of_disjunction (d: disjunction) (level: int): string =
  match d with
  | ConjunctionUnderDisjunction c ->
    string_of_conjunction c level
  | Disjunction (c, d) ->
    "Disjunction ("
    ^ indentations_with_newline (level + 1)
    ^ (string_of_conjunction c (level + 1))
    ^ ","
    ^ indentations_with_newline (level + 1)
    ^ (string_of_disjunction d (level + 1))
    ^ indentations_with_newline level
    ^ ")"



and string_of_conjunction (c: conjunction) (level: int): string =
  match c with
  | EqualityUnderConjunction e ->
    string_of_eq_expr e level
  | Conjunction (e, c) ->
    "Conjunction ("
    ^ indentations_with_newline (level + 1)
    ^ (string_of_eq_expr e (level + 1))
    ^ ","
    ^ indentations_with_newline (level + 1)
    ^ (string_of_conjunction c (level + 1))
    ^ indentations_with_newline level
    ^ ")"



and string_of_eq_expr (ee: eq_expr) (level: int): string =
  match ee with
  | RelationUnderEqExpr re ->
    string_of_rel_expr re level
  | Equality (op, re, ee) ->
    "Equality ("
    ^ indentations_with_newline (level + 1)
    ^ (string_of_eq_op op)
    ^ ","
    ^ indentations_with_newline (level + 1)
    ^ (string_of_rel_expr re (level + 1))
    ^ ","
    ^ indentations_with_newline (level + 1)
    ^ (string_of_eq_expr ee (level + 1))
    ^ indentations_with_newline level
    ^ ")"



and string_of_rel_expr (re: rel_expr) (level: int): string =
  match re with
  | ArithmeticUnderRelExpr ae ->
    string_of_arith_expr ae level
  | Relation (op, ae, re) ->
    "Relation ("
    ^ indentations_with_newline (level + 1)
    ^ (string_of_rel_op op)
    ^ ","
    ^ indentations_with_newline (level + 1)
    ^ (string_of_arith_expr ae (level + 1))
    ^ ","
    ^ indentations_with_newline (level + 1)
    ^ (string_of_rel_expr re (level + 1))
    ^ indentations_with_newline level
    ^ ")"


and string_of_arith_expr (ae: arith_expr) (level: int) =
match ae with
| Plus (t, e) ->
  "Plus ("
  ^ indentations_with_newline (level + 1)
  ^ (string_of_arith_term t (level + 1))
  ^ ","
  ^ indentations_with_newline (level + 1)
  ^ (string_of_arith_expr e (level + 1))
  ^ indentations_with_newline level
  ^ ")"

| Minus (t, e) ->
  "Minus ("
  ^ indentations_with_newline (level + 1)
  ^ (string_of_arith_term t (level + 1))
  ^ ","
  ^ indentations_with_newline (level + 1)
  ^ (string_of_arith_expr e (level + 1))
  ^ indentations_with_newline level
  ^ ")"

| Term t ->
  string_of_arith_term t (level + 1)


and string_of_arith_term (at: term) (level: int) =
match at with
| Mul (f, t) ->
  "Mul ("
  ^ indentations_with_newline (level + 1)
  ^ (string_of_arith_term f (level + 1))
  ^ ","
  ^ indentations_with_newline (level + 1)
  ^ (string_of_arith_factor t (level + 1))
  ^ indentations_with_newline level
  ^ ")"
| Div (f, t) ->
  "Div ("
  ^ indentations_with_newline (level + 1)
  ^ (string_of_arith_term f (level + 1))
  ^ ","
  ^ indentations_with_newline (level + 1)
  ^ (string_of_arith_factor t (level + 1))
  ^ indentations_with_newline level
  ^ ")"
| Mod (f, t) ->
  "Mod ("
  ^ indentations_with_newline (level + 1)
  ^ (string_of_arith_term f (level + 1))
  ^ ","
  ^ indentations_with_newline (level + 1)
  ^ (string_of_arith_factor t (level + 1))
  ^ indentations_with_newline level
  ^ ")"
| Factor f ->
  string_of_arith_factor f (level + 1)



and string_of_arith_factor (af: factor) (level: int) =
match af with
| Integer n ->
  "Integer (" ^ (string_of_int n) ^ ")"
| Boolean b ->
  "Boolean (" ^ (string_of_bool b) ^ ")"
| String s ->
  "String (" ^ s ^ ")"
| Nothing ->
  "Nothing"
| Id s ->
  "Id (" ^ s ^ ")"
| ParenFactor e ->
  "Paren ("
  ^ indentations_with_newline (level + 1)
  ^ (string_of_expr e (level + 1))
  ^ indentations_with_newline level
  ^ ")"
| App (e1, e2) ->
  "App ("
  ^ indentations_with_newline (level + 1)
  ^ (string_of_arith_factor e1 (level + 1))
  ^ ","
  ^ indentations_with_newline (level + 1)
  ^ (string_of_arith_factor e2 (level + 1))
  ^ indentations_with_newline level
  ^ ")"
| Opposite f ->
  "Opposite ("
  ^ indentations_with_newline (level + 1)
  ^ string_of_arith_factor f (level + 1)
  ^ indentations_with_newline level
  ^ ")"
| Vector expressions ->
  "Vector ("
  ^ indentations_with_newline (level + 1)
  ^ (String.concat (",\n" ^ indentations_with_newline (level + 1)) (List.map (fun e -> string_of_expr e (level + 1)) expressions))
  ^ indentations_with_newline level
  ^ ")"
| Nil -> "Nil"

let string_of_expr (e: expr) = string_of_expr e 0

