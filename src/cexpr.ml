open Types
open Expr
open Typechecker

(*
type pat =
  | IdPat of string
  | NothingPat


type rel_op =
  | LT
  | GT
  | LE
  | GE

type eq_op =
  | EQ
  | NE

type expr =
  | Function of pat * compound_type option * expr
  | Ternary of expr * expr * expr
  | DisjunctionExpr of disjunction


and disjunction =
  | Disjunction of conjunction * disjunction
  | ConjunctionUnderDisjunction of conjunction


and conjunction =
  | Conjunction of eq_expr * conjunction
  | EqualityUnderConjunction of eq_expr

and eq_expr =
  | Equality of eq_op * rel_expr * eq_expr
  | RelationUnderEqExpr of rel_expr


and rel_expr =
  | Relation of rel_op * arith_expr * rel_expr
  | ArithmeticUnderRelExpr of arith_expr


and arith_expr =
  | Plus of term * arith_expr
  | Minus of term * arith_expr
  | Term of term


and term = 
  | Mul of factor * term
  | Div of factor * term
  | Mod of factor * term
  | Factor of factor


and factor =
  | Boolean of bool
  | String of string
  | Nothing
  | Integer of int
  | Id of string
  | ParenFactor of expr
  | App of factor * factor
  | Opposite of factor
*)



type c_expr =
  | EFunction of pat * t option * c_expr
  | ETernary of c_expr * c_expr * c_expr
  | EBool of bool
  | EString of string
  | ENothing
  | EInt of int
  | EId of string
  | EApp of c_expr * c_expr
  | EBop of c_bop * c_expr * c_expr


and c_bop =
  | CPlus
  | CMinus
  | CMul
  | CDiv
  | CMod
  | CEQ
  | CNE
  | CLT
  | CGT
  | CLE
  | CGE
  | CAnd
  | COr

(* condense expression *)
let rec condense_expr: expr -> c_expr =
  function
  | Function (pat, ct_opt, expr) -> EFunction (pat, 
  (
  match ct_opt with
  | None -> None
  | Some ct -> Some (condense_type ct)
  )
  
  , condense_expr expr)
  | Ternary (e1, e2, e3) -> ETernary (condense_expr e1, condense_expr e2, condense_expr e3)
  | DisjunctionExpr disj -> condense_disjunction disj

and condense_disjunction: disjunction -> c_expr =
  function
  | Disjunction (conj, disj) -> EBop (COr, condense_conjunction conj, condense_disjunction disj)
  | ConjunctionUnderDisjunction conj -> condense_conjunction conj

and condense_conjunction: conjunction -> c_expr =
  function
  | Conjunction (eq_expr, conj) -> EBop (CAnd, condense_eq_expr eq_expr, condense_conjunction conj)
  | EqualityUnderConjunction eq_expr -> condense_eq_expr eq_expr

and condense_eq_expr: eq_expr -> c_expr =
  function
  | Equality (eq_op, rel_expr, eq_expr) ->
    begin
      match eq_op with
      | EQ -> EBop (CEQ, condense_rel_expr rel_expr, condense_eq_expr eq_expr)
      | NE -> EBop (CNE, condense_rel_expr rel_expr, condense_eq_expr eq_expr)
    end
  | RelationUnderEqExpr rel_expr -> condense_rel_expr rel_expr


and condense_rel_expr: rel_expr -> c_expr =
  function
  | Relation (rel_op, arith_expr, rel_expr) ->
    begin
      match rel_op with
      | LT -> EBop (CLT, condense_arith_expr arith_expr, condense_rel_expr rel_expr)
      | GT -> EBop (CGT, condense_arith_expr arith_expr, condense_rel_expr rel_expr)
      | LE -> EBop (CLE, condense_arith_expr arith_expr, condense_rel_expr rel_expr)
      | GE -> EBop (CGE, condense_arith_expr arith_expr, condense_rel_expr rel_expr)
    end
  | ArithmeticUnderRelExpr arith_expr -> condense_arith_expr arith_expr

and condense_arith_expr: arith_expr -> c_expr =
  function
  | Plus (term, arith_expr) -> EBop (CPlus, condense_term term, condense_arith_expr arith_expr)
  | Minus (term, arith_expr) -> EBop (CMinus, condense_term term, condense_arith_expr arith_expr)
  | Term term -> condense_term term

and condense_term: term -> c_expr =
  function
  | Mul (factor, term) -> EBop (CMul, condense_factor factor, condense_term term)
  | Div (factor, term) -> EBop (CDiv, condense_factor factor, condense_term term)
  | Mod (factor, term) -> EBop (CMod, condense_factor factor, condense_term term)
  | Factor factor -> condense_factor factor

and condense_factor: factor -> c_expr =
  function
  | Boolean b -> EBool b
  | String s -> EString s
  | Nothing -> ENothing
  | Integer i -> EInt i
  | Id s -> EId s
  | ParenFactor expr -> condense_expr expr
  | App (factor1, factor2) -> EApp (condense_factor factor1, condense_factor factor2)
  | Opposite factor -> EBop (CMinus, EInt 0, condense_factor factor)

and factor_type_to_t: factor_type -> Typechecker.t =
  function
  | BooleanType -> Typechecker.BoolType
  | StringType -> Typechecker.StringType
  | NothingType -> Typechecker.NothingType
  | IntegerType -> Typechecker.IntType
  | ParenFactorType expr -> condense_type expr


and condense_type: compound_type -> Typechecker.t =
  function
  | BasicType bt -> factor_type_to_t bt
  | FunctionType (i, o) ->
    FunctionType (factor_type_to_t i, condense_type o)

(* to string functions *)


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
  | Var _ -> "type_var"