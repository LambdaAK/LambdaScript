open Types



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

type infix_op = 
  | PlusInfix
  | MinusInfix
  | MulInfix
  | DivInfix
  | ModInfix
  | AndInfix
  | OrInfix
  | LTInfix
  | GTInfix
  | LEInfix
  | GEInfix
  | EQInfix
  | NEInfix



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
