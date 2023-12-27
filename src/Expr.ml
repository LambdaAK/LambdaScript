type pat =
  | ConsPat of sub_pat * pat
  | SubPat of sub_pat

and sub_pat =
  | IdPat of string
  | UnitPat
  | VectorPat of pat list
  | WildcardPat
  | IntPat of int
  | StringPat of string
  | BoolPat of bool
  | NilPat
  | Pat of pat

type rel_op =
  | LT
  | GT
  | LE
  | GE

type eq_op =
  | EQ
  | NE

type compound_type =
  | FunctionType of factor_type * compound_type
  | BasicType of factor_type

and factor_type =
  | IntegerType
  | StringType
  | BooleanType
  | UnitType
  | FloatType
  | TypeVarWritten of string
  | ParenFactorType of compound_type
  | VectorType of compound_type list
  | ListType of compound_type

type defn =
  | Defn of pat * compound_type option * expr
  | DefnRec of pat * compound_type option * expr

and switch_branch = pat * expr

and expr =
  | Function of pat * compound_type option * expr
  | Ternary of expr * expr * expr
  | ConsExpr of cons_expr
  | BindRec of pat * compound_type option * expr * expr
  | Switch of expr * switch_branch list

and cons_expr =
  | Cons of disjunction * cons_expr
  | DisjunctionUnderCons of disjunction

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
  | Mul of term * factor
  | Div of term * factor
  | Mod of term * factor
  | Factor of factor

and factor =
  | Boolean of bool
  | String of string
  | Unit
  | Integer of int
  | FloatFactor of float
  | Id of string
  | ParenFactor of expr
  | App of factor * factor
  | Opposite of factor
  | Vector of expr list
  | Nil
  | ListSugar of expr list (* list represents a list literal like [1;2;3;4;5] *)
  | ListEnumeration of expr * expr
  | ListComprehension of expr * generator list

and generator = pat * expr

type program = defn list
