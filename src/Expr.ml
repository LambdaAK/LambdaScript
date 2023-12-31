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
  | InfixPat of string

and rel_op =
  | EQ
  | NE
  | LT
  | GT
  | LE
  | GE

and compound_type =
  | FunctionType of factor_type * compound_type
  | BasicType of factor_type
  | UnionType of union_type

and union_type = constructor list

and factor_type =
  | IntegerType
  | StringType
  | BooleanType
  | UnitType
  | FloatType
  | TypeName of string
  | TypeVarWritten of string
  | ParenFactorType of compound_type (* identifier for a type *)
  | VectorType of compound_type list
  | ListType of compound_type

and defn =
  | Defn of pat * compound_type option * expr
  | DefnRec of pat * compound_type option * expr
  | TypeDefn of string * compound_type
  | UnionDefn of string * union_type

and switch_branch = pat * expr

and expr =
  | Function of pat * compound_type option * expr
  | Ternary of expr * expr * expr
  | ConsExpr of cons_expr
  | BindRec of pat * compound_type option * expr * expr
  | Switch of expr * switch_branch list

and constructor =
  | NullaryConstructor of string
  | UnaryConstructor of
      string * compound_type (* represents a variant type constructor *)

and cons_expr =
  (* :: *)
  | Cons of disjunction * cons_expr
  | DisjunctionUnderCons of disjunction

and disjunction =
  (* || *)
  | Disjunction of conjunction * disjunction
  | ConjunctionUnderDisjunction of conjunction

and conjunction =
  (* && *)
  | Conjunction of rel_expr * conjunction
  | RelationUnderConjunction of rel_expr

and rel_expr =
  (* =... <... >... *)
  | Relation of rel_op * rel_expr * arith_expr
  | CustomRelExpr of string * rel_expr * arith_expr
  | ArithmeticUnderRelExpr of arith_expr

and arith_expr =
  (* +... -... *)
  | Plus of arith_expr * term
  | Minus of arith_expr * term
  | CustomArithExpr of string * arith_expr * term
  | Term of term

and term =
  (* *... /... %... *)
  | Mul of term * app_factor
  | Div of term * app_factor
  | Mod of term * app_factor
  | CustomTerm of string * term * app_factor
  | Factor of app_factor

and app_factor =
  | Application of app_factor * factor
  | FactorUnderApplication of factor

and factor =
  | Boolean of bool
  | String of string
  | Unit
  | Integer of int
  | FloatFactor of float
  | Id of string
  | Constructor of string
  | ParenFactor of expr
  | Opposite of factor
  | Vector of expr list
  | Nil
  | ListSugar of expr list (* list represents a list literal like [1;2;3;4;5] *)
  | ListEnumeration of expr * expr
  | ListComprehension of expr * generator list

and generator = pat * expr
and program = defn list
