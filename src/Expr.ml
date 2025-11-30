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
  | VariantPat of string * pat option

type rel_op =
  | EQ
  | NE
  | LT
  | GT
  | LE
  | GE

(* Type applications

   There is type currying, but, there is no partial application A type always
   gets fully evaluated.

   In the representation for a polymorphic type, it is not recursive. It is
   parameterized by a list of type variables of fixed length

   Syntax for type application is `pair<t1, t2>`, where t1 and t2 are
   mono_type. *)

type compound_type =
  | FunctionType of factor_type * compound_type
  | BasicType of factor_type

and factor_type =
  | IntegerType
  | StringType
  | BooleanType
  | UnitType
  | FloatType
  | TypeVarWritten of string (* 'a, 'b, etc. *)
  | TypeName of string (* t, u, etc. *)
  | ParenFactorType of compound_type
  | VectorType of compound_type list
  | ListType of compound_type
  | TypeApp of string * compound_type list

type defn =
  | Defn of pat * compound_type option * expr
  | DefnRec of pat * compound_type option * expr
  | TypeDef of string * string list * compound_type
  | SumTypeDef of string * string list * (string * compound_type option) list
  | SumTypeDefRec of string * string list * (string * compound_type option) list

and switch_branch = pat * expr

and expr_or_defn =
  | Expr of expr
  | Definition of defn

and expr =
  | Function of pat * compound_type option * expr
  | Ternary of expr * expr * expr
  | ConsExpr of cons_expr
  | Bind of pat * compound_type option * expr * expr
  | BindRec of pat * compound_type option * expr * expr
  | Switch of expr * switch_branch list
  | Block of expr_or_defn list

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
  | ParenFactor of expr
  | Opposite of factor
  | Vector of expr list
  | Nil
  | ListSugar of expr list (* list represents a list literal like [1;2;3;4;5] *)
  | ListEnumeration of expr * expr
  | ListComprehension of expr * generator list

and generator = pat * expr

type program = defn list
