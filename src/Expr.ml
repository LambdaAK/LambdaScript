

(** pat represents a pattern *)
type pat =
  | ConsPat of sub_pat * pat
  | SubPat of sub_pat
  | AppPat of sub_pat * sub_pat
(* the left sub_pat is (what should be ) a constructor the right sub_pat is the
   argument of the constructor *)

and sub_pat =
  | IdPat of string
  | UnitPat
  | VectorPat of pat list
  | WildcardPat
  | IntPat of int
  | StringPat of string
  | BoolPat of bool
  | NilPat
  | ConstructorPat of string
  | Pat of pat
  | InfixPat of string

(** rel_op represents a built-in relational operator *)
and rel_op =
  | EQ
  | NE
  | LT
  | GT
  | LE
  | GE

(** compound_type represents a type *)
and compound_type =
  | FunctionType of factor_app_type * compound_type
  | BasicType of factor_app_type
  | UnionType of union_type
  | PolymorphicType of string * compound_type
(* string is the argument, compound_type is the body (can be another polymorphic
   type) *)

and union_type = constructor list
(** union_type represents a union type, which is just a list of constructors *)

(** factor_app_type represents either a factor type, or the application of one
    factor_type to another factor_type *)
and factor_app_type =
  | AppType of factor_app_type * factor_type
  | FactorType of factor_type

(** factor_type represents a type that is not an application of one type to
    another

    - TypeName of string represents a type that is a type name written in the
      source code
    - TypeVarWritten of string represents a type variable written in the source
      code *)
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

(** defn represents a definition
    - Defn represents a non-recursive definition
    - DefnRec represents a recursive definition
    - TypeDefn represents a type alias definition
    - UnionDefn represents a union type definition *)
and defn =
  | Defn of pat * compound_type option * expr
  | DefnRec of pat * compound_type option * expr
  | TypeDefn of string * compound_type * string list
    (* string is the name, compound_type is the type itself, and string list is
       the list of polymorphic arguments, left to right *)
  | UnionDefn of
      string * union_type * string list (* string list is the variables *)

and switch_branch = pat * expr
(** switch_branch represents a branch of a switch expression *)

(** expr represents an expression
    - Function represents a \p -> e
    - Ternary represents if e1 then e2 else e3
    - ConsExpr represents e1 :: e2
    - BindRec represents let rec p = e1 in e2
    - Switch represents switch e => p1 -> e1 | p2 -> e2 | ... | pn -> en end *)
and expr =
  | Function of pat * compound_type option * expr
  | Ternary of expr * expr * expr
  | ConsExpr of cons_expr
  | BindRec of pat * compound_type option * expr * expr
  | Switch of expr * switch_branch list

(** constructor represents a constructor for a union type
    - NullaryConstructor represents a nullary constructor (no arguments)
    - UnaryConstructor represents a unary constructor (one argument) *)
and constructor =
  | NullaryConstructor of string
  | UnaryConstructor of
      string * compound_type (* represents a variant type constructor *)

(** cons_expr represents either
    - a cons expression (e1 :: e2)
    - something of higher precedence (e1) *)
and cons_expr =
  (* :: *)
  | Cons of disjunction * cons_expr
  | DisjunctionUnderCons of disjunction

(** disjunction represents either
    - a disjunction (e1 || e2)
    - something of higher precedence (e1) *)
and disjunction =
  (* || *)
  | Disjunction of conjunction * disjunction
  | ConjunctionUnderDisjunction of conjunction

(** conjunction represents either
    - a conjunction (e1 && e2)
    - something of higher precedence (e1) *)
and conjunction =
  (* && *)
  | Conjunction of rel_expr * conjunction
  | RelationUnderConjunction of rel_expr

(** rel_expr represents either
    - a relational expression (e1 < e2)
    - something of higher precedence (e1) *)
and rel_expr =
  (* =... <... >... *)
  | Relation of rel_op * rel_expr * arith_expr
  | CustomRelExpr of string * rel_expr * arith_expr
  | ArithmeticUnderRelExpr of arith_expr

(** arith_expr represents either
    - an arithmetic expression (e1 + e2)
    - something of higher precedence (e1) *)
and arith_expr =
  (* +... -... *)
  | Plus of arith_expr * term
  | Minus of arith_expr * term
  | CustomArithExpr of string * arith_expr * term
  | Term of term

(** term represents either
    - a term (e1 * e2)
    - something of higher precedence (e1) *)
and term =
  (* *... /... %... *)
  | Mul of term * app_factor
  | Div of term * app_factor
  | Mod of term * app_factor
  | CustomTerm of string * term * app_factor
  | Factor of app_factor

(** app_factor represents either
    - an application of a app_factor to factor (e1 e2)
    - something of higher precedence (e1) *)
and app_factor =
  | Application of app_factor * factor
  | FactorUnderApplication of factor

(** factor contains the base cases for the expression tree *)
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
(** generator represents a generator in a list comprehension *)

and program = defn list
(** program represents a program. It is just a list of definitions*)
