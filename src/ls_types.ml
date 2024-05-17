type ls_type = 
| TPoly of string * ls_type
| TNonPoly of non_poly

and non_poly =
  | Variant of variant_constructor list
  | TSimple of t_simple

and variant_constructor =
  | NullaryCons of string
  | UnaryCons of string * non_poly

and t_simple =
  | TypeName of string
  | Function of t_simple * t_simple
  | VectorType of t_simple list
  | UniversalType of int