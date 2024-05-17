type ls_type =
  | Fixed of poly
  | Finite of poly

and poly =
  | Poly
  | NonPoly of non_poly

and non_poly =
  | TInt
  | TUnit
  | TFunction of non_poly * non_poly
  | TypeName of string
  | TVector of non_poly list
  | TList of non_poly
  | TApp of non_poly * non_poly
  | TUniversal of int
