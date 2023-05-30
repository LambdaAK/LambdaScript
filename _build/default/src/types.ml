type compound_type =
  | FunctionType of factor_type * compound_type
  | BasicType of factor_type


and factor_type =
  | IntegerType
  | StringType
  | BooleanType
  | NothingType
  | ParenFactorType of compound_type