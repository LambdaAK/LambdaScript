type expr =
| IntegerValue of int
| BooleanValue of bool
| StringValue of string
| NothingValue
| Id of string
| App of expr * expr
| Ternary of expr * expr * expr
