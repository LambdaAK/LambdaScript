open Cexpr
open Ctostring

[@@@coverage off]

module CToStringCode : CToString = struct
  let string_of_c_expr (_ : c_expr) = ""
  let string_of_c_type (_ : c_type) = ""
  let string_of_c_pat (_ : c_pat) = ""
end
