open Cexpr

module type CToString = sig
  val string_of_c_expr : c_expr -> string
  val string_of_c_defn : c_defn -> string
  val string_of_c_type : c_type -> string
  val string_of_c_pat : c_pat -> string
  val string_of_c_kind : c_kind -> string
end
