open Cexpr

module type CToString = sig
  val string_of_c_expr : c_expr -> string
  (** string_of_c_expr returns a string representation of the given c_expr *)

  val string_of_c_defn : c_defn -> string
  (** string_of_c_defn returns a string representation of the given c_defn *)

  (** string_of_c_type returns a string representation of the given c_type *)

  val string_of_c_type : c_type -> string

  (** string_of_c_pat returns a string representation of the given c_pat *)

  val string_of_c_pat : c_pat -> string

  val string_of_c_kind : c_kind -> string
  (** string_of_c_kind returns a string representation of the given c_kind *)
end
