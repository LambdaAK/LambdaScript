open Expr
open New_cexpr

let rec condense_pat : pat -> c_pat = failwith "not implemented: condense_pat"

and condense_sub_pat : sub_pat -> c_pat =
  failwith "not implemented: condense_sub_pat"

let rec condense_defn : defn -> c_defn =
  failwith "not implemented: condense_defn"

and condense_expr : expr -> c_expr = failwith "not implemented: condense_expr"

and condense_cons_expr : cons_expr -> c_expr =
  failwith "not implemented: condense_cons_expr"

and condense_disjunction : disjunction -> c_expr =
  failwith "not implemented: condense_disjunction"

and condense_conjunction : conjunction -> c_expr =
  failwith "not implemented: condense_conjunction"

and condense_rel_expr : rel_expr -> c_expr =
  failwith "not implemented: condense_rel_expr"

and condense_arith_expr : arith_expr -> c_expr =
  failwith "not implemented: condense_arith_expr"

and condense_term : term -> c_expr = failwith "not implemented: condense_term"

and condense_app_factor : app_factor -> c_expr =
  failwith "not implemented: condense_app_factor"

and condense_factor : factor -> c_expr =
  failwith "not implemented: condense_factor"

(* Condense types *)

and condense_factor_type : factor_type -> mono_type =
  failwith "not implemented: condense_factor_type"

and condense_compound_type : compound_type -> mono_type =
  failwith "not implemented: condense_compound_type"

and condense_type : compound_type -> c_type =
  failwith "not implemented: condense_type"
