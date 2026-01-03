open Cexpr

(** Minimal JavaScript code generation - bare infrastructure only *)

let runtime_helpers =
  {js|
function println(str) { console.log(str); }
function int_to_str(n) { return String(n); }
|js}

let rec gen_expr (e : c_expr) : string =
  match e with
  | EInt n -> string_of_int n
  | EId id -> id
  | EBop (CPlus, e1, e2) ->
      Printf.sprintf "(%s + %s)" (gen_expr e1) (gen_expr e2)
  | EApp (e1, e2) -> Printf.sprintf "%s(%s)" (gen_expr e1) (gen_expr e2)
  | _ -> failwith "TODO: Not implemented"

let gen_defn (d : c_defn) : string =
  match d with
  | CDefn (CIdPat id, _, expr, _, _) ->
      Printf.sprintf "const %s = %s;" id (gen_expr expr)
  | CDefn (CUnitPat, _, expr, _, _) -> Printf.sprintf "%s;" (gen_expr expr)
  | _ -> failwith "TODO: Not implemented"

let gen_program (program : c_program) : string =
  runtime_helpers ^ "\n"
  ^ (List.map gen_defn program |> String.concat "\n")
  ^ "\n"
