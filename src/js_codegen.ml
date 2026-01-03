open Cexpr

(** Minimal JavaScript code generation - bare infrastructure only *)

let runtime_helpers =
  {js|
function println(str) { console.log(str); }
function int_to_str(n) { return String(n); }

class PatternMatchError extends Error {
  constructor(msg) { super(msg); }
}

// Variant/ADT representation
class Variant {
  constructor(tag, payload = null) {
    this.tag = tag;
    this.payload = payload;
  }
}

// List helpers
function listEnum(start, end) {
  const result = [];
  for (let i = start; i <= end; i++) {
    result.push(i);
  }
  return result;
}
|js}

let var_counter = ref 0

let fresh_var () =
  var_counter := !var_counter + 1;
  "_v" ^ string_of_int !var_counter

(** Generate pattern matching code. Returns (test_code, bindings) *)
let rec gen_pattern_match (scrutinee : string) (pat : c_pat) :
    string * (string * string) list =
  match pat with
  | CIdPat id -> ("true", [ (id, scrutinee) ])
  | CWildcardPat -> ("true", [])
  | CUnitPat -> (Printf.sprintf "(%s === undefined)" scrutinee, [])
  | CIntPat n -> (Printf.sprintf "(%s === %d)" scrutinee n, [])
  | CBoolPat b ->
      ( Printf.sprintf "(%s === %s)" scrutinee (if b then "true" else "false"),
        [] )
  | CStringPat s ->
      (Printf.sprintf "(%s === \"%s\")" scrutinee (String.escaped s), [])
  | CCharPat c -> (Printf.sprintf "(%s === %d)" scrutinee (Char.code c), [])
  | CNilPat ->
      ( Printf.sprintf "(Array.isArray(%s) && %s.length === 0)" scrutinee
          scrutinee,
        [] )
  | CVectorPat pats ->
      let len = List.length pats in
      let checks =
        List.mapi
          (fun i pat ->
            let elem_access = Printf.sprintf "%s[%d]" scrutinee i in
            let test, bindings = gen_pattern_match elem_access pat in
            (test, bindings))
          pats
      in
      let all_tests = List.map fst checks in
      let all_bindings = List.concat (List.map snd checks) in
      let combined_test =
        Printf.sprintf "(Array.isArray(%s) && %s.length === %d && %s)" scrutinee
          scrutinee len
          (String.concat " && " all_tests)
      in
      (combined_test, all_bindings)
  | CConsPat (head_pat, tail_pat) ->
      let head_access = Printf.sprintf "%s[0]" scrutinee in
      let tail_access = Printf.sprintf "%s.slice(1)" scrutinee in
      let head_test, head_bindings = gen_pattern_match head_access head_pat in
      let tail_test, tail_bindings = gen_pattern_match tail_access tail_pat in
      let combined_test =
        Printf.sprintf "(Array.isArray(%s) && %s.length > 0 && %s && %s)"
          scrutinee scrutinee head_test tail_test
      in
      (combined_test, head_bindings @ tail_bindings)
  | CVariantPat (tag, None) ->
      ( Printf.sprintf
          "(%s instanceof Variant && %s.tag === '%s' && %s.payload === null)"
          scrutinee scrutinee tag scrutinee,
        [] )
  | CVariantPat (tag, Some payload_pat) ->
      let payload_access = Printf.sprintf "%s.payload" scrutinee in
      let payload_test, payload_bindings =
        gen_pattern_match payload_access payload_pat
      in
      let combined_test =
        Printf.sprintf
          "(%s instanceof Variant && %s.tag === '%s' && %s.payload !== null && \
           %s)"
          scrutinee scrutinee tag scrutinee payload_test
      in
      (combined_test, payload_bindings)

let js_bop = function
  | CPlus -> "+"
  | CMinus -> "-"
  | CMul -> "*"
  | CDiv -> "/"
  | CMod -> "%"
  | CAnd -> "&&"
  | COr -> "||"
  | CEQ -> "==="
  | CNE -> "!=="
  | CLT -> "<"
  | CGT -> ">"
  | CLE -> "<="
  | CGE -> ">="
  | CCons -> "cons" (* Special handling *)
  | CConcat -> "+"

(** Try to collect a cons chain into a list of elements. Returns Some(elements)
    if it's a complete chain ending in [], None otherwise *)
let rec collect_cons_chain (e : c_expr) : c_expr list option =
  match e with
  | ENil -> Some []
  | EBop (CCons, head, tail) -> (
      match collect_cons_chain tail with
      | Some tail_elems -> Some (head :: tail_elems)
      | None -> None)
  | _ -> None

let rec gen_expr (e : c_expr) : string =
  match e with
  | EInt n -> string_of_int n
  | EId id -> id
  | EBop (CCons, e1, e2) -> (
      (* Try to collect the full cons chain *)
      match collect_cons_chain (EBop (CCons, e1, e2)) with
      | Some elems ->
          (* Complete list literal *)
          let js_elems = List.map gen_expr elems in
          "[" ^ String.concat ", " js_elems ^ "]"
      | None ->
          (* Partial cons - concatenate arrays *)
          Printf.sprintf "[%s].concat(%s)" (gen_expr e1) (gen_expr e2))
  | EBop (op, e1, e2) ->
      let js_op = js_bop op in
      Printf.sprintf "(%s %s %s)" (gen_expr e1) js_op (gen_expr e2)
  | EApp (e1, e2) -> Printf.sprintf "%s(%s)" (gen_expr e1) (gen_expr e2)
  | EUnit -> "undefined"
  | EChar c -> Printf.sprintf "%d" (Char.code c)
  | ENil -> "[]"
  | EFunction (pat, _, body) -> (
      match pat with
      | CIdPat id ->
          (* Simple parameter *)
          Printf.sprintf "((%s) => %s)" id (gen_expr body)
      | CWildcardPat ->
          (* Wildcard parameter *)
          Printf.sprintf "((_) => %s)" (gen_expr body)
      | CUnitPat ->
          (* Unit parameter - no args *)
          Printf.sprintf "(() => %s)" (gen_expr body)
      | _ ->
          (* Complex pattern - need pattern matching *)
          let param = fresh_var () in
          let test, bindings = gen_pattern_match param pat in
          let bindings_code =
            List.map
              (fun (var, expr) -> Printf.sprintf "const %s = %s;" var expr)
              bindings
            |> String.concat " "
          in
          Printf.sprintf
            "((%s) => { if (!(%s)) throw new PatternMatchError('Pattern match \
             failed'); %s return %s; })"
            param test bindings_code (gen_expr body))
  | ETernary (cond, then_e, else_e) ->
      Printf.sprintf "(%s ? %s : %s)" (gen_expr cond) (gen_expr then_e)
        (gen_expr else_e)
  | EBool b -> if b then "true" else "false"
  | EString s -> "\"" ^ String.escaped s ^ "\""
  | EFloat f -> string_of_float f
  | EVector exprs ->
      let js_exprs = List.map gen_expr exprs in
      "[" ^ String.concat ", " js_exprs ^ "]"
  | ESwitch (scrutinee, branches) ->
      let scrutinee_var = fresh_var () in
      let rec gen_branches = function
        | [] -> "throw new PatternMatchError('No pattern matched');"
        | (pat, expr) :: rest ->
            let test, bindings = gen_pattern_match scrutinee_var pat in
            let bindings_code =
              List.map
                (fun (var, expr_str) ->
                  Printf.sprintf "const %s = %s;" var expr_str)
                bindings
              |> String.concat " "
            in
            Printf.sprintf "if (%s) { %s return %s; } else { %s }" test
              bindings_code (gen_expr expr) (gen_branches rest)
      in
      Printf.sprintf "(() => { const %s = %s; %s })()" scrutinee_var
        (gen_expr scrutinee) (gen_branches branches)
  | EListEnumeration (start_e, end_e) ->
      Printf.sprintf "listEnum(%s, %s)" (gen_expr start_e) (gen_expr end_e)
  | EListComprehension _ -> failwith "TODO: List comprehensions not implemented"
  | EBind (pat, _, e1, e2, _) -> (
      (* let pat = e1 in e2 *)
      match pat with
      | CIdPat id ->
          (* Simple variable binding *)
          Printf.sprintf "(() => { const %s = %s; return %s; })()" id
            (gen_expr e1) (gen_expr e2)
      | CUnitPat ->
          (* let () = e1 in e2 - side effect *)
          Printf.sprintf "(() => { %s; return %s; })()" (gen_expr e1)
            (gen_expr e2)
      | _ ->
          (* Pattern matching binding *)
          let value_var = fresh_var () in
          let test, bindings = gen_pattern_match value_var pat in
          let bindings_code =
            List.map
              (fun (var, expr) -> Printf.sprintf "const %s = %s;" var expr)
              bindings
            |> String.concat " "
          in
          Printf.sprintf
            "(() => { const %s = %s; if (!(%s)) throw new \
             PatternMatchError('Pattern match failed'); %s return %s; })()"
            value_var (gen_expr e1) test bindings_code (gen_expr e2))
  | EBindRec (pat, _, e1, e2, _) -> (
      (* let rec pat = e1 in e2 *)
      match pat with
      | CIdPat id ->
          (* Recursive variable binding *)
          Printf.sprintf "(() => { let %s; %s = %s; return %s; })()" id id
            (gen_expr e1) (gen_expr e2)
      | _ -> failwith "TODO: Pattern matching in recursive let bindings")
  | EBindMutRec (bindings, body) ->
      (* let rec pat1 = e1 and pat2 = e2 ... in body *)
      let declarations =
        List.map
          (fun (pat, _, _, _, _) ->
            match pat with
            | CIdPat id -> Printf.sprintf "let %s;" id
            | _ ->
                failwith
                  "TODO: Pattern matching in mutually recursive let bindings")
          bindings
        |> String.concat " "
      in
      let assignments =
        List.map
          (fun (pat, _, expr, _, _) ->
            match pat with
            | CIdPat id -> Printf.sprintf "%s = %s;" id (gen_expr expr)
            | _ ->
                failwith
                  "TODO: Pattern matching in mutually recursive let bindings")
          bindings
        |> String.concat " "
      in
      Printf.sprintf "(() => { %s %s return %s; })()" declarations assignments
        (gen_expr body)
  | _ -> failwith "TODO: Expression not implemented"

let gen_defn (d : c_defn) : string =
  match d with
  | CDefn (CIdPat id, _, expr, _, _) ->
      Printf.sprintf "const %s = %s;" id (gen_expr expr)
  | CDefn (CUnitPat, _, expr, _, _) -> Printf.sprintf "%s;" (gen_expr expr)
  | CDefnRec (CIdPat id, _, expr, _, _) ->
      (* Recursive definition - use let to allow forward reference *)
      Printf.sprintf "let %s;\n%s = %s;" id id (gen_expr expr)
  | CDefnRec _ -> failwith "TODO: Pattern matching in recursive definitions"
  | CDefnMutRec bindings ->
      (* Mutually recursive definitions *)
      let declarations =
        List.map
          (fun (pat, _, _, _, _) ->
            match pat with
            | CIdPat id -> Printf.sprintf "let %s;" id
            | _ ->
                failwith
                  "TODO: Pattern matching in mutually recursive definitions")
          bindings
        |> String.concat "\n"
      in
      let assignments =
        List.map
          (fun (pat, _, expr, _, _) ->
            match pat with
            | CIdPat id -> Printf.sprintf "%s = %s;" id (gen_expr expr)
            | _ ->
                failwith
                  "TODO: Pattern matching in mutually recursive definitions")
          bindings
        |> String.concat "\n"
      in
      Printf.sprintf "%s\n%s" declarations assignments
  | CDefn _ -> failwith "TODO: Pattern matching in definitions"
  | CTypeAlias _ ->
      (* Type aliases don't generate runtime code *)
      "// Type alias (no runtime representation)"
  | CSumType (_name, _type_params, constructors)
  | CSumTypeRec (_name, _type_params, constructors) ->
      (* Generate constructor functions for each variant *)
      let ctors =
        List.map
          (fun (ctor_name, payload_type_opt) ->
            match payload_type_opt with
            | None ->
                (* Nullary constructor - constant value *)
                Printf.sprintf "const %s = new Variant('%s', null);" ctor_name
                  ctor_name
            | Some _ ->
                (* Constructor with payload - function *)
                Printf.sprintf
                  "const %s = (payload) => new Variant('%s', payload);"
                  ctor_name ctor_name)
          constructors
        |> String.concat "\n"
      in
      ctors
  | CSumTypeRecMutRec types ->
      (* Mutually recursive sum types *)
      let all_ctors =
        List.concat_map
          (fun (_name, _type_params, constructors) ->
            List.map
              (fun (ctor_name, payload_type_opt) ->
                match payload_type_opt with
                | None ->
                    Printf.sprintf "const %s = new Variant('%s', null);"
                      ctor_name ctor_name
                | Some _ ->
                    Printf.sprintf
                      "const %s = (payload) => new Variant('%s', payload);"
                      ctor_name ctor_name)
              constructors)
          types
        |> String.concat "\n"
      in
      all_ctors

let gen_program (program : c_program) : string =
  runtime_helpers ^ "\n"
  ^ (List.map gen_defn program |> String.concat "\n")
  ^ "\n"
