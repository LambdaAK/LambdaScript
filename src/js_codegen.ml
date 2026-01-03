open Cexpr

(** Minimal JavaScript code generation - bare infrastructure only *)

let runtime_helpers =
  {js|
function println(str) { console.log(str); }
function int_to_str(n) { return String(n); }

class PatternMatchError extends Error {
  constructor(msg) { super(msg); }
}

// List representation
class Cons {
  constructor(head, tail) {
    this.head = head;
    this.tail = tail;
  }
}

const nil = { type: 'nil' };

// List helpers
function listToString(lst) {
  const arr = [];
  let current = lst;
  while (current instanceof Cons) {
    arr.push(current.head);
    current = current.tail;
  }
  return '[' + arr.join(', ') + ']';
}

function listEnum(start, end) {
  let result = nil;
  for (let i = end; i >= start; i--) {
    result = new Cons(i, result);
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
  | CNilPat -> (Printf.sprintf "(%s?.type === 'nil')" scrutinee, [])
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
      let head_access = Printf.sprintf "%s.head" scrutinee in
      let tail_access = Printf.sprintf "%s.tail" scrutinee in
      let head_test, head_bindings = gen_pattern_match head_access head_pat in
      let tail_test, tail_bindings = gen_pattern_match tail_access tail_pat in
      let combined_test =
        Printf.sprintf "(%s instanceof Cons && %s && %s)" scrutinee head_test
          tail_test
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

let rec gen_expr (e : c_expr) : string =
  match e with
  | EInt n -> string_of_int n
  | EId id -> id
  | EBop (CCons, e1, e2) ->
      (* Special handling for cons operator *)
      Printf.sprintf "(new Cons(%s, %s))" (gen_expr e1) (gen_expr e2)
  | EBop (op, e1, e2) ->
      let js_op = js_bop op in
      Printf.sprintf "(%s %s %s)" (gen_expr e1) js_op (gen_expr e2)
  | EApp (e1, e2) -> Printf.sprintf "%s(%s)" (gen_expr e1) (gen_expr e2)
  | EUnit -> "undefined"
  | EChar c -> Printf.sprintf "%d" (Char.code c)
  | ENil -> "nil"
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
  | _ -> failwith "TODO: Type definitions not implemented"

let gen_program (program : c_program) : string =
  runtime_helpers ^ "\n"
  ^ (List.map gen_defn program |> String.concat "\n")
  ^ "\n"
