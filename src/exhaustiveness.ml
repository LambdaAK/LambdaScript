(** Exhaustiveness and redundancy of [case] expressions.

    The algorithm is Maranget's ("Warnings for pattern matching", JFP 2007):
    a vector of patterns is {e useful} with respect to a matrix of rows if
    some value matches it and no row. A [case] is exhaustive when a wildcard
    is not useful after all its arms, and an arm is redundant when it is not
    useful after the arms before it.

    Patterns are checked at the scrutinee's type (from the type checker):
    [Bool], [Unit], lists, tuples, records and sum types have finitely many
    constructors; integers, strings, characters and floats do not, so a match
    on them is exhaustive only with a catch-all arm. Both back ends report
    the problems as warnings; a value no arm matches is a run-time error. *)

open Cexpr
module T = Typecheck

type pat =
  | Wild
  | Con of string * pat list  (** constructor and arguments *)
  | Lit of string  (** a literal of a type with infinitely many values *)

type ctx = { static_env : static_env; ctor_env : T.constructor_env; type_env : T.type_env }

(* ------------------------------------------------------------------------ *)
(* Types                                                                    *)
(* ------------------------------------------------------------------------ *)

let rec shape (cx : ctx) (t : mono_type) : mono_type =
  match t with
  | FixedPoint (n, b) -> (
      match Mono.fix_args n b with Some args -> CTypeApp (n, args) | None -> CTypeApp (n, []))
  | TypeName n -> expand cx n []
  | CTypeApp ("List", [ a ]) -> CListType a
  | CTypeApp (n, args) -> expand cx n args
  | RecordType fs ->
      let fs = List.filter (fun (f, _) -> f <> row_field) (T.flatten_record_fields fs) in
      RecordType (List.sort (fun (a, _) (b, _) -> compare a b) fs)
  | t -> t

and expand cx name args =
  match List.find_opt (fun (n, _, _) -> String.equal n name) cx.type_env with
  | Some (_, params, def) when (not (Mono.is_sum_def name def)) && List.length params = List.length args ->
      shape cx (Mono.subst_named (List.combine params args) def)
  | _ -> CTypeApp (name, args)

let record_ctor (fields : string list) : string = "{" ^ String.concat "," fields ^ "}"

(** Payload type of constructor [c] in a value of type [t]. *)
let payload_ty (cx : ctx) (c : string) (t : mono_type) : mono_type =
  match List.assoc_opt c cx.static_env with
  | Some sch -> (
      let _, m = Mono.scheme_parts sch in
      match m with
      | FunctionType (p, result) ->
          let theta = Hashtbl.create 4 in
          Mono.match_ty theta result t;
          Mono.resolve theta p
      | _ -> TypeVar "?")
  | None -> TypeVar "?"

(** The constructors of [t] with their argument types, or [None] if [t] has
    infinitely many values (or is unknown). *)
let ctors_of (cx : ctx) (t : mono_type) : (string * mono_type list) list option =
  match shape cx t with
  | BoolType -> Some [ ("true", []); ("false", []) ]
  | UnitType -> Some [ ("()", []) ]
  | CListType a -> Some [ ("[]", []); ("::", [ a; CListType a ]) ]
  | VectorType ts -> Some [ ("(,)", ts) ]
  | RecordType fs -> Some [ (record_ctor (List.map fst fs), List.map snd fs) ]
  | CTypeApp (n, _) as t' -> (
      match List.find_opt (fun (n', _, _) -> String.equal n n') cx.ctor_env with
      | Some (_, _, cs) ->
          Some (List.map (fun (c, p) -> (c, match p with None -> [] | Some _ -> [ payload_ty cx c t' ])) cs)
      | None -> None)
  | _ -> None

(** Arguments of constructor [c] at type [t]. *)
let ctor_args (cx : ctx) (t : mono_type) (c : string) (arity : int) : mono_type list =
  match ctors_of cx t with
  | Some cs -> ( match List.assoc_opt c cs with Some ts -> ts | None -> List.init arity (fun _ -> TypeVar "?"))
  | None -> List.init arity (fun _ -> TypeVar "?")

(* ------------------------------------------------------------------------ *)
(* Patterns                                                                 *)
(* ------------------------------------------------------------------------ *)

let rec conv (cx : ctx) (t : mono_type) (p : c_pat) : pat =
  match p with
  | CIdPat _ | CWildcardPat -> Wild
  | CUnitPat -> Con ("()", [])
  | CBoolPat b -> Con (string_of_bool b, [])
  | CNilPat | CVariantPat ("[]", None) -> Con ("[]", [])
  | CConsPat (a, b) | CVariantPat ("::", Some (CVectorPat [ a; b ])) -> (
      match ctor_args cx t "::" 2 with
      | [ ta; tl ] -> Con ("::", [ conv cx ta a; conv cx tl b ])
      | _ -> Con ("::", [ conv cx (TypeVar "?") a; conv cx (TypeVar "?") b ]))
  | CVectorPat ps ->
      let ts = ctor_args cx t "(,)" (List.length ps) in
      let ts = if List.length ts = List.length ps then ts else List.map (fun _ -> TypeVar "?") ps in
      Con ("(,)", List.map2 (conv cx) ts ps)
  | CRecordPat fs -> (
      match shape cx t with
      | RecordType all ->
          Con
            ( record_ctor (List.map fst all),
              List.map
                (fun (f, ft) -> match List.assoc_opt f fs with Some q -> conv cx ft q | None -> Wild)
                all )
      | _ ->
          let fs = List.sort (fun (a, _) (b, _) -> compare a b) fs in
          Con (record_ctor (List.map fst fs), List.map (fun (_, q) -> conv cx (TypeVar "?") q) fs))
  | CVariantPat (c, None) -> Con (c, [])
  | CVariantPat (c, Some q) -> Con (c, [ conv cx (payload_ty cx c t) q ])
  | CIntPat n -> Lit (string_of_int n)
  | CStringPat s -> Lit (Printf.sprintf "%S" s)
  | CCharPat c -> Lit (Printf.sprintf "%C" c)

let rec show (p : pat) : string =
  let atom p =
    match p with
    | Con (_, _ :: _) when (match p with Con ("(,)", _) -> false | Con (c, _) -> c.[0] <> '{' | _ -> true) ->
        "(" ^ show p ^ ")"
    | _ -> show p
  in
  match p with
  | Wild -> "_"
  | Lit l -> l
  | Con ("::", [ a; b ]) -> atom a ^ " :: " ^ show b
  | Con ("(,)", ps) -> "(" ^ String.concat ", " (List.map show ps) ^ ")"
  | Con (c, ps) when String.length c > 0 && c.[0] = '{' ->
      let names = String.split_on_char ',' (String.sub c 1 (String.length c - 2)) in
      let shown = List.filter (fun (_, q) -> q <> Wild) (List.combine names ps) in
      if shown = [] then "{ ... }"
      else "{ " ^ String.concat ", " (List.map (fun (f, q) -> f ^ ": " ^ show q) shown) ^ " }"
  | Con (c, []) -> c
  | Con (c, ps) -> c ^ " " ^ String.concat " " (List.map atom ps)

(* ------------------------------------------------------------------------ *)
(* The algorithm                                                            *)
(* ------------------------------------------------------------------------ *)

let wilds n = List.init n (fun _ -> Wild)

let rec split n xs =
  if n = 0 then ([], xs) else match xs with [] -> ([], []) | x :: r -> let a, b = split (n - 1) r in (x :: a, b)

let specialize (c : string) (n : int) (rows : pat list list) : pat list list =
  List.filter_map
    (function
      | Con (c', args) :: rest when String.equal c c' -> Some (args @ rest)
      | Wild :: rest -> Some (wilds n @ rest)
      | _ -> None)
    rows

let specialize_lit (l : string) (rows : pat list list) : pat list list =
  List.filter_map
    (function Lit l' :: rest when String.equal l l' -> Some rest | Wild :: rest -> Some rest | _ -> None)
    rows

let default (rows : pat list list) : pat list list =
  List.filter_map (function Wild :: rest -> Some rest | _ -> None) rows

let head_ctors (rows : pat list list) : string list =
  List.sort_uniq compare (List.filter_map (function Con (c, _) :: _ -> Some c | _ -> None) rows)

(** The type of a column: [t], or what the constructors in it say when [t]
    is not known. *)
let column_type (cx : ctx) (t : mono_type) (rows : pat list list) : mono_type =
  match (shape cx t, rows) with
  | (TypeVar _ | TCtorApp _), _ -> (
      match List.find_map (function Con (c, args) :: _ -> Some (c, args) | _ -> None) rows with
      | Some (("true" | "false"), _) -> BoolType
      | Some ("()", _) -> UnitType
      | Some (("[]" | "::"), _) -> CListType (TypeVar "?")
      | Some ("(,)", args) -> VectorType (List.map (fun _ -> TypeVar "?") args)
      | Some (c, _) -> (
          match List.assoc_opt c cx.static_env with
          | Some sch -> (
              let _, m = Mono.scheme_parts sch in
              match m with FunctionType (_, r) -> r | r -> r)
          | None -> t)
      | None -> t)
  | t', _ -> t'

let complete (sigma : string list) (cs : (string * mono_type list) list) : bool =
  sigma <> [] && List.for_all (fun (c, _) -> List.mem c sigma) cs

let rec useful (cx : ctx) (rows : pat list list) (q : pat list) (tys : mono_type list) : bool =
  match (q, tys) with
  | [], _ -> rows = []
  | _, [] -> rows = []
  | q1 :: qs, t :: ts -> (
      let t = column_type cx t (([ q1 ] @ qs) :: rows) in
      match q1 with
      | Con (c, args) ->
          useful cx (specialize c (List.length args) rows) (args @ qs) (ctor_args cx t c (List.length args) @ ts)
      | Lit l -> useful cx (specialize_lit l rows) qs ts
      | Wild -> (
          let sigma = head_ctors rows in
          match ctors_of cx t with
          | Some cs when complete sigma cs ->
              List.exists
                (fun (c, atys) ->
                  let n = List.length atys in
                  useful cx (specialize c n rows) (wilds n @ qs) (atys @ ts))
                cs
          | _ -> useful cx (default rows) qs ts))

(** A vector of patterns that no row matches, if any. *)
let rec witness (cx : ctx) (rows : pat list list) (tys : mono_type list) : pat list option =
  match tys with
  | [] -> if rows = [] then Some [] else None
  | t :: ts -> (
      let t = column_type cx t rows in
      let sigma = head_ctors rows in
      match ctors_of cx t with
      | Some cs when complete sigma cs ->
          List.find_map
            (fun (c, atys) ->
              let n = List.length atys in
              match witness cx (specialize c n rows) (atys @ ts) with
              | Some w ->
                  let args, rest = split n w in
                  Some (Con (c, args) :: rest)
              | None -> None)
            cs
      | ctors -> (
          match witness cx (default rows) ts with
          | None -> None
          | Some w ->
              let head =
                match ctors with
                | Some cs when sigma <> [] -> (
                    match List.find_opt (fun (c, _) -> not (List.mem c sigma)) cs with
                    | Some (c, atys) -> Con (c, wilds (List.length atys))
                    | None -> Wild)
                | _ -> Wild
              in
              Some (head :: w)))

(* ------------------------------------------------------------------------ *)
(* Programs                                                                 *)
(* ------------------------------------------------------------------------ *)

type warning = { offset : int option; message : string }

(** Byte offset of the first located identifier in [e]. *)
let rec first_offset (e : c_expr) : int option =
  let first l = List.find_map first_offset l in
  match e with
  | EId (_, Some (s, _)) -> Some s
  | EId (_, None) | EInt _ | EFloat _ | EBool _ | EString _ | EChar _ | EUnit | ENil -> None
  | EApp (a, b) | EBop (_, a, b) | EListEnumeration (a, b) -> first [ a; b ]
  | EFunction (_, _, b) | EFieldAccess (b, _) -> first_offset b
  | EBind (_, _, a, b, _) | EBindRec (_, _, a, b, _) -> first [ a; b ]
  | EBindMutRec (bs, b) -> first (List.map (fun (_, _, e, _, _) -> e) bs @ [ b ])
  | EBlock _ -> None
  | ETernary (a, b, c) -> first [ a; b; c ]
  | ESwitch (s, brs) -> first (s :: List.map snd brs)
  | EVector es -> first es
  | EListComprehension (b, gs) -> first (b :: List.map snd gs)
  | ERecordLit fs -> first (List.map snd fs)
  | ERecordUpdate (r, fs) -> first (r :: List.map snd fs)

(** Check the [case] expressions of a definition that was just type-checked
    with {!Typecheck.type_logging} on ({!Typecheck.last_defn_equations} hold
    its equations). *)
let check_defn (cx : ctx) (d : c_defn) : warning list =
  let sol = Mono.solution !T.last_defn_equations in
  let warnings = ref [] in
  let check_switch (node : c_expr) (scr : c_expr) (brs : c_switch_branch list) =
    let t =
      match T.NodeTbl.find_opt T.node_types scr with
      | Some t when scr <> ENil && scr <> EUnit -> Mono.resolve sol t
      | _ -> TypeVar "?"
    in
    let rows = List.map (fun (p, _) -> [ conv cx t p ]) brs in
    let offset = first_offset node in
    let add message = warnings := { offset; message } :: !warnings in
    List.iteri
      (fun i row ->
        let before = List.filteri (fun j _ -> j < i) rows in
        if not (useful cx before row [ t ]) then
          add (Printf.sprintf "redundant case arm: `%s` can never match" (show (List.hd row))))
      rows;
    match witness cx rows [ t ] with
    | Some [ w ] -> add (Printf.sprintf "this case is not exhaustive: `%s` is not matched" (show w))
    | Some _ -> add "this case is not exhaustive"
    | None -> ()
  in
  let rec walk (e : c_expr) : unit =
    match e with
    | ESwitch (s, brs) ->
        check_switch e s brs;
        walk s;
        List.iter (fun (_, b) -> walk b) brs
    | EId _ | EInt _ | EFloat _ | EBool _ | EString _ | EChar _ | EUnit | ENil -> ()
    | EApp (a, b) | EBop (_, a, b) | EListEnumeration (a, b) ->
        walk a;
        walk b
    | EFunction (_, _, b) | EFieldAccess (b, _) -> walk b
    | EBind (_, _, a, b, _) | EBindRec (_, _, a, b, _) ->
        walk a;
        walk b
    | EBindMutRec (bs, b) ->
        List.iter (fun (_, _, e, _, _) -> walk e) bs;
        walk b
    | EBlock parts ->
        List.iter
          (function
            | Expr x -> walk x
            | Defn (CDefn (_, _, _, x, _, _) | CDefnRec (_, _, _, x, _, _)) -> walk x
            | Defn (CDefnMutRec ds) -> List.iter (fun (_, _, _, x, _, _) -> walk x) ds
            | Defn _ -> ())
          parts
    | ETernary (a, b, c) ->
        walk a;
        walk b;
        walk c
    | EVector es -> List.iter walk es
    | EListComprehension (b, gs) ->
        walk b;
        List.iter (fun (_, g) -> walk g) gs
    | ERecordLit fs -> List.iter (fun (_, x) -> walk x) fs
    | ERecordUpdate (r, fs) ->
        walk r;
        List.iter (fun (_, x) -> walk x) fs
  in
  (match d with
  | CDefn (_, _, _, b, _, _) | CDefnRec (_, _, _, b, _, _) -> walk b
  | CDefnMutRec ds -> List.iter (fun (_, _, _, b, _, _) -> walk b) ds
  | _ -> ());
  List.rev !warnings

(** Type-check [d] like {!Typecheck.generate_defn}, and report its
    non-exhaustive and redundant [case]s through [warn]. *)
let generate_defn_checked ~(warn : string -> unit) (static_env : static_env)
    (type_env : T.type_env) (ctor_env : T.constructor_env) (d : c_defn) :
    (static_env * T.type_env * T.constructor_env) T.type_check_result =
  T.NodeTbl.reset T.node_types;
  T.NodeTbl.reset T.node_bindings;
  let saved = !T.type_logging in
  T.type_logging := true;
  let r =
    Fun.protect
      ~finally:(fun () -> T.type_logging := saved)
      (fun () -> T.generate_defn static_env type_env d)
  in
  (match r with
  | T.Ok (b, te, ce) ->
      let cx = { static_env = b @ static_env; ctor_env = ce @ ctor_env; type_env = te @ type_env } in
      let where =
        match d with
        | CDefn (CIdPat x, _, _, _, _, _) | CDefnRec (CIdPat x, _, _, _, _, _) -> "in `" ^ x ^ "`: "
        | _ -> ""
      in
      List.iter (fun w -> warn (where ^ w.message)) (check_defn cx d)
  | T.Error _ -> ());
  r
