open Expr

module StringSet = Set.Make (String)

let option_map f = function None -> None | Some x -> Some (f x)

type symbols = {
  values : (string, string) Hashtbl.t;
  types : (string, string) Hashtbl.t;
  ctors : (string, string) Hashtbl.t;
  traits : (string, string) Hashtbl.t;
  modules : (string, unit) Hashtbl.t;
}

let make_symbols () =
  {
    values = Hashtbl.create 256;
    types = Hashtbl.create 256;
    ctors = Hashtbl.create 256;
    traits = Hashtbl.create 128;
    modules = Hashtbl.create 128;
  }

let path_to_key (path : string list) : string = String.concat "." path

let scoped_key (path : string list) (name : string) : string =
  path_to_key path ^ "|" ^ name

let qualify (path : string list) (name : string) : string =
  if path = [] then name else String.concat "." (path @ [ name ])

let drop_last (path : string list) : string list =
  match List.rev path with [] -> [] | _ :: t -> List.rev t

let split_qualified_name (s : string) : string list =
  String.split_on_char '.' s |> List.filter (fun p -> p <> "")

let module_exists (sym : symbols) (path : string list) : bool =
  Hashtbl.mem sym.modules (path_to_key path)

let rec resolve_scoped (tbl : (string, string) Hashtbl.t) (path : string list)
    (name : string) : string option =
  match Hashtbl.find_opt tbl (scoped_key path name) with
  | Some fq -> Some fq
  | None ->
      if path = [] then None else resolve_scoped tbl (drop_last path) name

let rec find_module_path_from_scope (sym : symbols) (scope : string list)
    (rel : string list) : string list option =
  let candidate = scope @ rel in
  if module_exists sym candidate then Some candidate
  else if scope = [] then None
  else find_module_path_from_scope sym (drop_last scope) rel

let dedup_strings (xs : string list) : string list =
  List.sort_uniq String.compare xs

let resolve_from_uses_exn ~(kind : string) (name : string)
    (candidates : (string * string) list) : string option =
  match candidates with
  | [] -> None
  | [ (_, only) ] -> Some only
  | many ->
      let providers = many |> List.map fst |> dedup_strings in
      failwith
        ("forge: ambiguous " ^ kind ^ " '" ^ name ^ "' from use: "
       ^ String.concat ", " providers)

let resolve_unqualified_from_tbl (tbl : (string, string) Hashtbl.t)
    ~(kind : string) (_sym : symbols) (path : string list)
    (uses : string list list)
    (name : string) : string option =
  match resolve_scoped tbl path name with
  | Some fq -> Some fq
  | None ->
      let candidates =
        List.filter_map
          (fun use_path ->
            match Hashtbl.find_opt tbl (scoped_key use_path name) with
            | Some fq -> Some (path_to_key use_path, fq)
            | None -> None)
          uses
      in
      let uniq =
        candidates
        |> List.sort_uniq (fun (a1, v1) (a2, v2) ->
               let c = String.compare v1 v2 in
               if c <> 0 then c else String.compare a1 a2)
      in
      resolve_from_uses_exn ~kind name uniq

let resolve_qualified_from_tbl (tbl : (string, string) Hashtbl.t)
    ~(kind : string) (sym : symbols) (path : string list) (uses : string list list)
    (segments : string list) : string option =
  match List.rev segments with
  | [] | [ _ ] -> None
  | name :: rev_mod ->
      let module_segs = List.rev rev_mod in
      let direct_candidate =
        match find_module_path_from_scope sym path module_segs with
        | Some mod_path -> (
            match Hashtbl.find_opt tbl (scoped_key mod_path name) with
            | Some fq -> Some (path_to_key mod_path, fq)
            | None -> None)
        | None -> None
      in
      let from_uses =
        List.filter_map
          (fun use_path ->
            let mod_path = use_path @ module_segs in
            if module_exists sym mod_path then
              match Hashtbl.find_opt tbl (scoped_key mod_path name) with
              | Some fq -> Some (path_to_key mod_path, fq)
              | None -> None
            else None)
          uses
      in
      let candidates =
        (match direct_candidate with Some c -> c :: from_uses | None -> from_uses)
        |> List.sort_uniq (fun (a1, v1) (a2, v2) ->
               let c = String.compare v1 v2 in
               if c <> 0 then c else String.compare a1 a2)
      in
      resolve_from_uses_exn ~kind (String.concat "." segments) candidates

let resolve_name_with_tbl (tbl : (string, string) Hashtbl.t) ~(kind : string)
    (sym : symbols) (path : string list) (uses : string list list) (name : string)
    : string =
  if String.contains name '.' then
    let segs = split_qualified_name name in
    match resolve_qualified_from_tbl tbl ~kind sym path uses segs with
    | Some fq -> fq
    | None -> name
  else
    match resolve_unqualified_from_tbl tbl ~kind sym path uses name with
    | Some fq -> fq
    | None -> name

let resolve_type_name = resolve_name_with_tbl
let resolve_trait_name = resolve_name_with_tbl
let resolve_ctor_name = resolve_name_with_tbl

let resolve_value_or_ctor_name (sym : symbols) (path : string list)
    (uses : string list list) (name : string) : string =
  let kind = "value" in
  if String.contains name '.' then
    let segs = split_qualified_name name in
    (match resolve_qualified_from_tbl sym.values ~kind sym path uses segs with
    | Some fq -> fq
    | None -> (
        match
          resolve_qualified_from_tbl sym.ctors ~kind:"constructor" sym path uses
            segs
        with
        | Some fq -> fq
        | None -> name))
  else
    match
      resolve_unqualified_from_tbl sym.values ~kind sym path uses name
    with
    | Some fq -> fq
    | None -> (
        match
          resolve_unqualified_from_tbl sym.ctors ~kind:"constructor" sym path uses
            name
        with
        | Some fq -> fq
        | None -> name)

let resolve_qualified_value_or_ctor_name (sym : symbols) (path : string list)
    (uses : string list list) (segments : string list) : string option =
  match
    resolve_qualified_from_tbl sym.values ~kind:"value" sym path uses segments
  with
  | Some fq -> Some fq
  | None ->
      resolve_qualified_from_tbl sym.ctors ~kind:"constructor" sym path uses
        segments

let resolve_use_path_exn (sym : symbols) (path : string list) (raw : string list)
    : string list =
  match find_module_path_from_scope sym path raw with
  | Some p -> p
  | None ->
      failwith
        ("forge: unknown module in use: " ^ String.concat "." raw)

let rec bound_ids_in_sub_pat : sub_pat -> string list = function
  | IdPat s | InfixPat s -> [ s ]
  | Pat p -> bound_ids_in_pat p
  | VectorPat ps -> List.concat (List.map bound_ids_in_pat ps)
  | RecordPat fs -> List.concat (List.map (fun (_, p) -> bound_ids_in_pat p) fs)
  | VariantPat (_, Some p) -> bound_ids_in_pat p
  | VariantPat (_, None) -> []
  | UnitPat | WildcardPat | IntPat _ | CharPat _ | StringPat _ | BoolPat _
  | NilPat ->
      []

and bound_ids_in_pat : pat -> string list = function
  | SubPat sp -> bound_ids_in_sub_pat sp
  | ConsPat (sp, p) -> bound_ids_in_sub_pat sp @ bound_ids_in_pat p

let collect_value_bindings (path : string list) (pat : pat) (sym : symbols) :
    unit =
  List.iter
    (fun id -> Hashtbl.replace sym.values (scoped_key path id) (qualify path id))
    (bound_ids_in_pat pat)

let collect_ctor_bindings (path : string list)
    (ctors : (string * compound_type option) list) (sym : symbols) : unit =
  List.iter
    (fun (name, _) ->
      Hashtbl.replace sym.ctors (scoped_key path name) (qualify path name))
    ctors

let rec collect_declared_symbols (path : string list) (sym : symbols)
    (defns : defn list) : unit =
  List.iter
    (fun d ->
      match d with
      | Defn (p, _, _, _, _, _) | DefnRec (p, _, _, _, _, _) ->
          collect_value_bindings path p sym
      | DefnMutRec defs ->
          List.iter
            (fun (p, _, _, _, _, _) -> collect_value_bindings path p sym)
            defs
      | MacroDef _ -> ()
      | TypeDef (name, _, _) ->
          Hashtbl.replace sym.types (scoped_key path name) (qualify path name)
      | SumTypeDef (name, _, ctors) | SumTypeDefRec (name, _, ctors) ->
          Hashtbl.replace sym.types (scoped_key path name) (qualify path name);
          collect_ctor_bindings path ctors sym
      | SumTypeDefMutRec groups ->
          List.iter
            (fun (name, _, ctors) ->
              Hashtbl.replace sym.types (scoped_key path name) (qualify path name);
              collect_ctor_bindings path ctors sym)
            groups
      | ClassDef (name, _, _, _) ->
          Hashtbl.replace sym.traits (scoped_key path name) (qualify path name)
      | InstanceDef _ -> ()
      | UseDef _ -> ()
      | ImportDef _ -> ()
      | ModDef (name, nested) ->
          let mod_path = path @ [ name ] in
          Hashtbl.replace sym.modules (path_to_key mod_path) ();
          collect_declared_symbols mod_path sym nested)
    defns

let add_pat_bound_ids (bound : StringSet.t) (p : pat) : StringSet.t =
  List.fold_left (fun acc id -> StringSet.add id acc) bound (bound_ids_in_pat p)

let rec rewrite_compound_type (sym : symbols) (path : string list)
    (uses : string list list) (ct : compound_type) : compound_type =
  match ct with
  | BasicType ft -> BasicType (rewrite_factor_type sym path uses ft)
  | FunctionType (ft, out) ->
      FunctionType
        ( rewrite_factor_type sym path uses ft,
          rewrite_compound_type sym path uses out )

and rewrite_factor_type (sym : symbols) (path : string list)
    (uses : string list list) (ft : factor_type) : factor_type =
  match ft with
  | TypeName n ->
      TypeName (resolve_type_name sym.types ~kind:"type" sym path uses n)
  | TypeApp (name, args) ->
      TypeApp
        ( resolve_type_name sym.types ~kind:"type" sym path uses name,
          List.map (rewrite_compound_type sym path uses) args )
  | ParenFactorType ct -> ParenFactorType (rewrite_compound_type sym path uses ct)
  | VectorType cts ->
      VectorType (List.map (rewrite_compound_type sym path uses) cts)
  | ListType ct -> ListType (rewrite_compound_type sym path uses ct)
  | RecordTypeWritten fields ->
      RecordTypeWritten
        (List.map
           (fun (n, ct) -> (n, rewrite_compound_type sym path uses ct))
           fields)
  | ( IntegerType | StringType | BooleanType | CharType | UnitType | FloatType
    | TypeVarWritten _ ) as t ->
      t

let rewrite_constraints (sym : symbols) (path : string list)
    (uses : string list list) (cs : (string * compound_type) list) :
    (string * compound_type) list =
  List.map
    (fun (cls, ty) ->
      ( resolve_trait_name sym.traits ~kind:"trait" sym path uses cls,
        rewrite_compound_type sym path uses ty ))
    cs

let rec rewrite_sub_pat_local (sym : symbols) (path : string list)
    (uses : string list list) (sp : sub_pat) : sub_pat =
  match sp with
  | VariantPat (name, payload) ->
      VariantPat
        ( resolve_ctor_name sym.ctors ~kind:"constructor" sym path uses name,
          option_map (rewrite_pat_local sym path uses) payload )
  | Pat p -> Pat (rewrite_pat_local sym path uses p)
  | VectorPat ps -> VectorPat (List.map (rewrite_pat_local sym path uses) ps)
  | RecordPat fs ->
      RecordPat
        (List.map (fun (n, p) -> (n, rewrite_pat_local sym path uses p)) fs)
  | (IdPat _ | InfixPat _ | UnitPat | WildcardPat | IntPat _ | CharPat _
    | StringPat _ | BoolPat _ | NilPat) as s ->
      s

and rewrite_pat_local (sym : symbols) (path : string list)
    (uses : string list list) (p : pat) : pat =
  match p with
  | SubPat sp -> SubPat (rewrite_sub_pat_local sym path uses sp)
  | ConsPat (sp, rest) ->
      ConsPat
        (rewrite_sub_pat_local sym path uses sp, rewrite_pat_local sym path uses rest)

let rec qualify_sub_pat_top (sym : symbols) (path : string list)
    (uses : string list list) (sp : sub_pat) : sub_pat =
  match sp with
  | IdPat s -> IdPat (qualify path s)
  | InfixPat s -> InfixPat (qualify path s)
  | VariantPat (name, payload) ->
      VariantPat
        ( resolve_ctor_name sym.ctors ~kind:"constructor" sym path uses name,
          option_map (qualify_pat_top sym path uses) payload )
  | Pat p -> Pat (qualify_pat_top sym path uses p)
  | VectorPat ps -> VectorPat (List.map (qualify_pat_top sym path uses) ps)
  | RecordPat fs ->
      RecordPat
        (List.map (fun (n, p) -> (n, qualify_pat_top sym path uses p)) fs)
  | (UnitPat | WildcardPat | IntPat _ | CharPat _ | StringPat _ | BoolPat _
    | NilPat) as s ->
      s

and qualify_pat_top (sym : symbols) (path : string list) (uses : string list list)
    (p : pat) : pat =
  match p with
  | SubPat sp -> SubPat (qualify_sub_pat_top sym path uses sp)
  | ConsPat (sp, rest) ->
      ConsPat
        (qualify_sub_pat_top sym path uses sp, qualify_pat_top sym path uses rest)

let rewrite_id (sym : symbols) (path : string list) (uses : string list list)
    (bound : StringSet.t) (name : string) : string =
  if StringSet.mem name bound then name
  else resolve_value_or_ctor_name sym path uses name

let first_segment_is_bound (bound : StringSet.t) (segments : string list) : bool =
  match segments with h :: _ -> StringSet.mem h bound | [] -> false

let rec factor_to_qualified_segments : factor -> string list option = function
  | Id s -> Some [ s ]
  | FieldAccess (base, fld) -> (
      match factor_to_qualified_segments base with
      | Some xs -> Some (xs @ [ fld ])
      | None -> None)
  | _ -> None

let rec rewrite_expr (sym : symbols) (path : string list)
    (uses : string list list) (bound : StringSet.t) (e : expr) : expr =
  match e with
  | Function (p, ann, body) ->
      let p' = rewrite_pat_local sym path uses p in
      let bound' = add_pat_bound_ids bound p' in
      Function
        ( p',
          option_map (rewrite_compound_type sym path uses) ann,
          rewrite_expr sym path uses bound' body )
  | Ternary (a, b, c) ->
      Ternary
        ( rewrite_expr sym path uses bound a,
          rewrite_expr sym path uses bound b,
          rewrite_expr sym path uses bound c )
  | ConsExpr ce -> ConsExpr (rewrite_cons_expr sym path uses bound ce)
  | Bind (p, ann, e1, e2, rt) ->
      let p' = rewrite_pat_local sym path uses p in
      let bound' = add_pat_bound_ids bound p' in
      Bind
        ( p',
          option_map (rewrite_compound_type sym path uses) ann,
          rewrite_expr sym path uses bound e1,
          rewrite_expr sym path uses bound' e2,
          option_map (rewrite_compound_type sym path uses) rt )
  | BindRec (p, ann, e1, e2, rt) ->
      let p' = rewrite_pat_local sym path uses p in
      let bound' = add_pat_bound_ids bound p' in
      BindRec
        ( p',
          option_map (rewrite_compound_type sym path uses) ann,
          rewrite_expr sym path uses bound' e1,
          rewrite_expr sym path uses bound' e2,
          option_map (rewrite_compound_type sym path uses) rt )
  | BindMutRec (bindings, body) ->
      let bindings' =
        List.map
          (fun (p, ann, rhs, rt, n) ->
            let p' = rewrite_pat_local sym path uses p in
            (p', ann, rhs, rt, n))
          bindings
      in
      let bound' =
        List.fold_left
          (fun acc (p, _, _, _, _) -> add_pat_bound_ids acc p)
          bound bindings'
      in
      let bindings'' =
        List.map
          (fun (p, ann, rhs, rt, n) ->
            ( p,
              option_map (rewrite_compound_type sym path uses) ann,
              rewrite_expr sym path uses bound' rhs,
              option_map (rewrite_compound_type sym path uses) rt,
              n ))
          bindings'
      in
      BindMutRec (bindings'', rewrite_expr sym path uses bound' body)
  | Switch (scrut, branches) ->
      Switch
        ( rewrite_expr sym path uses bound scrut,
          List.map
            (fun (p, rhs) ->
              let p' = rewrite_pat_local sym path uses p in
              let bound' = add_pat_bound_ids bound p' in
              (p', rewrite_expr sym path uses bound' rhs))
            branches )
  | Block parts ->
      let rec rewrite_parts bound_acc acc = function
        | [] -> List.rev acc
        | part :: rest -> (
            match part with
            | Expr e0 ->
                let e' = rewrite_expr sym path uses bound_acc e0 in
                rewrite_parts bound_acc (Expr e' :: acc) rest
            | Definition d0 ->
                let d' = rewrite_local_defn sym path uses bound_acc d0 in
                let bound_acc' =
                  List.fold_left
                    (fun b id -> StringSet.add id b)
                    bound_acc (local_defn_bound_ids d0)
                in
                rewrite_parts bound_acc' (Definition d' :: acc) rest)
      in
      Block (rewrite_parts bound [] parts)

and rewrite_cons_expr (sym : symbols) (path : string list)
    (uses : string list list) (bound : StringSet.t) (ce : cons_expr) : cons_expr =
  match ce with
  | Cons (d, ce') ->
      Cons
        ( rewrite_disjunction sym path uses bound d,
          rewrite_cons_expr sym path uses bound ce' )
  | DisjunctionUnderCons d ->
      DisjunctionUnderCons (rewrite_disjunction sym path uses bound d)
  | Pipeline (a, b) ->
      Pipeline
        ( rewrite_cons_expr sym path uses bound a,
          rewrite_cons_expr sym path uses bound b )

and rewrite_disjunction (sym : symbols) (path : string list)
    (uses : string list list) (bound : StringSet.t) (d : disjunction) :
    disjunction =
  match d with
  | Disjunction (a, b) ->
      Disjunction
        ( rewrite_conjunction sym path uses bound a,
          rewrite_disjunction sym path uses bound b )
  | ConjunctionUnderDisjunction a ->
      ConjunctionUnderDisjunction (rewrite_conjunction sym path uses bound a)

and rewrite_conjunction (sym : symbols) (path : string list)
    (uses : string list list) (bound : StringSet.t) (c : conjunction) :
    conjunction =
  match c with
  | Conjunction (a, b) ->
      Conjunction
        ( rewrite_rel_expr sym path uses bound a,
          rewrite_conjunction sym path uses bound b )
  | RelationUnderConjunction a ->
      RelationUnderConjunction (rewrite_rel_expr sym path uses bound a)

and rewrite_rel_expr (sym : symbols) (path : string list)
    (uses : string list list) (bound : StringSet.t) (r : rel_expr) : rel_expr =
  match r with
  | Relation (op, a, b) ->
      Relation
        ( op,
          rewrite_rel_expr sym path uses bound a,
          rewrite_arith_expr sym path uses bound b )
  | CustomRelExpr (op, a, b) ->
      CustomRelExpr
        ( op,
          rewrite_rel_expr sym path uses bound a,
          rewrite_arith_expr sym path uses bound b )
  | ArithmeticUnderRelExpr a ->
      ArithmeticUnderRelExpr (rewrite_arith_expr sym path uses bound a)

and rewrite_arith_expr (sym : symbols) (path : string list)
    (uses : string list list) (bound : StringSet.t) (a : arith_expr) : arith_expr =
  match a with
  | Plus (l, r) ->
      Plus
        ( rewrite_arith_expr sym path uses bound l,
          rewrite_term sym path uses bound r )
  | Minus (l, r) ->
      Minus
        ( rewrite_arith_expr sym path uses bound l,
          rewrite_term sym path uses bound r )
  | CustomArithExpr (op, l, r) ->
      CustomArithExpr
        ( op,
          rewrite_arith_expr sym path uses bound l,
          rewrite_term sym path uses bound r )
  | Term t -> Term (rewrite_term sym path uses bound t)

and rewrite_term (sym : symbols) (path : string list) (uses : string list list)
    (bound : StringSet.t) (t : term) : term =
  match t with
  | Mul (a, b) ->
      Mul (rewrite_term sym path uses bound a, rewrite_app_factor sym path uses bound b)
  | Div (a, b) ->
      Div (rewrite_term sym path uses bound a, rewrite_app_factor sym path uses bound b)
  | Mod (a, b) ->
      Mod (rewrite_term sym path uses bound a, rewrite_app_factor sym path uses bound b)
  | CustomTerm (op, a, b) ->
      CustomTerm
        ( op,
          rewrite_term sym path uses bound a,
          rewrite_app_factor sym path uses bound b )
  | Factor af -> Factor (rewrite_app_factor sym path uses bound af)

and rewrite_app_factor (sym : symbols) (path : string list)
    (uses : string list list) (bound : StringSet.t) (af : app_factor) : app_factor =
  match af with
  | Application (a, b) ->
      Application
        ( rewrite_app_factor sym path uses bound a,
          rewrite_factor sym path uses bound b )
  | FactorUnderApplication f ->
      FactorUnderApplication (rewrite_factor sym path uses bound f)

and rewrite_factor (sym : symbols) (path : string list) (uses : string list list)
    (bound : StringSet.t) (f : factor) : factor =
  match f with
  | Id name -> Id (rewrite_id sym path uses bound name)
  | ParenFactor e -> ParenFactor (rewrite_expr sym path uses bound e)
  | Opposite f0 -> Opposite (rewrite_factor sym path uses bound f0)
  | Vector es -> Vector (List.map (rewrite_expr sym path uses bound) es)
  | ListSugar es -> ListSugar (List.map (rewrite_expr sym path uses bound) es)
  | ListEnumeration (a, b) ->
      ListEnumeration
        (rewrite_expr sym path uses bound a, rewrite_expr sym path uses bound b)
  | ListComprehension (body, generators) ->
      let rec rewrite_gens bound_acc acc = function
        | [] -> (List.rev acc, bound_acc)
        | (p, ge) :: rest ->
            let p' = rewrite_pat_local sym path uses p in
            let ge' = rewrite_expr sym path uses bound_acc ge in
            let bound_acc' = add_pat_bound_ids bound_acc p' in
            rewrite_gens bound_acc' ((p', ge') :: acc) rest
      in
      let gens', bound_for_body = rewrite_gens bound [] generators in
      ListComprehension
        (rewrite_expr sym path uses bound_for_body body, gens')
  | RecordLit fields ->
      RecordLit
        (List.map
           (fun (n, e0) -> (n, rewrite_expr sym path uses bound e0))
           fields)
  | RecordUpdate (base, fields) ->
      RecordUpdate
        ( rewrite_expr sym path uses bound base,
          List.map
            (fun (n, e0) -> (n, rewrite_expr sym path uses bound e0))
            fields )
  | FieldAccess (base, fld) as original -> (
      match factor_to_qualified_segments original with
      | Some segs when not (first_segment_is_bound bound segs) -> (
          match resolve_qualified_value_or_ctor_name sym path uses segs with
          | Some fq -> Id fq
          | _ -> FieldAccess (rewrite_factor sym path uses bound base, fld))
      | _ -> FieldAccess (rewrite_factor sym path uses bound base, fld))
  | MacroInvoke (name, args) ->
      MacroInvoke (name, List.map (rewrite_expr sym path uses bound) args)
  | (Boolean _ | String _ | Unit | Integer _ | Char _ | FloatFactor _ | Nil) as x ->
      x

and rewrite_local_defn (sym : symbols) (path : string list)
    (uses : string list list) (bound : StringSet.t) (d : defn) : defn =
  match d with
  | Defn (p, cs, ann, body, rt, n) ->
      Defn
        ( rewrite_pat_local sym path uses p,
          rewrite_constraints sym path uses cs,
          option_map (rewrite_compound_type sym path uses) ann,
          rewrite_expr sym path uses bound body,
          option_map (rewrite_compound_type sym path uses) rt,
          n )
  | DefnRec (p, cs, ann, body, rt, n) ->
      let p' = rewrite_pat_local sym path uses p in
      let bound' = add_pat_bound_ids bound p' in
      DefnRec
        ( p',
          rewrite_constraints sym path uses cs,
          option_map (rewrite_compound_type sym path uses) ann,
          rewrite_expr sym path uses bound' body,
          option_map (rewrite_compound_type sym path uses) rt,
          n )
  | DefnMutRec defs ->
      let pats =
        List.map
          (fun (p, cs, ann, body, rt, n) ->
            ( rewrite_pat_local sym path uses p,
              rewrite_constraints sym path uses cs,
              option_map (rewrite_compound_type sym path uses) ann,
              body,
              option_map (rewrite_compound_type sym path uses) rt,
              n ))
          defs
      in
      let bound' =
        List.fold_left
          (fun acc (p, _, _, _, _, _) -> add_pat_bound_ids acc p)
          bound pats
      in
      DefnMutRec
        (List.map
           (fun (p, cs, ann, body, rt, n) ->
             (p, cs, ann, rewrite_expr sym path uses bound' body, rt, n))
           pats)
  | UseDef _ | ModDef _ | ImportDef _ ->
      failwith "forge: use/mod/import are not supported inside expression blocks"
  | MacroDef _ ->
      failwith "forge: macro_rules! is not supported inside expression blocks"
  | _ -> d

and local_defn_bound_ids (d : defn) : string list =
  match d with
  | Defn (p, _, _, _, _, _) | DefnRec (p, _, _, _, _, _) -> bound_ids_in_pat p
  | DefnMutRec defs ->
      List.concat (List.map (fun (p, _, _, _, _, _) -> bound_ids_in_pat p) defs)
  | MacroDef _ -> []
  | _ -> []

let method_names_of_trait_items (items : trait_item list) : StringSet.t =
  List.fold_left
    (fun acc item ->
      match item with
      | TraitVal (m, _) | TraitLet (m, _) -> StringSet.add m acc)
    StringSet.empty items

let qualify_decl_name (path : string list) (name : string) : string =
  if String.contains name '.' then name else qualify path name

let rewrite_top_defn_non_mod (sym : symbols) (path : string list)
    (uses : string list list) (d : defn) : defn list =
  match d with
  | Defn (p, cs, ann, body, rt, n) ->
      [
        Defn
          ( qualify_pat_top sym path uses p,
            rewrite_constraints sym path uses cs,
            option_map (rewrite_compound_type sym path uses) ann,
            rewrite_expr sym path uses StringSet.empty body,
            option_map (rewrite_compound_type sym path uses) rt,
            n );
      ]
  | DefnRec (p, cs, ann, body, rt, n) ->
      [
        DefnRec
          ( qualify_pat_top sym path uses p,
            rewrite_constraints sym path uses cs,
            option_map (rewrite_compound_type sym path uses) ann,
            rewrite_expr sym path uses StringSet.empty body,
            option_map (rewrite_compound_type sym path uses) rt,
            n );
      ]
  | DefnMutRec defs ->
      let defs' =
        List.map
          (fun (p, cs, ann, body, rt, n) ->
            ( qualify_pat_top sym path uses p,
              rewrite_constraints sym path uses cs,
              option_map (rewrite_compound_type sym path uses) ann,
              rewrite_expr sym path uses StringSet.empty body,
              option_map (rewrite_compound_type sym path uses) rt,
              n ))
          defs
      in
      [ DefnMutRec defs' ]
  | TypeDef (name, args, body) ->
      [
        TypeDef
          ( qualify_decl_name path name,
            args,
            rewrite_compound_type sym path uses body );
      ]
  | SumTypeDef (name, args, ctors) ->
      [
        SumTypeDef
          ( qualify_decl_name path name,
            args,
            List.map
              (fun (c, payload) ->
                ( qualify_decl_name path c,
                  option_map (rewrite_compound_type sym path uses) payload ))
              ctors );
      ]
  | SumTypeDefRec (name, args, ctors) ->
      [
        SumTypeDefRec
          ( qualify_decl_name path name,
            args,
            List.map
              (fun (c, payload) ->
                ( qualify_decl_name path c,
                  option_map (rewrite_compound_type sym path uses) payload ))
              ctors );
      ]
  | SumTypeDefMutRec groups ->
      [
        SumTypeDefMutRec
          (List.map
             (fun (name, args, ctors) ->
               ( qualify_decl_name path name,
                 args,
                 List.map
                   (fun (c, payload) ->
                     ( qualify_decl_name path c,
                       option_map (rewrite_compound_type sym path uses) payload ))
                   ctors ))
             groups);
      ]
  | ClassDef (name, params, requires, items) ->
      let method_names = method_names_of_trait_items items in
      let items' =
        List.map
          (function
            | TraitVal (m, ty) ->
                TraitVal (m, rewrite_compound_type sym path uses ty)
            | TraitLet (m, e) ->
                TraitLet (m, rewrite_expr sym path uses method_names e))
          items
      in
      [
        ClassDef
          ( qualify_decl_name path name,
            params,
            rewrite_constraints sym path uses requires,
            items' );
      ]
  | InstanceDef (cls, head_ty, requires, impls) ->
      let method_names =
        List.fold_left (fun acc (m, _) -> StringSet.add m acc) StringSet.empty
          impls
      in
      let impls' =
        List.map
          (fun (m, e) -> (m, rewrite_expr sym path uses method_names e))
          impls
      in
      [
        InstanceDef
          ( resolve_trait_name sym.traits ~kind:"trait" sym path uses cls,
            rewrite_compound_type sym path uses head_ty,
            rewrite_constraints sym path uses requires,
            impls' );
      ]
  | ModDef _ | UseDef _ ->
      []
  | ImportDef _ ->
      failwith "forge: import statements must be resolved before module expansion"
  | MacroDef _ ->
      []

let rec rewrite_top_defns (sym : symbols) (path : string list)
    (uses : string list list) (defns : defn list) : defn list =
  let rec go active_uses acc = function
    | [] -> List.rev acc
    | ImportDef _ :: _ ->
        failwith
          "forge: import statements must be resolved before module expansion"
    | UseDef raw_path :: rest ->
        let resolved = resolve_use_path_exn sym path raw_path in
        go (resolved :: active_uses) acc rest
    | ModDef (name, nested) :: rest ->
        let nested_defs = rewrite_top_defns sym (path @ [ name ]) active_uses nested in
        go active_uses (List.rev_append nested_defs acc) rest
    | d :: rest ->
        let defs = rewrite_top_defn_non_mod sym path active_uses d in
        go active_uses (List.rev_append defs acc) rest
  in
  go uses [] defns

let rec has_expandables_defn (d : defn) : bool =
  match d with
  | ModDef (_, nested) -> true || has_expandables nested
  | UseDef _ -> true
  | ImportDef _ -> true
  | _ -> false

and has_expandables (defns : defn list) : bool =
  List.exists has_expandables_defn defns

let expand_program (defns : defn list) : defn list =
  let defns = Macro_expand.expand_program defns in
  if not (has_expandables defns) then defns
  else
    let sym = make_symbols () in
    collect_declared_symbols [] sym defns;
    rewrite_top_defns sym [] [] defns

let collect_active_toplevel_uses (sym : symbols) (defns : defn list) :
    string list list =
  let rec go (uses : string list list) = function
    | [] -> uses
    | UseDef raw_path :: rest ->
        let resolved = resolve_use_path_exn sym [] raw_path in
        go (resolved :: uses) rest
    | _ :: rest -> go uses rest
  in
  go [] defns

let expand_expr_in_context (context_defns : defn list) (e : expr) : expr =
  let e = Macro_expand.expand_expr_in_context context_defns e in
  let sym = make_symbols () in
  collect_declared_symbols [] sym context_defns;
  let uses = collect_active_toplevel_uses sym context_defns in
  rewrite_expr sym [] uses StringSet.empty e
