(** Typed monomorphisation for the native back end.

    Input: a class-free program (after {!Specialize}). Every top-level
    definition is type-checked again with {!Typecheck.type_logging} on, which
    records the inferred type of every node; under the solution of its
    definition, and the instantiation of the copy being built, every node then
    has a concrete type.

    Output: a {!Tast.program} in which every polymorphic function (top-level or
    local) is copied once per type it is used at. A type variable that no use
    determines (the element type of an unused [\[\]], say) is [Unit]: by
    parametricity no value of that type is ever inspected. *)

open Cexpr
open Tast
module T = Typecheck
module SM = Map.Make (String)

exception Unsupported of string

let unsupported msg = raise (Unsupported msg)

(* ------------------------------------------------------------------------ *)
(* Types                                                                    *)
(* ------------------------------------------------------------------------ *)

(** A solution (or an instantiation): type variable -> type. *)
type subst = (string, mono_type) Hashtbl.t

let solution (eqs : T.type_equations) : subst =
  let h = Hashtbl.create 256 in
  (match T.reduce_eq eqs [] with
  | s ->
      List.iter
        (fun (k, t) ->
          match k with TypeVar v -> if not (Hashtbl.mem h v) then Hashtbl.add h v t | _ -> ())
        s
  | exception _ -> ());
  h

let rec resolve (s : subst) (t : mono_type) : mono_type =
  let r = resolve s in
  match t with
  | TypeVar v -> (
      match Hashtbl.find_opt s v with
      | Some (TypeVar v') when String.equal v v' -> t
      | Some t' -> r t'
      | None -> t)
  | TCtorApp (w, args) -> (
      let args = List.map r args in
      match r (TypeVar w) with
      | TypeVar w' -> TCtorApp (w', args)
      | TypeName n -> CTypeApp (n, args)
      | CTypeApp (n, pre) -> CTypeApp (n, pre @ args)
      | CListType _ -> ( match args with [ e ] -> CListType e | _ -> TCtorApp (w, args))
      | _ -> TCtorApp (w, args))
  | FunctionType (a, b) -> FunctionType (r a, r b)
  | VectorType ts -> VectorType (List.map r ts)
  | CListType a -> CListType (r a)
  | CTypeApp (n, args) -> CTypeApp (n, List.map r args)
  | FixedPoint (n, b) -> FixedPoint (n, r b)
  | RecordType fs -> RecordType (T.flatten_record_fields (List.map (fun (n, t) -> (n, r t)) fs))
  | IntType | FloatType | BoolType | StringType | CharType | UnitType | TypeName _ -> t

let rec fix_args (name : string) (b : mono_type) : mono_type list option =
  match b with
  | CTypeApp (n, args) when String.equal n name -> Some args
  | FixedPoint (_, b') -> fix_args name b'
  | _ -> None

(** Type declarations ([type_env]) in scope, for aliases. *)
let type_env : T.type_env ref = ref []

let is_sum_def (name : string) (def : mono_type) : bool =
  match def with
  | CTypeApp (n, _) | FixedPoint (n, _) -> String.equal n name
  | _ -> false

let rec subst_named (sub : (string * mono_type) list) (t : mono_type) : mono_type =
  let h = Hashtbl.create 4 in
  List.iter (fun (v, ty) -> Hashtbl.replace h v ty) sub;
  resolve h t

(** The canonical concrete form of a type (see {!Tast}). *)
and norm (t : mono_type) : mono_type =
  match t with
  | TypeVar _ | TCtorApp _ -> UnitType
  | IntType | FloatType | BoolType | StringType | CharType | UnitType -> t
  | CListType a -> CListType (norm a)
  | VectorType ts -> VectorType (List.map norm ts)
  | FunctionType (a, b) -> FunctionType (norm a, norm b)
  | RecordType fs ->
      let fs = List.filter (fun (f, _) -> f <> row_field) (T.flatten_record_fields fs) in
      RecordType (List.sort (fun (a, _) (b, _) -> compare a b) (List.map (fun (f, t) -> (f, norm t)) fs))
  | CTypeApp ("List", [ a ]) -> CListType (norm a)
  | CTypeApp (name, args) -> expand_alias name (List.map norm args)
  | TypeName name -> expand_alias name []
  | FixedPoint (name, b) -> (
      match fix_args name b with
      | Some args -> norm (CTypeApp (name, args))
      | None -> CTypeApp (name, []))

and expand_alias (name : string) (args : mono_type list) : mono_type =
  match List.find_opt (fun (n, _, _) -> String.equal n name) !type_env with
  | Some (_, params, def) when (not (is_sum_def name def)) && List.length params = List.length args ->
      norm (subst_named (List.combine params args) def)
  | _ -> CTypeApp (name, args)

let type_key (t : mono_type) : string = C_to_string.string_of_mono_type t

(** Bind the variables of [pat] (a scheme's type) so that it becomes the
    concrete type [t]. *)
let rec match_ty (theta : subst) (pat : mono_type) (t : mono_type) : unit =
  let bind v ty = if not (Hashtbl.mem theta v) then Hashtbl.replace theta v ty in
  let pairs ps ts = if List.length ps = List.length ts then List.iter2 (match_ty theta) ps ts in
  match (pat, t) with
  | TypeVar v, _ -> bind v t
  | TCtorApp (w, ps), CTypeApp (n, ts) when List.length ts >= List.length ps ->
      let k = List.length ts - List.length ps in
      let pre = List.filteri (fun i _ -> i < k) ts in
      let post = List.filteri (fun i _ -> i >= k) ts in
      bind w (if pre = [] then TypeName n else CTypeApp (n, pre));
      pairs ps post
  | TCtorApp (w, [ p ]), CListType e ->
      bind w (CListType UnitType);
      match_ty theta p e
  | FunctionType (a, b), FunctionType (c, d) ->
      match_ty theta a c;
      match_ty theta b d
  | CListType a, CListType b -> match_ty theta a b
  | CTypeApp ("List", [ a ]), _ -> match_ty theta (CListType a) t
  | VectorType ps, VectorType ts -> pairs ps ts
  | CTypeApp (_, ps), CTypeApp (_, ts) -> pairs ps ts
  | FixedPoint (n, b), _ -> (
      match fix_args n b with Some args -> match_ty theta (CTypeApp (n, args)) t | None -> ())
  | RecordType pfs, RecordType tfs -> (
      let pfs = T.flatten_record_fields pfs in
      List.iter
        (fun (f, pt) ->
          if f <> row_field then
            match List.assoc_opt f tfs with Some tt -> match_ty theta pt tt | None -> ())
        pfs;
      match List.assoc_opt row_field pfs with
      | Some (TypeVar r) ->
          bind r (RecordType (List.filter (fun (f, _) -> not (List.mem_assoc f pfs)) tfs))
      | _ -> ())
  | _ -> ()

(** Pair the type variables of [a] with those at the same places in [b]
    (the same type, solved separately: unifying record rows creates fresh
    variables, so two solutions of one set of equations may name them
    differently). *)
let rec align (acc : (string * string) list ref) (a : mono_type) (b : mono_type) : unit =
  let pairs xs ys = if List.length xs = List.length ys then List.iter2 (align acc) xs ys in
  match (a, b) with
  | TypeVar v, TypeVar w -> if not (List.mem_assoc v !acc) then acc := (v, w) :: !acc
  | TCtorApp (v, xs), TCtorApp (w, ys) ->
      if not (List.mem_assoc v !acc) then acc := (v, w) :: !acc;
      pairs xs ys
  | FunctionType (a1, a2), FunctionType (b1, b2) ->
      align acc a1 b1;
      align acc a2 b2
  | CListType x, CListType y | FixedPoint (_, x), FixedPoint (_, y) -> align acc x y
  | VectorType xs, VectorType ys | CTypeApp (_, xs), CTypeApp (_, ys) -> pairs xs ys
  | RecordType fs, RecordType gs ->
      let fs = T.flatten_record_fields fs and gs = T.flatten_record_fields gs in
      List.iter (fun (n, t) -> match List.assoc_opt n gs with Some u -> align acc t u | None -> ()) fs
  | _ -> ()

let rec scheme_parts (t : c_type) : string list * mono_type =
  match t with
  | PolyType (v, t) ->
      let vs, m = scheme_parts t in
      (v :: vs, m)
  | Constrained (_, t) -> scheme_parts t
  | Mono m -> ([], m)

(* ------------------------------------------------------------------------ *)
(* Environment                                                              *)
(* ------------------------------------------------------------------------ *)

let static_env : static_env ref = ref []

(** [\[\]] is a constant constructor, so all its occurrences are one
    physical node; {!freshen} replaces each by [__nil ()], a distinct node
    whose type the type checker records. *)
let nil_name = "__forge_nil"

let nil_scheme : c_type = PolyType ("a", Mono (FunctionType (UnitType, CListType (TypeVar "a"))))
let ctor_env : T.constructor_env ref = ref []

let prims : string list =
  List.filter_map
    (fun (n, v, _) -> match v with BuiltInFunction _ -> Some n | _ -> None)
    Env.built_ins

let ctor_tag (c : string) : int option =
  List.find_map
    (fun (_, _, ctors) ->
      let rec go i = function
        | [] -> None
        | (c', _) :: rest -> if String.equal c c' then Some i else go (i + 1) rest
      in
      go 0 ctors)
    !ctor_env

(** Payload type of constructor [c] in a value of the concrete type [t]. *)
let ctor_payload (c : string) (t : mono_type) : mono_type option =
  match List.assoc_opt c !static_env with
  | None -> unsupported ("unknown constructor " ^ c)
  | Some sch -> (
      let _, m = scheme_parts sch in
      match m with
      | FunctionType (payload, result) ->
          let theta = Hashtbl.create 4 in
          match_ty theta result t;
          Some (norm (resolve theta payload))
      | _ -> None)

(* ------------------------------------------------------------------------ *)
(* Top-level definitions                                                    *)
(* ------------------------------------------------------------------------ *)

type top_kind =
  | KFun of c_expr  (** a function; body is a lambda *)
  | KVal of c_expr  (** a value *)
  | KPat  (** bound by a top-level pattern (monomorphic global) *)

type top = { t_name : string; t_vars : string list; t_scheme : mono_type; t_kind : top_kind; t_sol : subst }

let tops : (string, top) Hashtbl.t = Hashtbl.create 256

(** Copy names, by [name|type]. *)
let copies : (string, string) Hashtbl.t = Hashtbl.create 256

(** Monomorphic functions already generated. *)
let generated : (string, unit) Hashtbl.t = Hashtbl.create 256

let funs : item list ref = ref []

(** Copies of polymorphic values, emitted before the item that needs them. *)
let pending_vals : item list ref = ref []

let counter = ref 0

let fresh (base : string) : string =
  incr counter;
  Printf.sprintf "%s__%d" base !counter

(* ------------------------------------------------------------------------ *)
(* Expressions                                                              *)
(* ------------------------------------------------------------------------ *)

(** A local variable: its unique name in the output, or a polymorphic
    binding copied per use. *)
type local = LMono of string | LPoly of local_poly

and local_poly = {
  lp_name : string;
  lp_scheme : mono_type;
  lp_copies : (string * (string * subst)) list ref;  (** key -> name, instantiation *)
}

type ctx = { sol : subst; theta : subst; scope : local SM.t }

let ty_of (ctx : ctx) (t : mono_type) : mono_type = norm (resolve ctx.theta (resolve ctx.sol t))

let node_ty (ctx : ctx) (e : c_expr) : mono_type =
  match T.NodeTbl.find_opt T.node_types e with
  | Some t -> ty_of ctx t
  | None -> unsupported ("internal: no type for " ^ C_to_string.string_of_expr e)

(** Bind [names] to fresh unique names. *)
let bind_names (ctx : ctx) (names : string list) : ctx * string list =
  let uniq = List.map (fun n -> (n, fresh n)) names in
  ( { ctx with scope = List.fold_left (fun sc (n, u) -> SM.add n (LMono u) sc) ctx.scope uniq },
    List.map snd uniq )

(** Quantified variables of a local binding's scheme that this copy leaves
    free. *)
let local_poly_scheme (ctx : ctx) (node : c_expr) (x : string) : mono_type option =
  let rhs = match node with EBind (_, _, e1, _, _) | EBindRec (_, _, e1, _, _) -> Some e1 | _ -> None in
  match (T.NodeTbl.find_opt T.node_bindings node, rhs) with
  | None, _ | _, None -> None
  | Some binds, Some e1 -> (
      match (List.assoc_opt x binds, T.NodeTbl.find_opt T.node_types e1) with
      | None, _ | _, None -> None
      | Some sch, Some t1 ->
          let vars, m = scheme_parts sch in
          (* The scheme's variables, named as in this definition's solution. *)
          let m' = resolve ctx.sol t1 in
          let pairs = ref [] in
          align pairs m m';
          let vars =
            List.filter_map
              (fun v ->
                match List.assoc_opt v !pairs with
                | Some w -> ( match resolve ctx.sol (TypeVar w) with TypeVar w' -> Some w' | _ -> None)
                | None -> None)
              vars
          in
          let m = m' in
          let free = List.filter (fun v -> not (Hashtbl.mem ctx.theta v)) vars in
          let rec mentions v t =
            match t with
            | TypeVar w -> String.equal v w
            | TCtorApp (w, ts) -> String.equal v w || List.exists (mentions v) ts
            | FunctionType (a, b) -> mentions v a || mentions v b
            | VectorType ts | CTypeApp (_, ts) -> List.exists (mentions v) ts
            | CListType a | FixedPoint (_, a) -> mentions v a
            | RecordType fs -> List.exists (fun (_, t) -> mentions v t) fs
            | _ -> false
          in
          if List.exists (fun v -> mentions v m) free then Some m else None)

(** Type of the variable a [let] binds (for the expected type of its
    right-hand side). *)
let binder_ty (ctx : ctx) (node : c_expr) (p : c_pat) : mono_type option =
  match p with
  | CIdPat x -> (
      match T.NodeTbl.find_opt T.node_bindings node with
      | Some binds -> (
          match List.assoc_opt x binds with
          | Some sch -> Some (ty_of ctx (snd (scheme_parts sch)))
          | None -> None)
      | None -> None)
  | _ -> None

let rec request (top : top) (occ : mono_type) : string =
  match top.t_kind with
  | KPat -> top.t_name
  | KVal _ when top.t_vars = [] -> top.t_name
  | KFun body when top.t_vars = [] ->
      if not (Hashtbl.mem generated top.t_name) then (
        Hashtbl.replace generated top.t_name ();
        let ctx = { sol = top.t_sol; theta = Hashtbl.create 1; scope = SM.empty } in
        let e = conv ctx body in
        funs := Fun (top.t_name, e) :: !funs);
      top.t_name
  | KFun _ | KVal _ -> (
      let key = top.t_name ^ "|" ^ type_key occ in
      match Hashtbl.find_opt copies key with
      | Some n -> n
      | None ->
          let n = fresh top.t_name in
          Hashtbl.replace copies key n;
          let theta = Hashtbl.create 8 in
          match_ty theta (resolve top.t_sol top.t_scheme) occ;
          let ctx = { sol = top.t_sol; theta; scope = SM.empty } in
          (match top.t_kind with
          | KFun body ->
              (* [conv] may add other copies to [funs]: read it afterwards. *)
              let e = conv ctx body in
              funs := Fun (n, e) :: !funs
          | KVal rhs ->
              let e = conv ~expect:occ ctx rhs in
              pending_vals := Val ({ p = PVar n; pty = e.ty }, e) :: !pending_vals
          | KPat -> ());
          n)

(** [expect]: the type the context requires, used for [\[\]] (all [ENil]
    nodes are the same physical node, so the logged type of one is useless). *)
and conv ?expect (ctx : ctx) (e : c_expr) : expr =
  let ty () = node_ty ctx e in
  let mk d = { e = d; ty = ty () } in
  let dom_cod t = match t with FunctionType (a, b) -> (Some a, Some b) | _ -> (None, None) in
  let elem t = match t with Some (CListType a) -> Some a | _ -> None in
  match e with
  | EInt n -> { e = Int n; ty = IntType }
  | EFloat f -> { e = Float f; ty = FloatType }
  | EBool b -> { e = Bool b; ty = BoolType }
  | EString s -> { e = Str s; ty = StringType }
  | EChar c -> { e = Char c; ty = CharType }
  | EUnit -> { e = Unit; ty = UnitType }
  | ENil -> { e = Nil; ty = (match expect with Some t -> t | None -> CListType UnitType) }
  | EApp (EId (n, _), EUnit) when String.equal n nil_name -> mk Nil
  | EId (x, _) -> conv_id ctx e x
  | EApp (a, b) ->
      let a' = conv ctx a in
      let b' = conv ?expect:(fst (dom_cod a'.ty)) ctx b in
      mk (App (a', b'))
  | EFunction (p, _, body) -> (
      match ty () with
      | FunctionType (dom, cod) as t ->
          let tp, ctx' = conv_pat ctx p dom in
          { e = Lam (tp, conv ~expect:cod ctx' body); ty = t }
      | _ -> unsupported "internal: lambda without a function type")
  | EBind (p, _, e1, e2, _) -> conv_let ctx e p e1 e2
  | EBindRec (p, _, e1, e2, _) -> conv_letrec ctx e [ (p, e1) ] e2
  | EBindMutRec (bs, body) -> conv_letrec ctx e (List.map (fun (p, _, e1, _, _) -> (p, e1)) bs) body
  | ETernary (c, a, b) ->
      let t = ty () in
      let c' = conv ctx c in
      let a' = conv ~expect:t ctx a in
      let b' = conv ~expect:t ctx b in
      { e = If (c', a', b'); ty = t }
  | ESwitch (s, brs) ->
      let t = ty () in
      let s' = conv ctx s in
      let brs' =
        List.map
          (fun (p, b) ->
            let tp, ctx' = conv_pat ctx p s'.ty in
            (tp, conv ~expect:t ctx' b))
          brs
      in
      { e = Case (s', brs'); ty = t }
  | EBop (op, a, b) -> (
      let t = ty () in
      match op with
      | CCons ->
          let a' = conv ?expect:(elem (Some t)) ctx a in
          let b' = conv ~expect:t ctx b in
          { e = Bop (op, a', b'); ty = t }
      | _ ->
          (* [xs == \[\]]: each side gives the other its type. *)
          let a' = if a = ENil then None else Some (conv ctx a) in
          let b' = conv ?expect:(Stdlib.Option.map (fun (x : expr) -> x.ty) a') ctx b in
          let a' = match a' with Some x -> x | None -> conv ~expect:b'.ty ctx a in
          { e = Bop (op, a', b'); ty = t })
  | EVector es -> (
      match ty () with
      | VectorType ts as t when List.length ts = List.length es ->
          { e = Tuple (List.map2 (fun x et -> conv ~expect:et ctx x) es ts); ty = t }
      | _ -> mk (Tuple (List.map (conv ctx) es)))
  | EListEnumeration (a, b) ->
      let a' = conv ctx a in
      let b' = conv ctx b in
      mk (Range (a', b'))
  | EListComprehension (body, gens) ->
      let gens', ctx' =
        List.fold_left
          (fun (acc, c) (p, src) ->
            let s' = conv c src in
            match s'.ty with
            | CListType elt ->
                let tp, c' = conv_pat c p elt in
                ((tp, s') :: acc, c')
            | _ -> unsupported "internal: comprehension over a non-list")
          ([], ctx) gens
      in
      let t = ty () in
      let body' = conv ?expect:(elem (Some t)) ctx' body in
      { e = Comp (body', List.rev gens'); ty = t }
  | ERecordLit fs ->
      let t = ty () in
      let ft f = match t with RecordType tfs -> List.assoc_opt f tfs | _ -> None in
      let fs' = List.map (fun (f, x) -> (f, conv ?expect:(ft f) ctx x)) fs in
      { e = Record (List.sort (fun (a, _) (b, _) -> compare a b) fs'); ty = t }
  | ERecordUpdate (r, fs) ->
      let r' = conv ctx r in
      let ft f = match r'.ty with RecordType tfs -> List.assoc_opt f tfs | _ -> None in
      let fs' = List.map (fun (f, x) -> (f, conv ?expect:(ft f) ctx x)) fs in
      mk (Update (r', fs'))
  | EFieldAccess (r, f) ->
      let r' = conv ctx r in
      mk (Field (r', f))
  | EBlock _ -> unsupported "type declarations inside a block"

and conv_id (ctx : ctx) (node : c_expr) (x : string) : expr =
  let occ () = node_ty ctx node in
  match SM.find_opt x ctx.scope with
  | Some (LMono u) -> { e = Var u; ty = occ () }
  | Some (LPoly lp) ->
      let t = occ () in
      let key = type_key t in
      let n =
        match List.assoc_opt key !(lp.lp_copies) with
        | Some (n, _) -> n
        | None ->
            let th = Hashtbl.copy ctx.theta in
            match_ty th lp.lp_scheme t;
            let n = fresh lp.lp_name in
            lp.lp_copies := !(lp.lp_copies) @ [ (key, (n, th)) ];
            n
      in
      { e = Var n; ty = t }
  | None -> (
      match Hashtbl.find_opt tops x with
      | Some top ->
          let t = occ () in
          { e = Global (request top t); ty = t }
      | None -> (
          match ctor_tag x with
          | Some tag -> { e = Ctor (x, tag); ty = occ () }
          | None ->
              if List.mem x prims then { e = Prim x; ty = occ () }
              else unsupported ("unbound name `" ^ x ^ "`")))

and conv_let ctx node p e1 e2 =
  match p with
  | CIdPat x when local_poly_scheme ctx node x <> None ->
      let scheme = Stdlib.Option.get (local_poly_scheme ctx node x) in
      let lp = { lp_name = x; lp_scheme = scheme; lp_copies = ref [] } in
      let body = conv ~expect:(node_ty ctx node) { ctx with scope = SM.add x (LPoly lp) ctx.scope } e2 in
      List.fold_right
        (fun (_, (n, th)) acc ->
          let c = { ctx with theta = th } in
          let e1' = conv ?expect:(binder_ty c node p) c e1 in
          { e = Let ({ p = PVar n; pty = e1'.ty }, e1', acc); ty = acc.ty })
        !(lp.lp_copies) body
  | _ ->
      let e1' = conv ?expect:(binder_ty ctx node p) ctx e1 in
      let tp, ctx' = conv_pat ctx p e1'.ty in
      let e2' = conv ~expect:(node_ty ctx node) ctx' e2 in
      { e = Let (tp, e1', e2'); ty = e2'.ty }

and conv_letrec ctx node bs body =
  let names =
    List.map
      (fun (p, _) ->
        match p with
        | CIdPat x -> x
        | _ -> unsupported "let rec needs a name")
      bs
  in
  match (bs, names) with
  | [ (_, e1) ], [ x ] when local_poly_scheme ctx node x <> None ->
      let scheme = Stdlib.Option.get (local_poly_scheme ctx node x) in
      let lp = { lp_name = x; lp_scheme = scheme; lp_copies = ref [] } in
      let ctx_rec = { ctx with scope = SM.add x (LPoly lp) ctx.scope } in
      let body' = conv ~expect:(node_ty ctx node) ctx_rec body in
      let rec loop done_ acc =
        match List.filter (fun (k, _) -> not (List.mem k done_)) !(lp.lp_copies) with
        | [] -> List.rev acc
        | (k, (n, th)) :: _ -> loop (k :: done_) ((n, conv { ctx_rec with theta = th } e1) :: acc)
      in
      let group = loop [] [] in
      if group = [] then body' else { e = LetRec (group, body'); ty = body'.ty }
  | _ ->
      let ctx', uniq = bind_names ctx names in
      let group = List.map2 (fun u (p, e1) -> (u, conv ?expect:(binder_ty ctx node p) ctx' e1)) uniq bs in
      let body' = conv ~expect:(node_ty ctx node) ctx' body in
      { e = LetRec (group, body'); ty = body'.ty }

(** Type pattern [p] at [ty]; its variables are bound to fresh names in the
    returned context (kept as they are with [~rename:false], for top-level
    patterns, whose variables are globals). *)
and conv_pat ?(rename = true) (ctx : ctx) (p : c_pat) (ty : mono_type) : pat * ctx =
  let scope = ref ctx.scope in
  let var x =
    if rename then (
      let u = fresh x in
      scope := SM.add x (LMono u) !scope;
      u)
    else x
  in
  let rec go p ty =
    let mk d = { p = d; pty = ty } in
    match p with
    | CIdPat x -> mk (PVar (var x))
    | CWildcardPat -> mk PWild
    | CUnitPat -> mk PUnit
    | CNilPat -> mk PNil
    | CIntPat n -> mk (PInt n)
    | CBoolPat b -> mk (PBool b)
    | CStringPat s -> mk (PStr s)
    | CCharPat c -> mk (PChar c)
    | CConsPat (a, b) -> (
        match ty with
        | CListType t ->
            let a' = go a t in
            mk (PCons (a', go b ty))
        | _ -> unsupported "internal: cons pattern on a non-list")
    | CVectorPat ps -> (
        match ty with
        | VectorType ts when List.length ts = List.length ps -> mk (PTuple (List.map2 go ps ts))
        | _ -> unsupported "internal: tuple pattern on a non-tuple")
    | CRecordPat fs -> (
        match ty with
        | RecordType tfs ->
            mk
              (PRecord
                 (List.map
                    (fun (f, q) ->
                      match List.assoc_opt f tfs with
                      | Some t -> (f, go q t)
                      | None -> unsupported ("internal: no field " ^ f))
                    fs))
        | _ -> unsupported "internal: record pattern on a non-record")
    | CVariantPat ("[]", None) -> mk PNil
    | CVariantPat ("::", Some (CVectorPat [ a; b ])) -> go (CConsPat (a, b)) ty
    | CVariantPat (c, arg) -> (
        match ctor_tag c with
        | None -> unsupported ("unknown constructor " ^ c)
        | Some tag ->
            let payload =
              match arg with
              | None -> None
              | Some q -> (
                  match ctor_payload c ty with
                  | Some pt -> Some (go q pt)
                  | None -> unsupported ("constructor " ^ c ^ " has no payload"))
            in
            mk (PCtor (c, tag, payload)))
  in
  let tp = go p ty in
  (tp, { ctx with scope = !scope })

(* ------------------------------------------------------------------------ *)
(* Programs                                                                 *)
(* ------------------------------------------------------------------------ *)

(** The library functions the interpreter defines in Forge ({!Env.code_mapping}:
    [map], [filter], operators as values, ...), as top-level definitions to
    prepend to a program. *)
let builtin_defns () : c_defn list =
  List.map
    (fun (id, code) ->
      let tokens = Lex.lex (code |> String.to_seq |> List.of_seq) in
      let token_types = List.map (fun (t : Lex.token) -> t.token_type) tokens in
      let e =
        match Parser.ExprParser.expr_parser token_types with
        | Some (e, _) -> Condense.condense_expr e
        | None -> failwith ("internal: cannot parse the definition of " ^ id)
      in
      let rename inner =
        Specialize.map_free
          (fun x -> if String.equal x inner then Some (EId (id, None)) else None)
          Specialize.SS.empty
      in
      match e with
      | EBindRec (CIdPat inner, _, e1, EId (inner', _), _) when String.equal inner inner' ->
          CDefnRec (CIdPat id, [], None, rename inner e1, None, 0)
      | EBind (CIdPat inner, _, e1, EId (inner', _), _) when String.equal inner inner' ->
          CDefn (CIdPat id, [], None, e1, None, 0)
      | e -> CDefn (CIdPat id, [], None, e, None, 0))
    Env.code_mapping

let is_lambda = function EFunction _ -> true | _ -> false

(** Rebuild every node, so that each occurrence is a distinct physical node
    (the node-type table is keyed by physical identity; {!Specialize} shares
    nodes). [ENil] and [EUnit] are constants and stay shared. *)
let rec freshen (e : c_expr) : c_expr =
  match e with
  | EId (x, l) -> EId (x, l)
  | EInt n -> EInt n
  | EFloat f -> EFloat f
  | EBool b -> EBool b
  | EString s -> EString s
  | EChar c -> EChar c
  | ENil ->
      (* [Sys.opaque_identity]: a constant expression would be allocated
         once, statically, and shared again. *)
      EApp (EId (Sys.opaque_identity nil_name, None), EUnit)
  | EUnit -> e
  | _ -> Specialize.map_children (fun _ x -> freshen x) Specialize.SS.empty e

let freshen_defn (d : c_defn) : c_defn =
  match d with
  | CDefn (p, cs, t, b, r, n) -> CDefn (p, cs, t, freshen b, r, n)
  | CDefnRec (p, cs, t, b, r, n) -> CDefnRec (p, cs, t, freshen b, r, n)
  | CDefnMutRec ds -> CDefnMutRec (List.map (fun (p, cs, t, b, r, n) -> (p, cs, t, freshen b, r, n)) ds)
  | d -> d

(** Monomorphise a class-free program. *)
let program (defs : c_defn list) : program =
  Hashtbl.reset tops;
  Hashtbl.reset copies;
  Hashtbl.reset generated;
  funs := [];
  pending_vals := [];
  counter := 0;
  T.NodeTbl.reset T.node_types;
  T.NodeTbl.reset T.node_bindings;
  static_env := (nil_name, nil_scheme) :: Build_env.build_full_static_env ();
  type_env := [];
  ctor_env := [];
  let items = ref [] in
  let emit it =
    items := List.rev_append (List.rev !pending_vals) !items;
    pending_vals := [];
    items := it :: !items
  in
  T.type_logging := true;
  Fun.protect
    ~finally:(fun () -> T.type_logging := false)
    (fun () ->
      List.iter
        (fun d ->
          let d = freshen_defn d in
          match T.generate_defn !static_env !type_env d with
          | T.Error err ->
              unsupported
                ("internal: the specialised program does not type-check: "
                ^ T.string_of_type_check_error err)
          | T.Ok (binds, tenv, cenv) -> (
              static_env := binds @ !static_env;
              type_env := tenv @ !type_env;
              ctor_env := cenv @ !ctor_env;
              let sol () = solution !T.last_defn_equations in
              let register name kind s =
                match List.assoc_opt name binds with
                | Some sch ->
                    let vars, m = scheme_parts sch in
                    (* Name the scheme's variables as in [s] (see [align]). *)
                    let body = match kind with KFun b | KVal b -> Some b | KPat -> None in
                    let vars, m =
                      match body with
                      | Some b -> (
                          match T.NodeTbl.find_opt T.node_types b with
                          | Some t ->
                              let m' = resolve s t in
                              let pairs = ref [] in
                              align pairs m m';
                              ( List.filter_map
                                  (fun v ->
                                    match List.assoc_opt v !pairs with
                                    | Some w -> ( match resolve s (TypeVar w) with TypeVar w' -> Some w' | _ -> None)
                                    | None -> None)
                                  vars,
                                m' )
                          | None -> (vars, m))
                      | None -> (vars, m)
                    in
                    (* A type variable left free at top level (the type
                       checker does not generalise mutually recursive
                       definitions) is as good as quantified. *)
                    let rec free acc t =
                      match t with
                      | TypeVar v -> if List.mem v acc then acc else v :: acc
                      | TCtorApp (w, ts) -> List.fold_left free (if List.mem w acc then acc else w :: acc) ts
                      | FunctionType (a, b) -> free (free acc a) b
                      | VectorType ts | CTypeApp (_, ts) -> List.fold_left free acc ts
                      | CListType a | FixedPoint (_, a) -> free acc a
                      | RecordType fs -> List.fold_left (fun acc (_, t) -> free acc t) acc fs
                      | _ -> acc
                    in
                    let vars = vars @ List.filter (fun v -> not (List.mem v vars)) (free [] (resolve s m)) in
                    Hashtbl.replace tops name
                      { t_name = name; t_vars = vars; t_scheme = m; t_kind = kind; t_sol = s }
                | None -> ()
              in
              match d with
              | CDefn (CIdPat x, _, _, body, _, _) | CDefnRec (CIdPat x, _, _, body, _, _)
                when is_lambda body ->
                  register x (KFun body) (sol ())
              | CDefn (CIdPat x, _, _, body, _, _) | CDefnRec (CIdPat x, _, _, body, _, _) -> (
                  let s = sol () in
                  register x (KVal body) s;
                  match Hashtbl.find_opt tops x with
                  | Some top when top.t_vars = [] ->
                      let ctx = { sol = s; theta = Hashtbl.create 1; scope = SM.empty } in
                      let e = conv ~expect:(ty_of ctx top.t_scheme) ctx body in
                      emit (Val ({ p = PVar x; pty = e.ty }, e))
                  | _ -> ())
              | CDefn (p, _, _, body, _, _) | CDefnRec (p, _, _, body, _, _) ->
                  let s = sol () in
                  List.iter (fun x -> register x KPat s) (Condense.c_pat_bound_vars p);
                  let ctx = { sol = s; theta = Hashtbl.create 1; scope = SM.empty } in
                  let e = conv ctx body in
                  emit (Val (fst (conv_pat ~rename:false ctx p e.ty), e))
              | CDefnMutRec ds ->
                  let s = sol () in
                  List.iter
                    (fun (p, _, _, body, _, _) ->
                      match p with
                      | CIdPat x when is_lambda body -> register x (KFun body) s
                      | _ -> unsupported "mutually recursive definitions must be functions")
                    ds
              | CClassDecl _ | CTypeAlias _ | CSumType _ | CSumTypeRec _ | CSumTypeRecMutRec _ -> ()))
        defs);
  if !pending_vals <> [] then (
    items := List.rev_append (List.rev !pending_vals) !items;
    pending_vals := []);
  List.rev !items @ List.rev !funs
