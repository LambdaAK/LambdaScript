(** Static type-class specialisation for the native compiler.

    The evidence-based elaborator ({!Typecheck.elaborate_defn} with
    [~rewrite_constrained_calls:true]) turns a program into explicit
    dictionary passing: constrained bindings take dictionaries as leading
    [__dict_*] parameters, and every [impl] is a dictionary record
    [__forge_dict_*] (a function of the dictionaries it requires).

    In a whole program every dictionary that is actually passed is closed
    (an instance applied to instances), so the native compiler removes
    dictionaries altogether:

    - a constrained binding is copied once per tuple of dictionaries it is
      used with ([println] at [Show String] becomes [println__sp3]);
    - a method of a known dictionary ([d.show] with [d] = [Show (List Int)])
      becomes a reference to that instance's implementation, specialised in
      the same way.

    The result is an ordinary class-free program, which is type-checked again
    before it is lowered. A predicate the elaborator found ambiguous (it falls
    back to run-time dispatch in the interpreter, e.g. [show \[\]]) is resolved
    to the [Unit] (or [Int]) instance: by parametricity such a dictionary is
    never applied to a value. *)

open Cexpr

exception Unsupported of string

let unsupported msg = raise (Unsupported msg)

module SS = Set.Make (String)

let is_dict_param (n : string) : bool = String.starts_with ~prefix:"__dict_" n
let is_instance_name (n : string) : bool = String.starts_with ~prefix:"__forge_dict_" n
let pat_vars : c_pat -> string list = Condense.c_pat_bound_vars
let add_pat (bound : SS.t) (p : c_pat) : SS.t = SS.union bound (SS.of_list (pat_vars p))

(** {1 Traversal of free identifiers} *)

(** Replace every free identifier [x] of [e] (not bound locally, nor in
    [bound]) for which [f x] is [Some e']. *)
let rec map_free (f : string -> c_expr option) (bound : SS.t) (e : c_expr) : c_expr =
  let go = map_free f bound in
  let under p = map_free f (add_pat bound p) in
  match e with
  | EId (x, _) -> (
      if SS.mem x bound then e else match f x with Some e' -> e' | None -> e)
  | EFunction (p, t, b) -> EFunction (p, t, under p b)
  | EBind (p, t, e1, e2, r) -> EBind (p, t, go e1, under p e2, r)
  | EBindRec (p, t, e1, e2, r) -> EBindRec (p, t, under p e1, under p e2, r)
  | EBindMutRec (bs, body) ->
      let bound' = List.fold_left (fun acc (p, _, _, _, _) -> add_pat acc p) bound bs in
      let g = map_free f bound' in
      EBindMutRec (List.map (fun (p, t, e1, r, n) -> (p, t, g e1, r, n)) bs, g body)
  | EBlock parts -> EBlock (map_free_block f bound parts)
  | ETernary (a, b, c) -> ETernary (go a, go b, go c)
  | ESwitch (s, brs) -> ESwitch (go s, List.map (fun (p, b) -> (p, under p b)) brs)
  | EApp (a, b) -> EApp (go a, go b)
  | EBop (o, a, b) -> EBop (o, go a, go b)
  | EVector es -> EVector (List.map go es)
  | EListEnumeration (a, b) -> EListEnumeration (go a, go b)
  | EListComprehension (body, gens) ->
      let gens', bound' =
        List.fold_left
          (fun (acc, bnd) (p, ge) -> ((p, map_free f bnd ge) :: acc, add_pat bnd p))
          ([], bound) gens
      in
      EListComprehension (map_free f bound' body, List.rev gens')
  | ERecordLit fs -> ERecordLit (List.map (fun (n, x) -> (n, go x)) fs)
  | ERecordUpdate (r, fs) -> ERecordUpdate (go r, List.map (fun (n, x) -> (n, go x)) fs)
  | EFieldAccess (r, m) -> EFieldAccess (go r, m)
  | EBool _ | EString _ | EUnit | EInt _ | EChar _ | EFloat _ | ENil -> e

and map_free_block f bound parts =
  match parts with
  | [] -> []
  | Expr e :: rest -> Expr (map_free f bound e) :: map_free_block f bound rest
  | Defn d :: rest ->
      let d', bound' = map_free_defn f bound d in
      Defn d' :: map_free_block f bound' rest

and map_free_defn f bound d : c_defn * SS.t =
  match d with
  | CDefn (p, cs, t, e1, r, k) -> (CDefn (p, cs, t, map_free f bound e1, r, k), add_pat bound p)
  | CDefnRec (p, cs, t, e1, r, k) ->
      let bound' = add_pat bound p in
      (CDefnRec (p, cs, t, map_free f bound' e1, r, k), bound')
  | CDefnMutRec ds ->
      let bound' = List.fold_left (fun acc (p, _, _, _, _, _) -> add_pat acc p) bound ds in
      ( CDefnMutRec
          (List.map (fun (p, cs, t, e1, r, k) -> (p, cs, t, map_free f bound' e1, r, k)) ds),
        bound' )
  | d -> (d, bound)

(** Rewrite every application spine [x a1 ... an] (n >= 0) whose head is a
    free identifier: [f x args] (with the arguments already rewritten) gives
    the replacement, or [None] to keep the spine. *)
let rec map_free_spines (f : string -> c_expr list -> c_expr option) (bound : SS.t)
    (e : c_expr) : c_expr =
  let rec spine e acc = match e with EApp (g, a) -> spine g (a :: acc) | h -> (h, acc) in
  match e with
  | EApp _ | EId _ -> (
      let h, args = spine e [] in
      let args' = List.map (map_free_spines f bound) args in
      let rebuild h = List.fold_left (fun acc a -> EApp (acc, a)) h args' in
      match h with
      | EId (x, _) when not (SS.mem x bound) -> (
          match f x args' with Some e' -> e' | None -> rebuild h)
      | EId _ -> rebuild h
      | _ -> rebuild (map_free_spines f bound h))
  | _ ->
      (* Every other construct: recurse with [map_free]'s scoping, through a
         replacement that never fires at the top. *)
      map_children (map_free_spines f) bound e

(** Apply [g bound'] to the immediate subexpressions of [e], where [bound']
    adds the variables [e] binds around each of them. *)
and map_children (g : SS.t -> c_expr -> c_expr) (bound : SS.t) (e : c_expr) : c_expr =
  let go = g bound in
  let under p = g (add_pat bound p) in
  match e with
  | EId _ | EBool _ | EString _ | EUnit | EInt _ | EChar _ | EFloat _ | ENil -> e
  | EApp (a, b) -> EApp (go a, go b)
  | EFunction (p, t, b) -> EFunction (p, t, under p b)
  | EBind (p, t, e1, e2, r) -> EBind (p, t, go e1, under p e2, r)
  | EBindRec (p, t, e1, e2, r) -> EBindRec (p, t, under p e1, under p e2, r)
  | EBindMutRec (bs, body) ->
      let bound' = List.fold_left (fun acc (p, _, _, _, _) -> add_pat acc p) bound bs in
      EBindMutRec (List.map (fun (p, t, e1, r, n) -> (p, t, g bound' e1, r, n)) bs, g bound' body)
  | EBlock parts ->
      let rec block bound = function
        | [] -> []
        | Expr x :: rest -> Expr (g bound x) :: block bound rest
        | Defn d :: rest ->
            let d', bound' =
              match d with
              | CDefn (p, cs, t, e1, r, k) -> (CDefn (p, cs, t, g bound e1, r, k), add_pat bound p)
              | CDefnRec (p, cs, t, e1, r, k) ->
                  let b' = add_pat bound p in
                  (CDefnRec (p, cs, t, g b' e1, r, k), b')
              | CDefnMutRec ds ->
                  let b' = List.fold_left (fun acc (p, _, _, _, _, _) -> add_pat acc p) bound ds in
                  (CDefnMutRec (List.map (fun (p, cs, t, e1, r, k) -> (p, cs, t, g b' e1, r, k)) ds), b')
              | d -> (d, bound)
            in
            Defn d' :: block bound' rest
      in
      EBlock (block bound parts)
  | ETernary (a, b, c) -> ETernary (go a, go b, go c)
  | ESwitch (sc, brs) -> ESwitch (go sc, List.map (fun (p, b) -> (p, under p b)) brs)
  | EBop (o, a, b) -> EBop (o, go a, go b)
  | EVector es -> EVector (List.map go es)
  | EListEnumeration (a, b) -> EListEnumeration (go a, go b)
  | EListComprehension (body, gens) ->
      let gens', bound' =
        List.fold_left (fun (acc, bnd) (p, ge) -> ((p, g bnd ge) :: acc, add_pat bnd p)) ([], bound) gens
      in
      EListComprehension (g bound' body, List.rev gens')
  | ERecordLit fs -> ERecordLit (List.map (fun (n, x) -> (n, go x)) fs)
  | ERecordUpdate (r, fs) -> ERecordUpdate (go r, List.map (fun (n, x) -> (n, go x)) fs)
  | EFieldAccess (r, m) -> EFieldAccess (go r, m)

(** {1 Unique top-level names}

    Specialised copies are emitted where they are first used, which may be
    after a later top-level definition has shadowed a name the original
    refers to. Every top-level definition but the last of each name is
    therefore renamed first, so that names are unique. *)

let defn_names (d : c_defn) : string list =
  match d with
  | CDefn (p, _, _, _, _, _) | CDefnRec (p, _, _, _, _, _) -> pat_vars p
  | CDefnMutRec ds -> List.concat_map (fun (p, _, _, _, _, _) -> pat_vars p) ds
  | _ -> []

let rename_duplicates (defs : c_defn list) : c_defn list =
  let total = Hashtbl.create 64 in
  (* Builtin primitives count as an earlier definition: a program that
     redefines [print_string] still means the builtin before that point. *)
  List.iter
    (fun (n, v, _) -> match v with BuiltInFunction _ -> Hashtbl.replace total n 1 | _ -> ())
    Env.built_ins;
  List.iter
    (fun d ->
      List.iter
        (fun n -> Hashtbl.replace total n (1 + Stdlib.Option.value ~default:0 (Hashtbl.find_opt total n)))
        (defn_names d))
    defs;
  if Hashtbl.fold (fun _ c acc -> acc || c > 1) total false = false then defs
  else
    let seen = Hashtbl.create 16 in
    let current : (string, string) Hashtbl.t = Hashtbl.create 16 in
    let bind_names d =
      List.iter
        (fun n ->
          let t = Hashtbl.find total n in
          if t > 1 then (
            let k = 1 + Stdlib.Option.value ~default:0 (Hashtbl.find_opt seen n) in
            Hashtbl.replace seen n k;
            Hashtbl.replace current n (if k = t then n else Printf.sprintf "%s__v%d" n k)))
        (defn_names d)
    in
    let subst e =
      map_free
        (fun x ->
          match Hashtbl.find_opt current x with
          | Some n when not (String.equal n x) -> Some (EId (n, None))
          | _ -> None)
        SS.empty e
    in
    let rec rename_pat p =
      match p with
      | CIdPat x -> CIdPat (Stdlib.Option.value ~default:x (Hashtbl.find_opt current x))
      | CConsPat (a, b) -> CConsPat (rename_pat a, rename_pat b)
      | CVectorPat ps -> CVectorPat (List.map rename_pat ps)
      | CRecordPat fs -> CRecordPat (List.map (fun (f, q) -> (f, rename_pat q)) fs)
      | CVariantPat (c, Some q) -> CVariantPat (c, Some (rename_pat q))
      | p -> p
    in
    List.map
      (fun d ->
        match d with
        | CDefn (p, cs, t, body, r, k) ->
            let body = subst body in
            bind_names d;
            CDefn (rename_pat p, cs, t, body, r, k)
        | CDefnRec (p, cs, t, body, r, k) ->
            bind_names d;
            CDefnRec (rename_pat p, cs, t, subst body, r, k)
        | CDefnMutRec ds ->
            bind_names d;
            CDefnMutRec
              (List.map (fun (p, cs, t, body, r, k) -> (rename_pat p, cs, t, subst body, r, k)) ds)
        | d -> d)
      defs

(** {1 Dictionaries} *)

(** A closed dictionary: an instance applied to the dictionaries it
    requires. *)
type dict = { inst : string; args : dict list }

let rec dict_key (d : dict) : string =
  if d.args = [] then d.inst
  else d.inst ^ "(" ^ String.concat "," (List.map dict_key d.args) ^ ")"

(** A definition taking dictionaries: a constrained binding, an instance
    dictionary, or a local definition of an instance (with the instance's own
    dictionaries already fixed in [g_sigma]). *)
type generic = {
  g_base : string;  (** name the specialised copies are named after *)
  g_params : string list;  (** dictionary parameters *)
  g_body : c_expr;  (** body under the dictionary parameters *)
  g_inst : bool;
  g_sigma : (string * dict) list;
  g_n : int;
}

let generics : (string, generic) Hashtbl.t = Hashtbl.create 64

(** Specialisation key -> name of the copy. *)
let spec_names : (string, string) Hashtbl.t = Hashtbl.create 64

(** [dict_key ^ "." ^ method] -> expression for that method. *)
let method_refs : (string, c_expr) Hashtbl.t = Hashtbl.create 64

(** Instances whose local definitions have been registered as generics. *)
let lifted_instances : (string, (string * c_expr) list) Hashtbl.t = Hashtbl.create 16

let counter = ref 0

let fresh_name (base : string) : string =
  incr counter;
  Printf.sprintf "%s__sp%d" base !counter

(** A specialised copy waiting to be emitted. *)
type spec = { s_name : string; s_body : c_expr; s_deps : string list; s_n : int }

let batch : spec list ref = ref []

(** Specialised names referred to by the body being rewritten. *)
let deps : string list ref list ref = ref []

let add_dep (n : string) : unit =
  match !deps with d :: _ -> if not (List.mem n !d) then d := n :: !d | [] -> ()

let rec leading_dict_params acc (e : c_expr) : string list * c_expr =
  match e with
  | EFunction (CIdPat p, _, b) when is_dict_param p -> leading_dict_params (p :: acc) b
  | b -> (List.rev acc, b)

let has_dict_params (e : c_expr) : bool = fst (leading_dict_params [] e) <> []

let rec spine (e : c_expr) (acc : c_expr list) : c_expr * c_expr list =
  match e with EApp (f, a) -> spine f (a :: acc) | h -> (h, acc)

let apply (f : c_expr) (args : c_expr list) : c_expr =
  List.fold_left (fun acc a -> EApp (acc, a)) f args

let rec split_at n xs =
  if n = 0 then ([], xs)
  else match xs with [] -> ([], []) | x :: r -> let a, b = split_at (n - 1) r in (x :: a, b)

(** Instance for an ambiguous predicate [cls _]: [Unit], else [Int]. *)
let default_instance (cls : string) : dict =
  let candidates =
    List.filter
      (fun (i : Forge_class_util.instance) ->
        String.equal i.inst_class cls && i.inst_requires = [] && Hashtbl.mem generics i.inst_dict)
      !Forge_class_util.instances
  in
  let with_head h = List.find_opt (fun (i : Forge_class_util.instance) -> i.inst_head = h) candidates in
  match (with_head UnitType, with_head IntType) with
  | Some i, _ | None, Some i -> { inst = i.inst_dict; args = [] }
  | None, None ->
      unsupported
        ("the type at which `" ^ cls
       ^ "` is used is ambiguous here (add a type annotation)")

(** The run-time dispatching dictionary of {!Typecheck.dynamic_dictionary}. *)
let dynamic_dict_class (fs : (string * c_expr) list) : string option =
  let names = List.sort compare (List.map fst fs) in
  List.find_map
    (fun (cls, ms) ->
      if
        fs <> [] && List.sort compare ms = names
        && List.for_all
             (fun (m, e) ->
               match e with
               | EId (a, _) -> String.equal a (Forge_class_util.method_alias cls m)
               | _ -> false)
             fs
      then Some cls
      else None)
    !Forge_class_util.class_methods

(** {1 Rewriting} *)

type local_generic = {
  l_params : string list;
  l_body : c_expr;
  l_specs : (string * (string * dict list)) list ref;  (** key -> (name, dictionaries) *)
}

type ctx = {
  sigma : (string * dict) list;  (** dictionary parameters in scope *)
  locals : (string * local_generic) list;  (** constrained local bindings in scope *)
  bound : SS.t;  (** other local variables in scope *)
  in_spec : bool;  (** rewriting a specialised copy *)
}

let top_ctx = { sigma = []; locals = []; bound = SS.empty; in_spec = false }

let bind_names (ctx : ctx) (names : string list) : ctx =
  {
    ctx with
    bound = SS.union ctx.bound (SS.of_list names);
    locals = List.filter (fun (n, _) -> not (List.mem n names)) ctx.locals;
  }

let bind_pat ctx p = bind_names ctx (pat_vars p)

let rec unconstrain (t : c_type) : c_type =
  match t with
  | Constrained (_, t) -> unconstrain t
  | PolyType (v, t) -> PolyType (v, unconstrain t)
  | Mono _ -> t

let rec mono_has_var (m : mono_type) : bool =
  match m with
  | TypeVar _ | TCtorApp _ -> true
  | IntType | BoolType | StringType | CharType | UnitType | FloatType | TypeName _ -> false
  | CTypeApp (_, ts) | VectorType ts -> List.exists mono_has_var ts
  | CListType t -> mono_has_var t
  | FunctionType (a, b) -> mono_has_var a || mono_has_var b
  | RecordType fs -> List.exists (fun (_, t) -> mono_has_var t) fs
  | FixedPoint (_, t) -> mono_has_var t

(** Annotations lose their class constraints; in a specialised copy, an
    annotation mentioning type variables (which may now be instantiated) is
    dropped. *)
let ann (ctx : ctx) (t : c_type option) : c_type option =
  match t with
  | None -> None
  | Some t -> (
      match unconstrain t with
      | PolyType _ when ctx.in_spec -> None
      | Mono m when ctx.in_spec && mono_has_var m -> None
      | t -> Some t)

let rec dict_of (ctx : ctx) (e : c_expr) : dict option =
  match spine e [] with
  | EId (p, _), [] when is_dict_param p -> (
      match List.assoc_opt p ctx.sigma with
      | Some d -> Some d
      | None -> unsupported ("unresolved type-class dictionary `" ^ p ^ "`"))
  | EId (n, _), args when is_instance_name n && not (SS.mem n ctx.bound) -> (
      match Hashtbl.find_opt generics n with
      | Some g when g.g_inst && List.length g.g_params = List.length args ->
          Some { inst = n; args = List.map (dict_exn ctx) args }
      | _ -> None)
  | ERecordLit fs, [] -> (
      match dynamic_dict_class fs with Some cls -> Some (default_instance cls) | None -> None)
  | _ -> None

and dict_exn ctx e =
  match dict_of ctx e with
  | Some d -> d
  | None -> unsupported ("cannot resolve a type-class dictionary: " ^ C_to_string.string_of_expr e)

(** Name of the copy of [g] for dictionaries [ds], generating it (and the
    copies it needs) on first use. *)
and request (g : generic) (key_base : string) (ds : dict list) : string =
  let key = key_base ^ "|" ^ String.concat "," (List.map dict_key ds) in
  match Hashtbl.find_opt spec_names key with
  | Some n ->
      add_dep n;
      n
  | None ->
      let n = fresh_name g.g_base in
      Hashtbl.replace spec_names key n;
      add_dep n;
      let my_deps = ref [] in
      deps := my_deps :: !deps;
      let ctx =
        { top_ctx with sigma = List.combine g.g_params ds @ g.g_sigma; in_spec = true }
      in
      let body = rw ctx g.g_body in
      deps := List.tl !deps;
      batch := { s_name = n; s_body = body; s_deps = !my_deps; s_n = g.g_n } :: !batch;
      n

(** Expression for method [m] of the closed dictionary [d]. *)
and method_ref (d : dict) (m : string) : c_expr =
  let dk = dict_key d in
  let key = dk ^ "." ^ m in
  match Hashtbl.find_opt method_refs key with
  | Some e ->
      (match e with EId (n, _) -> add_dep n | _ -> ());
      e
  | None ->
      let g =
        match Hashtbl.find_opt generics d.inst with
        | Some g -> g
        | None -> unsupported ("unknown instance dictionary `" ^ d.inst ^ "`")
      in
      let sigma = List.combine g.g_params d.args @ g.g_sigma in
      let fields =
        match Hashtbl.find_opt lifted_instances dk with
        | Some fs -> fs
        | None ->
            (* [let rec l1 = e1 in ... {m1: f1, ...}]: every local definition
               becomes a generic of its own, with the instance's dictionaries
               fixed. *)
            let rec peel acc e =
              match e with
              | EBindRec (CIdPat l, _, e1, e2, _) | EBind (CIdPat l, _, e1, e2, _) ->
                  peel ((l, e1) :: acc) e2
              | ERecordLit fs -> (List.rev acc, fs)
              | _ ->
                  unsupported
                    ("unexpected shape of the instance dictionary `" ^ d.inst ^ "`")
            in
            let locals, fs = peel [] g.g_body in
            let lifted = List.map (fun (l, _) -> (l, l ^ "@" ^ dk)) locals in
            let ren =
              map_free
                (fun x ->
                  match List.assoc_opt x lifted with
                  | Some n -> Some (EId (n, None))
                  | None -> None)
                SS.empty
            in
            List.iter
              (fun (l, e1) ->
                let ps, b = leading_dict_params [] e1 in
                Hashtbl.replace generics (List.assoc l lifted)
                  { g_base = l; g_params = ps; g_body = ren b; g_inst = false; g_sigma = sigma; g_n = 0 })
              locals;
            let fs = List.map (fun (f, e) -> (f, ren e)) fs in
            Hashtbl.replace lifted_instances dk fs;
            fs
      in
      let fe =
        match List.assoc_opt m fields with
        | Some fe -> fe
        | None -> unsupported ("instance `" ^ d.inst ^ "` has no method `" ^ m ^ "`")
      in
      let e' = rw { top_ctx with sigma; in_spec = true } fe in
      let e' =
        match e' with
        | EId _ -> e'
        | _ ->
            let n = fresh_name "__forge_method" in
            batch := { s_name = n; s_body = e'; s_deps = []; s_n = 0 } :: !batch;
            add_dep n;
            EId (n, None)
      in
      Hashtbl.replace method_refs key e';
      e'

and rw (ctx : ctx) (e : c_expr) : c_expr =
  match e with
  | EFieldAccess (de, m) -> (
      match dict_of ctx de with
      | Some d -> method_ref d m
      | None -> EFieldAccess (rw ctx de, m))
  | EApp _ | EId _ -> rw_app ctx e
  | EFunction (p, t, b) -> EFunction (p, ann ctx t, rw (bind_pat ctx p) b)
  | EBind (CIdPat h, _, e1, e2, _) when has_dict_params e1 ->
      let specs, e2' = rw_local ctx ~is_rec:false h e1 (fun c -> rw c e2) in
      List.fold_right (fun (n, b) acc -> EBind (CIdPat n, None, b, acc, None)) specs e2'
  | EBindRec (CIdPat h, _, e1, e2, _) when has_dict_params e1 -> (
      let specs, e2' = rw_local ctx ~is_rec:true h e1 (fun c -> rw c e2) in
      match specs with
      | [] -> e2'
      | [ (n, b) ] -> EBindRec (CIdPat n, None, b, e2', None)
      | _ -> EBindMutRec (List.map (fun (n, b) -> (CIdPat n, None, b, None, 0)) specs, e2'))
  | EBind (p, t, e1, e2, r) -> EBind (p, ann ctx t, rw ctx e1, rw (bind_pat ctx p) e2, ann ctx r)
  | EBindRec (p, t, e1, e2, r) ->
      let ctx' = bind_pat ctx p in
      EBindRec (p, ann ctx t, rw ctx' e1, rw ctx' e2, ann ctx r)
  | EBindMutRec (bs, body) ->
      if List.exists (fun (_, _, e1, _, _) -> has_dict_params e1) bs then
        unsupported "constrained local mutually recursive definitions";
      let ctx' = bind_names ctx (List.concat_map (fun (p, _, _, _, _) -> pat_vars p) bs) in
      EBindMutRec
        (List.map (fun (p, t, e1, r, n) -> (p, ann ctx t, rw ctx' e1, ann ctx r, n)) bs, rw ctx' body)
  | EBlock parts -> EBlock (rw_block ctx parts)
  | ETernary (a, b, c) -> ETernary (rw ctx a, rw ctx b, rw ctx c)
  | ESwitch (s, brs) -> ESwitch (rw ctx s, List.map (fun (p, b) -> (p, rw (bind_pat ctx p) b)) brs)
  | EBop (o, a, b) -> EBop (o, rw ctx a, rw ctx b)
  | EVector es -> EVector (List.map (rw ctx) es)
  | EListEnumeration (a, b) -> EListEnumeration (rw ctx a, rw ctx b)
  | EListComprehension (body, gens) ->
      let gens', ctx' =
        List.fold_left (fun (acc, c) (p, ge) -> ((p, rw c ge) :: acc, bind_pat c p)) ([], ctx) gens
      in
      EListComprehension (rw ctx' body, List.rev gens')
  | ERecordLit fs -> ERecordLit (List.map (fun (n, x) -> (n, rw ctx x)) fs)
  | ERecordUpdate (r, fs) -> ERecordUpdate (rw ctx r, List.map (fun (n, x) -> (n, rw ctx x)) fs)
  | EBool _ | EString _ | EUnit | EInt _ | EChar _ | EFloat _ | ENil -> e

and rw_app ctx e =
  let h, args = spine e [] in
  let take_dicts k =
    if List.length args < k then
      unsupported "a constrained function is used without its dictionaries";
    let ds, rest = split_at k args in
    (List.map (dict_exn ctx) ds, List.map (rw ctx) rest)
  in
  match h with
  | EId (x, _) when List.mem_assoc x ctx.locals ->
      let lg = List.assoc x ctx.locals in
      let ds, rest = take_dicts (List.length lg.l_params) in
      let key = String.concat "," (List.map dict_key ds) in
      let n =
        match List.assoc_opt key !(lg.l_specs) with
        | Some (n, _) -> n
        | None ->
            let n = fresh_name x in
            lg.l_specs := (key, (n, ds)) :: !(lg.l_specs);
            n
      in
      apply (EId (n, None)) rest
  | EId (x, _) when (not (SS.mem x ctx.bound)) && not (is_instance_name x) && Hashtbl.mem generics x
    ->
      let g = Hashtbl.find generics x in
      let ds, rest = take_dicts (List.length g.g_params) in
      apply (EId (request g x ds, None)) rest
  | EId (x, _) when is_dict_param x || (is_instance_name x && not (SS.mem x ctx.bound)) ->
      unsupported ("a type-class dictionary used as a value (`" ^ x ^ "`)")
  | EId _ -> apply h (List.map (rw ctx) args)
  | _ -> apply (rw ctx h) (List.map (rw ctx) args)

(** A constrained local binding [h = fn dicts -> e1] scoping over [scope]:
    rewrite the scope first (collecting the dictionaries [h] is used with),
    then one copy of the body per use. *)
and rw_local : 'a. ctx -> is_rec:bool -> string -> c_expr -> (ctx -> 'a) -> (string * c_expr) list * 'a =
 fun ctx ~is_rec h e1 scope ->
  let ps, b = leading_dict_params [] e1 in
  let lg = { l_params = ps; l_body = b; l_specs = ref [] } in
  let ctx_scope = { (bind_names ctx [ h ]) with locals = (h, lg) :: List.remove_assoc h ctx.locals } in
  let scope' = scope ctx_scope in
  let ctx_body = if is_rec then ctx_scope else ctx in
  let done_keys = Hashtbl.create 4 in
  let results = ref [] in
  let rec loop () =
    match List.find_opt (fun (k, _) -> not (Hashtbl.mem done_keys k)) (List.rev !(lg.l_specs)) with
    | None -> ()
    | Some (k, (n, ds)) ->
        Hashtbl.add done_keys k ();
        let c = { ctx_body with sigma = List.combine ps ds @ ctx_body.sigma; in_spec = true } in
        results := (n, rw c lg.l_body) :: !results;
        loop ()
  in
  loop ();
  (List.rev !results, scope')

and rw_block ctx parts =
  match parts with
  | [] -> []
  | Expr e :: rest -> Expr (rw ctx e) :: rw_block ctx rest
  | Defn (CDefn (CIdPat h, _, _, e1, _, k)) :: rest when has_dict_params e1 ->
      let specs, rest' = rw_local ctx ~is_rec:false h e1 (fun c -> rw_block c rest) in
      List.map (fun (n, b) -> Defn (CDefn (CIdPat n, [], None, b, None, k))) specs @ rest'
  | Defn (CDefnRec (CIdPat h, _, _, e1, _, k)) :: rest when has_dict_params e1 -> (
      let specs, rest' = rw_local ctx ~is_rec:true h e1 (fun c -> rw_block c rest) in
      match specs with
      | [ (n, b) ] -> Defn (CDefnRec (CIdPat n, [], None, b, None, k)) :: rest'
      | _ ->
          (if specs = [] then [] else
             [ Defn (CDefnMutRec (List.map (fun (n, b) -> (CIdPat n, [], None, b, None, k)) specs)) ])
          @ rest')
  | Defn (CDefn (p, _, t, e1, r, k)) :: rest ->
      Defn (CDefn (p, [], ann ctx t, rw ctx e1, ann ctx r, k)) :: rw_block (bind_pat ctx p) rest
  | Defn (CDefnRec (p, _, t, e1, r, k)) :: rest ->
      let ctx' = bind_pat ctx p in
      Defn (CDefnRec (p, [], ann ctx t, rw ctx' e1, ann ctx r, k)) :: rw_block ctx' rest
  | Defn (CDefnMutRec ds) :: rest ->
      if List.exists (fun (_, _, _, e1, _, _) -> has_dict_params e1) ds then
        unsupported "constrained local mutually recursive definitions";
      let ctx' = bind_names ctx (List.concat_map (fun (p, _, _, _, _, _) -> pat_vars p) ds) in
      Defn
        (CDefnMutRec
           (List.map (fun (p, _, t, e1, r, k) -> (p, [], ann ctx t, rw ctx' e1, ann ctx r, k)) ds))
      :: rw_block ctx' rest
  | Defn d :: rest -> Defn d :: rw_block ctx rest

(** {1 Emission} *)

(** The copies generated for one top-level definition, dependencies first;
    mutually recursive copies become one [CDefnMutRec]. *)
let emit_batch () : c_defn list =
  let specs = List.rev !batch in
  batch := [];
  let names = List.map (fun s -> s.s_name) specs in
  let by_name = Hashtbl.create 16 in
  List.iter (fun s -> Hashtbl.replace by_name s.s_name s) specs;
  (* Tarjan's algorithm: components come out dependencies first. *)
  let index = Hashtbl.create 16 and low = Hashtbl.create 16 in
  let on_stack = Hashtbl.create 16 in
  let stack = ref [] and next = ref 0 and out = ref [] in
  let rec visit n =
    Hashtbl.replace index n !next;
    Hashtbl.replace low n !next;
    incr next;
    stack := n :: !stack;
    Hashtbl.replace on_stack n ();
    let s = Hashtbl.find by_name n in
    List.iter
      (fun m ->
        if Hashtbl.mem by_name m then
          if not (Hashtbl.mem index m) then (
            visit m;
            Hashtbl.replace low n (min (Hashtbl.find low n) (Hashtbl.find low m)))
          else if Hashtbl.mem on_stack m then
            Hashtbl.replace low n (min (Hashtbl.find low n) (Hashtbl.find index m)))
      s.s_deps;
    if Hashtbl.find low n = Hashtbl.find index n then (
      let rec pop acc =
        match !stack with
        | m :: rest ->
            stack := rest;
            Hashtbl.remove on_stack m;
            if String.equal m n then m :: acc else pop (m :: acc)
        | [] -> acc
      in
      let comp = List.map (Hashtbl.find by_name) (pop []) in
      let d =
        match comp with
        | [ s ] when List.mem s.s_name s.s_deps ->
            CDefnRec (CIdPat s.s_name, [], None, s.s_body, None, s.s_n)
        | [ s ] -> CDefn (CIdPat s.s_name, [], None, s.s_body, None, s.s_n)
        | _ ->
            CDefnMutRec
              (List.map (fun s -> (CIdPat s.s_name, [], None, s.s_body, None, s.s_n)) comp)
      in
      out := d :: !out)
  in
  List.iter (fun n -> if not (Hashtbl.mem index n) then visit n) names;
  List.rev !out

let register (name : string) (body : c_expr) (n : int) : unit =
  let ps, b = leading_dict_params [] body in
  Hashtbl.replace generics name
    { g_base = name; g_params = ps; g_body = b; g_inst = is_instance_name name; g_sigma = []; g_n = n }

(** Specialise an elaborated program (see the module documentation). *)
let program (defs : c_defn list) : c_defn list =
  Hashtbl.reset generics;
  Hashtbl.reset spec_names;
  Hashtbl.reset method_refs;
  Hashtbl.reset lifted_instances;
  batch := [];
  deps := [];
  let defs = rename_duplicates defs in
  let top_deps = ref [] in
  deps := [ top_deps ];
  let out = ref [] in
  let emit d =
    let specs = emit_batch () in
    out := List.rev_append specs !out;
    out := d :: !out
  in
  List.iter
    (fun d ->
      match d with
      | (CDefn (CIdPat name, _, _, body, _, n) | CDefnRec (CIdPat name, _, _, body, _, n))
        when is_instance_name name || has_dict_params body ->
          register name body n
      | CDefnMutRec ds when List.exists (fun (_, _, _, body, _, _) -> has_dict_params body) ds ->
          List.iter
            (fun (p, _, _, body, _, n) ->
              match p with
              | CIdPat name -> register name body n
              | _ -> unsupported "constrained mutually recursive definitions need names")
            ds
      | CDefn (p, _, t, body, r, n) ->
          let d' = CDefn (p, [], ann top_ctx t, rw top_ctx body, ann top_ctx r, n) in
          emit d'
      | CDefnRec (p, _, t, body, r, n) ->
          let d' = CDefnRec (p, [], ann top_ctx t, rw top_ctx body, ann top_ctx r, n) in
          emit d'
      | CDefnMutRec ds ->
          let d' =
            CDefnMutRec
              (List.map
                 (fun (p, _, t, body, r, n) -> (p, [], ann top_ctx t, rw top_ctx body, ann top_ctx r, n))
                 ds)
          in
          emit d'
      | d -> emit d)
    defs;
  deps := [];
  List.rev !out
