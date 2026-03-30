(** Lower a subset of {!Cexpr.c_expr} to {!Min_ir}.

    Top-level [let rec] / [let rec … and …] use a fixup environment: callable
    stubs are installed before lowering bodies so direct calls resolve.

    Multi-argument functions compile to a **direct** multi-arg [func_def] for
    saturated calls (including recursive calls) plus curried **step** functions
    that build heap environments and [MkClos]/[ClosApply] values for partial
    application and first-class use. Lambdas use the same shape and thread
    captured values in step environments.

    Single-argument functions without captures use a plain direct function and
    [Fun]/[FnAddr] when used as values.

    [&&] and [||] lower to [IAnd]/[IOr] (both operands evaluated; not short-circuit). *)

exception Unsupported of string

let unsupported msg = raise (Unsupported msg)

open Cexpr
open Min_ir
open Ceval

let counter = ref 0

let param_counter = ref 0

let nested_emit_ctr = ref 0

let lambda_ty_counter = ref 0

let eta_expand_counter = ref 0

module S = Set.Make (String)

let reset_fresh () =
  counter := 0;
  param_counter := 0;
  nested_emit_ctr := 0;
  lambda_ty_counter := 0;
  eta_expand_counter := 0

let fresh_lambda_ty_key () =
  incr lambda_ty_counter;
  "__ls_lam_t" ^ string_of_int !lambda_ty_counter

let fresh () =
  incr counter;
  "_t" ^ string_of_int !counter

let fresh_param () =
  incr param_counter;
  "_p" ^ string_of_int !param_counter

type fn_ctx = {
  mutable completed : block list;
  mutable cur_label : string;
  mutable cur_instrs : instr list;
  mutable lbl_counter : int;
  mutable nested_funcs : func_def list;
}

let create_fn_ctx () : fn_ctx =
  {
    completed = [];
    cur_label = "entry";
    cur_instrs = [];
    lbl_counter = 0;
    nested_funcs = [];
  }

let mangle_nested_emit (logical : string) : string =
  incr nested_emit_ctr;
  logical ^ "__lsn" ^ string_of_int !nested_emit_ctr

let fresh_lbl (ctx : fn_ctx) prefix =
  ctx.lbl_counter <- ctx.lbl_counter + 1;
  prefix ^ "_" ^ string_of_int ctx.lbl_counter

let emit_instr (ctx : fn_ctx) i =
  ctx.cur_instrs <- ctx.cur_instrs @ [ i ]

let close_block (ctx : fn_ctx) term =
  let blk = { label = ctx.cur_label; instrs = ctx.cur_instrs; term } in
  ctx.completed <- ctx.completed @ [ blk ]

let open_block (ctx : fn_ctx) label =
  ctx.cur_label <- label;
  ctx.cur_instrs <- []

let rec peel_function_chain_to_min (acc : ty list) (m : mono_type) : ty list * ty
    =
  match m with
  | FunctionType (a, r) -> peel_function_chain_to_min (mono_to_min a :: acc) r
  | _ -> (List.rev acc, mono_to_min m)

and mono_to_min (m : mono_type) : ty =
  match m with
  | IntType -> I32
  | BoolType -> I1
  | StringType -> String
  | UnitType -> Unit
  | FunctionType _ ->
      let ps, rt = peel_function_chain_to_min [] m in
      if List.length ps >= 2 then Clos (ps, rt) else Fun (ps, rt)
  | TypeVar _ ->
      unsupported
        "Polymorphic type in native compile (e.g. 'a -> 'a): add monomorphic \
         type annotations on parameters and result, e.g. let f (x : int) : int = x"
  | FloatType | CharType | TypeName _ | VectorType _ | CListType _ | CTypeApp _
  | FixedPoint _ | RecordType _ ->
      unsupported "Type not supported for native parameter/return yet"

let rec ty_equal (a : ty) (b : ty) : bool =
  match (a, b) with
  | I32, I32 | I1, I1 | String, String | Unit, Unit | RawPtr, RawPtr -> true
  | Fun (p1, r1), Fun (p2, r2) ->
      List.length p1 = List.length p2
      && List.for_all2 ty_equal p1 p2
      && ty_equal r1 r2
  | Clos (p1, r1), Clos (p2, r2) ->
      List.length p1 = List.length p2
      && List.for_all2 ty_equal p1 p2
      && ty_equal r1 r2
  | _ -> false

(** [peel_inferred_param_monos n m] takes the first [n] argument types from a
    curried [FunctionType] chain ([m] must be the typechecker's type for the
    binding, instantiated). *)
let rec peel_inferred_param_monos (n : int) (m : mono_type) : mono_type list =
  if n = 0 then []
  else
    match m with
    | FunctionType (a, rest) ->
        a :: peel_inferred_param_monos (n - 1) rest
    | _ ->
        unsupported
          "Inferred type is not a function matching the number of parameters"

(** Parameter Mi types: explicit annotations win for lowering shape; unannotated
    parameters use types inferred by typechecking (see [static_env]). *)
let param_min_ir_tys (name : string) (param_anns : c_type option list)
    (static_env : static_env) : ty list =
  let n = List.length param_anns in
  let inferred_monos =
    match List.assoc_opt name static_env with
    | None ->
        unsupported
          ("Missing type for `" ^ name ^ "` in static environment (compiler bug)")
    | Some ct ->
        let m = Typecheck.instantiate ct in
        peel_inferred_param_monos n m
  in
  List.map2
    (fun ann inf_m ->
      match ann with
      | None -> mono_to_min inf_m
      | Some (Mono m) -> mono_to_min m
      | Some (PolyType _) ->
          unsupported
            "Polymorphic parameter annotation not supported for compilation")
    param_anns inferred_monos

(** [peel_efun body] — parameters (patterns + annotations), inner expression. *)
let rec peel_efun acc_p acc_a : c_expr -> c_pat list * c_type option list * c_expr
    = function
  | EFunction (pat, ann, rest) ->
      peel_efun (pat :: acc_p) (ann :: acc_a) rest
  | e ->
      (List.rev acc_p, List.rev acc_a, e)

let map_arith_bop : c_bop -> ibin option = function
  | CPlus -> Some Add
  | CMinus -> Some Sub
  | CMul -> Some Mul
  | CDiv -> Some Div
  | CMod -> Some Mod
  | _ -> None

let map_cmp : c_bop -> icmp option = function
  | CEQ -> Some Eq
  | CNE -> Some Ne
  | CLT -> Some Slt
  | CLE -> Some Sle
  | CGE -> Some Sge
  | _ -> None

(** Multi-arg / lambda callable: saturated calls use [multi_direct]; for curried
    values, [step_codes].([k]) matches [k] already-fixed arguments. Lambdas store
    captures in [cap_tys] / [cap_ops]. *)
type callable = {
  multi_direct : string;
  step_codes : string list;
  cap_tys : ty list;
  cap_ops : operand list;
  fixed : operand list;
  param_tys : ty list;
  ret_ty : ty;
}

type expr_result = LVal of operand * ty | LPartial of callable

type env_binding = Val of operand * ty | C of callable

type env = (string * env_binding) list

let pat_bound_simple : c_pat -> string list = function
  | CIdPat x -> [ x ]
  | CUnitPat | CWildcardPat -> []
  | _ ->
      unsupported "lambda parameter pattern not supported for native compilation"

let rec free_vars_cexpr : c_expr -> S.t = function
  | EId x -> S.singleton x
  | EInt _ | EBool _ | EString _ | EUnit -> S.empty
  | EFunction (p, _, e) ->
      let b = pat_bound_simple p in
      S.diff (free_vars_cexpr e) (S.of_list b)
  | EApp (a, b) -> S.union (free_vars_cexpr a) (free_vars_cexpr b)
  | EBop (_, a, b) -> S.union (free_vars_cexpr a) (free_vars_cexpr b)
  | ETernary (a, b, c) ->
      S.union (S.union (free_vars_cexpr a) (free_vars_cexpr b)) (free_vars_cexpr c)
  | EBind (CIdPat x, _, e1, e2, _) ->
      S.union (free_vars_cexpr e1) (S.remove x (free_vars_cexpr e2))
  | EBind _ ->
      unsupported "let pattern in closure analysis (lambda body) not supported"
  | EBlock parts ->
      List.fold_left
        (fun acc part ->
          match part with
          | Expr e -> S.union acc (free_vars_cexpr e)
          | Defn _ ->
              unsupported "definition in block during closure analysis")
        S.empty parts
  | _ ->
      unsupported "expression in lambda (closure analysis) not supported for compilation"

let rec list_take n xs =
  if n <= 0 then []
  else match xs with [] -> [] | h :: t -> h :: list_take (n - 1) t

let rec list_drop n xs =
  if n <= 0 then xs else match xs with [] -> [] | _ :: t -> list_drop (n - 1) t

let layout_byte_size_lower (xs : ty list) : int =
  let align_up x a =
    if x mod a = 0 then x else x + (a - (x mod a))
  in
  let sz_al = function
    | I32 -> (4, 4)
    | I1 -> (4, 4)
    | String | RawPtr | Clos _ -> (8, 8)
    | Unit -> (1, 8)
    | Fun _ -> (8, 8)
  in
  let acc = ref 0 in
  List.iter
    (fun t ->
      let sz, al = sz_al t in
      acc := align_up !acc al + sz)
    xs;
  max !acc 1

let rec capture_operand_for_var (env : env) (ctx : fn_ctx) (v : string) :
    ty * operand =
  match List.assoc_opt v env with
  | None -> unsupported ("Lambda captures unbound `" ^ v ^ "`")
  | Some (Val (op, t)) -> (t, op)
  | Some (C c) ->
      let op, clo_ty = materialize_clos_lower env ctx c in
      (clo_ty, op)

and materialize_clos_lower _env ctx (c : callable) :
    operand * ty =
  let k = List.length c.fixed in
  let n = List.length c.param_tys in
  if n = 1 && c.cap_tys = [] && k = 0 then
    (FnAddr (c.multi_direct, c.param_tys, c.ret_ty), Fun (c.param_tys, c.ret_ty))
  else if n = 1 && k <> 0 then
    unsupported "Internal: partial one-arg function without captures"
  else
    let step = List.nth c.step_codes k in
    let layout = c.cap_tys @ list_take k c.param_tys in
    let vals = c.cap_ops @ c.fixed in
    if List.length layout <> List.length vals then
      unsupported "Internal: env layout/values length mismatch";
    let clo_ty = Clos (list_drop k c.param_tys, c.ret_ty) in
    let tmp_c = fresh () in
    if layout = [] then (
      emit_instr ctx
        (Assign
           ( tmp_c,
             MkClos { code = step; env_ptr = RawNull; clo_ty } ));
      (Local tmp_c, clo_ty))
    else (
      let sz = layout_byte_size_lower layout in
      let tmp_e = fresh () in
      emit_instr ctx (Assign (tmp_e, RawMalloc sz));
      List.iteri
        (fun i op ->
          let d = fresh () in
          emit_instr ctx
            (Assign
               ( d,
                 EnvStore
                   {
                     env = Local tmp_e;
                     layout;
                     index = i;
                     value = op;
                   } )))
        vals;
      emit_instr ctx
        (Assign (tmp_c, MkClos { code = step; env_ptr = Local tmp_e; clo_ty }));
      (Local tmp_c, clo_ty))

let lambda_captures (env : env) (ctx : fn_ctx) (fv : S.t) :
    (string * ty * operand) list =
  let names = List.sort String.compare (S.elements fv) in
  List.map
    (fun v ->
      let t, op = capture_operand_for_var env ctx v in
      (v, t, op))
    names

(** After [n] curried arrow parameters, the monomorphic result type of the binding. *)
let rec mono_after_n_fun_args (n : int) (m : mono_type) : mono_type =
  if n = 0 then m
  else
    match m with
    | FunctionType (_, r) -> mono_after_n_fun_args (n - 1) r
    | _ ->
        unsupported
          "Inferred type is not a curried function matching its parameter count"

let ret_min_ty_of_user_fn (name : string) (num_params : int)
    (static_env : static_env) : ty =
  match List.assoc_opt name static_env with
  | None ->
      unsupported
        ("Missing type for `" ^ name ^ "` in static environment (compiler bug)")
  | Some ct ->
      let m = Typecheck.instantiate ct in
      mono_to_min (mono_after_n_fun_args num_params m)

let is_poly_static (name : string) (static_env : static_env) : bool =
  match List.assoc_opt name static_env with
  | Some (PolyType _) -> true
  | Some (Mono _) | None -> false

(** Map simple Min_IR types back to [mono_type] for typing local value bindings
    when resolving polymorphic call instantiations inside function bodies. *)
let min_ty_to_mono_opt : ty -> mono_type option = function
  | I32 -> Some IntType
  | I1 -> Some BoolType
  | String -> Some StringType
  | Unit -> Some UnitType
  | Fun _ | RawPtr | Clos _ -> None

(** Min_IR → mono for monomorph keys (curried [Fun] / [Clos] chains). *)
let rec min_ty_to_mono (t : ty) : mono_type =
  match t with
  | I32 -> IntType
  | I1 -> BoolType
  | String -> StringType
  | Unit -> UnitType
  | Fun (ps, r) ->
      List.fold_right
        (fun p acc -> FunctionType (min_ty_to_mono p, acc))
        ps (min_ty_to_mono r)
  | Clos (ps, r) ->
      List.fold_right
        (fun p acc -> FunctionType (min_ty_to_mono p, acc))
        ps (min_ty_to_mono r)
  | RawPtr ->
      unsupported
        "Internal: expected type for polymorphic instantiation used RawPtr"

(** Extend the global static environment with locals so [mono_fun_type_of_binary_app]
    sees parameters and [let]-bound names in the current lowering scope. *)
let static_env_for_mono_call (global : static_env) (env : env) : static_env =
  List.fold_left
    (fun acc (name, b) ->
      match b with
      | Val (_, t) -> (
          match min_ty_to_mono_opt t with
          | Some m -> (name, Mono m) :: acc
          | None -> acc)
      | C _ -> acc)
    [] env
  @ global

let replace_static_binding (name : string) (ct : c_type) (static_env : static_env)
    : static_env =
  (name, ct) :: List.remove_assoc name static_env

let mangle_poly_instance (name : string) (mono : mono_type) : string =
  let h = Hashtbl.hash (string_of_mono_type mono) in
  let s = if h < 0 then string_of_int (abs h) else string_of_int h in
  name ^ "__lsm" ^ s

let find_cdefn_function (name : string) (defs : c_defn list) :
    (c_pat list * c_type option list * c_expr) option =
  let rec find = function
    | [] -> None
    | CDefn (pat, _, body, _, _) :: rest -> (
        match pat with
        | CIdPat n when n = name ->
            let param_pats, anns, inner = peel_efun [] [] body in
            if param_pats = [] then find rest
            else Some (param_pats, anns, inner)
        | _ -> find rest)
    | _ :: rest -> find rest
  in
  find defs

(** Curried application spine: left-most head and arguments left-to-right. *)
let rec peel_app_spine e acc =
  match e with
  | EApp (f, a) -> peel_app_spine f (a :: acc)
  | EId s -> (`Id s, acc)
  | _ -> (`Other e, acc)

let rec curried_fun_arity_mono (m : mono_type) : int =
  match m with
  | FunctionType (_, r) -> 1 + curried_fun_arity_mono r
  | _ -> 0

let fresh_eta_param () =
  incr eta_expand_counter;
  "__ls_eta_" ^ string_of_int !eta_expand_counter

(** If [f] is polymorphic at compile time and [args] do not saturate its arity,
    return [fun y1 ... yk -> f args y1 ... yk] so the binding uses the same
    top-level polymorphic lowering path as an ordinary function. *)
let eta_poly_partial_spine (static_env : static_env) (f : string) (args : c_expr list)
    : c_expr option =
  if not (is_poly_static f static_env) then None
  else
    match List.assoc_opt f static_env with
    | None -> None
    | Some ct ->
        let m = Typecheck.instantiate ct in
        let arity = curried_fun_arity_mono m in
        let n = List.length args in
        if n >= arity then None
        else
          let k = arity - n in
          let ys = List.init k (fun _ -> fresh_eta_param ()) in
          let base = List.fold_left (fun acc a -> EApp (acc, a)) (EId f) args in
          let applied =
            List.fold_left (fun acc ynm -> EApp (acc, EId ynm)) base ys
          in
          Some
            (List.fold_right
               (fun ynm inner -> EFunction (CIdPat ynm, None, inner))
               ys applied)

(** [e] is exactly a curried spine [f a1 ... an] (not a larger expression). *)
let eta_expand_binding_rhs (static_env : static_env) (e : c_expr) : c_expr =
  match peel_app_spine e [] with
  | `Id f, args -> (
      match eta_poly_partial_spine static_env f args with
      | Some e' -> e'
      | None -> e)
  | _ -> e

let rec map_expr_eta_at_lets (static_env : static_env) (e : c_expr) : c_expr =
  match e with
  | EBind (pat, ta, e1, e2, rt) ->
      let e1' =
        eta_expand_binding_rhs static_env (map_expr_eta_at_lets static_env e1)
      in
      let e2' =
        eta_expand_binding_rhs static_env (map_expr_eta_at_lets static_env e2)
      in
      EBind (pat, ta, e1', e2', rt)
  | EBindRec (pat, ta, e1, e2, rt) ->
      let e1' =
        eta_expand_binding_rhs static_env (map_expr_eta_at_lets static_env e1)
      in
      let e2' =
        eta_expand_binding_rhs static_env (map_expr_eta_at_lets static_env e2)
      in
      EBindRec (pat, ta, e1', e2', rt)
  | EBindMutRec (binds, body) ->
      let binds' =
        List.map
          (fun (pat, ta, e1, rt, n) ->
            let e1' =
              eta_expand_binding_rhs static_env
                (map_expr_eta_at_lets static_env e1)
            in
            (pat, ta, e1', rt, n))
          binds
      in
      let body' =
        eta_expand_binding_rhs static_env (map_expr_eta_at_lets static_env body)
      in
      EBindMutRec (binds', body')
  | EFunction (pat, ann, body) ->
      EFunction (pat, ann, map_expr_eta_at_lets static_env body)
  | EApp (a, b) ->
      EApp (map_expr_eta_at_lets static_env a, map_expr_eta_at_lets static_env b)
  | EBop (op, a, b) ->
      EBop
        ( op,
          map_expr_eta_at_lets static_env a,
          map_expr_eta_at_lets static_env b )
  | ETernary (a, b, c) ->
      ETernary
        ( map_expr_eta_at_lets static_env a,
          map_expr_eta_at_lets static_env b,
          map_expr_eta_at_lets static_env c )
  | EBlock parts ->
      EBlock
        (List.map
           (function
             | Expr ex -> Expr (map_expr_eta_at_lets static_env ex)
             | Defn d -> Defn (map_defn_eta static_env d))
           parts)
  | ESwitch (e0, branches) ->
      ESwitch
        ( map_expr_eta_at_lets static_env e0,
          List.map
            (fun (p, be) -> (p, map_expr_eta_at_lets static_env be))
            branches )
  | EVector es -> EVector (List.map (map_expr_eta_at_lets static_env) es)
  | EListEnumeration (a, b) ->
      EListEnumeration
        (map_expr_eta_at_lets static_env a, map_expr_eta_at_lets static_env b)
  | EListComprehension (e0, gens) ->
      EListComprehension
        ( map_expr_eta_at_lets static_env e0,
          List.map
            (fun (pat, ge) -> (pat, map_expr_eta_at_lets static_env ge))
            gens )
  | ERecordLit fields ->
      ERecordLit
        (List.map
           (fun (s, ex) -> (s, map_expr_eta_at_lets static_env ex))
           fields)
  | ERecordUpdate (base, upd) ->
      ERecordUpdate
        ( map_expr_eta_at_lets static_env base,
          List.map
            (fun (s, ex) -> (s, map_expr_eta_at_lets static_env ex))
            upd )
  | EFieldAccess (e0, fld) ->
      EFieldAccess (map_expr_eta_at_lets static_env e0, fld)
  | EInt _ | EBool _ | EString _ | EUnit | EChar _ | EFloat _ | ENil | EId _ ->
      e

and map_defn_eta (static_env : static_env) (d : c_defn) : c_defn =
  match d with
  | CDefn (pat, ann, body, rt, n) ->
      CDefn
        ( pat,
          ann,
          map_expr_eta_at_lets static_env (eta_expand_binding_rhs static_env body),
          rt,
          n )
  | CDefnRec (pat, ann, body, rt, n) ->
      CDefnRec
        ( pat,
          ann,
          map_expr_eta_at_lets static_env (eta_expand_binding_rhs static_env body),
          rt,
          n )
  | CDefnMutRec defns ->
      CDefnMutRec
        (List.map
           (fun (pat, ann, body, rt, n) ->
             ( pat,
               ann,
               map_expr_eta_at_lets static_env
                 (eta_expand_binding_rhs static_env body),
               rt,
               n ))
           defns)
  | CTypeAlias _ | CSumType _ | CSumTypeRec _ | CSumTypeRecMutRec _ -> d

let eta_expand_program (static_env : static_env) (defs : c_defn list) : c_defn list
    =
  List.map (map_defn_eta static_env) defs

(** Collect (top-level function name, instantiated function type) pairs needed for
    monomorphization, including instances discovered inside specialized bodies
    (fixpoint). *)
let collect_mono_instantiations (defs : c_defn list) (static_env : static_env)
    (type_env : Typecheck.type_env) : (string * mono_type) list =
  let seen : (string * string, unit) Hashtbl.t = Hashtbl.create 64 in
  let all : (string * mono_type) list ref = ref [] in
  let q : (string * mono_type) Queue.t = Queue.create () in
  let add name mono =
    let key = (name, string_of_mono_type mono) in
    if not (Hashtbl.mem seen key) then (
      Hashtbl.add seen key ();
      all := (name, mono) :: !all;
      Queue.add (name, mono) q)
  in
  let rec collect_visit_expr (env : static_env) (e : c_expr) : unit =
    match e with
    | EApp (e1, e2) as app ->
        collect_visit_expr env e1;
        collect_visit_expr env e2;
        (* Polymorphic top-level used as an argument: infer required mono from context,
           e.g. [use id] needs [(id, int -> int)] when [use : (int -> 'b) -> 'b]. *)
        (match e2 with
        | EId g when is_poly_static g env -> (
            match
              Typecheck.mono_fun_type_of_binary_app env type_env e1 e2
            with
            | Ok t_fn -> (
                match t_fn with
                | FunctionType (arg_ty, _) when
                    Typecheck.mono_type_fully_concrete arg_ty ->
                    add g arg_ty
                | _ -> ())
            | Error _ -> ())
        | _ -> ());
        let head, args = peel_app_spine app [] in
        (match head with
        | `Id f when is_poly_static f env -> (
            match Typecheck.mono_fun_type_of_curried_app env type_env f args with
            | Ok m_fun when Typecheck.mono_type_fully_concrete m_fun -> add f m_fun
            | Ok _ | Error _ -> ())
        | _ -> ())
    | EBop (_, a, b) ->
        collect_visit_expr env a;
        collect_visit_expr env b
    | ETernary (a, b, c) ->
        collect_visit_expr env a;
        collect_visit_expr env b;
        collect_visit_expr env c
    | EBind (pat, _, e1, e2, _) ->
        collect_visit_expr env e1;
        (match Typecheck.type_of_c_expr env type_env e1 with
        | Ok ct -> (
            match bind_static pat ct with
            | Some bindings -> collect_visit_expr (bindings @ env) e2
            | None -> collect_visit_expr env e2)
        | Error _ -> collect_visit_expr env e2)
    | EBindRec (_, _, e1, e2, _) ->
        collect_visit_expr env e1;
        collect_visit_expr env e2
    | EBindMutRec (bindings, body) ->
        List.iter
          (fun (_, _, e1, _, _) -> collect_visit_expr env e1)
          bindings;
        collect_visit_expr env body
    | EBlock parts ->
        List.iter
          (function
            | Expr e -> collect_visit_expr env e
            | Defn d -> collect_visit_defn env d)
          parts
    | EFunction (_, _, body) -> collect_visit_expr env body
    | ESwitch (e0, branches) ->
        collect_visit_expr env e0;
        List.iter (fun (_, be) -> collect_visit_expr env be) branches
    | EVector es -> List.iter (collect_visit_expr env) es
    | EListEnumeration (a, b) ->
        collect_visit_expr env a;
        collect_visit_expr env b
    | EListComprehension (e0, gens) ->
        collect_visit_expr env e0;
        List.iter
          (fun (_, ge) -> collect_visit_expr env ge)
          gens
    | ERecordLit fields -> List.iter (fun (_, e0) -> collect_visit_expr env e0) fields
    | ERecordUpdate (base, upd) ->
        collect_visit_expr env base;
        List.iter (fun (_, e0) -> collect_visit_expr env e0) upd
    | EFieldAccess (e0, _) -> collect_visit_expr env e0
    | EInt _ | EBool _ | EString _ | EUnit | EChar _ | EFloat _ | ENil | EId _ ->
        ()
  and collect_visit_defn (env : static_env) (defn : c_defn) : unit =
    match defn with
    | CDefn (pat, _, body, _, _) -> visit_def_body env pat body
    | CDefnRec (pat, _, body, _, _) -> visit_def_body env pat body
    | CDefnMutRec ds ->
        List.iter (fun (pat, _, body, _, _) -> visit_def_body env pat body) ds
    | CTypeAlias _ | CSumType _ | CSumTypeRec _ | CSumTypeRecMutRec _ -> ()
  and visit_def_body (env : static_env) (pat : c_pat) (body : c_expr) : unit =
    match pat with
    | CIdPat _ ->
        let _, _, inner = peel_efun [] [] body in
        collect_visit_expr env inner
    | CUnitPat | CWildcardPat -> collect_visit_expr env body
    | _ -> ()
  in
  List.iter (collect_visit_defn static_env) defs;
  while not (Queue.is_empty q) do
    let name, mono = Queue.pop q in
    match find_cdefn_function name defs with
    | None -> ()
    | Some (param_pats, _, inner) ->
        let static_inst = replace_static_binding name (Mono mono) static_env in
        let param_monos =
          peel_inferred_param_monos (List.length param_pats) mono
        in
        let env_inst =
          List.fold_left
            (fun acc (pat, pm) ->
              match bind_static pat (Mono pm) with
              | Some b -> b @ acc
              | None -> acc)
            static_inst
            (List.combine param_pats param_monos)
        in
        collect_visit_expr env_inst inner
  done;
  List.sort
    (fun (a, ma) (b, mb) ->
      match String.compare a b with
      | 0 -> String.compare (string_of_mono_type ma) (string_of_mono_type mb)
      | c -> c)
    (List.rev !all)

(** Callable shape before lowering a body (recursive / mutual fixup).
    [emit_direct] is the LLVM symbol for the multi-arg direct function; [ty_key]
    names the binding for parameter type lookup. *)
let callable_stub ~(ty_key : string) ~(emit_direct : string)
    (param_anns : c_type option list) (static_env : static_env) : callable =
  let param_tys = param_min_ir_tys ty_key param_anns static_env in
  let ret_ty =
    ret_min_ty_of_user_fn ty_key (List.length param_tys) static_env
  in
  let n = List.length param_tys in
  let step_codes =
    if n >= 2 then List.init n (fun k -> emit_direct ^ "__ls_s" ^ string_of_int k)
    else []
  in
  {
    multi_direct = emit_direct;
    step_codes;
    cap_tys = [];
    cap_ops = [];
    fixed = [];
    param_tys;
    ret_ty;
  }

let build_mono_instance_env (instances : (string * mono_type) list)
    (defs : c_defn list) (static_env : static_env) : env =
  List.fold_left
    (fun acc (name, mono) ->
      match find_cdefn_function name defs with
      | None -> acc
      | Some (_, anns, _) ->
          let emit = mangle_poly_instance name mono in
          let stub =
            callable_stub ~ty_key:emit ~emit_direct:emit anns
              ((emit, Mono mono) :: static_env)
          in
          (emit, C stub) :: acc)
    [] instances

let callable_remaining c =
  List.length c.param_tys - List.length c.fixed

let blocks_assoc (ctx : fn_ctx) : (string * block) list =
  List.map (fun b -> (b.label, b)) ctx.completed

let emit_curried_step_intermediate ~(emit : string) (k : int) (cap_tys : ty list)
    (param_tys : ty list) (params : string list) (ret_ty : ty) : func_def =
  let name_s = emit ^ "__ls_s" ^ string_of_int k in
  let ctx_s = create_fn_ctx () in
  let env_nm = fresh_param () in
  let x_nm = List.nth params k in
  let old_layout = cap_tys @ list_take k param_tys in
  let new_layout = cap_tys @ list_take (k + 1) param_tys in
  let sz_new = layout_byte_size_lower new_layout in
  let tmp_r = fresh () in
  emit_instr ctx_s (Assign (tmp_r, RawMalloc sz_new));
  let n_old = List.length old_layout in
  for i = 0 to n_old - 1 do
    let ld = fresh () in
    let st = fresh () in
    emit_instr ctx_s
      (Assign
         ( ld,
           EnvLoad { env = Local env_nm; layout = old_layout; index = i } ));
    emit_instr ctx_s
      (Assign
         ( st,
           EnvStore
             {
               env = Local tmp_r;
               layout = new_layout;
               index = i;
               value = Local ld;
             } ))
  done;
  let stx = fresh () in
  emit_instr ctx_s
    (Assign
       ( stx,
         EnvStore
           {
             env = Local tmp_r;
             layout = new_layout;
             index = n_old;
             value = Local x_nm;
           } ));
  let clo_ty = Clos (list_drop (k + 1) param_tys, ret_ty) in
  let out = fresh () in
  let next_s = emit ^ "__ls_s" ^ string_of_int (k + 1) in
  emit_instr ctx_s
    (Assign
       ( out,
         MkClos { code = next_s; env_ptr = Local tmp_r; clo_ty } ));
  close_block ctx_s (Ret (Some (Local out)));
  {
    name = name_s;
    params = [ (env_nm, RawPtr); (x_nm, List.nth param_tys k) ];
    ret = clo_ty;
    entry = "entry";
    blocks = blocks_assoc ctx_s;
  }

let emit_curried_step_final ~(emit : string) (k : int) (cap_tys : ty list)
    (param_tys : ty list) (params : string list) (ret_ty : ty) : func_def =
  let name_s = emit ^ "__ls_s" ^ string_of_int k in
  let ctx_s = create_fn_ctx () in
  let env_nm = fresh_param () in
  let x_nm = List.nth params k in
  let old_layout = cap_tys @ list_take k param_tys in
  let m = List.length cap_tys in
  let n = List.length param_tys in
  let call_ops : operand list ref = ref [] in
  for i = 0 to m - 1 do
    let ld = fresh () in
    emit_instr ctx_s
      (Assign
         ( ld,
           EnvLoad { env = Local env_nm; layout = old_layout; index = i } ));
    call_ops := !call_ops @ [ Local ld ]
  done;
  for j = 0 to n - 2 do
    let ld = fresh () in
    emit_instr ctx_s
      (Assign
         ( ld,
           EnvLoad
             { env = Local env_nm; layout = old_layout; index = m + j } ));
    call_ops := !call_ops @ [ Local ld ]
  done;
  call_ops := !call_ops @ [ Local x_nm ];
  let term : term =
    if ret_ty = Unit then (
      emit_instr ctx_s (VoidCall (emit, !call_ops));
      Ret None)
    else (
      let t = fresh () in
      emit_instr ctx_s (Assign (t, Call (emit, !call_ops)));
      Ret (Some (Local t)))
  in
  close_block ctx_s term;
  {
    name = name_s;
    params = [ (env_nm, RawPtr); (x_nm, List.nth param_tys k) ];
    ret = ret_ty;
    entry = "entry";
    blocks = blocks_assoc ctx_s;
  }

let rec lower_expr_val (e : c_expr) (env : env) (ctx : fn_ctx)
    (static_env : static_env) (type_env : Typecheck.type_env) : operand * ty =
  match lower_expr e env ctx static_env type_env with
  | LVal (o, t) -> (o, t)
  | LPartial c -> (
      if callable_remaining c <= 0 then
        unsupported "Internal: saturated callable where a value was expected";
      materialize_clos_lower env ctx c)

(** Lower an expression used as the next argument in a call, using the callee's
    expected parameter type so polymorphic top-level names can be monomorphized
    without a synthetic lambda ([use id], etc.). *)
and lower_expr_val_as_call_arg (arg : c_expr) (expect : ty) (env : env)
    (ctx : fn_ctx) (static_env : static_env) (type_env : Typecheck.type_env) :
    operand * ty =
  match arg with
  | EId x when is_poly_static x static_env ->
      let m_expect = min_ty_to_mono expect in
      if not (Typecheck.mono_type_fully_concrete m_expect) then
        unsupported
          "Polymorphic value passed to a call needs a concrete parameter type at \
           this site for native compilation";
      let mangle = mangle_poly_instance x m_expect in
      begin
        match resolve_callable mangle env with
        | Some c ->
            let op, got = materialize_clos_lower env ctx c in
            if not (ty_equal got expect) then
              unsupported
                "Internal: monomorphized polymorphic function type mismatch at call";
            (op, got)
        | None ->
            unsupported
              ("Missing monomorphized specialization `" ^ mangle ^ "`")
      end
  | _ ->
      let o, got = lower_expr_val arg env ctx static_env type_env in
      if not (ty_equal got expect) then unsupported "call argument type mismatch";
      (o, got)

and lower_builtin_print name arg env ctx static_env type_env =
  let o2, t2 = lower_expr_val arg env ctx static_env type_env in
  if t2 <> String then unsupported "print/println expect a string argument";
  let arg_op =
    match o2 with
    | ConstStr _ ->
        let t = fresh () in
        emit_instr ctx (Assign (t, Copy o2));
        Local t
    | o -> o
  in
  emit_instr ctx (VoidCall (name, [ arg_op ]));
  LVal (ConstUnit, Unit)

and lower_builtin_int_to_str arg env ctx static_env type_env =
  let o2, t2 = lower_expr_val arg env ctx static_env type_env in
  if t2 <> I32 then unsupported "int_to_str expects i32";
  let t = fresh () in
  emit_instr ctx (Assign (t, Call ("int_to_str", [ o2 ])));
  LVal (Local t, String)

(** Apply call arguments [args] (already in order) to [c]; emit a call when saturated. *)
and apply_call_args (env : env) (ctx : fn_ctx) (static_env : static_env)
    (type_env : Typecheck.type_env) (c : callable) (args : c_expr list) :
    expr_result =
  let rec go c = function
    | [] ->
        if callable_remaining c = 0 then emit_saturated_call ctx c else LPartial c
    | arg :: rest -> (
        if callable_remaining c = 0 then unsupported "Too many arguments in call";
        let i = List.length c.fixed in
        let expect = List.nth c.param_tys i in
        let op, _got =
          lower_expr_val_as_call_arg arg expect env ctx static_env type_env
        in
        let c' = { c with fixed = c.fixed @ [ op ] } in
        match rest with
        | [] ->
            if callable_remaining c' = 0 then emit_saturated_call ctx c'
            else LPartial c'
        | _ -> go c' rest)
  in
  go c args

and emit_saturated_call ctx (c : callable) : expr_result =
  let cap_n = List.length c.cap_tys in
  let all_args = c.cap_ops @ c.fixed in
  if List.length all_args <> cap_n + List.length c.param_tys then
    unsupported "Internal: saturated call length mismatch";
  if c.ret_ty = Unit then (
    emit_instr ctx (VoidCall (c.multi_direct, all_args));
    LVal (ConstUnit, Unit))
  else (
    let t = fresh () in
    emit_instr ctx (Assign (t, Call (c.multi_direct, all_args)));
    LVal (Local t, c.ret_ty))

and apply_fun1 env ctx static_env type_env callee_op a_ty ret_ty args :
    expr_result =
  match args with
  | [ arg ] ->
      let oa, _ta =
        lower_expr_val_as_call_arg arg a_ty env ctx static_env type_env
      in
      if ret_ty = Unit then (
        emit_instr ctx (VoidIndirectCall (callee_op, [ a_ty ], [ oa ]));
        LVal (ConstUnit, Unit))
      else (
        let t = fresh () in
        emit_instr ctx (Assign (t, IndirectCall (callee_op, [ a_ty ], ret_ty, [ oa ])));
        LVal (Local t, ret_ty))
  | _ -> unsupported "simple function expects exactly one argument in this call"

and apply_clos_chain env ctx static_env type_env clos_op ps ret_ty args :
    expr_result =
  let rec go clo_op ps_left args_left =
    match (ps_left, args_left) with
    | [], _ :: _ -> unsupported "Too many arguments in closure call"
    | _ :: _, [] -> unsupported "Internal: closure chain needs an argument"
    | p :: prest, arg :: arest ->
        let oa, _ta =
          lower_expr_val_as_call_arg arg p env ctx static_env type_env
        in
        let next_ty =
          match prest with [] -> ret_ty | _ -> Clos (prest, ret_ty)
        in
        let tmp = fresh () in
        emit_instr ctx
          (Assign
             ( tmp,
               ClosApply { clo = clo_op; arg = oa; result_ty = next_ty } ));
        if prest = [] then LVal (Local tmp, ret_ty) else go (Local tmp) prest arest
    | [], [] -> unsupported "Internal: empty closure arity"
  in
  go clos_op ps args

and resolve_callable (name : string) (env : env) : callable option =
  match List.assoc_opt name env with
  | Some (C c) -> Some c
  | Some (Val _) | None -> None

and lower_expr (e : c_expr) (env : env) (ctx : fn_ctx) (static_env : static_env)
    (type_env : Typecheck.type_env) : expr_result =
  match e with
  | EInt n -> LVal (ConstI32 n, I32)
  | EBool b -> LVal (ConstI1 b, I1)
  | EString s -> LVal (ConstStr s, String)
  | EUnit -> LVal (ConstUnit, Unit)
  | EId x -> (
      match List.assoc_opt x env with
      | Some (Val (o, t)) -> LVal (o, t)
      | Some (C c) ->
          if callable_remaining c > 0 then LPartial c
          else unsupported ("`" ^ x ^ "` is already fully applied (compiler bug)")
      | None ->
          if is_poly_static x static_env then
            match eta_poly_partial_spine static_env x [] with
            | Some e_eta -> lower_expr e_eta env ctx static_env type_env
            | None ->
                unsupported
                  ("Polymorphic function `" ^ x
                 ^ "` cannot be used as a value here; call it fully applied")
          else unsupported ("Unbound name `" ^ x ^ "` (not a lowering target)"))
  | EBop (op, e1, e2) -> (
      match map_arith_bop op with
      | Some b ->
          let o1, t1 = lower_expr_val e1 env ctx static_env type_env in
          let o2, t2 = lower_expr_val e2 env ctx static_env type_env in
          if t1 <> I32 || t2 <> I32 then
            unsupported "Arithmetic expects i32 operands";
          let t = fresh () in
          emit_instr ctx (Assign (t, Binop (b, o1, o2)));
          LVal (Local t, I32)
      | None -> (
          match map_cmp op with
          | Some c ->
              let o1, t1 = lower_expr_val e1 env ctx static_env type_env in
              let o2, t2 = lower_expr_val e2 env ctx static_env type_env in
              if t1 <> I32 || t2 <> I32 then
                unsupported "Integer comparison expects i32 operands";
              let t = fresh () in
              emit_instr ctx (Assign (t, ICmp (c, o1, o2)));
              LVal (Local t, I1)
          | None -> (
              match op with
              | CGT ->
                  let o1, t1 = lower_expr_val e1 env ctx static_env type_env in
                  let o2, t2 = lower_expr_val e2 env ctx static_env type_env in
                  if t1 <> I32 || t2 <> I32 then
                    unsupported "Integer comparison expects i32 operands";
                  let t = fresh () in
                  emit_instr ctx (Assign (t, ICmp (Slt, o2, o1)));
                  LVal (Local t, I1)
              | CAnd ->
                  let o1, t1 = lower_expr_val e1 env ctx static_env type_env in
                  let o2, t2 = lower_expr_val e2 env ctx static_env type_env in
                  if t1 <> I1 || t2 <> I1 then
                    unsupported "&& expects bool operands";
                  let t = fresh () in
                  emit_instr ctx (Assign (t, IAnd (o1, o2)));
                  LVal (Local t, I1)
              | COr ->
                  let o1, t1 = lower_expr_val e1 env ctx static_env type_env in
                  let o2, t2 = lower_expr_val e2 env ctx static_env type_env in
                  if t1 <> I1 || t2 <> I1 then
                    unsupported "|| expects bool operands";
                  let t = fresh () in
                  emit_instr ctx (Assign (t, IOr (o1, o2)));
                  LVal (Local t, I1)
              | _ -> unsupported ("Binary operator not supported in Min_IR lowering yet"))))
  | EBind (CIdPat x, _ta, e1, e2, _rt) -> (
      match lower_expr e1 env ctx static_env type_env with
      | LVal (o1, t1) ->
          emit_instr ctx (Assign (x, Copy o1));
          let env' = (x, Val (Local x, t1)) :: env in
          lower_expr e2 env' ctx static_env type_env
      | LPartial c ->
          let env' = (x, C c) :: env in
          lower_expr e2 env' ctx static_env type_env)
  | EBind _ -> unsupported "let: only simple identifier patterns supported"
  | EBlock parts -> lower_block parts env ctx static_env type_env
  | EApp (e1, e2) ->
      let head, args = peel_app_spine e1 [ e2 ] in
      begin match head with
      | `Other e_fn -> (
          match lower_expr e_fn env ctx static_env type_env with
          | LPartial c -> apply_call_args env ctx static_env type_env c args
          | LVal (op_f, Fun (f_ps, r_ty)) -> (
              match f_ps with
              | [ a_ty ] ->
                  apply_fun1 env ctx static_env type_env op_f a_ty r_ty args
              | _ ->
                  unsupported "Call of a non-unary function pointer value")
          | LVal (op_c, Clos (ps, r_ty)) ->
              apply_clos_chain env ctx static_env type_env op_c ps r_ty args
          | LVal _ -> unsupported "Call of a non-function value")
      | `Id name -> (
          match name with
          | "print" -> (
              match args with
              | [ arg ] ->
                  lower_builtin_print "print" arg env ctx static_env type_env
              | _ -> unsupported "print expects exactly one argument")
          | "println" -> (
              match args with
              | [ arg ] ->
                  lower_builtin_print "println" arg env ctx static_env type_env
              | _ -> unsupported "println expects exactly one argument")
          | "int_to_str" -> (
              match args with
              | [ arg ] ->
                  lower_builtin_int_to_str arg env ctx static_env type_env
              | _ -> unsupported "int_to_str expects exactly one argument")
          | _ -> begin
              match resolve_callable name env with
              | Some c ->
                  apply_call_args env ctx static_env type_env c args
              | None -> begin
                  match List.assoc_opt name env with
                  | Some (Val (op, Fun ([ a_ty ], r_ty))) ->
                      apply_fun1 env ctx static_env type_env op a_ty r_ty args
                  | Some (Val (op, Clos (ps, r_ty))) ->
                      apply_clos_chain env ctx static_env type_env op ps r_ty args
                  | Some (C _) ->
                      unsupported
                        "Internal: callable binding not resolved as callable"
                  | Some (Val _) | None -> begin
                      if is_poly_static name static_env then
                        match args with
                        | [] ->
                            unsupported
                              "Internal: polymorphic call with empty arg list"
                        | first_arg :: rest_args ->
                            let static_for_mono =
                              static_env_for_mono_call static_env env
                            in
                            let all_args = first_arg :: rest_args in
                            begin
                              match
                                Typecheck.mono_fun_type_of_curried_app
                                  static_for_mono type_env name all_args
                              with
                              | Error err ->
                                  unsupported
                                    ("monomorph: "
                                    ^ Typecheck.string_of_type_check_error err)
                              | Ok m_fun
                                when not
                                       (Typecheck.mono_type_fully_concrete m_fun)
                                ->
                                  unsupported
                                    "Polymorphic partial application is not supported \
                                     for native compilation (call the function with \
                                     concrete arguments on all parameters)"
                              | Ok m_fun ->
                                  let mangle =
                                    mangle_poly_instance name m_fun
                                  in
                                  begin
                                    match resolve_callable mangle env with
                                    | Some c ->
                                        apply_call_args env ctx static_env
                                          type_env c all_args
                                    | None ->
                                        unsupported
                                          ("Missing monomorphized specialization for `"
                                          ^ name ^ "` — compiler bug")
                                  end
                            end
                      else
                        unsupported
                          ("Unknown function `" ^ name
                         ^ "` — declare it above the call, or it is not a \
                            function")
                  end
                end
            end
      )
      end
  | ETernary (cond, e_then, e_else) -> (
      let o_c, t_c = lower_expr_val cond env ctx static_env type_env in
      if t_c <> I1 then unsupported "if condition must be bool";
      let l_then = fresh_lbl ctx "then" in
      let l_else = fresh_lbl ctx "else" in
      let l_merge = fresh_lbl ctx "merge" in
      close_block ctx (BrCond (o_c, l_then, l_else));
      open_block ctx l_then;
      let o1, ty1 =
        match lower_expr e_then env ctx static_env type_env with
        | LVal (o, t) -> (o, t)
        | LPartial c -> materialize_clos_lower env ctx c
      in
      let l_then_exit = ctx.cur_label in
      close_block ctx (Br l_merge);
      open_block ctx l_else;
      let o2, ty2 =
        match lower_expr e_else env ctx static_env type_env with
        | LVal (o, t) -> (o, t)
        | LPartial c -> materialize_clos_lower env ctx c
      in
      let l_else_exit = ctx.cur_label in
      if not (ty_equal ty1 ty2) then
        unsupported "if branches must have the same type";
      (match ty1 with
      | Fun _ -> unsupported "if branches cannot be raw function pointer values"
      | _ -> ());
      close_block ctx (Br l_merge);
      open_block ctx l_merge;
      match ty1 with
      | Unit ->
          LVal (ConstUnit, Unit)
      | String ->
          let materialize o =
            match o with
            | ConstStr _ ->
                let t = fresh () in
                emit_instr ctx (Assign (t, Copy o));
                Local t
            | o -> o
          in
          let o1' = materialize o1 in
          let o2' = materialize o2 in
          let res = fresh () in
          emit_instr ctx
            (Phi (res, ty1, [ (l_then_exit, o1'); (l_else_exit, o2') ]));
          LVal (Local res, ty1)
      | _ ->
          let res = fresh () in
          emit_instr ctx
            (Phi (res, ty1, [ (l_then_exit, o1); (l_else_exit, o2) ]));
          LVal (Local res, ty1))
  | EBindRec (CIdPat name, _ta, e1, e2, _rt) -> (
      match Typecheck.type_rec_binding_rhs static_env type_env name e1 with
      | Error err ->
          unsupported ("let rec: " ^ Typecheck.string_of_type_check_error err)
      | Ok fn_ct ->
          let param_pats, anns, inner = peel_efun [] [] e1 in
          (match param_pats with
          | [] ->
              unsupported
                "let rec on non-function values is not supported for native compilation"
          | _ :: _ ->
              let static_here = (name, fn_ct) :: static_env in
              let emit = mangle_nested_emit name in
              let stub =
                callable_stub ~ty_key:name ~emit_direct:emit anns static_here
              in
              let outer_env = (name, C stub) :: env in
              let fn, nested =
                lower_user_function ~ty_key:name ~emit param_pats anns inner
                  outer_env static_here type_env
              in
              ctx.nested_funcs <- ctx.nested_funcs @ nested @ [ fn ];
              lower_expr e2 outer_env ctx static_here type_env))
  | EBindRec _ ->
      unsupported "let rec: only simple identifier patterns supported for compilation"
  | EFunction _ as lam -> (
      let param_pats, anns, inner_most = peel_efun [] [] lam in
      List.iter
        (function
          | CIdPat _ | CUnitPat | CWildcardPat -> ()
          | _ -> unsupported "lambda parameter pattern")
        param_pats;
      let bound = List.concat (List.map pat_bound_simple param_pats) in
      let fv = S.diff (free_vars_cexpr inner_most) (S.of_list bound) in
      (* Top-level polymorphic defs are not env-bound; call sites monomorphize. *)
      let fv =
        S.filter (fun v -> not (is_poly_static v static_env)) fv
      in
      let cap_entries = lambda_captures env ctx fv in
      match Typecheck.type_of_c_expr static_env type_env lam with
      | Error err ->
          unsupported ("lambda: " ^ Typecheck.string_of_type_check_error err)
      | Ok ct ->
          let mono_full = Typecheck.instantiate ct in
          let syn_key = fresh_lambda_ty_key () in
          let static_here = (syn_key, Mono mono_full) :: static_env in
          let mangled = mangle_nested_emit "lam" in
          let fn, nested =
            lower_user_function ~captures:cap_entries ~ty_key:syn_key ~emit:mangled
              param_pats anns inner_most env static_here type_env
          in
          ctx.nested_funcs <- ctx.nested_funcs @ nested @ [ fn ];
          let param_tys = param_min_ir_tys syn_key anns static_here in
          let ret_ty =
            ret_min_ty_of_user_fn syn_key (List.length param_tys) static_here
          in
          let n_fn = List.length param_tys in
          let cap_tys = List.map (fun (_, t, _) -> t) cap_entries in
          let cap_ops = List.map (fun (_, _, o) -> o) cap_entries in
          let step_codes =
            if n_fn >= 2 then
              List.init n_fn (fun k ->
                  mangled ^ "__ls_s" ^ string_of_int k)
            else if cap_tys <> [] then [ mangled ^ "__ls_s0" ]
            else []
          in
          let c =
            {
              multi_direct = mangled;
              step_codes;
              cap_tys;
              cap_ops;
              fixed = [];
              param_tys;
              ret_ty;
            }
          in
          if callable_remaining c > 0 then LPartial c
          else unsupported "Internal: zero-arity lambda")
  | EBindMutRec _ | ESwitch _ | ENil
  | EListEnumeration _ | EListComprehension _ | EVector _ | ERecordLit _
  | ERecordUpdate _ | EFieldAccess _ | EChar _ | EFloat _ ->
      unsupported "Expression form not supported in Min_IR lowering yet"

and lower_block (parts : c_expr_or_c_defn list) (env : env) (ctx : fn_ctx)
    (static_env : static_env) (type_env : Typecheck.type_env) : expr_result =
  match parts with
  | [] -> LVal (ConstUnit, Unit)
  | [ Expr e ] -> lower_expr e env ctx static_env type_env
  | Defn _ :: _ -> unsupported "Definitions inside blocks are not supported yet"
  | Expr e :: rest -> (
      match lower_expr e env ctx static_env type_env with
      | LVal _ -> lower_block rest env ctx static_env type_env
      | LPartial _ ->
          unsupported
            "Sequencing discard of a partially applied function is not supported")

and lower_user_function ?(captures : (string * ty * operand) list = [])
    ~(ty_key : string) ~(emit : string)
    (param_pats : c_pat list) (param_anns : c_type option list) (inner : c_expr)
    (outer_env : env) (static_env : static_env) (type_env : Typecheck.type_env) :
    func_def * func_def list =
  let param_tys = param_min_ir_tys ty_key param_anns static_env in
  if List.length param_pats <> List.length param_tys then
    unsupported "Internal: parameter pattern count mismatch";
  let cap_pairs = List.map (fun (v, t, _) -> (v, t)) captures in
  let cap_tys = List.map snd cap_pairs in
  let m_cap = List.length cap_pairs in
  let param_names_and_frags =
    List.map2
      (fun pat pt ->
        match pat with
        | CIdPat s -> (
            match pt with
            | Clos (ps, r) -> (s, [ (s, Val (Local s, Clos (ps, r))) ])
            | Fun (ps, r) -> (
                match ps with
                | [ _ ] -> (s, [ (s, Val (Local s, Fun (ps, r))) ])
                | _ ->
                    unsupported
                      "Curried higher-order parameter not supported for native \
                       compilation")
            | _ -> (s, [ (s, Val (Local s, pt)) ]))
        | CUnitPat ->
            let p = fresh_param () in
            (p, [])
        | CWildcardPat ->
            let p = fresh_param () in
            (p, [])
        | CIntPat _ | CBoolPat _ | CNilPat | CConsPat _ | CVectorPat _ | CCharPat _
        | CStringPat _ | CVariantPat _ ->
            unsupported
              "Function parameter pattern not supported for native compilation")
      param_pats param_tys
  in
  let params = List.map fst param_names_and_frags in
  let env_params = List.concat (List.map snd param_names_and_frags) in
  let direct_params = cap_pairs @ List.combine params param_tys in
  let merged =
    List.map (fun (v, t) -> (v, Val (Local v, t))) cap_pairs @ env_params
    @ outer_env
  in
  let ctx = create_fn_ctx () in
  let op, ret_ty =
    match lower_expr inner merged ctx static_env type_env with
    | LVal (o, t) -> (o, t)
    | LPartial c when List.length c.fixed = 0 ->
        materialize_clos_lower merged ctx c
    | LPartial _ ->
        unsupported
          "Returning a partially applied function is not supported"
  in
  let term : term =
    match ret_ty with Unit -> Ret None | _ -> Ret (Some op)
  in
  close_block ctx term;
  let nested_from_body = ctx.nested_funcs in
  let direct_fn =
    {
      name = emit;
      params = direct_params;
      ret = ret_ty;
      entry = "entry";
      blocks = blocks_assoc ctx;
    }
  in
  let n = List.length param_tys in
  let aux_steps =
    if n >= 2 then
      List.init (n - 1) (fun k ->
          emit_curried_step_intermediate ~emit k cap_tys param_tys params ret_ty)
      @ [ emit_curried_step_final ~emit (n - 1) cap_tys param_tys params ret_ty ]
    else if m_cap > 0 then
      [ emit_curried_step_final ~emit 0 cap_tys param_tys params ret_ty ]
    else []
  in
  (direct_fn, nested_from_body @ aux_steps)

let lower_c_expr_to_main (e : c_expr) : (func_def, string) result =
  try
    reset_fresh ();
    let ctx = create_fn_ctx () in
    let op, ret_ty =
      match lower_expr e [] ctx [] [] with
      | LVal (o, t) -> (o, t)
      | LPartial _ ->
          unsupported "Expression must be a value, not a bare or partial function"
    in
    let term : term =
      match ret_ty with Unit -> Ret None | _ -> Ret (Some op)
    in
    close_block ctx term;
    Ok
      {
        name = "main";
        params = [];
        ret = ret_ty;
        entry = "entry";
        blocks = blocks_assoc ctx;
      }
  with Unsupported msg -> Error msg

let lower_c_expr_to_prog (e : c_expr) : (prog, string) result =
  match lower_c_expr_to_main e with
  | Ok fn -> Ok { funcs = [ fn ]; entry = Some "main" }
  | Error e -> Error e

let lower_c_program (defs : c_defn list) (static_env : static_env)
    (type_env : Typecheck.type_env) : (prog, string) result =
  try
    reset_fresh ();
    let defs = eta_expand_program static_env defs in
    let instances = collect_mono_instantiations defs static_env type_env in
    let user_funs = ref [] in
    let env_mono = build_mono_instance_env instances defs static_env in
    List.iter
      (fun (name, mono) ->
        match find_cdefn_function name defs with
        | None -> ()
        | Some (param_pats, anns, inner) ->
            let emit = mangle_poly_instance name mono in
            let fn, nested =
              lower_user_function ~ty_key:emit ~emit param_pats anns inner env_mono
                ((emit, Mono mono) :: static_env)
                type_env
            in
            user_funs := !user_funs @ nested @ [ fn ])
      instances;
    let ctx_main = create_fn_ctx () in
    let rec walk env = function
      | [] -> ()
      | (CTypeAlias _ | CSumType _ | CSumTypeRec _ | CSumTypeRecMutRec _) :: rest
        ->
          walk env rest
      | CDefnRec (pat, _, body, _, _) :: rest -> (
          match pat with
          | CIdPat name -> (
              let param_pats, anns, inner = peel_efun [] [] body in
              match param_pats with
              | [] ->
                  unsupported
                    "let rec on non-function values is not supported for native compilation"
              | _ :: _ when is_poly_static name static_env ->
                  unsupported
                    "Polymorphic let rec is not supported for native compilation yet"
              | _ :: _ ->
                  let stub =
                    callable_stub ~ty_key:name ~emit_direct:name anns static_env
                  in
                  let fn, nested =
                    lower_user_function ~ty_key:name ~emit:name param_pats anns
                      inner ((name, C stub) :: env) static_env type_env
                  in
                  user_funs := !user_funs @ nested @ [ fn ];
                  walk ((name, C stub) :: env) rest)
          | CUnitPat | CWildcardPat | _ ->
              unsupported
                "let rec only supports identifier bindings in native compilation")
      | CDefnMutRec defns :: rest ->
          let parsed =
            List.map
              (fun (pat, _, body, _, _) ->
                match pat with
                | CIdPat name ->
                    let param_pats, anns, inner = peel_efun [] [] body in
                    (name, param_pats, anns, inner)
                | CUnitPat | CWildcardPat | _ ->
                    unsupported
                      "mutually recursive definitions need identifier bindings \
                       in native compilation")
              defns
          in
          List.iter
            (fun (_, param_pats, _, _) ->
              if param_pats = [] then
                unsupported
                  "mutually recursive non-function values are not supported for native compilation")
            parsed;
          List.iter
            (fun (name, param_pats, _, _) ->
              if param_pats <> [] && is_poly_static name static_env then
                unsupported
                  "Polymorphic mutually recursive functions are not supported for native compilation yet")
            parsed;
          let stubs =
            List.map
              (fun (name, _, anns, _) ->
                ( name,
                  callable_stub ~ty_key:name ~emit_direct:name anns static_env ))
              parsed
          in
          let env_with_stubs =
            List.fold_left
              (fun acc (n, c) -> (n, C c) :: acc)
              env stubs
          in
          List.iter
            (fun (name, param_pats, anns, inner) ->
              let fn, nested =
                lower_user_function ~ty_key:name ~emit:name param_pats anns inner
                  env_with_stubs static_env type_env
              in
              user_funs := !user_funs @ nested @ [ fn ])
            parsed;
          let env' =
            List.fold_left
              (fun acc (n, c) -> (n, C c) :: acc)
              env stubs
          in
          walk env' rest
      | CDefn (pat, _, body, _, _) :: rest ->
          (match pat with
          | CIdPat name -> (
              let param_pats, anns, inner = peel_efun [] [] body in
              match param_pats with
              | [] -> (
                  match lower_expr inner env ctx_main static_env type_env with
                  | LVal (o, t) ->
                      emit_instr ctx_main (Assign (name, Copy o));
                      let env' = (name, Val (Local name, t)) :: env in
                      walk env' rest
                  | LPartial c ->
                      let env' = (name, C c) :: env in
                      walk env' rest)
              | _ :: _ when is_poly_static name static_env -> walk env rest
              | _ :: _ ->
                  let stub =
                    callable_stub ~ty_key:name ~emit_direct:name anns static_env
                  in
                  let fn, nested =
                    lower_user_function ~ty_key:name ~emit:name param_pats anns
                      inner env static_env type_env
                  in
                  user_funs := !user_funs @ nested @ [ fn ];
                  walk ((name, C stub) :: env) rest)
          | CUnitPat | CWildcardPat -> (
              match lower_expr body env ctx_main static_env type_env with
              | LVal _ -> walk env rest
              | LPartial _ ->
                  unsupported
                    "Discarded expression cannot be a partially applied function")
          | _ ->
              unsupported
                "Top-level let only supports identifier, unit, or wildcard patterns in Min_IR lowering")
    in
    walk env_mono defs;
    close_block ctx_main (Ret None);
    let main_fn : func_def =
      {
        name = "main";
        params = [];
        ret = Unit;
        entry = "entry";
        blocks = blocks_assoc ctx_main;
      }
    in
    Ok
      {
        funcs = !user_funs @ ctx_main.nested_funcs @ [ main_fn ];
        entry = Some "main";
      }
  with Unsupported msg -> Error msg
