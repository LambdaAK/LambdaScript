(** Native lowering of typed monomorphic programs ({!Tast}, from {!Mono}) to
    {!Min_ir}.

    Representation:
    - [Int] and [Char] are [i64], [Bool] is [i1], [Float] is [double],
      [String] is a C string, lists are cons cells (null is [\[\]]), tuples and
      records (fields sorted by name) are LLVM structs, values of sum types are
      runtime variants (tag and boxed payload).
    - Every function value is a curried closure taking one argument.
    - A named function (top-level, or bound by a local [let] / [let rec]) is
      a {e known} function: a direct LLVM function taking its captured
      variables and all its parameters, called directly when it is applied to
      all of them. Partial applications and uses as values build closures
      whose step functions accumulate the arguments. Anonymous lambdas are
      known functions used as values.
    - Top-level values are globals, initialised by [main] in program order. *)

open Cexpr
open Tast
open Min_ir
module SM = Map.Make (String)

exception Unsupported of string

let unsupported msg = raise (Unsupported msg)

let rec ity (t : Tast.ty) : Min_ir.ty =
  match t with
  | IntType | CharType -> I64
  | BoolType -> I1
  | FloatType -> F64
  | StringType -> String
  | UnitType -> Unit
  | CListType a -> List (ity a)
  | VectorType ts -> Tuple (List.map ity ts)
  | RecordType _ -> Tuple (List.map (fun (_, t) -> ity t) (record_fields t))
  | FunctionType (a, b) -> Clos ([ ity a ], ity b)
  | CTypeApp _ | TypeName _ | TypeVar _ | TCtorApp _ | FixedPoint _ -> RawPtr

let cod (t : Tast.ty) : Tast.ty =
  match t with FunctionType (_, b) -> b | _ -> unsupported "internal: applying a non-function"

(* ------------------------------------------------------------------------ *)
(* Function bodies under construction                                       *)
(* ------------------------------------------------------------------------ *)

type fctx = {
  mutable blocks : (string * block) list;  (** finished blocks, last first *)
  mutable label : string;  (** the open block *)
  mutable instrs : instr list;  (** its instructions, last first *)
  mutable abort_label : string option;  (** shared "no pattern matched" block *)
}

let counter = ref 0

let fresh (prefix : string) : string =
  incr counter;
  Printf.sprintf "%s%d" prefix !counter

let new_fctx () = { blocks = []; label = "entry"; instrs = []; abort_label = None }
let emit (fc : fctx) (i : instr) : unit = fc.instrs <- i :: fc.instrs

let assign (fc : fctx) (r : rhs) : operand =
  let v = fresh "v" in
  emit fc (Assign (v, r));
  Local v

let close (fc : fctx) (t : term) : unit =
  fc.blocks <- (fc.label, { label = fc.label; instrs = List.rev fc.instrs; term = t }) :: fc.blocks

let open_block (fc : fctx) (l : string) : unit =
  fc.label <- l;
  fc.instrs <- []

(** Label of a block that reports a failed match (a run-time error). *)
let abort_label (fc : fctx) : string =
  match fc.abort_label with
  | Some l -> l
  | None ->
      let l = fresh "nomatch" in
      fc.abort_label <- Some l;
      l

let finish (fc : fctx) : (string * block) list =
  let extra =
    match fc.abort_label with
    | Some l -> [ (l, { label = l; instrs = [ VoidCall ("abort", []) ]; term = Unreachable }) ]
    | None -> []
  in
  List.rev fc.blocks @ extra

let funcs : func_def list ref = ref []

let add_func (f : func_def) : unit = funcs := f :: !funcs

(** Return [r] (of type [t]) from the function being built. *)
let ret (fc : fctx) (t : Tast.ty) (r : operand) : unit =
  if ity t = Unit then close fc (Ret None) else close fc (Ret (Some r))

(** A direct call; [Unit]-returning functions are void. *)
let call (fc : fctx) (sym : string) (args : operand list) (rt : Min_ir.ty) : operand =
  if rt = Unit then (
    emit fc (VoidCall (sym, args));
    ConstUnit)
  else assign fc (Call (sym, args))

let phi (fc : fctx) (t : Tast.ty) (incoming : (string * operand) list) : operand =
  if ity t = Unit then ConstUnit
  else
    let v = fresh "p" in
    emit fc (Phi (v, ity t, incoming));
    Local v

(* ------------------------------------------------------------------------ *)
(* Environments                                                             *)
(* ------------------------------------------------------------------------ *)

(** A function with a direct LLVM implementation. *)
type known = {
  k_sym : string;
  k_caps : (string * Min_ir.ty) list;  (** captured variables (unique names) *)
  k_params : Tast.ty list;  (** explicit parameters *)
  k_ret : Tast.ty;  (** result once all parameters are given *)
}

type binding = BVal of operand * Min_ir.ty | BKnown of known

type env = binding SM.t

(** Top-level functions and global values. *)
let topfuns : (string, known) Hashtbl.t = Hashtbl.create 256

let globals : (string, Min_ir.ty) Hashtbl.t = Hashtbl.create 64
let global_sym (x : string) : string = "g." ^ x

let rec peel (e : expr) : pat list * expr =
  match e.e with
  | Lam (p, b) ->
      let ps, body = peel b in
      (p :: ps, body)
  | _ -> ([], e)

let known_of (sym : string) (caps : (string * Min_ir.ty) list) (lam : expr) : known =
  let ps, body = peel lam in
  { k_sym = sym; k_caps = caps; k_params = List.map (fun p -> p.pty) ps; k_ret = body.ty }

(** Type of [k] applied to its first [m] parameters. *)
let known_after (k : known) (m : int) : Tast.ty =
  let rest = List.filteri (fun i _ -> i >= m) k.k_params in
  List.fold_right (fun p acc -> FunctionType (p, acc)) rest k.k_ret

(** Local variables mentioned in [e] (names are unique). *)
let rec vars_in (acc : string list ref) (e : expr) : unit =
  let go = vars_in acc in
  match e.e with
  | Var x -> if not (List.mem x !acc) then acc := x :: !acc
  | Int _ | Float _ | Bool _ | Str _ | Char _ | Unit | Nil | Global _ | Prim _ | Ctor _ -> ()
  | App (a, b) | Let (_, a, b) | Bop (_, a, b) | Range (a, b) ->
      go a;
      go b
  | Lam (_, b) | Field (b, _) -> go b
  | LetRec (bs, b) ->
      List.iter (fun (_, x) -> go x) bs;
      go b
  | Case (s, brs) ->
      go s;
      List.iter (fun (_, b) -> go b) brs
  | If (a, b, c) ->
      go a;
      go b;
      go c
  | Tuple es -> List.iter go es
  | Comp (b, gs) ->
      go b;
      List.iter (fun (_, s) -> go s) gs
  | Record fs -> List.iter (fun (_, x) -> go x) fs
  | Update (r, fs) ->
      go r;
      List.iter (fun (_, x) -> go x) fs

(** Variables a function body needs from [env]: its free local variables,
    and those of the known functions it refers to. *)
let captures (env : env) (es : expr list) : (string * Min_ir.ty) list =
  let names = ref [] in
  List.iter (vars_in names) es;
  let out = ref [] in
  let add (x, t) = if not (List.mem_assoc x !out) then out := (x, t) :: !out in
  List.iter
    (fun x ->
      match SM.find_opt x env with
      | Some (BVal (_, t)) -> add (x, t)
      | Some (BKnown k) -> List.iter add k.k_caps
      | None -> ())
    (List.rev !names);
  List.rev !out

let cap_operands (env : env) (k : known) : operand list =
  List.map
    (fun (x, _) ->
      match SM.find_opt x env with
      | Some (BVal (op, _)) -> op
      | _ -> unsupported ("internal: captured variable " ^ x ^ " is not in scope"))
    k.k_caps

(* ------------------------------------------------------------------------ *)
(* Closures                                                                 *)
(* ------------------------------------------------------------------------ *)

let steps : (string, unit) Hashtbl.t = Hashtbl.create 64
let step_sym (k : known) (m : int) : string = k.k_sym ^ ".s" ^ string_of_int m

(** Environment layout of the closure for step [m] of [k]: captures, then the
    first [m] arguments. *)
let step_layout (k : known) (m : int) : Min_ir.ty list =
  List.map snd k.k_caps @ List.map ity (List.filteri (fun i _ -> i < m) k.k_params)

let rec closure (fc : fctx) (k : known) (m : int) (vals : operand list) : operand =
  let layout = step_layout k m in
  let env_ptr =
    if layout = [] then RawNull
    else
      let p = assign fc (RawMalloc (Llvm_emit.layout_byte_size layout)) in
      List.iteri
        (fun index value -> ignore (assign fc (EnvStore { env = p; layout; index; value })))
        vals;
      p
  in
  ensure_step k m;
  let param = List.nth k.k_params m in
  assign fc
    (MkClos
       { code = step_sym k m; env_ptr; clo_ty = Clos ([ ity param ], ity (known_after k (m + 1))) })

(** Step [m] of [k]: [(env, x)] -> the call, or the closure for step [m+1]. *)
and ensure_step (k : known) (m : int) : unit =
  let sym = step_sym k m in
  if not (Hashtbl.mem steps sym) then (
    Hashtbl.replace steps sym ();
    let fc = new_fctx () in
    let envp = fresh "env" and x = fresh "x" in
    let layout = step_layout k m in
    let loaded =
      List.mapi (fun index _ -> assign fc (EnvLoad { env = Local envp; layout; index })) layout
    in
    let n = List.length k.k_params in
    let param = List.nth k.k_params m in
    let rt = known_after k (m + 1) in
    let r =
      if m = n - 1 then call fc k.k_sym (loaded @ [ Local x ]) (ity k.k_ret)
      else closure fc k (m + 1) (loaded @ [ Local x ])
    in
    ret fc rt r;
    add_func
      {
        name = sym;
        params = [ (envp, RawPtr); (x, ity param) ];
        ret = ity rt;
        entry = "entry";
        blocks = finish fc;
      })

(** Apply closure [f] (of type [t]) to [args] in turn. *)
let rec apply_closure (fc : fctx) (lower : expr -> operand) (f : operand) (t : Tast.ty)
    (args : expr list) : operand =
  match args with
  | [] -> f
  | a :: rest ->
      let v = lower a in
      let rt = cod t in
      let r =
        if ity rt = Unit then (
          ignore (assign fc (ClosApply { clo = f; arg = v; result_ty = Unit }));
          ConstUnit)
        else assign fc (ClosApply { clo = f; arg = v; result_ty = ity rt })
      in
      apply_closure fc lower r rt rest

(* ------------------------------------------------------------------------ *)
(* Structural comparison                                                    *)
(* ------------------------------------------------------------------------ *)

let comparators : (string, string) Hashtbl.t = Hashtbl.create 32

(** Constructors of a sum type: tag, name, payload type. *)
let constructors (t : Tast.ty) : (int * string * Tast.ty option) list =
  match t with
  | CTypeApp (name, _) -> (
      match List.find_opt (fun (n, _, _) -> String.equal n name) !Mono.ctor_env with
      | Some (_, _, ctors) ->
          List.mapi
            (fun i (c, p) -> (i, c, match p with None -> None | Some _ -> Mono.ctor_payload c t))
            ctors
      | None -> unsupported ("internal: unknown type " ^ name))
  | _ -> []

(** [compare a b] for values of type [t]: negative, zero or positive. *)
let rec compare_values (fc : fctx) (t : Tast.ty) (a : operand) (b : operand) : operand =
  match t with
  | FloatType -> assign fc (Call ("ls_cmp_float", [ a; b ]))
  | StringType -> assign fc (Call ("strcmp", [ a; b ]))
  | UnitType -> ConstI64 0
  | _ -> assign fc (Call (comparator t, [ a; b ]))

and comparator (t : Tast.ty) : string =
  let key = Mono.type_key t in
  match Hashtbl.find_opt comparators key with
  | Some s -> s
  | None ->
      let sym = fresh "cmp." in
      Hashtbl.replace comparators key sym;
      let fc = new_fctx () in
      let a = fresh "a" and b = fresh "b" in
      let la = Local a and lb = Local b in
      (* [ret_if c r]: return [r] when [c] holds, else continue. *)
      let ret_if (c : operand) (r : operand) =
        let yes = fresh "L" and no = fresh "L" in
        close fc (BrCond (c, yes, no));
        open_block fc yes;
        close fc (Ret (Some r));
        open_block fc no
      in
      let ret_nonzero (c : operand) = ret_if (assign fc (ICmp (Ne, c, ConstI64 0))) c in
      (match t with
      | IntType | CharType ->
          ret_if (assign fc (ICmp (Slt, la, lb))) (ConstI64 (-1));
          ret_if (assign fc (ICmp (Slt, lb, la))) (ConstI64 1);
          close fc (Ret (Some (ConstI64 0)))
      | BoolType ->
          ret_if (assign fc (ICmp (Eq, la, lb))) (ConstI64 0);
          ret_if la (ConstI64 1);
          close fc (Ret (Some (ConstI64 (-1))))
      | CListType el ->
          let an = assign fc (ICmp (Eq, la, RawNull)) in
          let bn = assign fc (ICmp (Eq, lb, RawNull)) in
          let both = assign fc (IAnd (an, bn)) in
          ret_if both (ConstI64 0);
          ret_if an (ConstI64 (-1));
          ret_if bn (ConstI64 1);
          let e = ity el in
          let ha = assign fc (ListHead { elem_ty = e; lst = la }) in
          let hb = assign fc (ListHead { elem_ty = e; lst = lb }) in
          ret_nonzero (compare_values fc el ha hb);
          let ta = assign fc (ListTail { elem_ty = e; lst = la }) in
          let tb = assign fc (ListTail { elem_ty = e; lst = lb }) in
          close fc (Ret (Some (assign fc (Call (sym, [ ta; tb ])))))
      | VectorType _ | RecordType _ ->
          let ts = match t with VectorType ts -> ts | _ -> List.map snd (record_fields t) in
          let elem_tys = List.map ity ts in
          List.iteri
            (fun index et ->
              let x = assign fc (TupleProj { tup = la; index; elem_tys }) in
              let y = assign fc (TupleProj { tup = lb; index; elem_tys }) in
              ret_nonzero (compare_values fc et x y))
            ts;
          close fc (Ret (Some (ConstI64 0)))
      | CTypeApp _ ->
          let ta = assign fc (VariantTag la) and tb = assign fc (VariantTag lb) in
          ret_if (assign fc (ICmp (Slt, ta, tb))) (ConstI64 (-1));
          ret_if (assign fc (ICmp (Slt, tb, ta))) (ConstI64 1);
          List.iter
            (fun (tag, _, payload) ->
              match payload with
              | None -> ()
              | Some pt ->
                  let here = fresh "L" and next = fresh "L" in
                  close fc (BrCond (assign fc (ICmp (Eq, ta, ConstI64 tag)), here, next));
                  open_block fc here;
                  let pa = assign fc (HeapUnbox (ity pt, assign fc (VariantPayload la))) in
                  let pb = assign fc (HeapUnbox (ity pt, assign fc (VariantPayload lb))) in
                  close fc (Ret (Some (compare_values fc pt pa pb)));
                  open_block fc next)
            (constructors t);
          close fc (Ret (Some (ConstI64 0)))
      | FunctionType _ ->
          emit fc (VoidCall ("ls_compare_functions", []));
          close fc (Ret (Some (ConstI64 0)))
      | _ -> close fc (Ret (Some (compare_values fc t la lb))));
      add_func
        { name = sym; params = [ (a, ity t); (b, ity t) ]; ret = I64; entry = "entry"; blocks = finish fc };
      sym

let equal_values (fc : fctx) (t : Tast.ty) (a : operand) (b : operand) : operand =
  match t with
  | IntType | CharType | BoolType -> assign fc (ICmp (Eq, a, b))
  | UnitType -> ConstI1 true
  | _ -> assign fc (ICmp (Eq, compare_values fc t a b, ConstI64 0))

(* ------------------------------------------------------------------------ *)
(* Pattern matching                                                         *)
(* ------------------------------------------------------------------------ *)

(** Test [op] against [p]: continue in a new block on success (returning the
    pattern's bindings), branch to [fail] otherwise. *)
let rec match_pat (fc : fctx) (env : env) (p : pat) (op : operand) (fail : string) : env =
  let test c =
    let ok = fresh "L" in
    close fc (BrCond (c, ok, fail));
    open_block fc ok
  in
  match p.p with
  | PWild | PUnit -> env
  | PVar x -> SM.add x (BVal (op, ity p.pty)) env
  | PInt n ->
      test (assign fc (ICmp (Eq, op, ConstI64 n)));
      env
  | PChar c ->
      test (assign fc (ICmp (Eq, op, ConstI64 (Char.code c))));
      env
  | PBool b ->
      test (assign fc (ICmp (Eq, op, ConstI1 b)));
      env
  | PStr s ->
      test (assign fc (ICmp (Eq, assign fc (Call ("strcmp", [ op; ConstStr s ])), ConstI64 0)));
      env
  | PFloat f ->
      test (assign fc (ICmp (Eq, assign fc (Call ("ls_cmp_float", [ op; ConstF64 f ])), ConstI64 0)));
      env
  | PNil ->
      test (assign fc (ICmp (Eq, op, RawNull)));
      env
  | PCons (h, t) ->
      test (assign fc (ICmp (Ne, op, RawNull)));
      let e = ity h.pty in
      let hv = assign fc (ListHead { elem_ty = e; lst = op }) in
      let tv = assign fc (ListTail { elem_ty = e; lst = op }) in
      let env = match_pat fc env h hv fail in
      match_pat fc env t tv fail
  | PTuple ps ->
      let elem_tys = List.map (fun q -> ity q.pty) ps in
      let env = ref env in
      List.iteri
        (fun index q -> env := match_pat fc !env q (assign fc (TupleProj { tup = op; index; elem_tys })) fail)
        ps;
      !env
  | PRecord fs ->
      let all = record_fields p.pty in
      let elem_tys = List.map (fun (_, t) -> ity t) all in
      List.fold_left
        (fun env (f, q) ->
          let rec index i = function
            | [] -> unsupported ("internal: no field " ^ f)
            | (g, _) :: rest -> if String.equal f g then i else index (i + 1) rest
          in
          let v = assign fc (TupleProj { tup = op; index = index 0 all; elem_tys }) in
          match_pat fc env q v fail)
        env fs
  | PCtor (_, tag, q) -> (
      test (assign fc (ICmp (Eq, assign fc (VariantTag op), ConstI64 tag)));
      match q with
      | None -> env
      | Some q ->
          let v = assign fc (HeapUnbox (ity q.pty, assign fc (VariantPayload op))) in
          match_pat fc env q v fail)

(* ------------------------------------------------------------------------ *)
(* Primitives                                                               *)
(* ------------------------------------------------------------------------ *)

let prim_arity (p : string) : int =
  match p with
  | "str_slice" -> 3
  | "str_concat" | "int_eq" | "float_eq" | "str_eq" | "char_eq" | "bool_eq" | "unit_eq"
  | "int_compare" | "float_compare" | "str_compare" | "char_compare" | "bool_compare"
  | "unit_compare" ->
      2
  | _ -> 1

let rec arg_types (t : Tast.ty) (n : int) : Tast.ty list =
  if n = 0 then [] else match t with FunctionType (a, b) -> a :: arg_types b (n - 1) | _ -> []

let prim_op (fc : fctx) (p : string) (tys : Tast.ty list) (vs : operand list) : operand =
  match (p, vs) with
  | "print_string", [ s ] ->
      emit fc (VoidCall ("print_string", [ s ]));
      ConstUnit
  | "print", [ s ] ->
      emit fc (VoidCall ("print", [ s ]));
      ConstUnit
  | "int_to_str", [ x ] -> assign fc (Call ("int_to_str", [ x ]))
  | "str_concat", [ a; b ] -> assign fc (Call ("str_concat", [ a; b ]))
  | "int_to_float", [ x ] -> assign fc (Call ("ls_int_to_float", [ x ]))
  | "float_to_int", [ x ] -> assign fc (Call ("ls_float_to_int", [ x ]))
  | "float_to_str", [ x ] -> assign fc (Call ("ls_float_to_str", [ x ]))
  | "char_to_str", [ x ] -> assign fc (Call ("ls_char_to_str", [ x ]))
  | ("string_to_list" | "list_of_string"), [ x ] -> assign fc (Call ("ls_string_to_list", [ x ]))
  | "str_length", [ x ] -> assign fc (Call ("ls_str_length", [ x ]))
  | "str_slice", [ s; a; b ] -> assign fc (Call ("ls_str_slice", [ s; a; b ]))
  | ("int_eq" | "float_eq" | "str_eq" | "char_eq" | "bool_eq" | "unit_eq"), [ a; b ] ->
      equal_values fc (List.hd tys) a b
  | ( ( "int_compare" | "float_compare" | "str_compare" | "char_compare" | "bool_compare"
      | "unit_compare" ),
      [ a; b ] ) ->
      assign fc (Call ("ls_ordering_of_cmp", [ compare_values fc (List.hd tys) a b ]))
  | _ -> unsupported ("internal: primitive " ^ p)

let prim_wrappers : (string, known) Hashtbl.t = Hashtbl.create 16

(** [p] (at type [t]) as a known function, for partial application. *)
let prim_known (p : string) (t : Tast.ty) : known =
  let key = p ^ ":" ^ Mono.type_key t in
  match Hashtbl.find_opt prim_wrappers key with
  | Some k -> k
  | None ->
      let n = prim_arity p in
      let tys = arg_types t n in
      let rt = List.fold_left (fun t _ -> cod t) t tys in
      let k = { k_sym = fresh ("prim." ^ p ^ "."); k_caps = []; k_params = tys; k_ret = rt } in
      Hashtbl.replace prim_wrappers key k;
      let fc = new_fctx () in
      let params = List.map (fun t -> (fresh "a", t)) tys in
      let r = prim_op fc p tys (List.map (fun (x, _) -> Local x) params) in
      ret fc rt r;
      add_func
        {
          name = k.k_sym;
          params = List.map (fun (x, t) -> (x, ity t)) params;
          ret = ity rt;
          entry = "entry";
          blocks = finish fc;
        };
      k

let ctor_wrappers : (string, known) Hashtbl.t = Hashtbl.create 16

let construct (fc : fctx) (tag : int) (payload_ty : Tast.ty) (v : operand) : operand =
  let box = assign fc (HeapBox (ity payload_ty, v)) in
  assign fc (VariantMk (tag, box))

(** Constructor [c] (of function type [t]) as a known function. *)
let ctor_known (c : string) (tag : int) (t : Tast.ty) : known =
  let key = c ^ ":" ^ Mono.type_key t in
  match Hashtbl.find_opt ctor_wrappers key with
  | Some k -> k
  | None -> (
      match t with
      | FunctionType (pt, rt) ->
          let k = { k_sym = fresh ("ctor." ^ c ^ "."); k_caps = []; k_params = [ pt ]; k_ret = rt } in
          Hashtbl.replace ctor_wrappers key k;
          let fc = new_fctx () in
          let x = fresh "a" in
          let r = construct fc tag pt (Local x) in
          ret fc rt r;
          add_func
            { name = k.k_sym; params = [ (x, ity pt) ]; ret = ity rt; entry = "entry"; blocks = finish fc };
          k
      | _ -> unsupported "internal: constructor without payload used as a function")

(* ------------------------------------------------------------------------ *)
(* Expressions                                                              *)
(* ------------------------------------------------------------------------ *)

let rec lower (fc : fctx) (env : env) (e : expr) : operand =
  match e.e with
  | Int n -> ConstI64 n
  | Char c -> ConstI64 (Char.code c)
  | Bool b -> ConstI1 b
  | Float f -> ConstF64 f
  | Str s -> ConstStr s
  | Unit -> ConstUnit
  | Nil -> (
      match e.ty with
      | CListType a -> assign fc (ListNil (ity a))
      | _ -> unsupported "internal: [] of a non-list type")
  | Var x -> (
      match SM.find_opt x env with
      | Some (BVal (op, _)) -> op
      | Some (BKnown k) -> closure fc k 0 (cap_operands env k)
      | None -> unsupported ("internal: unbound variable " ^ x))
  | Global x -> (
      match Hashtbl.find_opt topfuns x with
      | Some k -> closure fc k 0 []
      | None -> (
          match Hashtbl.find_opt globals x with
          | Some t -> assign fc (GlobalLoad (global_sym x, t))
          | None -> unsupported ("internal: unknown global " ^ x)))
  | Prim p -> if prim_arity p = 0 then prim_op fc p [] [] else closure fc (prim_known p e.ty) 0 []
  | Ctor (c, tag) -> (
      match e.ty with
      | FunctionType _ -> closure fc (ctor_known c tag e.ty) 0 []
      | _ -> assign fc (VariantMk (tag, RawNull)))
  | App _ -> lower_app fc env e
  | Lam _ ->
      let k = lift fc env "lam" e in
      closure fc k 0 (cap_operands env k)
  | Let ({ p = PVar x; _ }, ({ e = Lam _; _ } as lam), body) ->
      let k = lift fc env x lam in
      lower fc (SM.add x (BKnown k) env) body
  | Let (p, e1, e2) ->
      let v = lower fc env e1 in
      let env = match_pat fc env p v (abort_label fc) in
      lower fc env e2
  | LetRec (bs, body) ->
      List.iter
        (fun (x, b) ->
          match b.e with
          | Lam _ -> ()
          | _ -> unsupported ("`let rec " ^ x ^ "` must define a function"))
        bs;
      let caps = captures env (List.map snd bs) in
      let ks = List.map (fun (x, lam) -> (x, known_of (fresh ("f." ^ x ^ ".")) caps lam)) bs in
      let env_rec = List.fold_left (fun env (x, k) -> SM.add x (BKnown k) env) env ks in
      List.iter2 (fun (_, k) (_, lam) -> gen_direct k env_rec lam) ks bs;
      lower fc env_rec body
  | Case (s, brs) ->
      let v = lower fc env s in
      let join = fresh "L" in
      let results =
        List.map
          (fun (p, body) ->
            let next = fresh "L" in
            let env' = match_pat fc env p v next in
            let r = lower fc env' body in
            let from = fc.label in
            close fc (Br join);
            open_block fc next;
            (from, r))
          brs
      in
      close fc (Br (abort_label fc));
      open_block fc join;
      phi fc e.ty results
  | If (c, a, b) ->
      let cv = lower fc env c in
      let lt = fresh "L" and lf = fresh "L" and join = fresh "L" in
      close fc (BrCond (cv, lt, lf));
      open_block fc lt;
      let av = lower fc env a in
      let from_a = fc.label in
      close fc (Br join);
      open_block fc lf;
      let bv = lower fc env b in
      let from_b = fc.label in
      close fc (Br join);
      open_block fc join;
      phi fc e.ty [ (from_a, av); (from_b, bv) ]
  | Bop (op, a, b) -> lower_bop fc env e.ty op a b
  | Tuple es ->
      let vs = List.map (lower fc env) es in
      assign fc (TuplePack (List.map (fun (x : expr) -> ity x.ty) es, vs))
  | Record fs ->
      let vs = List.map (fun (_, x) -> lower fc env x) fs in
      assign fc (TuplePack (List.map (fun (_, (x : expr)) -> ity x.ty) fs, vs))
  | Field (r, f) ->
      let v = lower fc env r in
      let all = record_fields r.ty in
      let elem_tys = List.map (fun (_, t) -> ity t) all in
      let rec index i = function
        | [] -> unsupported ("internal: no field " ^ f)
        | (g, _) :: rest -> if String.equal f g then i else index (i + 1) rest
      in
      assign fc (TupleProj { tup = v; index = index 0 all; elem_tys })
  | Update (r, fs) ->
      let v = lower fc env r in
      let news = List.map (fun (f, x) -> (f, lower fc env x)) fs in
      let all = record_fields r.ty in
      let elem_tys = List.map (fun (_, t) -> ity t) all in
      let vs =
        List.mapi
          (fun index (f, _) ->
            match List.assoc_opt f news with
            | Some nv -> nv
            | None -> assign fc (TupleProj { tup = v; index; elem_tys }))
          all
      in
      assign fc (TuplePack (elem_tys, vs))
  | Range (a, b) -> lower_range fc env a b
  | Comp (body, gens) -> lower_comp fc env e.ty body gens

(** Lambda-lift [lam] (bound to [name] in a non-recursive [let], or
    anonymous) into a known function capturing what it needs from [env]. *)
and lift (_fc : fctx) (env : env) (name : string) (lam : expr) : known =
  let caps = captures env [ lam ] in
  let k = known_of (fresh ("f." ^ name ^ ".")) caps lam in
  gen_direct k env lam;
  k

(** The direct function of [k], whose body is [lam], defined in [env]. *)
and gen_direct (k : known) (env : env) (lam : expr) : unit =
  let ps, body = peel lam in
  let fc = new_fctx () in
  let caps = List.map (fun (x, t) -> (fresh "c", t, x)) k.k_caps in
  (* Only captured variables (now parameters) and known functions are valid
     inside the new function. *)
  let env =
    SM.filter_map (fun _ b -> match b with BKnown _ -> Some b | BVal _ -> None) env
  in
  let env = List.fold_left (fun env (c, t, x) -> SM.add x (BVal (Local c, t)) env) env caps in
  let args = List.map (fun (p : pat) -> (fresh "a", ity p.pty, p)) ps in
  let env =
    List.fold_left (fun env (a, _, p) -> match_pat fc env p (Local a) (abort_label fc)) env args
  in
  let r = lower fc env body in
  ret fc body.ty r;
  add_func
    {
      name = k.k_sym;
      params = List.map (fun (c, t, _) -> (c, t)) caps @ List.map (fun (a, t, _) -> (a, t)) args;
      ret = ity body.ty;
      entry = "entry";
      blocks = finish fc;
    }

and lower_app (fc : fctx) (env : env) (e : expr) : operand =
  let rec spine (e : expr) acc = match e.e with App (f, a) -> spine f (a :: acc) | _ -> (e, acc) in
  let head, args = spine e [] in
  let lw = lower fc env in
  let call_known k caps =
    let n = List.length k.k_params in
    if List.length args >= n then
      let first = List.filteri (fun i _ -> i < n) args in
      let rest = List.filteri (fun i _ -> i >= n) args in
      let vs = List.map lw first in
      let r = call fc k.k_sym (caps @ vs) (ity k.k_ret) in
      apply_closure fc lw r k.k_ret rest
    else
      let vs = List.map lw args in
      closure fc k (List.length args) (caps @ vs)
  in
  match head.e with
  | Var x -> (
      match SM.find_opt x env with
      | Some (BKnown k) -> call_known k (cap_operands env k)
      | Some (BVal (op, _)) -> apply_closure fc lw op head.ty args
      | None -> unsupported ("internal: unbound variable " ^ x))
  | Global x when Hashtbl.mem topfuns x -> call_known (Hashtbl.find topfuns x) []
  | Prim p ->
      let n = prim_arity p in
      if List.length args >= n then
        let first = List.filteri (fun i _ -> i < n) args in
        let rest = List.filteri (fun i _ -> i >= n) args in
        let vs = List.map lw first in
        let tys = arg_types head.ty n in
        let r = prim_op fc p tys vs in
        apply_closure fc lw r (List.fold_left (fun t _ -> cod t) head.ty tys) rest
      else call_known (prim_known p head.ty) []
  | Ctor (_, tag) -> (
      match (args, head.ty) with
      | [ a ], FunctionType (pt, _) -> construct fc tag pt (lw a)
      | _ -> unsupported "internal: constructor applied to several arguments")
  | _ ->
      let f = lw head in
      apply_closure fc lw f head.ty args

and lower_bop fc env (t : Tast.ty) (op : c_bop) (a : expr) (b : expr) : operand =
  match op with
  | CAnd | COr ->
      let av = lower fc env a in
      let from_a = fc.label in
      let rhs = fresh "L" and join = fresh "L" in
      if op = CAnd then close fc (BrCond (av, rhs, join)) else close fc (BrCond (av, join, rhs));
      open_block fc rhs;
      let bv = lower fc env b in
      let from_b = fc.label in
      close fc (Br join);
      open_block fc join;
      phi fc t [ (from_a, ConstI1 (op = COr)); (from_b, bv) ]
  | _ -> (
      let av = lower fc env a in
      let bv = lower fc env b in
      let bin k = assign fc (Binop (k, av, bv)) in
      let rel (c : icmp) (swap : bool) =
        match a.ty with
        | IntType | CharType -> assign fc (ICmp (c, (if swap then bv else av), if swap then av else bv))
        | _ ->
            let r = compare_values fc a.ty av bv in
            assign fc (ICmp (c, (if swap then ConstI64 0 else r), if swap then r else ConstI64 0))
      in
      match op with
      | CPlus -> bin Add
      | CMinus -> bin Sub
      | CMul -> bin Mul
      | CDiv -> bin Div
      | CMod -> bin Mod
      | CCons -> assign fc (ListCons { elem_ty = ity a.ty; head = av; tail = bv })
      | CConcat -> assign fc (Call ("str_concat", [ av; bv ]))
      | CEQ -> equal_values fc a.ty av bv
      | CNE -> (
          match equal_values fc a.ty av bv with
          | ConstI1 x -> ConstI1 (not x)
          | eq -> assign fc (ICmp (Eq, eq, ConstI1 false)))
      | CLT -> rel Slt false
      | CLE -> rel Sle false
      | CGE -> rel Sge false
      | CGT -> rel Slt true
      | CAnd | COr -> assert false)

(** [\[a...b\]]: a loop consing from [b] down to [a]. *)
and lower_range fc env a b =
  let lo = lower fc env a in
  let hi = lower fc env b in
  let nil = assign fc (ListNil I64) in
  let pre = fc.label in
  let head = fresh "L" and body = fresh "L" and exit = fresh "L" in
  let i = fresh "i" and acc = fresh "acc" and i' = fresh "i" and acc' = fresh "acc" in
  close fc (Br head);
  open_block fc head;
  emit fc (Phi (i, I64, [ (pre, hi); (body, Local i') ]));
  emit fc (Phi (acc, List I64, [ (pre, nil); (body, Local acc') ]));
  close fc (BrCond (assign fc (ICmp (Slt, Local i, lo)), exit, body));
  open_block fc body;
  emit fc (Assign (acc', ListCons { elem_ty = I64; head = Local i; tail = Local acc }));
  emit fc (Assign (i', Binop (Sub, Local i, ConstI64 1)));
  close fc (Br head);
  open_block fc exit;
  Local acc

(** Reverse list [l] (elements of type [el]). *)
and reverse_list fc (el : Min_ir.ty) (l : operand) : operand =
  let nil = assign fc (ListNil el) in
  let pre = fc.label in
  let head = fresh "L" and body = fresh "L" and exit = fresh "L" in
  let cur = fresh "cur" and acc = fresh "acc" and tl = fresh "tl" and acc' = fresh "acc" in
  close fc (Br head);
  open_block fc head;
  emit fc (Phi (cur, List el, [ (pre, l); (body, Local tl) ]));
  emit fc (Phi (acc, List el, [ (pre, nil); (body, Local acc') ]));
  close fc (BrCond (assign fc (ICmp (Eq, Local cur, RawNull)), exit, body));
  open_block fc body;
  let h = assign fc (ListHead { elem_ty = el; lst = Local cur }) in
  emit fc (Assign (tl, ListTail { elem_ty = el; lst = Local cur }));
  emit fc (Assign (acc', ListCons { elem_ty = el; head = h; tail = Local acc }));
  close fc (Br head);
  open_block fc exit;
  Local acc

(** [\[body | p1 <- l1, ...\]]: nested loops accumulating the results in
    reverse, then a reversal. Elements a pattern does not match are
    skipped. *)
and lower_comp fc env (t : Tast.ty) body gens =
  let el = match t with CListType a -> ity a | _ -> unsupported "internal: comprehension type" in
  let rec go env gens (acc_in : operand) : operand =
    match gens with
    | [] ->
        let v = lower fc env body in
        assign fc (ListCons { elem_ty = el; head = v; tail = acc_in })
    | ((p : pat), src) :: rest ->
        let l = lower fc env src in
        let src_el = ity p.pty in
        let pre = fc.label in
        let head = fresh "L" and bodyl = fresh "L" and skip = fresh "L" in
        let latch = fresh "L" and exit = fresh "L" in
        let cur = fresh "cur" and acc = fresh "acc" and tl = fresh "tl" and accn = fresh "acc" in
        close fc (Br head);
        open_block fc head;
        emit fc (Phi (cur, List src_el, [ (pre, l); (latch, Local tl) ]));
        emit fc (Phi (acc, List el, [ (pre, acc_in); (latch, Local accn) ]));
        close fc (BrCond (assign fc (ICmp (Eq, Local cur, RawNull)), exit, bodyl));
        open_block fc bodyl;
        let h = assign fc (ListHead { elem_ty = src_el; lst = Local cur }) in
        emit fc (Assign (tl, ListTail { elem_ty = src_el; lst = Local cur }));
        let env' = match_pat fc env p h skip in
        let inner = go env' rest (Local acc) in
        let inner_end = fc.label in
        close fc (Br latch);
        open_block fc skip;
        close fc (Br latch);
        open_block fc latch;
        emit fc (Phi (accn, List el, [ (inner_end, inner); (skip, Local acc) ]));
        close fc (Br head);
        open_block fc exit;
        Local acc
  in
  let rev = go env gens (assign fc (ListNil el)) in
  reverse_list fc el rev

(* ------------------------------------------------------------------------ *)
(* Programs                                                                 *)
(* ------------------------------------------------------------------------ *)

let rec pat_vars (p : pat) : (string * Tast.ty) list =
  match p.p with
  | PVar x -> [ (x, p.pty) ]
  | PWild | PUnit | PNil | PInt _ | PBool _ | PStr _ | PChar _ | PFloat _ -> []
  | PCons (a, b) -> pat_vars a @ pat_vars b
  | PTuple ps -> List.concat_map pat_vars ps
  | PRecord fs -> List.concat_map (fun (_, q) -> pat_vars q) fs
  | PCtor (_, _, q) -> ( match q with Some q -> pat_vars q | None -> [])

let program (items : Tast.program) : Min_ir.prog =
  counter := 0;
  funcs := [];
  Hashtbl.reset topfuns;
  Hashtbl.reset globals;
  Hashtbl.reset steps;
  Hashtbl.reset comparators;
  Hashtbl.reset prim_wrappers;
  Hashtbl.reset ctor_wrappers;
  List.iter
    (function
      | Tast.Fun (n, lam) -> Hashtbl.replace topfuns n (known_of ("f." ^ n) [] lam)
      | Tast.Val (p, _) -> List.iter (fun (x, t) -> Hashtbl.replace globals x (ity t)) (pat_vars p))
    items;
  let fc = new_fctx () in
  List.iter
    (function
      | Tast.Val (p, e) ->
          let v = lower fc SM.empty e in
          let env = match_pat fc SM.empty p v (abort_label fc) in
          List.iter
            (fun (x, t) ->
              match SM.find_opt x env with
              | Some (BVal (op, _)) -> emit fc (GlobalStore (global_sym x, ity t, op))
              | _ -> ())
            (pat_vars p)
      | Tast.Fun _ -> ())
    items;
  close fc (Ret None);
  let main = { name = "main"; params = []; ret = Unit; entry = "entry"; blocks = finish fc } in
  List.iter (function Tast.Fun (n, lam) -> gen_direct (Hashtbl.find topfuns n) SM.empty lam | Tast.Val _ -> ()) items;
  { funcs = List.rev !funcs @ [ main ]; entry = Some "main" }
