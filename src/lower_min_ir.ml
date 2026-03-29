(** Lower a subset of {!Cexpr.c_expr} to {!Min_ir}.

    Top-level [let rec] / [let rec … and …] use a fixup environment: callable
    stubs (parameter and return types from the typechecker) are installed before
    lowering bodies so direct calls resolve. Value-only recursive bindings are
    unsupported.

    Top-level [let f x y = e] becomes a separate [func_def]; [main] sequences
    value bindings and calls. Curried calls support **partial application**: names
    can bind to a [callable] with a prefix of arguments fixed; further
    [EApp] supplies the rest until a direct [Call]/[VoidCall] is emitted. Passing a
    partial application as a function argument is not supported.

    [&&] and [||] lower to [IAnd]/[IOr] (both operands evaluated; not short-circuit). *)

exception Unsupported of string

let unsupported msg = raise (Unsupported msg)

open Cexpr
open Min_ir

let counter = ref 0

let param_counter = ref 0

let nested_emit_ctr = ref 0

let reset_fresh () =
  counter := 0;
  param_counter := 0;
  nested_emit_ctr := 0

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

let mono_to_min (m : mono_type) : ty =
  match m with
  | IntType -> I32
  | BoolType -> I1
  | StringType -> String
  | UnitType -> Unit
  | FloatType | CharType | TypeVar _ | TypeName _ | FunctionType _
  | VectorType _ | CListType _ | CTypeApp _ | FixedPoint _ | RecordType _ ->
      unsupported "Type not supported for native parameter/return yet"

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

(** A callable is a direct LLVM function name plus a prefix of already-applied
    arguments (SSA operands). [param_tys] is the *full* parameter list of [base]. *)
type callable = {
  base : string;
  fixed : operand list;
  param_tys : ty list;
  ret_ty : ty;
}

type expr_result = LVal of operand * ty | LPartial of callable

type env_binding = Val of operand * ty | C of callable

type env = (string * env_binding) list

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

(** Callable shape for [name] before lowering its body (recursive / mutual fixup). *)
let callable_stub (name : string) (param_anns : c_type option list)
    (static_env : static_env) : callable =
  let param_tys = param_min_ir_tys name param_anns static_env in
  let ret_ty = ret_min_ty_of_user_fn name (List.length param_tys) static_env in
  { base = name; fixed = []; param_tys; ret_ty }

(** Curried application spine: left-most head and arguments left-to-right. *)
let rec peel_app_spine e acc =
  match e with
  | EApp (f, a) -> peel_app_spine f (a :: acc)
  | EId s -> (`Id s, acc)
  | _ -> (`Other e, acc)

let arity_remaining c =
  List.length c.param_tys - List.length c.fixed

let blocks_assoc (ctx : fn_ctx) : (string * block) list =
  List.map (fun b -> (b.label, b)) ctx.completed

let rec lower_expr_val (e : c_expr) (env : env) (ctx : fn_ctx)
    (static_env : static_env) (type_env : Typecheck.type_env) : operand * ty =
  match lower_expr e env ctx static_env type_env with
  | LVal (o, t) -> (o, t)
  | LPartial _ ->
      unsupported
        "Expected a value here, not a partially applied function (cannot pass a \
         partial application as an argument)"

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
        if arity_remaining c = 0 then emit_saturated_call ctx c
        else LPartial c
    | arg :: rest -> (
        if arity_remaining c = 0 then unsupported "Too many arguments in call";
        let i = List.length c.fixed in
        let expect = List.nth c.param_tys i in
        let op, got = lower_expr_val arg env ctx static_env type_env in
        if got <> expect then unsupported "call argument type mismatch";
        let c' = { c with fixed = c.fixed @ [ op ] } in
        match rest with
        | [] ->
            if arity_remaining c' = 0 then emit_saturated_call ctx c' else LPartial c'
        | _ -> go c' rest)
  in
  go c args

and emit_saturated_call ctx (c : callable) : expr_result =
  if List.length c.fixed <> List.length c.param_tys then
    unsupported "Internal: saturated call length mismatch";
  if c.ret_ty = Unit then (
    emit_instr ctx (VoidCall (c.base, c.fixed));
    LVal (ConstUnit, Unit))
  else (
    let t = fresh () in
    emit_instr ctx (Assign (t, Call (c.base, c.fixed)));
    LVal (Local t, c.ret_ty))

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
          if arity_remaining c > 0 then LPartial c
          else unsupported ("`" ^ x ^ "` is already fully applied (compiler bug)")
      | None -> unsupported ("Unbound name `" ^ x ^ "` (not a lowering target)"))
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
      (match head with
      | `Other _ ->
          unsupported "Call shape not supported (callee must be an identifier chain)"
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
          | _ -> (
              match resolve_callable name env with
              | Some c ->
                  apply_call_args env ctx static_env type_env c args
              | None ->
                  unsupported
                    ("Unknown function `" ^ name ^ "` — declare it above the call, \
                     or it is not a function"))))
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
        | LPartial _ ->
            unsupported "if branch cannot be a partially applied function value"
      in
      let l_then_exit = ctx.cur_label in
      close_block ctx (Br l_merge);
      open_block ctx l_else;
      let o2, ty2 =
        match lower_expr e_else env ctx static_env type_env with
        | LVal (o, t) -> (o, t)
        | LPartial _ ->
            unsupported "if branch cannot be a partially applied function value"
      in
      let l_else_exit = ctx.cur_label in
      if ty1 <> ty2 then unsupported "if branches must have the same type";
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
              let c0 = callable_stub name anns static_here in
              let stub = { c0 with base = emit } in
              let outer_env = (name, C stub) :: env in
              let fn, nested =
                lower_user_function ~ty_key:name ~emit param_pats anns inner
                  outer_env static_here type_env
              in
              ctx.nested_funcs <- ctx.nested_funcs @ nested @ [ fn ];
              lower_expr e2 outer_env ctx static_here type_env))
  | EBindRec _ ->
      unsupported "let rec: only simple identifier patterns supported for compilation"
  | EBindMutRec _ | EFunction _ | ESwitch _ | ENil
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

and lower_user_function ~(ty_key : string) ~(emit : string)
    (param_pats : c_pat list) (param_anns : c_type option list) (inner : c_expr)
    (outer_env : env) (static_env : static_env) (type_env : Typecheck.type_env) :
    func_def * func_def list =
  let param_tys = param_min_ir_tys ty_key param_anns static_env in
  if List.length param_pats <> List.length param_tys then
    unsupported "Internal: parameter pattern count mismatch";
  let param_names_and_frags =
    List.map2
      (fun pat pt ->
        match pat with
        | CIdPat s ->
            (s, [ (s, Val (Local s, pt)) ])
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
  let ctx = create_fn_ctx () in
  let merged = env_params @ outer_env in
  let op, ret_ty =
    match lower_expr inner merged ctx static_env type_env with
    | LVal (o, t) -> (o, t)
    | LPartial _ ->
        unsupported "Returning a function value from a user function is not supported"
  in
  let term : term =
    match ret_ty with Unit -> Ret None | _ -> Ret (Some op)
  in
  close_block ctx term;
  let nested = ctx.nested_funcs in
  ( {
      name = emit;
      params = List.combine params param_tys;
      ret = ret_ty;
      entry = "entry";
      blocks = blocks_assoc ctx;
    },
    nested )

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
    let user_funs = ref [] in
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
              | _ :: _ ->
                  let stub = callable_stub name anns static_env in
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
          let stubs =
            List.map
              (fun (name, _, anns, _) -> (name, callable_stub name anns static_env))
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
              | _ :: _ ->
                  let fn, nested =
                    lower_user_function ~ty_key:name ~emit:name param_pats anns
                      inner env static_env type_env
                  in
                  user_funs := !user_funs @ nested @ [ fn ];
                  let c =
                    {
                      base = name;
                      fixed = [];
                      param_tys = List.map snd fn.params;
                      ret_ty = fn.ret;
                    }
                  in
                  walk ((name, C c) :: env) rest)
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
    walk [] defs;
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
