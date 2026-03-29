(** Lower a subset of {!Cexpr.c_expr} to {!Min_ir}.

    Top-level [let f x y = e] becomes a separate [func_def]; [main] sequences
    value bindings and calls. Only direct, fully saturated calls (curried surface
    syntax) are supported — no partial application as a value.

    [&&] and [||] lower to [IAnd]/[IOr] (both operands evaluated; not short-circuit). *)

exception Unsupported of string

let unsupported msg = raise (Unsupported msg)

open Cexpr
open Min_ir

let counter = ref 0

let reset_fresh () =
  counter := 0

let fresh () =
  incr counter;
  "_t" ^ string_of_int !counter

type fn_ctx = {
  mutable completed : block list;
  mutable cur_label : string;
  mutable cur_instrs : instr list;
  mutable lbl_counter : int;
}

let create_fn_ctx () : fn_ctx =
  { completed = []; cur_label = "entry"; cur_instrs = []; lbl_counter = 0 }

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

(** [peel_efun body] — parameters (with annotations), inner expression. *)
let rec peel_efun acc_p acc_a : c_expr -> string list * c_type option list * c_expr
    = function
  | EFunction (CIdPat p, ann, rest) ->
      peel_efun (p :: acc_p) (ann :: acc_a) rest
  | EFunction _ ->
      unsupported "Function parameter must be a simple identifier for compilation"
  | e ->
      (List.rev acc_p, List.rev acc_a, e)

type fun_spec = {
  arity : int;
  param_tys : ty list;
  ret_ty : ty;
}

type env_binding = Val of operand * ty | Fun of fun_spec

type env = (string * env_binding) list

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

(** Curried call chain [f a b …]: [EApp (EApp (EId f, a), b)]. *)
let rec peel_call acc e =
  match e with
  | EApp (f, a) -> peel_call (a :: acc) f
  | EId s -> Some (s, acc)
  | _ -> None

let rec lower_builtin_print name arg env ctx =
  let o2, t2 = lower_expr arg env ctx in
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
  (ConstUnit, Unit)

and lower_builtin_int_to_str arg env ctx =
  let o2, t2 = lower_expr arg env ctx in
  if t2 <> I32 then unsupported "int_to_str expects i32";
  let t = fresh () in
  emit_instr ctx (Assign (t, Call ("int_to_str", [ o2 ])));
  (Local t, String)

and lower_expr (e : c_expr) (env : env) (ctx : fn_ctx) : operand * ty =
  match e with
  | EInt n -> (ConstI32 n, I32)
  | EBool b -> (ConstI1 b, I1)
  | EString s -> (ConstStr s, String)
  | EUnit -> (ConstUnit, Unit)
  | EId x -> (
      match List.assoc_opt x env with
      | Some (Val (o, t)) -> (o, t)
      | Some (Fun _) ->
          unsupported ("`" ^ x ^ "` is a function — add arguments to call it")
      | None -> unsupported ("Unbound name `" ^ x ^ "` (not a lowering target)"))
  | EBop (op, e1, e2) -> (
      match map_arith_bop op with
      | Some b ->
          let o1, t1 = lower_expr e1 env ctx in
          let o2, t2 = lower_expr e2 env ctx in
          if t1 <> I32 || t2 <> I32 then
            unsupported "Arithmetic expects i32 operands";
          let t = fresh () in
          emit_instr ctx (Assign (t, Binop (b, o1, o2)));
          (Local t, I32)
      | None -> (
          match map_cmp op with
          | Some c ->
              let o1, t1 = lower_expr e1 env ctx in
              let o2, t2 = lower_expr e2 env ctx in
              if t1 <> I32 || t2 <> I32 then
                unsupported "Integer comparison expects i32 operands";
              let t = fresh () in
              emit_instr ctx (Assign (t, ICmp (c, o1, o2)));
              (Local t, I1)
          | None -> (
              match op with
              | CGT ->
                  let o1, t1 = lower_expr e1 env ctx in
                  let o2, t2 = lower_expr e2 env ctx in
                  if t1 <> I32 || t2 <> I32 then
                    unsupported "Integer comparison expects i32 operands";
                  let t = fresh () in
                  emit_instr ctx (Assign (t, ICmp (Slt, o2, o1)));
                  (Local t, I1)
              | CAnd ->
                  let o1, t1 = lower_expr e1 env ctx in
                  let o2, t2 = lower_expr e2 env ctx in
                  if t1 <> I1 || t2 <> I1 then
                    unsupported "&& expects bool operands";
                  let t = fresh () in
                  emit_instr ctx (Assign (t, IAnd (o1, o2)));
                  (Local t, I1)
              | COr ->
                  let o1, t1 = lower_expr e1 env ctx in
                  let o2, t2 = lower_expr e2 env ctx in
                  if t1 <> I1 || t2 <> I1 then
                    unsupported "|| expects bool operands";
                  let t = fresh () in
                  emit_instr ctx (Assign (t, IOr (o1, o2)));
                  (Local t, I1)
              | _ -> unsupported ("Binary operator not supported in Min_IR lowering yet"))))
  | EBind (CIdPat x, _ta, e1, e2, _rt) ->
      let o1, t1 = lower_expr e1 env ctx in
      emit_instr ctx (Assign (x, Copy o1));
      let env' = (x, Val (Local x, t1)) :: env in
      lower_expr e2 env' ctx
  | EBind _ -> unsupported "let: only simple identifier patterns supported"
  | EBlock parts -> lower_block parts env ctx
  | EApp (e1, e2) -> (
      match peel_call [ e2 ] e1 with
      | Some ("print", [ arg ]) -> lower_builtin_print "print" arg env ctx
      | Some ("println", [ arg ]) -> lower_builtin_print "println" arg env ctx
      | Some ("int_to_str", [ arg ]) -> lower_builtin_int_to_str arg env ctx
      | Some (name, args) -> (
          match List.assoc_opt name env with
          | Some (Fun spec) ->
              if List.length args <> spec.arity then
                unsupported
                  (Printf.sprintf
                     "function `%s` expects %d arguments (got %d) — partial \
                      application is not compiled"
                     name spec.arity (List.length args));
              let arg_ops =
                List.map2
                  (fun arg_e expect_ty ->
                    let o, got = lower_expr arg_e env ctx in
                    if got <> expect_ty then
                      unsupported "call argument type mismatch";
                    o)
                  args spec.param_tys
              in
              if spec.ret_ty = Unit then (
                emit_instr ctx (VoidCall (name, arg_ops));
                (ConstUnit, Unit))
              else (
                let t = fresh () in
                emit_instr ctx (Assign (t, Call (name, arg_ops)));
                (Local t, spec.ret_ty))
          | Some (Val _) ->
              unsupported "Called name is a value, not a function"
          | None ->
              unsupported ("Unknown callee `" ^ name ^ "` (declare it above the call)"))
      | None -> unsupported "Call shape not supported (need f arg … arg)")
  | ETernary (cond, e_then, e_else) -> (
      let o_c, t_c = lower_expr cond env ctx in
      if t_c <> I1 then unsupported "if condition must be bool";
      let l_then = fresh_lbl ctx "then" in
      let l_else = fresh_lbl ctx "else" in
      let l_merge = fresh_lbl ctx "merge" in
      close_block ctx (BrCond (o_c, l_then, l_else));
      open_block ctx l_then;
      let o1, ty1 = lower_expr e_then env ctx in
      close_block ctx (Br l_merge);
      open_block ctx l_else;
      let o2, ty2 = lower_expr e_else env ctx in
      if ty1 <> ty2 then unsupported "if branches must have the same type";
      close_block ctx (Br l_merge);
      open_block ctx l_merge;
      match ty1 with
      | Unit ->
          (ConstUnit, Unit)
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
          emit_instr ctx (Phi (res, ty1, [ (l_then, o1'); (l_else, o2') ]));
          (Local res, ty1)
      | _ ->
          let res = fresh () in
          emit_instr ctx (Phi (res, ty1, [ (l_then, o1); (l_else, o2) ]));
          (Local res, ty1))
  | EBindRec _ | EBindMutRec _ | EFunction _ | ESwitch _ | ENil
  | EListEnumeration _ | EListComprehension _ | EVector _ | ERecordLit _
  | ERecordUpdate _ | EFieldAccess _ | EChar _ | EFloat _ ->
      unsupported "Expression form not supported in Min_IR lowering yet"

and lower_block (parts : c_expr_or_c_defn list) (env : env) (ctx : fn_ctx) :
    operand * ty =
  match parts with
  | [] -> (ConstUnit, Unit)
  | [ Expr e ] -> lower_expr e env ctx
  | Defn _ :: _ -> unsupported "Definitions inside blocks are not supported yet"
  | Expr e :: rest ->
      let _o1, _t1 = lower_expr e env ctx in
      lower_block rest env ctx

let blocks_assoc (ctx : fn_ctx) : (string * block) list =
  List.map (fun b -> (b.label, b)) ctx.completed

let lower_user_function (name : string) (params : string list)
    (param_anns : c_type option list) (inner : c_expr) (env : env)
    (static_env : static_env) : func_def =
  let param_tys = param_min_ir_tys name param_anns static_env in
  let ctx = create_fn_ctx () in
  let env_params =
    List.fold_right
      (fun (p, pt) acc -> (p, Val (Local p, pt)) :: acc)
      (List.combine params param_tys)
      []
  in
  let merged = env_params @ env in
  let op, ret_ty = lower_expr inner merged ctx in
  let term : term =
    match ret_ty with Unit -> Ret None | _ -> Ret (Some op)
  in
  close_block ctx term;
  {
    name;
    params = List.combine params param_tys;
    ret = ret_ty;
    entry = "entry";
    blocks = blocks_assoc ctx;
  }

let lower_c_expr_to_main (e : c_expr) : (func_def, string) result =
  try
    reset_fresh ();
    let ctx = create_fn_ctx () in
    let op, ret_ty = lower_expr e [] ctx in
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

let lower_c_program (defs : c_defn list) (static_env : static_env) :
    (prog, string) result =
  try
    reset_fresh ();
    let user_funs = ref [] in
    let ctx_main = create_fn_ctx () in
    let rec walk env = function
      | [] -> ()
      | (CTypeAlias _ | CSumType _ | CSumTypeRec _ | CSumTypeRecMutRec _) :: rest
        ->
          walk env rest
      | (CDefnRec _ | CDefnMutRec _) :: _ ->
          unsupported
            "Recursive top-level definitions are not supported in Min_IR lowering yet"
      | CDefn (pat, _, body, _, _) :: rest ->
          (match pat with
          | CIdPat name -> (
              let params, anns, inner = peel_efun [] [] body in
              match params with
              | [] ->
                  let o, t = lower_expr inner env ctx_main in
                  let env' = (name, Val (Local name, t)) :: env in
                  emit_instr ctx_main (Assign (name, Copy o));
                  walk env' rest
              | _ :: _ ->
                  let fn =
                    lower_user_function name params anns inner env static_env
                  in
                  user_funs := !user_funs @ [ fn ];
                  let spec =
                    {
                      arity = List.length fn.params;
                      param_tys = List.map snd fn.params;
                      ret_ty = fn.ret;
                    }
                  in
                  walk ((name, Fun spec) :: env) rest)
          | CUnitPat | CWildcardPat ->
              let _o, _t = lower_expr body env ctx_main in
              walk env rest
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
    Ok { funcs = !user_funs @ [ main_fn ]; entry = Some "main" }
  with Unsupported msg -> Error msg
