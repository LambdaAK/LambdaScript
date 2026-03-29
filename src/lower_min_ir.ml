(** Lower a subset of {!Cexpr.c_expr} to {!Min_ir}.

    Uses a per-function block builder so control flow ([ETernary] / [if … then … else …])
    can add multiple basic blocks, [Phi] at merges, and non-unit branches. *)

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

type env = (string * (operand * ty)) list

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
  | _ -> None

let rec lower_expr (e : c_expr) (env : env) (ctx : fn_ctx) : operand * ty =
  match e with
  | EInt n -> (ConstI32 n, I32)
  | EBool b -> (ConstI1 b, I1)
  | EString s -> (ConstStr s, String)
  | EUnit -> (ConstUnit, Unit)
  | EId x -> (
      match List.assoc_opt x env with
      | Some (o, t) -> (o, t)
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
              | _ -> unsupported ("Binary operator not supported in Min_IR lowering yet"))))
  | EBind (CIdPat x, _ta, e1, e2, _rt) ->
      let o1, t1 = lower_expr e1 env ctx in
      emit_instr ctx (Assign (x, Copy o1));
      let env' = (x, (Local x, t1)) :: env in
      lower_expr e2 env' ctx
  | EBind _ -> unsupported "let: only simple identifier patterns supported"
  | EBlock parts -> lower_block parts env ctx
  | EApp (EId ("print" | "println" as name), e2) ->
      let o2, t2 = lower_expr e2 env ctx in
      if t2 <> String then unsupported "print/println expect a string argument";
      let arg =
        match o2 with
        | ConstStr _ ->
            let t = fresh () in
            emit_instr ctx (Assign (t, Copy o2));
            Local t
        | o -> o
      in
      emit_instr ctx (VoidCall (name, [ arg ]));
      (ConstUnit, Unit)
  | EApp (EId "int_to_str", e2) ->
      let o2, t2 = lower_expr e2 env ctx in
      if t2 <> I32 then unsupported "int_to_str expects i32";
      let t = fresh () in
      emit_instr ctx (Assign (t, Call ("int_to_str", [ o2 ])));
      (Local t, String)
  | EApp _ ->
      unsupported "Only direct calls print / println / int_to_str are supported"
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

(** Single-function program: [main] with no parameters. *)
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

let lower_c_program (defs : c_defn list) : (prog, string) result =
  try
    reset_fresh ();
    let ctx = create_fn_ctx () in
    let rec walk env = function
      | [] -> ()
      | (CTypeAlias _ | CSumType _ | CSumTypeRec _ | CSumTypeRecMutRec _) :: rest
        ->
          walk env rest
      | (CDefnRec _ | CDefnMutRec _) :: _ ->
          unsupported
            "Recursive top-level definitions are not supported in Min_IR lowering yet"
      | CDefn (pat, _, body, _, _) :: rest ->
          let o, t = lower_expr body env ctx in
          let env' =
            match pat with
            | CIdPat x ->
                emit_instr ctx (Assign (x, Copy o));
                (x, (Local x, t)) :: env
            | CUnitPat | CWildcardPat -> env
            | _ ->
                unsupported
                  "Top-level let only supports identifier, unit, or wildcard patterns in Min_IR lowering"
          in
          walk env' rest
    in
    walk [] defs;
    close_block ctx (Ret None);
    Ok
      {
        funcs =
          [
            {
              name = "main";
              params = [];
              ret = Unit;
              entry = "entry";
              blocks = blocks_assoc ctx;
            };
          ];
        entry = Some "main";
      }
  with Unsupported msg -> Error msg
