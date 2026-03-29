(** Lower a tiny subset of {!Cexpr.c_expr} to {!Min_ir}.

    Pipeline position: after typechecking (same order surface code uses before
    [eval]), so you receive only well-scoped [c_expr] trees.

    Supported forms (extend with new [match] arms as the compiler grows):
    - [EInt], [EBool], [EString], [EUnit]
    - [EBop]: [CPlus], [CMinus], [CMul], [CDiv], [CMod]; [CEQ], [CNE], [CLT],
      [CGT] (via swapped [Slt])
    - [EBind] with [CIdPat] only
    - [EBlock] / [EBlock]-style: sequence of [Expr] parts; last value is the
      block value (definitions in blocks: not yet)
    - [EApp]: [print], [println], [int_to_str] with callee [EId]

    Everything else raises {!Unsupported}. *)

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

let rec lower_expr (e : c_expr) (env : env) : instr list * operand * ty =
  match e with
  | EInt n -> ([], ConstI32 n, I32)
  | EBool b -> ([], ConstI1 b, I1)
  | EString s -> ([], ConstStr s, String)
  | EUnit -> ([], ConstUnit, Unit)
  | EId x -> (
      match List.assoc_opt x env with
      | Some (o, t) -> ([], o, t)
      | None -> unsupported ("Unbound name `" ^ x ^ "` (not a lowering target)"))
  | EBop (op, e1, e2) -> (
      match map_arith_bop op with
      | Some b ->
          let i1, o1, t1 = lower_expr e1 env in
          let i2, o2, t2 = lower_expr e2 env in
          if t1 <> I32 || t2 <> I32 then
            unsupported "Arithmetic expects i32 operands";
          let t = fresh () in
          (i1 @ i2 @ [ Assign (t, Binop (b, o1, o2)) ], Local t, I32)
      | None -> (
          match map_cmp op with
          | Some c ->
              let i1, o1, t1 = lower_expr e1 env in
              let i2, o2, t2 = lower_expr e2 env in
              if t1 <> I32 || t2 <> I32 then
                unsupported "Integer comparison expects i32 operands";
              let t = fresh () in
              (i1 @ i2 @ [ Assign (t, ICmp (c, o1, o2)) ], Local t, I1)
          | None -> (
              match op with
              | CGT ->
                  let i1, o1, t1 = lower_expr e1 env in
                  let i2, o2, t2 = lower_expr e2 env in
                  if t1 <> I32 || t2 <> I32 then
                    unsupported "Integer comparison expects i32 operands";
                  let t = fresh () in
                  (* a > b  ==  b < a *)
                  ( i1 @ i2 @ [ Assign (t, ICmp (Slt, o2, o1)) ],
                    Local t,
                    I1 )
              | _ -> unsupported ("Binary operator not supported in Min_ir lowering yet"))))
  | EBind (CIdPat x, _ta, e1, e2, _rt) ->
      let i1, o1, t1 = lower_expr e1 env in
      let env' = (x, (Local x, t1)) :: env in
      let i2, o2, t2 = lower_expr e2 env' in
      (i1 @ [ Assign (x, Copy o1) ] @ i2, o2, t2)
  | EBind _ -> unsupported "let: only simple identifier patterns supported"
  | EBlock parts -> lower_block parts env
  | EApp (EId ("print" | "println" as name), e2) ->
      let i2, o2, t2 = lower_expr e2 env in
      if t2 <> String then unsupported "print/println expect a string argument";
      (i2 @ [ VoidCall (name, [ o2 ]) ], ConstUnit, Unit)
  | EApp (EId "int_to_str", e2) ->
      let i2, o2, t2 = lower_expr e2 env in
      if t2 <> I32 then unsupported "int_to_str expects i32";
      let t = fresh () in
      (i2 @ [ Assign (t, Call ("int_to_str", [ o2 ])) ], Local t, String)
  | EApp _ -> unsupported "Only direct calls print / println / int_to_str are supported"
  | EBindRec _ | EBindMutRec _ | EFunction _ | ETernary _ | ESwitch _ | ENil
  | EListEnumeration _ | EListComprehension _ | EVector _ | ERecordLit _
  | ERecordUpdate _ | EFieldAccess _ | EChar _ | EFloat _ ->
      unsupported "Expression form not supported in Min_ir lowering yet"

and lower_block (parts : c_expr_or_c_defn list) (env : env) :
    instr list * operand * ty =
  match parts with
  | [] -> ([], ConstUnit, Unit)
  | [ Expr e ] -> lower_expr e env
  | Defn _ :: _ -> unsupported "Definitions inside blocks are not supported yet"
  | Expr e :: rest ->
      let i1, _o1, _t1 = lower_expr e env in
      let i2, o2, t2 = lower_block rest env in
      (i1 @ i2, o2, t2)

(** Single-function program: [main] with no parameters, one basic block. *)
let lower_c_expr_to_main (e : c_expr) : (func_def, string) result =
  try
    reset_fresh ();
    let instrs, op, ret_ty = lower_expr e [] in
    let term : term =
      match ret_ty with
      | Unit -> Ret None
      | _ -> Ret (Some op)
    in
    let blk : block =
      { label = "entry"; instrs; term }
    in
    Ok
      {
        name = "main";
        params = [];
        ret = ret_ty;
        entry = "entry";
        blocks = [ ("entry", blk) ];
      }
  with Unsupported msg -> Error msg

(** Package a main function as a {!Min_ir.prog} with [entry = Some "main"]. *)
let lower_c_expr_to_prog (e : c_expr) : (prog, string) result =
  match lower_c_expr_to_main e with
  | Ok fn -> Ok { funcs = [ fn ]; entry = Some "main" }
  | Error e -> Error e

(** Lower type-checked top-level definitions to a single [main] (effect + binds
    only). Type/sum definitions produce no IR and are skipped. *)
let lower_c_program (defs : c_defn list) : (prog, string) result =
  try
    reset_fresh ();
    let rec walk (env : env) (acc : instr list) = function
      | [] -> (acc, env)
      | (CTypeAlias _ | CSumType _ | CSumTypeRec _ | CSumTypeRecMutRec _) :: rest
        ->
          walk env acc rest
      | (CDefnRec _ | CDefnMutRec _) :: _ ->
          unsupported
            "Recursive top-level definitions are not supported in Min_IR lowering yet"
      | CDefn (pat, _, body, _, _) :: rest ->
          let i, o, t = lower_expr body env in
          let acc', env' =
            match pat with
            | CIdPat x ->
                ( acc @ i @ [ Assign (x, Copy o) ],
                  (x, (Local x, t)) :: env )
            | CUnitPat | CWildcardPat -> (acc @ i, env)
            | _ ->
                unsupported
                  "Top-level let only supports identifier, unit, or wildcard patterns in Min_IR lowering"
          in
          walk env' acc' rest
    in
    let instrs, _env = walk [] [] defs in
    let blk : block =
      { label = "entry"; instrs; term = Ret None }
    in
    let fn : func_def =
      {
        name = "main";
        params = [];
        ret = Unit;
        entry = "entry";
        blocks = [ ("entry", blk) ];
      }
    in
    Ok { funcs = [ fn ]; entry = Some "main" }
  with Unsupported msg -> Error msg
