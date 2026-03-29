(** Minimal compiler IR for the first LLVM backend.

    Intended pipeline (later): [Cexpr] or surface AST -> [Min_ir] -> LLVM IR.

    Scope of this IR (intentionally tiny):
    - Types [i32], [i1], [string], [unit]. [string] is an opaque value (LLVM
      will use something like [i8*] + runtime allocation for literals).
    - Local bindings via [Assign]; every intermediate has a name (LLVM-friendly)
    - Side-effecting I/O via [VoidCall] (see {!runtime_void_symbols})
    - String conversion matching LambdaScript builtins: [int_to_str] as [Call]
    - Control flow: labeled basic blocks; [Br], [BrCond]; [Ret]
    - [Phi] only at block heads (LLVM convention) for SSA merge points

    Not represented yet: memory model beyond opaque strings, structs, closures,
    tags, GC details, varargs, exceptions. *)

type ty =
  | I32
  | I1
  | String
  | Unit
  (** Monomorphic function pointer ([param types], return). LLVM: e.g. [i32
      (i32)*]. *)
  | Fun of ty list * ty

type ibin = Add | Sub | Mul | Div | Mod

type icmp = Eq | Ne | Ult | Slt | Sle | Sge

(** Values that can flow through SSA edges (constants or locals). *)
type operand =
  | Local of string
  | ConstI32 of int
  | ConstI1 of bool
  (* UTF-8 string literal; lowering from [EString]. Codegen links to runtime. *)
  | ConstStr of string
  (* Placeholder result of [void] I/O; used when sequencing [println] etc. *)
  | ConstUnit
  (** Address of a module function with the given parameter/return Min_ir shape. *)
  | FnAddr of string * ty list * ty

type rhs =
  | Copy of operand
  | Binop of ibin * operand * operand
  | ICmp of icmp * operand * operand
  (* Boolean [i1] (non-short-circuit; both operands evaluated). *)
  | IAnd of operand * operand
  | IOr of operand * operand
  (* Direct call: either a [func_def] in the same [prog] or a runtime symbol
     (e.g. [int_to_str] — see {!runtime_string_symbols}). *)
  | Call of string * operand list
  (* Indirect call: callee operand has LLVM type [(param_tys -> ret_ty)*]. *)
  | IndirectCall of operand * ty list * ty * operand list

type instr =
  | Assign of string * rhs
  (* [Phi (dst, ty, [(lbl, op); ...])]: one incoming operand per predecessor
     block [lbl]. [dst] must dominate uses per usual SSA rules. *)
  | Phi of string * ty * (string * operand) list
  (* Side-effect only; no result. Names match LambdaScript builtins — see
     {!runtime_void_symbols}. *)
  | VoidCall of string * operand list
  | VoidIndirectCall of operand * ty list * operand list

type term =
  | Ret of operand option (* [None] = return void / unit *)
  | Br of string (* unconditional jump to label *)
  | BrCond of operand * string * string (* if i1 then label1 else label2 *)

type block = {
  label : string;
  (* Phis are kept as ordinary [instr]s but should appear first in [instrs]
     when lowering to LLVM. *)
  instrs : instr list;
  term : term;
}

type func_def = {
  name : string;
  params : (string * ty) list;
  ret : ty;
  entry : string;
  blocks : (string * block) list;
}

type prog = { funcs : func_def list; entry : string option }

(** I/O builtins: [VoidCall ("print", \[s\])], [VoidCall ("println", \[s\])]. *)
let runtime_void_symbols = [ "print"; "println" ]

(** String-producing runtime (not defined in [prog.funcs]): use
    [Assign (tmp, Call ("int_to_str", \[operand\]))]. *)
let runtime_string_symbols = [ "int_to_str" ]

(* ----- helpers for well-formedness checks later ----- *)

let block_map (f : func_def) : (string, block) Hashtbl.t =
  let t = Hashtbl.create (List.length f.blocks) in
  List.iter (fun (lbl, blk) -> Hashtbl.add t lbl blk) f.blocks;
  t

(** All labels defined in [f]. *)
let labels_of_func (f : func_def) : string list = List.map fst f.blocks

(* ----- pretty-print (debug / incremental testing) ----- *)

let rec string_of_ty = function
  | I32 -> "i32"
  | I1 -> "i1"
  | String -> "string"
  | Unit -> "unit"
  | Fun (ps, r) ->
      let ps_s = String.concat ", " (List.map string_of_ty ps) in
      Printf.sprintf "fn(%s) -> %s" ps_s (string_of_ty r)

let string_of_ibin = function
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Div -> "div"
  | Mod -> "mod"

let string_of_icmp = function
  | Eq -> "eq"
  | Ne -> "ne"
  | Ult -> "ult"
  | Slt -> "slt"
  | Sle -> "sle"
  | Sge -> "sge"

let escape_string s =
  let b = Buffer.create (String.length s + 8) in
  String.iter
    (fun c ->
      match c with
      | '\\' -> Buffer.add_string b "\\\\"
      | '"' -> Buffer.add_string b "\\\""
      | '\n' -> Buffer.add_string b "\\n"
      | '\t' -> Buffer.add_string b "\\t"
      | '\r' -> Buffer.add_string b "\\r"
      | c when Char.code c < 32 ->
          Printf.bprintf b "\\u{%02x}" (Char.code c)
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let string_of_operand = function
  | Local x -> "%" ^ x
  | ConstI32 n -> string_of_int n
  | ConstI1 b -> if b then "true" else "false"
  | ConstStr s -> Printf.sprintf "\"%s\"" (escape_string s)
  | ConstUnit -> "()"
  | FnAddr (n, ps, r) ->
      Printf.sprintf "&%s : %s" n (string_of_ty (Fun (ps, r)))

let string_of_rhs = function
  | Copy o -> string_of_operand o
  | Binop (b, a, c) ->
      Printf.sprintf "%s %s, %s" (string_of_ibin b) (string_of_operand a)
        (string_of_operand c)
  | ICmp (c, a, b) ->
      Printf.sprintf "icmp %s %s, %s" (string_of_icmp c) (string_of_operand a)
        (string_of_operand b)
  | IAnd (a, b) ->
      Printf.sprintf "and %s, %s" (string_of_operand a) (string_of_operand b)
  | IOr (a, b) ->
      Printf.sprintf "or %s, %s" (string_of_operand a) (string_of_operand b)
  | Call (f, args) ->
      let args_s = String.concat ", " (List.map string_of_operand args) in
      Printf.sprintf "call @%s(%s)" f args_s
  | IndirectCall (c, _pts, rt, args) ->
      let args_s = String.concat ", " (List.map string_of_operand args) in
      Printf.sprintf "indirect_call %s(%s) -> %s"
        (string_of_operand c) args_s (string_of_ty rt)

let string_of_instr = function
  | Assign (dst, rhs) ->
      Printf.sprintf "  %s = %s" dst (string_of_rhs rhs)
  | VoidCall (f, args) ->
      let args_s = String.concat ", " (List.map string_of_operand args) in
      Printf.sprintf "  void call @%s(%s)" f args_s
  | VoidIndirectCall (c, _pts, args) ->
      let args_s = String.concat ", " (List.map string_of_operand args) in
      Printf.sprintf "  void indirect %s(%s)"
        (string_of_operand c) args_s
  | Phi (dst, t, incomings) ->
      let parts =
        String.concat ", "
          (List.map
             (fun (lbl, op) ->
               Printf.sprintf "[ %s, %s ]" lbl (string_of_operand op))
             incomings)
      in
      Printf.sprintf "  %s = phi %s %s" dst (string_of_ty t) parts

let string_of_term = function
  | Ret None -> "  ret void"
  | Ret (Some o) -> Printf.sprintf "  ret %s" (string_of_operand o)
  | Br lbl -> Printf.sprintf "  br %s" lbl
  | BrCond (cond, t, f) ->
      Printf.sprintf "  br %s ? %s : %s" (string_of_operand cond) t f

let string_of_block (b : block) : string =
  let body = List.map string_of_instr b.instrs |> String.concat "\n" in
  Printf.sprintf "%s:\n%s\n%s" b.label body (string_of_term b.term)

let string_of_func (f : func_def) : string =
  let params_s =
    String.concat ", "
      (List.map (fun (n, t) -> Printf.sprintf "%s: %s" n (string_of_ty t)) f.params)
  in
  let blocks_s =
    List.map (fun (_, blk) -> string_of_block blk) f.blocks |> String.concat "\n"
  in
  Printf.sprintf "fn %s(%s) -> %s {\n%s\n}\n" f.name params_s (string_of_ty f.ret)
    blocks_s

let string_of_prog (p : prog) : string =
  let funcs_s = List.map string_of_func p.funcs |> String.concat "\n" in
  let entry_s =
    match p.entry with
    | None -> ""
    | Some n -> Printf.sprintf "entry: @%s\n" n
  in
  entry_s ^ funcs_s
