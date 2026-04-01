(** Minimal compiler IR for the first LLVM backend.

    Intended pipeline (later): [Cexpr] or surface AST -> [Min_ir] -> LLVM IR.

    Scope of this IR (intentionally tiny):
    - Types [i32], [i1], [string], [unit], homogeneous [list] (opaque [i8*] in
      LLVM: cons nodes via [ls_malloc], see list struct layout in codegen).
    - [string] is an opaque value (LLVM will use something like [i8*] + runtime).
    - Local bindings via [Assign]; every intermediate has a name (LLVM-friendly)
    - Side-effecting I/O via [VoidCall] (see {!runtime_void_symbols})
    - String conversion matching LambdaScript builtins: [int_to_str] as [Call]
    - Control flow: labeled basic blocks; [Br], [BrCond]; [Ret]
    - [Phi] only at block heads (LLVM convention) for SSA merge points

    Closures use runtime [ls_mkclos]/[ls_malloc]; arity [>=2] function values use
    [Clos]; env payloads use [EnvLoad]/[EnvStore]. Not represented:
    GC/free, tags beyond this, varargs, exceptions. *)

type ty =
  | I32
  | I1
  | String
  | Unit
  (** Heterogeneous tuple; lowered to an LLVM struct type. *)
  | Tuple of ty list
  (** Monomorphic function pointer ([param types], return). LLVM: e.g. [i32
      (i32)*]. *)
  | Fun of ty list * ty
  (** Opaque environment / raw pointer (LLVM [i8*]). *)
  | RawPtr
  (** First-class curried function: [args] remaining left-to-right, then [ret]. *)
  | Clos of ty list * ty
  (** Homogeneous linked list ([nil] is null; cons cells are heap-allocated). *)
  | List of ty

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
  (** [i8*] null — environment pointer for zero-capture currying roots. *)
  | RawNull

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
  (* [env_ptr] points to packed fields [layout] (see {!EnvLoad}/{!EnvStore}). *)
  | MkClos of { code : string; env_ptr : operand; clo_ty : ty }
  (* Apply curried closure [clos] (type [Clos (a :: rest, ret)]) to [arg]. *)
  | ClosApply of { clo : operand; arg : operand; result_ty : ty }
  (* Packed environment blob — index [i] in struct [layout]. *)
  | EnvLoad of { env : operand; layout : ty list; index : int }
  | EnvStore of { env : operand; layout : ty list; index : int; value : operand }
  (* [byte_size] must match [sizeof layout] for the LLVM field layout. *)
  | RawMalloc of int
  (* Tuple value; [elem_tys] matches operand element types in order. *)
  | TuplePack of ty list * operand list
  | TupleProj of { tup : operand; index : int; elem_tys : ty list }
  (** Empty list of element type [elem_ty] (null [i8*] at codegen). *)
  | ListNil of ty
  (** Allocate a cons cell [{ head; tail }] with [tail] already a list value. *)
  | ListCons of { elem_ty : ty; head : operand; tail : operand }
  (** [unsafe]: non-[nil] list only. *)
  | ListHead of { elem_ty : ty; lst : operand }
  | ListTail of { elem_ty : ty; lst : operand }
  (** Heap-allocate a copy of a value of type [ty] (for variant payloads). *)
  | HeapBox of ty * operand
  (** Load a value of type [ty] from a heap box ([i8*]). *)
  | HeapUnbox of ty * operand
  (** Sum type: [ls_variant_mk(tag, payload_ptr)] — [payload_ptr] may be null. *)
  | VariantMk of int * operand
  (** Discriminant [i32] from a variant ([i8*]). *)
  | VariantTag of operand
  (** Untyped payload pointer from a variant ([i8*] → [i8*]). *)
  | VariantPayload of operand

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
  | Unreachable

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
    [Assign (tmp, Call ("int_to_str", \[operand\]))] or
    [Assign (tmp, Call ("str_concat", \[s1; s2\]))]. *)
let runtime_string_symbols = [ "int_to_str"; "str_concat"; "strcmp" ]

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
  | Tuple ts ->
      "(" ^ String.concat ", " (List.map string_of_ty ts) ^ ")"
  | RawPtr -> "rawptr"
  | Clos (ps, r) ->
      let ps_s = String.concat ", " (List.map string_of_ty ps) in
      Printf.sprintf "clos(%s) -> %s" ps_s (string_of_ty r)
  | Fun (ps, r) ->
      let ps_s = String.concat ", " (List.map string_of_ty ps) in
      Printf.sprintf "fn(%s) -> %s" ps_s (string_of_ty r)
  | List e -> Printf.sprintf "list %s" (string_of_ty e)

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
  | RawNull -> "nullptr"

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
  | MkClos { code; env_ptr; clo_ty } ->
      Printf.sprintf "mkclos @%s env=%s : %s" code
        (string_of_operand env_ptr) (string_of_ty clo_ty)
  | ClosApply { clo; arg; result_ty } ->
      Printf.sprintf "clos_apply %s (%s) -> %s"
        (string_of_operand clo) (string_of_operand arg)
        (string_of_ty result_ty)
  | EnvLoad { env; layout = _; index } ->
      Printf.sprintf "env_load %s[%d]" (string_of_operand env) index
  | EnvStore { env; layout = _; index; value } ->
      Printf.sprintf "env_store %s[%d] = %s" (string_of_operand env) index
        (string_of_operand value)
  | RawMalloc n -> Printf.sprintf "raw_malloc(%d)" n
  | TuplePack (ts, ops) ->
      let ts_s = String.concat ", " (List.map string_of_ty ts) in
      let ops_s = String.concat ", " (List.map string_of_operand ops) in
      Printf.sprintf "tuple_pack [%s] (%s)" ts_s ops_s
  | TupleProj { tup; index; elem_tys } ->
      Printf.sprintf "tuple_proj %s[%d] : %s" (string_of_operand tup) index
        (String.concat "," (List.map string_of_ty elem_tys))
  | ListNil e -> Printf.sprintf "list_nil %s" (string_of_ty e)
  | ListCons { elem_ty; head; tail } ->
      Printf.sprintf "list_cons(%s head=%s tail=%s)"
        (string_of_ty elem_ty) (string_of_operand head) (string_of_operand tail)
  | ListHead { elem_ty; lst } ->
      Printf.sprintf "list_head %s %s" (string_of_ty elem_ty)
        (string_of_operand lst)
  | ListTail { elem_ty; lst } ->
      Printf.sprintf "list_tail %s %s" (string_of_ty elem_ty)
        (string_of_operand lst)
  | HeapBox (t, o) ->
      Printf.sprintf "heap_box %s %s" (string_of_ty t) (string_of_operand o)
  | HeapUnbox (t, o) ->
      Printf.sprintf "heap_unbox %s %s" (string_of_ty t) (string_of_operand o)
  | VariantMk (tag, p) ->
      Printf.sprintf "variant_mk %d %s" tag (string_of_operand p)
  | VariantTag o -> Printf.sprintf "variant_tag %s" (string_of_operand o)
  | VariantPayload o ->
      Printf.sprintf "variant_payload %s" (string_of_operand o)

let string_of_instr = function
  | Assign (dst, rhs) -> Printf.sprintf "  %s = %s" dst (string_of_rhs rhs)
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
  | Unreachable -> "  unreachable"

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
