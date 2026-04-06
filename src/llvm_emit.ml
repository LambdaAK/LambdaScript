(** Emit LLVM IR text from {!Min_ir.prog}. Link with [runtime/ls_runtime.c] using
    [clang]. Runtime: [ls_print], [ls_println], [ls_int_to_str], [ls_str_concat]. *)

open Min_ir

type emit_ctx = {
  mutable prelude : string list;
  mutable str_counter : int;
  mutable env_layout_id : int;
  mutable tuple_id : int;
  mutable tuple_registry : (ty list * string) list;
  mutable list_id : int;
  mutable list_registry : (ty * string) list;
  (** Zeroinit sentinel cons cells (one per list cell struct name). Used so
      [ListHead]/[ListTail] never GEP/load through a null [i8*] when native
      pattern tests emit those ops speculatively before a null guard reaches
      the branch. *)
  mutable list_sentinels : (string * string) list;
  (** Globals used as safe fallbacks when [HeapUnbox] sees a null pointer from
      speculative [VariantPayload] in native pattern matching. *)
  mutable heap_unbox_dummies : (ty * string) list;
  (** Fresh SSA names for temps introduced while emitting call operands. *)
  mutable emit_aux_id : int;
}

let ctx_create () =
  {
    prelude = [];
    str_counter = 0;
    env_layout_id = 0;
    tuple_id = 0;
    tuple_registry = [];
    list_id = 0;
    list_registry = [];
    list_sentinels = [];
    heap_unbox_dummies = [];
    emit_aux_id = 0;
  }

let ensure_list_sentinel (ctx : emit_ctx) (struct_n : string) : string =
  match List.assoc_opt struct_n ctx.list_sentinels with
  | Some g -> g
  | None ->
      let g =
        "ls_lsent_"
        ^ String.map (function '.' -> '_' | c -> c) struct_n
      in
      let line =
        Printf.sprintf "@%s = private unnamed_addr constant %%%s zeroinitializer"
          g struct_n
      in
      ctx.prelude <- line :: ctx.prelude;
      ctx.list_sentinels <- (struct_n, g) :: ctx.list_sentinels;
      g

let fresh_emit_aux (ctx : emit_ctx) : string =
  ctx.emit_aux_id <- ctx.emit_aux_id + 1;
  "ls_aux" ^ string_of_int ctx.emit_aux_id

let rec ty_list_equal_ll (a : ty list) (b : ty list) : bool =
  match (a, b) with
  | [], [] -> true
  | ha :: ta, hb :: tb -> ty_equal_ll ha hb && ty_list_equal_ll ta tb
  | _ -> false

and ty_equal_ll (a : ty) (b : ty) : bool =
  match (a, b) with
  | I32, I32 | I1, I1 | F64, F64 | String, String | Unit, Unit | RawPtr, RawPtr
    -> true
  | Tuple ts1, Tuple ts2 -> ty_list_equal_ll ts1 ts2
  | List e1, List e2 -> ty_equal_ll e1 e2
  | Fun (p1, r1), Fun (p2, r2) ->
      ty_list_equal_ll p1 p2 && ty_equal_ll r1 r2
  | Clos (p1, r1), Clos (p2, r2) ->
      ty_list_equal_ll p1 p2 && ty_equal_ll r1 r2
  | _ -> false

let lookup_tuple_layout (ctx : emit_ctx) (ts : ty list) : string option =
  List.find_map
    (fun (k, n) -> if ty_list_equal_ll k ts then Some n else None)
    ctx.tuple_registry

let ls_clos_ty = "%ls.clos"

let ls_clos_ptr = ls_clos_ty ^ "*"

let rec llvm_fun_ptr_ty_ctx (ctx : emit_ctx) (params : ty list) (ret : ty) :
    string =
  let pl =
    List.map
      (fun t ->
        if t = Unit then "i8" else llvm_ll_ty_ctx ctx t)
      params
    |> String.concat ", "
  in
  let rl =
    match ret with
    | Unit -> "void"
    | t -> llvm_ll_ty_ctx ctx t
  in
  Printf.sprintf "%s (%s)*" rl pl

and llvm_ll_ty_ctx (ctx : emit_ctx) (t : ty) : string =
  match t with
  | I32 -> "i32"
  | I1 -> "i1"
  | F64 -> "double"
  | String -> "i8*"
  | Unit -> "void"
  | RawPtr -> "i8*"
  | Clos _ -> "i8*"
  | List _ -> "i8*"
  | Tuple ts -> "%" ^ register_tuple_layout ctx ts
  | Fun (ps, r) -> llvm_fun_ptr_ty_ctx ctx ps r

and register_tuple_layout (ctx : emit_ctx) (ts : ty list) : string =
  match lookup_tuple_layout ctx ts with
  | Some n -> n
  | None ->
      List.iter
        (function
          | Tuple inner -> ignore (register_tuple_layout ctx inner)
          | List e -> ignore (register_list_cell_layout ctx e)
          | _ -> ())
        ts;
      ctx.tuple_id <- ctx.tuple_id + 1;
      let name = Printf.sprintf "ls.tuple.%d" ctx.tuple_id in
      let fields =
        List.map (llvm_struct_elem_ty ctx) ts |> String.concat ", "
      in
      let line = Printf.sprintf "%%%s = type { %s }" name fields in
      ctx.prelude <- line :: ctx.prelude;
      ctx.tuple_registry <- (ts, name) :: ctx.tuple_registry;
      name

and register_list_cell_layout (ctx : emit_ctx) (elem_ty : ty) : string =
  match
    List.find_map
      (fun (k, n) -> if ty_equal_ll k elem_ty then Some n else None)
      ctx.list_registry
  with
  | Some n -> n
  | None ->
      (* Ensure nested tuple / list LLVM types exist before defining the cell. *)
      (match elem_ty with
      | Tuple ts -> ignore (register_tuple_layout ctx ts)
      | List e -> ignore (register_list_cell_layout ctx e)
      | _ -> ());
      ctx.list_id <- ctx.list_id + 1;
      let name = Printf.sprintf "ls.lcell.%d" ctx.list_id in
      let head_ll = llvm_struct_elem_ty ctx elem_ty in
      let line = Printf.sprintf "%%%s = type { %s, i8* }" name head_ll in
      ctx.prelude <- line :: ctx.prelude;
      ctx.list_registry <- (elem_ty, name) :: ctx.list_registry;
      name

and llvm_struct_elem_ty (ctx : emit_ctx) (t : ty) : string =
  match t with
  | I32 -> "i32"
  | I1 -> "i1"
  | F64 -> "double"
  | String | RawPtr | Clos _ | List _ -> "i8*"
  | Unit -> "i8"
  | Fun (ps, r) -> llvm_fun_ptr_ty_ctx ctx ps r
  | Tuple ts ->
      let n = register_tuple_layout ctx ts in
      Printf.sprintf "%%%s" n

let heap_unbox_dummy_i8_expr (ctx : emit_ctx) (elem_ty : ty) (g : string) :
    string =
  match elem_ty with
  | I1 -> Printf.sprintf "bitcast (i32* @%s to i8*)" g
  | Tuple ts ->
      let struct_n = register_tuple_layout ctx ts in
      Printf.sprintf "bitcast (%%%s* @%s to i8*)" struct_n g
  | _ ->
      let ll = llvm_ll_ty_ctx ctx elem_ty in
      Printf.sprintf "bitcast (%s* @%s to i8*)" ll g

let heap_unbox_dummy_global (ctx : emit_ctx) (elem_ty : ty) : string * string =
  (* Returns [(global_name, i8* source expression bitcasting that global)]. *)
  match
    List.find_opt (fun (k, _) -> ty_equal_ll k elem_ty) ctx.heap_unbox_dummies
  with
  | Some (_, g) -> (g, heap_unbox_dummy_i8_expr ctx elem_ty g)
  | None ->
      let g = "ls_ubox_" ^ string_of_int (List.length ctx.heap_unbox_dummies) in
      let line =
        match elem_ty with
        | I1 ->
            Printf.sprintf "@%s = private unnamed_addr global i32 0" g
        | Tuple ts ->
            let struct_n = register_tuple_layout ctx ts in
            Printf.sprintf "@%s = private unnamed_addr global %%%s zeroinitializer"
              g struct_n
        | _ ->
            let ll = llvm_ll_ty_ctx ctx elem_ty in
            Printf.sprintf "@%s = private unnamed_addr global %s zeroinitializer"
              g ll
      in
      ctx.prelude <- line :: ctx.prelude;
      ctx.heap_unbox_dummies <- (elem_ty, g) :: ctx.heap_unbox_dummies;
      (g, heap_unbox_dummy_i8_expr ctx elem_ty g)

(** [unit] is not an LLVM value type; use [i8] as the ABI carrier for [unit]
    parameters, call arguments, and SSA locals that hold [unit]. *)
let llvm_value_ty_ctx (ctx : emit_ctx) (t : ty) : string =
  match t with
  | Unit -> "i8"
  | Fun (ps, r) -> llvm_fun_ptr_ty_ctx ctx ps r
  | RawPtr -> "i8*"
  | Clos _ -> "i8*"
  | t -> llvm_ll_ty_ctx ctx t

let llvm_env_field_ll_ty (ctx : emit_ctx) (t : ty) : string =
  match t with
  | I1 -> "i32"
  | I32 -> "i32"
  | F64 -> "double"
  | String | RawPtr | Clos _ | List _ -> "i8*"
  | Unit -> "i8"
  | Fun (ps, r) -> llvm_fun_ptr_ty_ctx ctx ps r
  | Tuple _ as tup -> llvm_struct_elem_ty ctx tup

let register_env_layout (ctx : emit_ctx) (layout : ty list) : string =
  ctx.env_layout_id <- ctx.env_layout_id + 1;
  let name = Printf.sprintf "ls.env.%d" ctx.env_layout_id in
  let fields =
    List.map (llvm_env_field_ll_ty ctx) layout |> String.concat ", "
  in
  let line = Printf.sprintf "%%%s = type { %s }" name fields in
  ctx.prelude <- line :: ctx.prelude;
  name

let rec layout_byte_size (xs : ty list) : int =
  let align_up x a =
    if x mod a = 0 then x else x + (a - (x mod a))
  in
  let sz_al = function
    | I32 -> (4, 4)
    | I1 -> (4, 4)
    | F64 -> (8, 8)
    | String | RawPtr | Clos _ | List _ -> (8, 8)
    | Unit -> (1, 8)
    | Fun _ -> (8, 8)
    | Tuple ts ->
        let n = layout_byte_size ts in
        (max n 1, 8)
  in
  let acc = ref 0 in
  List.iter
    (fun t ->
      let sz, al = sz_al t in
      acc := align_up !acc al + sz)
    xs;
  max !acc 1

let list_cell_byte_size (elem_ty : ty) : int = layout_byte_size [ elem_ty; RawPtr ]

let step_fn_ptr_ty (ctx : emit_ctx) (fd : func_def) : string =
  let pl =
    List.map (fun (_, t) -> llvm_value_ty_ctx ctx t) fd.params
    |> String.concat ", "
  in
  let rl =
    match fd.ret with
    | Unit -> "void"
    | t -> llvm_ll_ty_ctx ctx t
  in
  Printf.sprintf "%s (%s)*" rl pl

let ibin_ll : ibin -> string = function
  | Add -> "add nsw"
  | Sub -> "sub nsw"
  | Mul -> "mul nsw"
  | Div -> "sdiv"
  | Mod -> "srem"

let icmp_ll : icmp -> string = function
  | Eq -> "eq"
  | Ne -> "ne"
  | Ult -> "ult"
  | Slt -> "slt"
  | Sle -> "sle"
  | Sge -> "sge"

let callee_ll : string -> string = function
  | "print" -> "ls_print"
  | "print_string" -> "ls_println"
  | "abort" -> "ls_abort"
  | "int_to_str" -> "ls_int_to_str"
  | "str_concat" -> "ls_str_concat"
  | "strcmp" -> "strcmp"
  | n -> n

let llvm_c_escape s =
  let b = Buffer.create (String.length s * 2) in
  String.iter
    (fun c ->
      match c with
      | '\"' -> Buffer.add_string b "\\22"
      | '\\' -> Buffer.add_string b "\\5C"
      | '\n' -> Buffer.add_string b "\\0A"
      | '\t' -> Buffer.add_string b "\\09"
      | '\r' -> Buffer.add_string b "\\0D"
      | c when Char.code c < 32 || Char.code c > 126 ->
          Printf.bprintf b "\\%02X" (Char.code c)
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let term_succs (t : term) : string list =
  match t with
  | Ret _ | Unreachable -> []
  | Br l -> [ l ]
  | BrCond (_, l1, l2) -> [ l1; l2 ]

let ordered_blocks (entry : string) (blocks : (string * block) list) : block list
    =
  let tbl : (string, block) Hashtbl.t = Hashtbl.create 16 in
  List.iter (fun (l, b) -> Hashtbl.add tbl l b) blocks;
  let visited : (string, unit) Hashtbl.t = Hashtbl.create 16 in
  let out = ref [] in
  let rec visit lbl =
    if Hashtbl.mem visited lbl then ()
    else (
      Hashtbl.replace visited lbl ();
      let blk = Hashtbl.find tbl lbl in
      out := !out @ [ blk ];
      List.iter visit (term_succs blk.term))
  in
  visit entry;
  List.iter
    (fun (l, blk) ->
      if not (Hashtbl.mem visited l) then (
        Hashtbl.replace visited l ();
        out := !out @ [ blk ]))
    blocks;
  !out

let runtime_declarations : string =
  "declare void @ls_print(i8*)\n\
   declare void @ls_println(i8*)\n\
   declare void @ls_abort()\n\
   declare i8* @ls_int_to_str(i32)\n\
   declare i8* @ls_str_concat(i8*, i8*)\n\
   declare i32 @strcmp(i8*, i8*)\n\
   declare i32 @ls_float_eq(double, double)\n\
   declare i8* @ls_ordering_int32(i32, i32)\n\
   declare i8* @ls_ordering_str(i8*, i8*)\n\
   declare i8* @ls_ordering_bool(i8, i8)\n\
   declare i8* @ls_ordering_float(double, double)\n\
   declare i8* @ls_ordering_char(i32, i32)\n\
   declare i8* @ls_malloc(i64)\n\
   declare i8* @ls_mkclos(i8*, i8*)\n\
   declare i8* @ls_variant_mk(i32, i8*)\n\
   declare i32 @ls_variant_tag(i8*)\n\
   declare i8* @ls_variant_payload(i8*)\n"

let module_banner =
  "; Generated by Forge compiler (Min_ir → LLVM IR)\n\
   source_filename = \"forge\"\n\n\
   %ls.clos = type { i8*, i8* }\n\n"

module H = Hashtbl

let emit_global_string ctx (dst : string) (s : string) : string =
  let idx = ctx.str_counter in
  ctx.str_counter <- idx + 1;
  let lit = llvm_c_escape s in
  let n = String.length s + 1 in
  let gname = Printf.sprintf ".str.%d" idx in
  let line =
    Printf.sprintf "@%s = private unnamed_addr constant [%d x i8] c\"%s\\00\""
      gname n lit
  in
  ctx.prelude <- line :: ctx.prelude;
  (* No parentheses around [N x i8] — required for Apple clang LLVM IR parser. *)
  Printf.sprintf
    "  %%%s = getelementptr inbounds [%d x i8], [%d x i8]* @%s, i64 0, i64 0"
    dst n n gname

let operand_min_ty (h : (string, ty) H.t) : operand -> ty = function
  | ConstI32 _ -> I32
  | ConstI1 _ -> I1
  | ConstF64 _ -> F64
  | ConstStr _ -> String
  | ConstUnit -> Unit
  | FnAddr (_, ps, r) -> Fun (ps, r)
  | RawNull -> RawPtr
  | Local x -> (
      match H.find_opt h x with
      | Some t -> t
      | None -> failwith ("llvm_emit: unknown local " ^ x))

let emit_operand (ctx : emit_ctx) (h : (string, ty) H.t) (op : operand) :
    string * string =
  match op with
  | ConstI32 n -> ("i32", string_of_int n)
  | ConstI1 b -> ("i1", if b then "true" else "false")
  | ConstF64 f -> ("double", Printf.sprintf "double %.17g" f)
  | ConstStr _ -> failwith "llvm_emit: ConstStr must use Copy/global"
  | ConstUnit -> ("i8", "0")
  | FnAddr (name, ps, r) ->
      let fpty = llvm_fun_ptr_ty_ctx ctx ps r in
      (fpty, "@" ^ name)
  | RawNull -> ("i8*", "null")
  | Local x -> (llvm_value_ty_ctx ctx (H.find h x), "%" ^ x)

let map_call_args (ctx : emit_ctx) (h : (string, ty) H.t) (lines : string list ref)
    (fd : func_def) (args : Min_ir.operand list) : string list =
  if List.length args <> List.length fd.params then
    failwith "llvm_emit: call arity mismatch";
  List.map2
    (fun (_, pty) op ->
      match (pty, op) with
      | Unit, ConstUnit -> "i8 0"
      | Unit, Local x ->
          if H.find h x <> Unit then
            failwith "llvm_emit: unit call argument local must have unit type";
          Printf.sprintf "i8 %%%s" x
      | String, ConstStr s ->
          let tmp = fresh_emit_aux ctx in
          lines := !lines @ [ emit_global_string ctx tmp s ];
          H.replace h tmp String;
          Printf.sprintf "i8* %%%s" tmp
      | _ ->
          let got_ll, v = emit_operand ctx h op in
          let exp_ll = llvm_value_ty_ctx ctx pty in
          if got_ll <> exp_ll then
            failwith
              (Printf.sprintf "llvm_emit: call arg expected %s, got %s" exp_ll
                 got_ll);
          Printf.sprintf "%s %s" exp_ll v)
    fd.params args

let emit_tuple_ssa_copy (ctx : emit_ctx) (h : (string, ty) H.t)
    (lines : string list ref) (dst : string) (src_op : operand)
    (elem_tys : ty list) : unit =
  let tty = Tuple elem_tys in
  let struct_ll = llvm_ll_ty_ctx ctx tty in
  let got_ll, src_v = emit_operand ctx h src_op in
  if got_ll <> struct_ll then
    failwith "llvm_emit: tuple copy type mismatch";
  let last = List.length elem_tys - 1 in
  let acc = ref "" in
  List.iteri
    (fun i ty ->
      let ev = dst ^ "_ev" ^ string_of_int i in
      let elt_ll = llvm_struct_elem_ty ctx ty in
      lines :=
        !lines
        @ [
            Printf.sprintf "  %%%s = extractvalue %s %s, %d" ev struct_ll src_v i;
          ];
      let nm = if i = last then dst else dst ^ "_agg" ^ string_of_int i in
      let agg_in =
        if i = 0 then "undef" else Printf.sprintf "%%%s" !acc
      in
      lines :=
        !lines
        @ [
            Printf.sprintf
              "  %%%s = insertvalue %s %s, %s %%%s, %d" nm struct_ll agg_in
              elt_ll ev i;
          ];
      acc := nm)
    elem_tys;
  H.replace h dst tty

let emit_copy_dst (ctx : emit_ctx) (h : (string, ty) H.t)
    (lines : string list ref) (dst : string) (o : operand) : unit =
  match o with
  | ConstStr s ->
      lines := !lines @ [ emit_global_string ctx dst s ];
      H.replace h dst String
  | ConstI32 _ | ConstI1 _ ->
      let t, v = emit_operand ctx h o in
      let ins =
        if t = "i32" then Printf.sprintf "  %%%s = add nsw i32 %s, 0" dst v
        else Printf.sprintf "  %%%s = xor i1 %s, false" dst v
      in
      lines := !lines @ [ ins ];
      H.replace h dst (operand_min_ty h o)
  | ConstF64 _ ->
      let _, v = emit_operand ctx h o in
      let ins = Printf.sprintf "  %%%s = fadd double %s, 0.0" dst v in
      lines := !lines @ [ ins ];
      H.replace h dst F64
  | ConstUnit ->
      lines := !lines @ [ Printf.sprintf "  %%%s = add i8 0, 0" dst ];
      H.replace h dst Unit
  | RawNull ->
      lines := !lines @ [ Printf.sprintf "  %%%s = bitcast i8* null to i8*" dst ];
      H.replace h dst RawPtr
  | FnAddr (name, ps, rt) ->
      let fpty = llvm_fun_ptr_ty_ctx ctx ps rt in
      lines :=
        !lines
        @ [
            Printf.sprintf "  %%%s = bitcast %s @%s to %s" dst fpty name fpty;
          ];
      H.replace h dst (Fun (ps, rt))
  | Local _ -> (
      let ty = operand_min_ty h o in
      match ty with
      | Tuple ts -> emit_tuple_ssa_copy ctx h lines dst o ts
      | _ ->
          let t, v = emit_operand ctx h o in
          let ins =
            if ty = Unit then Printf.sprintf "  %%%s = add i8 %s, 0" dst v
            else if (match ty with Fun _ -> true | _ -> false) then
              Printf.sprintf "  %%%s = bitcast %s %s to %s" dst t v t
            else if t = "i32" then Printf.sprintf "  %%%s = add nsw i32 %s, 0" dst v
            else if t = "i1" then Printf.sprintf "  %%%s = xor i1 %s, false" dst v
            else if t = "double" then
              Printf.sprintf "  %%%s = fadd double %s, 0.0" dst v
            else if t = "i8*" then
              Printf.sprintf "  %%%s = bitcast i8* %s to i8*" dst v
            else if (match ty with Clos _ | RawPtr -> true | _ -> false) then
              Printf.sprintf "  %%%s = bitcast i8* %s to i8*" dst v
            else failwith "llvm_emit: copy"
          in
          lines := !lines @ [ ins ];
          H.replace h dst ty)

let phi_incoming_val (h : (string, ty) H.t) (exp_ty : ty) (op : operand) :
    string =
  let ty_err () =
    failwith
      ("llvm_emit: phi arm type mismatch (expected "
      ^ Min_ir.string_of_ty exp_ty ^ ")")
  in
  let check_exp_const got_const =
    if not (ty_equal_ll got_const exp_ty) then ty_err ()
  in
  match op with
  | ConstI32 n ->
      check_exp_const I32;
      string_of_int n
  | ConstI1 b ->
      check_exp_const I1;
      if b then "true" else "false"
  | ConstF64 f ->
      check_exp_const F64;
      Printf.sprintf "double %.17g" f
  | Local x -> (
      (* Merge blocks may be emitted before all predecessors in block order;
         the local may not be in [h] yet. LLVM phis only reference values from
         the named predecessor block, so this is valid IR. *)
      match H.find_opt h x with
      | Some t when not (ty_equal_ll t exp_ty) -> ty_err ()
      | Some _ | None -> ());
      "%" ^ x
  | ConstStr _ ->
      failwith "llvm_emit: phi cannot use string literal; materialize to a local"
  | ConstUnit -> failwith "llvm_emit: phi cannot use unit"
  | FnAddr _ ->
      failwith "llvm_emit: phi cannot use function address; materialize to a local"
  | RawNull ->
      (match exp_ty with
      | RawPtr | Clos _ | List _ -> ()
      | _ -> failwith "llvm_emit: phi null only for rawptr, closure, or list type");
      "i8* null"

let emit_instr ctx (fn_sigs : (string, func_def) H.t)
    (h : (string, ty) H.t) (lines : string list ref) (instr : instr) : unit =
  match instr with
  | Phi (dst, t, incomings) ->
      let ll_t = llvm_ll_ty_ctx ctx t in
      let parts =
        String.concat ", "
          (List.map
             (fun (lbl, op) ->
               let v = phi_incoming_val h t op in
               Printf.sprintf "[ %s, %%%s ]" v lbl)
             incomings)
      in
      lines := !lines @ [ Printf.sprintf "  %%%s = phi %s %s" dst ll_t parts ];
      H.replace h dst t
  | VoidCall (name, args) -> (
      match name with
      | "print" | "print_string" -> (
          match args with
          | [ arg ] ->
              let at, av =
                match arg with
                | ConstStr s ->
                    let tmp = fresh_emit_aux ctx in
                    lines := !lines @ [ emit_global_string ctx tmp s ];
                    H.replace h tmp String;
                    ("i8*", "%" ^ tmp)
                | _ -> emit_operand ctx h arg
              in
              if at <> "i8*" then failwith "llvm_emit: print/println expect i8*";
              let c = callee_ll name in
              lines :=
                !lines @ [ Printf.sprintf "  call void @%s(%s %s)" c at av ]
          | _ -> failwith "llvm_emit: print/println expect one argument")
      | "abort" -> (
          match args with
          | [] ->
              let c = callee_ll "abort" in
              lines := !lines @ [ Printf.sprintf "  call void @%s()" c ]
          | _ -> failwith "llvm_emit: abort takes no arguments")
      | _ ->
          let fd = H.find fn_sigs name in
          if fd.ret <> Unit then
            failwith "llvm_emit: value-returning call must use Assign, not void";
          let parts = map_call_args ctx h lines fd args in
          lines :=
            !lines
            @ [ Printf.sprintf "  call void @%s(%s)" name (String.concat ", " parts) ]
      )
  | VoidIndirectCall (callee_op, ptys, args) ->
      let callee_ty = llvm_fun_ptr_ty_ctx ctx ptys Unit in
      let ct, cv = emit_operand ctx h callee_op in
      if ct <> callee_ty then
        failwith "llvm_emit: void indirect callee type mismatch";
      let parts =
        List.map2
          (fun pty op ->
            match (pty, op) with
            | Unit, ConstUnit -> "i8 0"
            | Unit, Local x ->
                if H.find h x <> Unit then
                  failwith "llvm_emit: unit indirect arg must be unit local";
                Printf.sprintf "i8 %%%s" x
            | String, ConstStr s ->
                let tmp = fresh_emit_aux ctx in
                lines := !lines @ [ emit_global_string ctx tmp s ];
                H.replace h tmp String;
                Printf.sprintf "i8* %%%s" tmp
            | _ ->
                let got_ll, v = emit_operand ctx h op in
                let exp_ll =
                    if pty = Unit then "i8" else llvm_ll_ty_ctx ctx pty
                  in
                if got_ll <> exp_ll then
                  failwith "llvm_emit: void indirect call arg type mismatch";
                Printf.sprintf "%s %s" exp_ll v)
          ptys args
      in
      lines :=
        !lines
        @ [ Printf.sprintf "  call void %s(%s)" cv (String.concat ", " parts) ]
  | Assign (dst, rhs) -> (
      match rhs with
      | Copy o -> emit_copy_dst ctx h lines dst o
      | Binop (b, o1, o2) ->
          let t1, v1 = emit_operand ctx h o1 in
          let t2, v2 = emit_operand ctx h o2 in
          if t1 <> "i32" || t2 <> "i32" then
            failwith "llvm_emit: binop expects i32";
          lines :=
            !lines
            @ [ Printf.sprintf "  %%%s = %s i32 %s, %s" dst (ibin_ll b) v1 v2 ];
          H.replace h dst I32
      | ICmp (c, o1, o2) ->
          let t1, v1 = emit_operand ctx h o1 in
          let t2, v2 = emit_operand ctx h o2 in
          if t1 = "i8*" && t2 = "i8*" then (
            if c <> Eq && c <> Ne then
              failwith "llvm_emit: icmp on pointers supports eq/ne only";
            lines :=
              !lines
              @ [
                  Printf.sprintf "  %%%s = icmp %s i8* %s, %s" dst (icmp_ll c) v1
                    v2;
                ];
            H.replace h dst I1)
          else
            let ty_s =
              if t1 = t2 && (t1 = "i32" || t1 = "i1" || t1 = "i8") then t1
              else failwith "llvm_emit: icmp expects matching i32, i1, i8, or i8* operands"
            in
            lines :=
              !lines
              @ [
                  Printf.sprintf "  %%%s = icmp %s %s %s, %s" dst (icmp_ll c) ty_s
                    v1 v2;
                ];
            H.replace h dst I1
      | IAnd (o1, o2) | IOr (o1, o2) as rhs_logic ->
          let t1, v1 = emit_operand ctx h o1 in
          let t2, v2 = emit_operand ctx h o2 in
          if t1 <> "i1" || t2 <> "i1" then failwith "llvm_emit: and/or expect i1";
          let op = match rhs_logic with IAnd _ -> "and" | _ -> "or" in
          lines :=
            !lines @ [ Printf.sprintf "  %%%s = %s i1 %s, %s" dst op v1 v2 ];
          H.replace h dst I1
      | Call (name, args) -> (
          match name with
          | "int_to_str" -> (
              match args with
              | [ a ] ->
                  let ta, va = emit_operand ctx h a in
                  if ta <> "i32" then failwith "llvm_emit: int_to_str expects i32";
                  let c = callee_ll "int_to_str" in
                  lines :=
                    !lines
                    @ [
                        Printf.sprintf "  %%%s = call i8* @%s(i32 %s)" dst c va;
                      ];
                  H.replace h dst String
              | _ -> failwith "llvm_emit: int_to_str arity")
          | "str_concat" -> (
              match args with
              | [ a; b ] ->
                  let i8star_arg op =
                    match op with
                    | ConstStr s ->
                        let tmp = fresh_emit_aux ctx in
                        lines := !lines @ [ emit_global_string ctx tmp s ];
                        H.replace h tmp String;
                        Printf.sprintf "i8* %%%s" tmp
                    | _ ->
                        let ta, va = emit_operand ctx h op in
                        if ta <> "i8*" then
                          failwith "llvm_emit: str_concat expects two strings";
                        Printf.sprintf "i8* %s" va
                  in
                  let aa = i8star_arg a in
                  let ab = i8star_arg b in
                  let c = callee_ll "str_concat" in
                  lines :=
                    !lines
                    @ [
                        Printf.sprintf "  %%%s = call i8* @%s(%s, %s)" dst c aa ab;
                      ];
                  H.replace h dst String
              | _ -> failwith "llvm_emit: str_concat arity")
          | "strcmp" -> (
              match args with
              | [ a; b ] ->
                  let i8star_arg op =
                    match op with
                    | ConstStr s ->
                        let tmp = fresh_emit_aux ctx in
                        lines := !lines @ [ emit_global_string ctx tmp s ];
                        H.replace h tmp String;
                        Printf.sprintf "i8* %%%s" tmp
                    | _ ->
                        let ta, va = emit_operand ctx h op in
                        if ta <> "i8*" then
                          failwith "llvm_emit: strcmp expects two strings";
                        Printf.sprintf "i8* %s" va
                  in
                  let aa = i8star_arg a in
                  let ab = i8star_arg b in
                  let c = callee_ll "strcmp" in
                  lines :=
                    !lines
                    @ [
                        Printf.sprintf "  %%%s = call i32 @%s(%s, %s)" dst c aa ab;
                      ];
                  H.replace h dst I32
              | _ -> failwith "llvm_emit: strcmp arity")
          | "float_eq" -> (
              match args with
              | [ a; b ] ->
                  let ta, va = emit_operand ctx h a in
                  let tb, vb = emit_operand ctx h b in
                  if ta <> "double" || tb <> "double" then
                    failwith "llvm_emit: float_eq expects two doubles";
                  lines :=
                    !lines
                    @ [
                        Printf.sprintf
                          "  %%%s = call i32 @ls_float_eq(double %s, double %s)" dst
                          va vb;
                      ];
                  H.replace h dst I32
              | _ -> failwith "llvm_emit: float_eq arity")
          | "ordering_int32" -> (
              match args with
              | [ a; b ] ->
                  let ta, va = emit_operand ctx h a in
                  let tb, vb = emit_operand ctx h b in
                  if ta <> "i32" || tb <> "i32" then
                    failwith "llvm_emit: ordering_int32 expects i32";
                  lines :=
                    !lines
                    @ [
                        Printf.sprintf
                          "  %%%s = call i8* @ls_ordering_int32(i32 %s, i32 %s)" dst va
                          vb;
                      ];
                  H.replace h dst RawPtr
              | _ -> failwith "llvm_emit: ordering_int32 arity")
          | "ordering_char" -> (
              match args with
              | [ a; b ] ->
                  let ta, va = emit_operand ctx h a in
                  let tb, vb = emit_operand ctx h b in
                  if ta <> "i32" || tb <> "i32" then
                    failwith "llvm_emit: ordering_char expects i32";
                  lines :=
                    !lines
                    @ [
                        Printf.sprintf
                          "  %%%s = call i8* @ls_ordering_char(i32 %s, i32 %s)" dst va vb;
                      ];
                  H.replace h dst RawPtr
              | _ -> failwith "llvm_emit: ordering_char arity")
          | "ordering_str" -> (
              match args with
              | [ a; b ] ->
                  let i8star_arg op =
                    match op with
                    | ConstStr s ->
                        let tmp = fresh_emit_aux ctx in
                        lines := !lines @ [ emit_global_string ctx tmp s ];
                        H.replace h tmp String;
                        Printf.sprintf "i8* %%%s" tmp
                    | _ ->
                        let ta, va = emit_operand ctx h op in
                        if ta <> "i8*" then
                          failwith "llvm_emit: ordering_str expects strings";
                        Printf.sprintf "i8* %s" va
                  in
                  let aa = i8star_arg a in
                  let ab = i8star_arg b in
                  lines :=
                    !lines
                    @ [
                        Printf.sprintf "  %%%s = call i8* @ls_ordering_str(%s, %s)" dst aa
                          ab;
                      ];
                  H.replace h dst RawPtr
              | _ -> failwith "llvm_emit: ordering_str arity")
          | "ordering_bool" -> (
              match args with
              | [ a; b ] ->
                  let zext_i1 op =
                    let t, v = emit_operand ctx h op in
                    if t <> "i1" then failwith "llvm_emit: ordering_bool expects i1";
                    let tmp = fresh_emit_aux ctx in
                    lines :=
                      !lines
                      @ [ Printf.sprintf "  %%%s = zext i1 %s to i8" tmp v ];
                    Printf.sprintf "i8 %%%s" tmp
                  in
                  let aa = zext_i1 a in
                  let ab = zext_i1 b in
                  lines :=
                    !lines
                    @ [
                        Printf.sprintf "  %%%s = call i8* @ls_ordering_bool(%s, %s)" dst aa
                          ab;
                      ];
                  H.replace h dst RawPtr
              | _ -> failwith "llvm_emit: ordering_bool arity")
          | "ordering_float" -> (
              match args with
              | [ a; b ] ->
                  let ta, va = emit_operand ctx h a in
                  let tb, vb = emit_operand ctx h b in
                  if ta <> "double" || tb <> "double" then
                    failwith "llvm_emit: ordering_float expects double";
                  lines :=
                    !lines
                    @ [
                        Printf.sprintf
                          "  %%%s = call i8* @ls_ordering_float(double %s, double %s)" dst
                          va vb;
                      ];
                  H.replace h dst RawPtr
              | _ -> failwith "llvm_emit: ordering_float arity")
          | "ls_variant_mk" -> (
              match args with
              | [ tag; pl ] ->
                  let tt, tv = emit_operand ctx h tag in
                  let pt, pv = emit_operand ctx h pl in
                  if tt <> "i32" || pt <> "i8*" then
                    failwith "llvm_emit: ls_variant_mk(i32, i8*)";
                  lines :=
                    !lines
                    @ [
                        Printf.sprintf "  %%%s = call i8* @ls_variant_mk(i32 %s, i8* %s)"
                          dst tv pv;
                      ];
                  H.replace h dst RawPtr
              | _ -> failwith "llvm_emit: ls_variant_mk arity")
          | "ls_variant_tag" -> (
              match args with
              | [ v ] ->
                  let vt, vv = emit_operand ctx h v in
                  if vt <> "i8*" then failwith "llvm_emit: ls_variant_tag expects i8*";
                  lines :=
                    !lines
                    @ [
                        Printf.sprintf "  %%%s = call i32 @ls_variant_tag(i8* %s)" dst
                          vv;
                      ];
                  H.replace h dst I32
              | _ -> failwith "llvm_emit: ls_variant_tag arity")
          | "ls_variant_payload" -> (
              match args with
              | [ v ] ->
                  let vt, vv = emit_operand ctx h v in
                  if vt <> "i8*" then
                    failwith "llvm_emit: ls_variant_payload expects i8*";
                  lines :=
                    !lines
                    @ [
                        Printf.sprintf
                          "  %%%s = call i8* @ls_variant_payload(i8* %s)" dst vv;
                      ];
                  H.replace h dst RawPtr
              | _ -> failwith "llvm_emit: ls_variant_payload arity")
          | _ ->
              let fd = H.find fn_sigs name in
              if fd.ret = Unit then
                failwith "llvm_emit: void call should use VoidCall";
              let parts = map_call_args ctx h lines fd args in
              let ret_ll = llvm_ll_ty_ctx ctx fd.ret in
              lines :=
                !lines
                @ [
                    Printf.sprintf "  %%%s = call %s @%s(%s)" dst ret_ll name
                      (String.concat ", " parts);
                  ];
              H.replace h dst fd.ret)
      | IndirectCall (callee_op, ptys, ret_ty, args) -> (
          match ret_ty with
          | Unit -> failwith "llvm_emit: indirect value call cannot return unit"
          | _ ->
              let callee_ty = llvm_fun_ptr_ty_ctx ctx ptys ret_ty in
              let ct, cv = emit_operand ctx h callee_op in
              if ct <> callee_ty then
                failwith "llvm_emit: indirect callee type mismatch";
              let parts =
                List.map2
                  (fun pty op ->
                    match (pty, op) with
                    | Unit, ConstUnit -> "i8 0"
                    | Unit, Local x ->
                        if H.find h x <> Unit then
                          failwith "llvm_emit: unit indirect arg must be unit local";
                        Printf.sprintf "i8 %%%s" x
                    | String, ConstStr s ->
                        let tmp = fresh_emit_aux ctx in
                        lines := !lines @ [ emit_global_string ctx tmp s ];
                        H.replace h tmp String;
                        Printf.sprintf "i8* %%%s" tmp
                    | _ ->
                        let got_ll, v = emit_operand ctx h op in
                        let exp_ll =
                            if pty = Unit then "i8" else llvm_ll_ty_ctx ctx pty
                          in
                        if got_ll <> exp_ll then
                          failwith "llvm_emit: indirect call arg type mismatch";
                        Printf.sprintf "%s %s" exp_ll v)
                  ptys args
              in
              let ret_ll = llvm_ll_ty_ctx ctx ret_ty in
              lines :=
                !lines
                @ [
                    Printf.sprintf "  %%%s = call %s %s(%s)" dst ret_ll cv
                      (String.concat ", " parts);
                  ];
              H.replace h dst ret_ty)
      | TuplePack (elem_tys, ops) ->
          if List.length ops <> List.length elem_tys then
            failwith "llvm_emit: tuple pack length";
          let tty = Tuple elem_tys in
          let struct_ll = llvm_ll_ty_ctx ctx tty in
          let last = List.length elem_tys - 1 in
          let acc = ref "" in
          List.iteri
            (fun i (op, ety) ->
              let exp_ll = llvm_struct_elem_ty ctx ety in
              let ov =
                match (ety, op) with
                | String, ConstStr s ->
                    let tmp = fresh_emit_aux ctx in
                    lines := !lines @ [ emit_global_string ctx tmp s ];
                    H.replace h tmp String;
                    "%" ^ tmp
                | _ ->
                    let got_ll, raw = emit_operand ctx h op in
                    if got_ll <> exp_ll then
                      failwith "llvm_emit: tuple pack field type";
                    raw
              in
              let nm = if i = last then dst else dst ^ "_pk" ^ string_of_int i in
              let agg_in =
                if i = 0 then "undef" else Printf.sprintf "%%%s" !acc
              in
              lines :=
                !lines
                @ [
                    Printf.sprintf
                      "  %%%s = insertvalue %s %s, %s %s, %d" nm struct_ll
                      agg_in exp_ll ov i;
                  ];
              acc := nm)
            (List.combine ops elem_tys);
          H.replace h dst tty
      | TupleProj { tup; index; elem_tys } ->
          if index < 0 || index >= List.length elem_tys then
            failwith "llvm_emit: tuple proj index";
          let tty = Tuple elem_tys in
          let struct_ll = llvm_ll_ty_ctx ctx tty in
          let tt, tv = emit_operand ctx h tup in
          if tt <> struct_ll then failwith "llvm_emit: tuple proj aggregate";
          lines :=
            !lines
            @ [
                Printf.sprintf "  %%%s = extractvalue %s %s, %d" dst struct_ll tv
                  index;
              ];
          H.replace h dst (List.nth elem_tys index)
      | ListNil elem_ty ->
          lines :=
            !lines
            @ [ Printf.sprintf "  %%%s = bitcast i8* null to i8*" dst ];
          H.replace h dst (List elem_ty)
      | ListCons { elem_ty; head; tail } ->
          let struct_n = register_list_cell_layout ctx elem_ty in
          let bytes = list_cell_byte_size elem_ty in
          let raw = dst ^ "_raw" in
          lines :=
            !lines
            @ [
                Printf.sprintf "  %%%s = call i8* @ls_malloc(i64 %d)" raw bytes;
              ];
          let cp = dst ^ "_cp" in
          lines :=
            !lines
            @ [
                Printf.sprintf "  %%%s = bitcast i8* %%%s to %%%s*" cp raw
                  struct_n;
              ];
          let head_ll = llvm_struct_elem_ty ctx elem_ty in
          let ht, hv = emit_operand ctx h head in
          if ht <> head_ll then failwith "llvm_emit: list cons head type mismatch";
          let gh = dst ^ "_gh" in
          lines :=
            !lines
            @ [
                Printf.sprintf
                  "  %%%s = getelementptr inbounds %%%s, %%%s* %%%s, i32 0, i32 0"
                  gh struct_n struct_n cp;
              ];
          lines :=
            !lines
            @ [
                Printf.sprintf "  store %s %s, %s* %%%s" head_ll hv head_ll gh;
              ];
          let tt, tv = emit_operand ctx h tail in
          if tt <> "i8*" then failwith "llvm_emit: list cons tail must be i8*";
          let gt = dst ^ "_gt" in
          lines :=
            !lines
            @ [
                Printf.sprintf
                  "  %%%s = getelementptr inbounds %%%s, %%%s* %%%s, i32 0, i32 1"
                  gt struct_n struct_n cp;
              ];
          lines :=
            !lines
            @ [ Printf.sprintf "  store i8* %s, i8** %%%s" tv gt ];
          lines :=
            !lines
            @ [
                Printf.sprintf "  %%%s = bitcast %%%s* %%%s to i8*" dst struct_n
                  cp;
              ];
          H.replace h dst (List elem_ty)
      | ListHead { elem_ty; lst } ->
          let struct_n = register_list_cell_layout ctx elem_ty in
          let lt, lv = emit_operand ctx h lst in
          if lt <> "i8*" then failwith "llvm_emit: list head expects list i8*";
          let g_sent = ensure_list_sentinel ctx struct_n in
          let nn = dst ^ "_nn" in
          let lroot = dst ^ "_lroot" in
          lines :=
            !lines
            @ [
                Printf.sprintf "  %%%s = icmp ne i8* %s, null" nn lv;
                Printf.sprintf
                  "  %%%s = select i1 %%%s, i8* %s, i8* bitcast (%%%s* @%s to \
                   i8*)" lroot nn lv struct_n g_sent;
              ];
          let cp = dst ^ "_lcp" in
          lines :=
            !lines
            @ [
                Printf.sprintf "  %%%s = bitcast i8* %%%s to %%%s*" cp lroot
                  struct_n;
              ];
          let gh = dst ^ "_gh" in
          let head_ll = llvm_struct_elem_ty ctx elem_ty in
          lines :=
            !lines
            @ [
                Printf.sprintf
                  "  %%%s = getelementptr inbounds %%%s, %%%s* %%%s, i32 0, i32 0"
                  gh struct_n struct_n cp;
              ];
          if head_ll = "i1" then (
            let w = dst ^ "_lb" in
            lines :=
              !lines
              @ [
                  Printf.sprintf "  %%%s = load i1, i1* %%%s" w gh;
                  Printf.sprintf "  %%%s = xor i1 %%%s, false" dst w;
                ];
            H.replace h dst I1)
          else (
            lines :=
              !lines
              @ [
                  Printf.sprintf "  %%%s = load %s, %s* %%%s" dst head_ll head_ll gh;
                ];
            H.replace h dst elem_ty)
      | ListTail { elem_ty; lst } ->
          let struct_n = register_list_cell_layout ctx elem_ty in
          let lt, lv = emit_operand ctx h lst in
          if lt <> "i8*" then failwith "llvm_emit: list tail expects list i8*";
          let g_sent = ensure_list_sentinel ctx struct_n in
          let nn = dst ^ "_tnn" in
          let lroot = dst ^ "_troot" in
          lines :=
            !lines
            @ [
                Printf.sprintf "  %%%s = icmp ne i8* %s, null" nn lv;
                Printf.sprintf
                  "  %%%s = select i1 %%%s, i8* %s, i8* bitcast (%%%s* @%s to \
                   i8*)" lroot nn lv struct_n g_sent;
              ];
          let cp = dst ^ "_tcp" in
          lines :=
            !lines
            @ [
                Printf.sprintf "  %%%s = bitcast i8* %%%s to %%%s*" cp lroot
                  struct_n;
              ];
          let gt = dst ^ "_tg" in
          lines :=
            !lines
            @ [
                Printf.sprintf
                  "  %%%s = getelementptr inbounds %%%s, %%%s* %%%s, i32 0, i32 1"
                  gt struct_n struct_n cp;
              ];
          lines :=
            !lines
            @ [ Printf.sprintf "  %%%s = load i8*, i8** %%%s" dst gt ];
          H.replace h dst (List elem_ty)
      | RawMalloc n ->
          lines :=
            !lines
            @ [
                Printf.sprintf "  %%%s = call i8* @ls_malloc(i64 %d)" dst n;
              ];
          H.replace h dst RawPtr
      | EnvLoad { env; layout; index } ->
      let struct_n = register_env_layout ctx layout in
      let res_min = List.nth layout index in
      let fld_ll = llvm_env_field_ll_ty ctx res_min in
      let _, env_v = emit_operand ctx h env in
      let eb = dst ^ "_ebuf" in
      let eg = dst ^ "_eg" in
      lines :=
        !lines
        @ [
            Printf.sprintf
              "  %%%s = bitcast i8* %s to %%%s*" eb env_v struct_n;
            Printf.sprintf
              "  %%%s = getelementptr inbounds %%%s, %%%s* %%%s, i32 0, i32 %d"
              eg struct_n struct_n eb index;
          ];
      if res_min = I1 then (
        let wl = dst ^ "_wi" in
        lines :=
          !lines
          @ [
              Printf.sprintf "  %%%s = load %s, %s* %%%s" wl fld_ll fld_ll eg;
              Printf.sprintf "  %%%s = icmp ne i32 %%%s, 0" dst wl;
            ];
        H.replace h dst I1)
      else (
        lines :=
          !lines
          @ [ Printf.sprintf "  %%%s = load %s, %s* %%%s" dst fld_ll fld_ll eg ];
        H.replace h dst res_min)
  | EnvStore { env; layout; index; value } ->
      let struct_n = register_env_layout ctx layout in
      let v_min = List.nth layout index in
      let fld_ll = llvm_env_field_ll_ty ctx v_min in
      let _, env_v = emit_operand ctx h env in
      let eb = dst ^ "_esb" in
      let eg = dst ^ "_esg" in
      lines :=
        !lines
        @ [
            Printf.sprintf "  %%%s = bitcast i8* %s to %%%s*" eb env_v struct_n;
            Printf.sprintf
              "  %%%s = getelementptr inbounds %%%s, %%%s* %%%s, i32 0, i32 %d"
              eg struct_n struct_n eb index;
          ];
      if v_min = I1 then (
        let vt, vv = emit_operand ctx h value in
        if vt <> "i1" then failwith "llvm_emit: env i1 store";
        let w = dst ^ "_wz" in
        lines :=
          !lines
          @ [
              Printf.sprintf "  %%%s = zext i1 %s to i32" w vv;
              Printf.sprintf "  store i32 %%%s, i32* %%%s" w eg;
            ])
      else
        let got_ll, vv = emit_operand ctx h value in
        if got_ll <> fld_ll then
          failwith "llvm_emit: env store type mismatch";
        lines :=
          !lines
          @ [ Printf.sprintf "  store %s %s, %s* %%%s" fld_ll vv fld_ll eg ]
  | HeapBox (elem_ty, op) ->
      let bytes = layout_byte_size [ elem_ty ] in
      lines :=
        !lines
        @ [
            Printf.sprintf "  %%%s = call i8* @ls_malloc(i64 %d)" dst bytes;
          ];
      let bp = dst ^ "_bptr" in
      (match elem_ty with
      | I32 ->
          let got_ll, vv = emit_operand ctx h op in
          if got_ll <> "i32" then failwith "llvm_emit: HeapBox i32";
          lines :=
            !lines
            @ [
                Printf.sprintf "  %%%s = bitcast i8* %%%s to i32*" bp dst;
                Printf.sprintf "  store i32 %s, i32* %%%s" vv bp;
              ]
      | I1 ->
          let got_ll, vv = emit_operand ctx h op in
          if got_ll <> "i1" then failwith "llvm_emit: HeapBox i1";
          let wz = dst ^ "_z1" in
          lines :=
            !lines
            @ [
                Printf.sprintf "  %%%s = zext i1 %s to i32" wz vv;
                Printf.sprintf "  %%%s = bitcast i8* %%%s to i32*" bp dst;
                Printf.sprintf "  store i32 %%%s, i32* %%%s" wz bp;
              ]
      | F64 ->
          let got_ll, vv = emit_operand ctx h op in
          if got_ll <> "double" then failwith "llvm_emit: HeapBox f64";
          lines :=
            !lines
            @ [
                Printf.sprintf "  %%%s = bitcast i8* %%%s to double*" bp dst;
                Printf.sprintf "  store double %s, double* %%%s" vv bp;
              ]
      | Unit ->
          lines :=
            !lines
            @ [
                Printf.sprintf "  %%%s = bitcast i8* %%%s to i8*" bp dst;
                Printf.sprintf "  store i8 0, i8* %%%s" bp;
              ]
      | String | RawPtr | List _ | Clos _ ->
          let got_ll, vv = emit_operand ctx h op in
          if got_ll <> "i8*" then failwith "llvm_emit: HeapBox pointer";
          lines :=
            !lines
            @ [
                Printf.sprintf "  %%%s = bitcast i8* %%%s to i8**" bp dst;
                Printf.sprintf "  store i8* %s, i8** %%%s" vv bp;
              ]
      | Fun (ps, r) ->
          let fpty = llvm_fun_ptr_ty_ctx ctx ps r in
          let got_ll, vv = emit_operand ctx h op in
          if got_ll <> fpty then failwith "llvm_emit: HeapBox fun";
          lines :=
            !lines
            @ [
                Printf.sprintf "  %%%s = bitcast i8* %%%s to %s*" bp dst fpty;
                Printf.sprintf "  store %s %s, %s* %%%s" fpty vv fpty bp;
              ]
      | Tuple _ as tup ->
          let struct_ll = llvm_ll_ty_ctx ctx tup in
          let got_ll, vv = emit_operand ctx h op in
          if got_ll <> struct_ll then failwith "llvm_emit: HeapBox tuple";
          lines :=
            !lines
            @ [
                Printf.sprintf "  %%%s = bitcast i8* %%%s to %s*" bp dst struct_ll;
                Printf.sprintf "  store %s %s, %s* %%%s" struct_ll vv struct_ll bp;
              ]
      );
      H.replace h dst RawPtr
  | HeapUnbox (elem_ty, op) ->
      let pt, pv = emit_operand ctx h op in
      if pt <> "i8*" then failwith "llvm_emit: HeapUnbox expects i8*";
      (match elem_ty with
      | Unit ->
          lines := !lines @ [ Printf.sprintf "  %%%s = add i8 0, 0" dst ];
          H.replace h dst Unit
      | _ ->
          let _, dummy_i8 = heap_unbox_dummy_global ctx elem_ty in
          let nn = dst ^ "_unn" in
          let psafe = dst ^ "_pubox" in
          lines :=
            !lines
            @ [
                Printf.sprintf "  %%%s = icmp ne i8* %s, null" nn pv;
                Printf.sprintf "  %%%s = select i1 %%%s, i8* %s, i8* %s" psafe nn
                  pv dummy_i8;
              ];
          let bp = dst ^ "_uptr" in
          (match elem_ty with
          | I32 ->
              lines :=
                !lines
                @ [
                    Printf.sprintf "  %%%s = bitcast i8* %%%s to i32*" bp psafe;
                    Printf.sprintf "  %%%s = load i32, i32* %%%s" dst bp;
                  ];
              H.replace h dst I32
          | I1 ->
              let wl = dst ^ "_wi" in
              lines :=
                !lines
                @ [
                    Printf.sprintf "  %%%s = bitcast i8* %%%s to i32*" bp psafe;
                    Printf.sprintf "  %%%s = load i32, i32* %%%s" wl bp;
                    Printf.sprintf "  %%%s = icmp ne i32 %%%s, 0" dst wl;
                  ];
              H.replace h dst I1
          | F64 ->
              lines :=
                !lines
                @ [
                    Printf.sprintf "  %%%s = bitcast i8* %%%s to double*" bp psafe;
                    Printf.sprintf "  %%%s = load double, double* %%%s" dst bp;
                  ];
              H.replace h dst F64
          | String | RawPtr | List _ | Clos _ ->
              lines :=
                !lines
                @ [
                    Printf.sprintf "  %%%s = bitcast i8* %%%s to i8**" bp psafe;
                    Printf.sprintf "  %%%s = load i8*, i8** %%%s" dst bp;
                  ];
              H.replace h dst elem_ty
          | Fun (ps, r) ->
              let fpty = llvm_fun_ptr_ty_ctx ctx ps r in
              lines :=
                !lines
                @ [
                    Printf.sprintf "  %%%s = bitcast i8* %%%s to %s*" bp psafe fpty;
                    Printf.sprintf "  %%%s = load %s, %s* %%%s" dst fpty fpty bp;
                  ];
              H.replace h dst elem_ty
          | Tuple _ as tup ->
              let struct_ll = llvm_ll_ty_ctx ctx tup in
              lines :=
                !lines
                @ [
                    Printf.sprintf "  %%%s = bitcast i8* %%%s to %s*" bp psafe
                      struct_ll;
                    Printf.sprintf "  %%%s = load %s, %s* %%%s" dst struct_ll
                      struct_ll bp;
                  ];
              H.replace h dst elem_ty
          | Unit -> assert false))
  | VariantMk (tag, payload_op) ->
      let tag_op = ConstI32 tag in
      let tt, tv = emit_operand ctx h tag_op in
      let pt, pv = emit_operand ctx h payload_op in
      if tt <> "i32" || pt <> "i8*" then
        failwith "llvm_emit: VariantMk";
      lines :=
        !lines
        @ [
            Printf.sprintf "  %%%s = call i8* @ls_variant_mk(i32 %s, i8* %s)" dst
              tv pv;
          ];
      H.replace h dst RawPtr
  | VariantTag vop ->
      let vt, vv = emit_operand ctx h vop in
      if vt <> "i8*" then failwith "llvm_emit: VariantTag";
      lines :=
        !lines
        @ [ Printf.sprintf "  %%%s = call i32 @ls_variant_tag(i8* %s)" dst vv ];
      H.replace h dst I32
  | VariantPayload vop ->
      let vt, vv = emit_operand ctx h vop in
      if vt <> "i8*" then failwith "llvm_emit: VariantPayload";
      lines :=
        !lines
        @ [
            Printf.sprintf "  %%%s = call i8* @ls_variant_payload(i8* %s)" dst vv;
          ];
      H.replace h dst RawPtr
  | MkClos { code; env_ptr; clo_ty } -> (
      match clo_ty with
      | Clos _ ->
          let fd = H.find fn_sigs code in
          let fpty = step_fn_ptr_ty ctx fd in
          let _, env_v = emit_operand ctx h env_ptr in
          let ctmp = dst ^ "_codep" in
          lines :=
            !lines
            @ [
                Printf.sprintf "  %%%s = bitcast %s @%s to i8*" ctmp fpty code;
                Printf.sprintf "  %%%s = call i8* @ls_mkclos(i8* %%%s, i8* %s)" dst
                  ctmp env_v;
              ];
          H.replace h dst clo_ty
      | _ -> failwith "llvm_emit: MkClos result must be Clos")
  | ClosApply { clo; arg; result_ty } ->
      let _, clos_v = emit_operand ctx h clo in
      let aty, av =
        match arg with
        | ConstStr s ->
            let tmp = fresh_emit_aux ctx in
            lines := !lines @ [ emit_global_string ctx tmp s ];
            H.replace h tmp String;
            ("i8*", "%" ^ tmp)
        | _ -> emit_operand ctx h arg
      in
      let ret_ll = llvm_ll_ty_ctx ctx result_ty in
      let p = dst ^ "_cp" in
      let g_code = dst ^ "_gcd" in
      let v_code = dst ^ "_vcd" in
      let g_env = dst ^ "_gen" in
      let v_env = dst ^ "_ven" in
      let fnp = dst ^ "_fn" in
      let fpty = Printf.sprintf "%s (i8*, %s)*" ret_ll aty in
      lines :=
        !lines
        @ [
            Printf.sprintf "  %%%s = bitcast i8* %s to %s" p clos_v ls_clos_ptr;
            Printf.sprintf
              "  %%%s = getelementptr inbounds %s, %s %%%s, i32 0, i32 0" g_code
              ls_clos_ty ls_clos_ptr p;
            Printf.sprintf "  %%%s = load i8*, i8** %%%s" v_code g_code;
            Printf.sprintf
              "  %%%s = getelementptr inbounds %s, %s %%%s, i32 0, i32 1" g_env
              ls_clos_ty ls_clos_ptr p;
            Printf.sprintf "  %%%s = load i8*, i8** %%%s" v_env g_env;
            Printf.sprintf "  %%%s = bitcast i8* %%%s to %s" fnp v_code fpty;
            Printf.sprintf "  %%%s = call %s %%%s(i8* %%%s, %s %s)" dst ret_ll fnp
              v_env aty av;
          ];
      H.replace h dst result_ty)

let emit_term (ctx : emit_ctx) (h : (string, ty) H.t)
    (lines : string list ref) ~(ret : ty) ~(is_c_main : bool) (t : term) : unit
    =
  let emit_ret op_opt =
    match (is_c_main, ret, op_opt) with
    | true, Unit, None -> lines := !lines @ [ "  ret i32 0" ]
    | true, _, Some o ->
        let ty, v = emit_operand ctx h o in
        if ty = "i32" then lines := !lines @ [ Printf.sprintf "  ret i32 %s" v ]
        else
          failwith "llvm_emit: main must return i32 for C ABI (extend zext)"
    | false, Unit, None -> lines := !lines @ [ "  ret void" ]
    | false, I32, Some o ->
        let _ty, v = emit_operand ctx h o in
        lines := !lines @ [ Printf.sprintf "  ret i32 %s" v ]
    | false, I1, Some o ->
        let _ty, v = emit_operand ctx h o in
        lines := !lines @ [ Printf.sprintf "  ret i1 %s" v ]
    | false, String, Some o -> (
        match o with
        | ConstStr s ->
            let tmp = fresh_emit_aux ctx in
            lines := !lines @ [ emit_global_string ctx tmp s ];
            H.replace h tmp String;
            lines := !lines @ [ Printf.sprintf "  ret i8* %%%s" tmp ]
        | _ ->
            let _ty, v = emit_operand ctx h o in
            lines := !lines @ [ Printf.sprintf "  ret i8* %s" v ])
    | false, Fun _, Some o ->
        let ty, v = emit_operand ctx h o in
        lines := !lines @ [ Printf.sprintf "  ret %s %s" ty v ]
    | false, Tuple _, Some o ->
        let ty, v = emit_operand ctx h o in
        lines := !lines @ [ Printf.sprintf "  ret %s %s" ty v ]
    | false, List _, Some o ->
        let _ty, v = emit_operand ctx h o in
        lines := !lines @ [ Printf.sprintf "  ret i8* %s" v ]
    | false, (RawPtr | Clos _), Some o ->
        let _ty, v = emit_operand ctx h o in
        lines := !lines @ [ Printf.sprintf "  ret i8* %s" v ]
    | _ -> failwith "llvm_emit: bad return"
  in
  match t with
  | Ret None -> emit_ret None
  | Ret (Some o) -> emit_ret (Some o)
  | Br l -> lines := !lines @ [ Printf.sprintf "  br label %%%s" l ]
  | BrCond (cond, l1, l2) ->
      let ty, v = emit_operand ctx h cond in
      if ty <> "i1" then failwith "llvm_emit: br cond expects i1";
      lines :=
        !lines
        @ [
            Printf.sprintf "  br i1 %s, label %%%s, label %%%s" v l1 l2;
          ]
  | Unreachable -> lines := !lines @ [ "  unreachable" ]

let init_param_types (f : func_def) : (string, ty) H.t =
  let h = H.create 16 in
  List.iter (fun (p, t) -> H.replace h p t) f.params;
  h

let emit_func ctx (fn_sigs : (string, func_def) H.t) (f : func_def) : string =
  let is_c_main = f.name = "main" in
  let ret_s =
    if is_c_main && f.ret = Unit then "i32"
    else llvm_ll_ty_ctx ctx f.ret
  in
  let params_s =
    String.concat ", "
      (List.map
         (fun (p, t) ->
           Printf.sprintf "%s %%%s" (llvm_value_ty_ctx ctx t) p)
         f.params)
  in
  let h = init_param_types f in
  let blks = ordered_blocks f.entry f.blocks in
  let body_lines = ref [] in
  List.iter
    (fun blk ->
      body_lines := !body_lines @ [ Printf.sprintf "%s:" blk.label ];
      List.iter (fun i -> emit_instr ctx fn_sigs h body_lines i) blk.instrs;
      emit_term ctx h body_lines ~ret:f.ret ~is_c_main blk.term)
    blks;
  Printf.sprintf "define %s @%s(%s) {\n%s\n}\n" ret_s f.name params_s
    (String.concat "\n" !body_lines)

let emit_prog (p : prog) : string =
  let ctx = ctx_create () in
  let fn_sigs : (string, func_def) H.t = H.create 16 in
  List.iter (fun f -> H.replace fn_sigs f.name f) p.funcs;
  let func_text =
    List.map (emit_func ctx fn_sigs) p.funcs |> String.concat "\n"
  in
  let globals = String.concat "\n" (List.rev ctx.prelude) in
  let gl_sep = if ctx.prelude = [] then "" else "\n" in
  module_banner ^ runtime_declarations ^ "\n" ^ globals ^ gl_sep ^ func_text
