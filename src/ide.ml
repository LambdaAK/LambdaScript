(** Program analysis for editors (the [forge_ide] tool and the VS Code
    language server).

    [analyze] runs the front end on a buffer (with the prelude and the files
    it imports) and returns, as JSON:
    - diagnostics: lexical, syntax, import and type errors (one per failing
      definition; later definitions are still checked) and the
      exhaustiveness warnings;
    - symbols: every definition — top-level values and functions, types,
      constructors, traits, methods, macros, modules, and the local
      variables and parameters of the main file — with its location, its
      type and its doc comment;
    - references: every use of a name in the main file, resolved to its
      symbol, with the type at that use.

    Positions: [Condense] logs the span of each identifier it consumes. The
    identifier queue it reads holds the tokens of the prelude and the main
    file, with the tokens of each imported file spliced in where it is
    imported (the order in which definitions are condensed); every file has
    its own range of offsets, so a span names its file. *)

open Cexpr
module T = Typecheck

(* ------------------------------------------------------------------------ *)
(* Files and positions                                                      *)
(* ------------------------------------------------------------------------ *)

(** Offsets of imported file [k] start at [k * file_stride]. *)
let file_stride = 1 lsl 32

type file = { path : string; text : string; base : int }

type position = { line : int; col : int }

(** Line and UTF-16 column of byte [off] in [text]. *)
let position_of (text : string) (off : int) : position =
  let off = max 0 (min off (String.length text)) in
  let line = ref 0 and col = ref 0 in
  let i = ref 0 in
  while !i < off do
    let c = Char.code text.[!i] in
    if c = 10 then (
      incr line;
      col := 0;
      incr i)
    else
      let len = if c < 0x80 then 1 else if c < 0xE0 then 2 else if c < 0xF0 then 3 else 4 in
      col := !col + if len = 4 then 2 else 1;
      i := !i + len
  done;
  { line = !line; col = !col }

(** Byte offset of the start of 0-based line [l]. *)
let line_start (text : string) (l : int) : int =
  let rec go i k = if k = 0 || i >= String.length text then i else if text.[i] = '\n' then go (i + 1) (k - 1) else go (i + 1) k in
  go 0 l

(* ------------------------------------------------------------------------ *)
(* JSON                                                                     *)
(* ------------------------------------------------------------------------ *)

let json_string (s : string) : string =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n"
      | '\r' -> Buffer.add_string b "\\r"
      | '\t' -> Buffer.add_string b "\\t"
      | c when Char.code c < 0x20 -> Buffer.add_string b (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '"';
  Buffer.contents b

let json_obj (fields : (string * string) list) : string =
  "{" ^ String.concat "," (List.map (fun (k, v) -> json_string k ^ ":" ^ v) fields) ^ "}"

let json_list (items : string list) : string = "[" ^ String.concat "," items ^ "]"

(* ------------------------------------------------------------------------ *)
(* Results                                                                  *)
(* ------------------------------------------------------------------------ *)

type location = { file : string; start_pos : position; end_pos : position }

type diagnostic = { severity : int; (* 1 error, 2 warning *) message : string; loc : location }

type symbol = {
  id : int;
  name : string;  (** as written ([area], not [Geometry.area]) *)
  qname : string;  (** qualified *)
  kind : string;
  sloc : location option;  (** none for built-ins *)
  mutable full_end : position option;  (** end of a top-level definition *)
  mutable ty : string;
  doc : string;
  container : string;
  top : bool;
  mutable scope : location option;  (** where a local is visible *)
  annotated : bool;
}

type reference = { rloc : location; target : int; rty : string }

let location_json (l : location) : string =
  json_obj
    [
      ("file", json_string l.file);
      ( "range",
        json_list (List.map string_of_int [ l.start_pos.line; l.start_pos.col; l.end_pos.line; l.end_pos.col ]) );
    ]

(* ------------------------------------------------------------------------ *)
(* Built-in documentation                                                   *)
(* ------------------------------------------------------------------------ *)

let builtin_docs =
  [
    ("print", "Print a string, without a newline.");
    ("print_string", "Print a string followed by a newline.");
    ("int_to_str", "Decimal form of an integer.");
    ("int_to_float", "The integer as a float.");
    ("float_to_int", "Truncates toward zero.");
    ("float_to_str", "The shortest form that reads back as the same float.");
    ("char_to_str", "A one-character string.");
    ("string_to_list", "The characters of a string.");
    ("list_of_string", "The characters of a string.");
    ("str_length", "Number of characters.");
    ("str_concat", "Concatenation, the same as `^`.");
    ("str_slice", "`str_slice s start len`: the substring of length `len` from `start` (from 0), clamped to the string.");
    ("list_length", "Number of elements.");
    ("list_head", "The first element (a run-time error on `[]`).");
    ("list_tail", "All but the first element (a run-time error on `[]`).");
    ("list_nth", "The element at an index, from 0 (a run-time error when out of range).");
    ("map", "Apply a function to every element.");
    ("filter", "Keep the elements that satisfy a predicate.");
    ("reduce_left", "Fold from the left: `reduce_left f z [x1, x2]` is `f (f z x1) x2`.");
    ("reduce_right", "Fold from the right: `reduce_right f [x1, x2] z` is `f x1 (f x2 z)`.");
    ("tuple_fst", "The first component of a pair.");
    ("tuple_snd", "The second component of a pair.");
    ("not", "Negation.");
    ("sqrt", "Square root.");
    ("exp", "Exponential.");
    ("log", "Natural logarithm.");
    ("char_to_int", "The code of a character (0 to 255).");
    ("int_to_char", "The character with a code; a run-time error outside 0 to 255.");
    ("string_of_list", "The string of a list of characters.");
    ("+.", "Float addition.");
    ("-.", "Float subtraction.");
    ("*.", "Float multiplication.");
    ("/.", "Float division.");
  ]

(* ------------------------------------------------------------------------ *)
(* Analysis                                                                 *)
(* ------------------------------------------------------------------------ *)

let rec pat_binders (p : c_pat) : c_pat list =
  match p with
  | CIdPat "_" -> []
  | CIdPat _ -> [ p ]
  | CConsPat (a, b) -> pat_binders a @ pat_binders b
  | CVectorPat ps -> List.concat_map pat_binders ps
  | CRecordPat fs -> List.concat_map (fun (_, q) -> pat_binders q) fs
  | CVariantPat (_, Some q) -> pat_binders q
  | _ -> []

let pat_name = function CIdPat s -> s | _ -> ""

let peel_functions (e : c_expr) : bool = match e with EFunction _ -> true | _ -> false

let display_type (m : mono_type) : string = C_to_string.string_of_c_type (Mono m)
let display_scheme (t : c_type) : string = C_to_string.string_of_c_type t

let last_segment (s : string) : string =
  match String.rindex_opt s '.' with
  | Some i when i > 0 && i < String.length s - 1 -> String.sub s (i + 1) (String.length s - i - 1)
  | _ -> s

let container_of (s : string) : string =
  match String.rindex_opt s '.' with Some i when i > 0 -> String.sub s 0 i | _ -> ""

let analyze ~(src_path : string) ~(source : string) : string =
  let diagnostics : diagnostic list ref = ref [] in
  let symbols : symbol list ref = ref [] in
  let refs : reference list ref = ref [] in
  let next_id = ref 0 in
  (* Files: 0 is the prelude followed by the main buffer. *)
  let full_source = Prelude.prepend_to_source ~enabled:true ~src_path source in
  let delta = String.length full_source - String.length source in
  let prelude_path = match Prelude.resolved_path () with Some p -> p | None -> "prelude.forge" in
  let files : file list ref = ref [] in
  let file_of_offset (off : int) : (string * string * int) option =
    (* path, text, local offset *)
    if off < file_stride then
      if off >= delta then Some (src_path, source, off - delta)
      else Some (prelude_path, String.sub full_source 0 delta, off)
    else
      match List.find_opt (fun f -> off >= f.base && off < f.base + file_stride) !files with
      | Some f -> Some (f.path, f.text, off - f.base)
      | None -> None
  in
  let loc_of_span (lo, hi) : location option =
    match file_of_offset lo with
    | Some (path, text, l) -> Some { file = path; start_pos = position_of text l; end_pos = position_of text (l + (hi - lo)) }
    | None -> None
  in
  let in_main (lo : int) = lo >= delta && lo < file_stride in
  let main_line_loc (l : int) : location =
    let l = max 0 l in
    let lo = line_start source l in
    let hi = try String.index_from source lo '\n' with Not_found -> String.length source in
    { file = src_path; start_pos = position_of source lo; end_pos = position_of source hi }
  in
  let add_diag severity message loc = diagnostics := { severity; message; loc } :: !diagnostics in
  let new_symbol ?(top = false) ?(annotated = false) ?(doc = "") ~kind ~qname ~ty (sloc : location option) : symbol =
    let s =
      {
        id = !next_id;
        name = last_segment qname;
        qname;
        kind;
        sloc;
        full_end = None;
        ty;
        doc;
        container = container_of qname;
        top;
        scope = None;
        annotated;
      }
    in
    incr next_id;
    symbols := s :: !symbols;
    s
  in
  (* Doc comment: the [//] lines right above line [l] of [text]. *)
  let doc_above (text : string) (l : int) : string =
    let lines = String.split_on_char '\n' text in
    let arr = Array.of_list lines in
    let rec go k acc =
      if k < 0 then acc
      else
        let t = String.trim arr.(k) in
        if String.length t >= 2 && String.sub t 0 2 = "//" then
          go (k - 1) (String.trim (String.sub t 2 (String.length t - 2)) :: acc)
        else acc
    in
    if l - 1 < Array.length arr then String.concat "\n" (go (l - 1) []) else ""
  in
  let doc_for_span (lo : int) : string =
    match file_of_offset lo with Some (_, text, l) -> doc_above text (position_of text l).line | None -> ""
  in
  let finish () =
    let diags =
      List.rev_map
        (fun d ->
          json_obj
            [
              ("severity", string_of_int d.severity);
              ("message", json_string d.message);
              ("location", location_json d.loc);
            ])
        !diagnostics
    in
    let syms =
      List.rev_map
        (fun s ->
          json_obj
            ([
               ("id", string_of_int s.id);
               ("name", json_string s.name);
               ("qname", json_string s.qname);
               ("kind", json_string s.kind);
               ("type", json_string s.ty);
               ("doc", json_string s.doc);
               ("container", json_string s.container);
               ("top", if s.top then "true" else "false");
               ("annotated", if s.annotated then "true" else "false");
             ]
            @ (match s.sloc with Some l -> [ ("location", location_json l) ] | None -> [])
            @ (match s.full_end with
              | Some p -> [ ("fullEnd", json_list [ string_of_int p.line; string_of_int p.col ]) ]
              | None -> [])
            @ match s.scope with Some l -> [ ("scope", location_json l) ] | None -> []))
        !symbols
    in
    let by_id = Hashtbl.create 256 in
    List.iter (fun (s : symbol) -> Hashtbl.replace by_id s.id s) !symbols;
    let rs =
      List.rev_map
        (fun r ->
          let r =
            if r.rty = "" then
              match Hashtbl.find_opt by_id r.target with Some (s : symbol) -> { r with rty = s.ty } | None -> r
            else r
          in
          json_obj
            [ ("location", location_json r.rloc); ("target", string_of_int r.target); ("type", json_string r.rty) ])
        !refs
    in
    json_obj [ ("diagnostics", json_list diags); ("symbols", json_list syms); ("references", json_list rs) ]
  in
  let prefix_lines = Diag.lines_in_prefix full_source delta in
  let user_line (full_line : int) = full_line - 1 - prefix_lines in
  try
    (* 1. Lex and parse. *)
    let full_tokens =
      try Lex.lex (full_source |> String.to_seq |> List.of_seq)
      with Lex.Lex_error (msg, line) ->
        add_diag 1 msg (main_line_loc (user_line line));
        raise Exit
    in
    let program =
      try Parser.parse_program_tokens ~boundary:delta full_tokens
      with Parser.Parse_error (msg, line) ->
        add_diag 1 (Diag.clean msg) (main_line_loc (user_line line));
        raise Exit
    in
    (* 2. The identifier queue, with imported files spliced in. *)
    let emitted = Hashtbl.create 8 in
    Hashtbl.replace emitted (Import_resolve.absolute_path src_path) ();
    let rec splice ~(base_dir : string) (tokens : Lex.token list) : Lex.token list =
      match tokens with
      | ({ Lex.token_type = Lex.Import; _ } as t) :: ({ Lex.token_type = Lex.StringToken raw; _ } as s) :: rest -> (
          let target = try Some (Import_resolve.resolve_import_target_exn ~base_dir raw) with _ -> None in
          match target with
          | Some path when not (Hashtbl.mem emitted path) -> (
              Hashtbl.replace emitted path ();
              match Import_resolve.read_file path with
              | text ->
                  let base = (List.length !files + 1) * file_stride in
                  files := { path; text; base } :: !files;
                  let toks =
                    try Lex.lex (text |> String.to_seq |> List.of_seq) with _ -> []
                  in
                  let toks =
                    List.map (fun (k : Lex.token) -> { k with byte_start = k.byte_start + base; byte_end = k.byte_end + base }) toks
                  in
                  (t :: s :: splice ~base_dir:(Filename.dirname path) toks) @ splice ~base_dir rest
              | exception _ -> t :: s :: splice ~base_dir rest)
          | _ -> t :: s :: splice ~base_dir rest)
      | t :: rest -> t :: splice ~base_dir rest
      | [] -> []
    in
    let queue_tokens = splice ~base_dir:(Filename.dirname (Import_resolve.absolute_path src_path)) full_tokens in
    Condense.set_id_queue_from_tokens queue_tokens;
    let import_line () =
      List.find_map
        (fun (t : Lex.token) -> if t.token_type = Lex.Import && t.byte_start >= delta then Some (user_line t.line) else None)
        full_tokens
    in
    let program =
      try Import_resolve.resolve_program ~root_file:src_path ~base_dir:(Filename.dirname src_path) program with
      | Failure msg ->
          add_diag 1 (Diag.clean msg) (main_line_loc (Stdlib.Option.value (import_line ()) ~default:0));
          Condense.clear_id_queue ();
          raise Exit
      | Diag.Located (f, line, msg) ->
          add_diag 1 (Printf.sprintf "%s:%d: %s" (Filename.basename f) line (Diag.clean msg))
            (main_line_loc (Stdlib.Option.value (import_line ()) ~default:0));
          Condense.clear_id_queue ();
          raise Exit
    in
    let prelude_n =
      if delta <= 0 then 0
      else
        let n = Prelude.defn_count_for_source_fragment (String.sub full_source 0 delta) in
        if n > 0 then n else Prelude.defn_count_when_parsed ()
    in
    let condensed =
      try
        Condense.condense_program
          ?user_id_byte_min_after_prelude:(if delta = 0 then None else Some (prelude_n, delta))
          program
      with Failure msg ->
        add_diag 1 (Diag.clean msg) (main_line_loc 0);
        Condense.clear_id_queue ();
        raise Exit
    in
    Condense.clear_id_queue ();
    let pat_span (p : c_pat) : (int * int) option = Condense.PatTbl.find_opt Condense.pat_spans p in
    (* 3. Declarations that do not become EIds: types and constructors, and
       (from the tokens) traits, methods, macros and modules. *)
    let top_table : (string, symbol) Hashtbl.t = Hashtbl.create 256 in
    let type_table : (string, symbol) Hashtbl.t = Hashtbl.create 64 in
    let trait_table : (string, symbol) Hashtbl.t = Hashtbl.create 32 in
    let macro_table : (string, symbol) Hashtbl.t = Hashtbl.create 16 in
    let log = List.rev !Condense.name_log in
    List.iter
      (fun (kind, name, lo, hi) ->
        match kind with
        | "type_decl" ->
            let s = new_symbol ~top:true ~kind:"type" ~qname:name ~ty:"" ~doc:(doc_for_span lo) (loc_of_span (lo, hi)) in
            Hashtbl.replace type_table name s
        | "ctor_decl" ->
            let s = new_symbol ~top:true ~kind:"constructor" ~qname:name ~ty:"" ~doc:(doc_for_span lo) (loc_of_span (lo, hi)) in
            Hashtbl.replace top_table name s
        | _ -> ())
      log;
    (* Token scan over every file for traits, methods, macros, modules. *)
    let scan_tokens (toks : Lex.token list) =
      let rec go in_trait toks =
        match toks with
        | { Lex.token_type = Lex.Trait | Lex.Inter; _ } :: ({ Lex.token_type = Lex.Id n; _ } as t) :: rest ->
            let s =
              new_symbol ~top:true ~kind:"trait" ~qname:n ~ty:"" ~doc:(doc_for_span t.byte_start)
                (loc_of_span (t.byte_start, t.byte_end))
            in
            Hashtbl.replace trait_table n s;
            go true rest
        | { Lex.token_type = Lex.Val; _ } :: ({ Lex.token_type = Lex.Id n; _ } as t) :: rest when in_trait ->
            let s =
              new_symbol ~top:true ~kind:"method" ~qname:n ~ty:"" ~doc:(doc_for_span t.byte_start)
                (loc_of_span (t.byte_start, t.byte_end))
            in
            Hashtbl.replace top_table n s;
            go in_trait rest
        | { Lex.token_type = Lex.End; _ } :: rest -> go false rest
        | { Lex.token_type = Lex.MacroRules; _ } :: { Lex.token_type = Lex.Bang; _ } :: ({ Lex.token_type = Lex.Id n; _ } as t) :: rest ->
            let s =
              new_symbol ~top:true ~kind:"macro" ~qname:n ~ty:"" ~doc:(doc_for_span t.byte_start)
                (loc_of_span (t.byte_start, t.byte_end))
            in
            Hashtbl.replace macro_table n s;
            go in_trait rest
        | { Lex.token_type = Lex.ModKw; _ } :: ({ Lex.token_type = Lex.Id n; _ } as t) :: rest ->
            ignore
              (new_symbol ~top:true ~kind:"module" ~qname:n ~ty:"" ~doc:(doc_for_span t.byte_start)
                 (loc_of_span (t.byte_start, t.byte_end)));
            go in_trait rest
        | _ :: rest -> go in_trait rest
        | [] -> ()
      in
      go false toks
    in
    scan_tokens queue_tokens;
    (* [let NAME] / [let rec NAME] / [and NAME] in any file: a fallback
       location for top-level names whose binder has no recorded span. *)
    let let_name_spans : (string, int * int) Hashtbl.t = Hashtbl.create 64 in
    let rec scan_lets = function
      | { Lex.token_type = Lex.Let | Lex.And | Lex.Rec; _ } :: ({ Lex.token_type = Lex.Id n; _ } as t) :: rest ->
          if not (Hashtbl.mem let_name_spans n) then Hashtbl.replace let_name_spans n (t.byte_start, t.byte_end);
          scan_lets rest
      | _ :: rest -> scan_lets rest
      | [] -> ()
    in
    scan_lets queue_tokens;
    (* Built-ins, created on first use. *)
    let static_env0 = Build_env.build_full_static_env () in
    let builtin (name : string) : symbol option =
      match Hashtbl.find_opt top_table name with
      | Some s -> Some s
      | None -> (
          match List.assoc_opt name static_env0 with
          | Some t ->
              let s =
                new_symbol ~top:true ~kind:"builtin" ~qname:name ~ty:(display_scheme t)
                  ~doc:(Stdlib.Option.value (List.assoc_opt name builtin_docs) ~default:"")
                  None
              in
              Hashtbl.replace top_table name s;
              Some s
          | None -> None)
    in
    (* 4. Type-check definition by definition; walk the main file's. *)
    let static_env = ref static_env0 and type_env = ref [] and ctor_env = ref [] in
    let top_level_main : (symbol * int) list ref = ref [] in
    let walk_defn (d : c_defn) (sol : Mono.subst option) (main : bool) =
      let ty_of_node (e : c_expr) : string =
        match (sol, T.NodeTbl.find_opt T.node_types e) with
        | Some sol, Some t -> display_type (Mono.resolve sol t)
        | _ -> ""
      in
      let mono_of_node (e : c_expr) : mono_type option =
        match (sol, T.NodeTbl.find_opt T.node_types e) with Some sol, Some t -> Some (Mono.resolve sol t) | _ -> None
      in
      let add_ref lo hi (s : symbol) ty =
        if main && in_main lo then
          match loc_of_span (lo, hi) with Some l -> refs := { rloc = l; target = s.id; rty = ty } :: !refs | None -> ()
      in
      let local_syms : symbol list ref = ref [] in
      (* Binders of [p] as local symbols; [ty_for] gives each one's type. *)
      let bind ~kind (scope : (string * symbol) list) (p : c_pat) (ty_for : c_pat -> string) =
        List.fold_left
          (fun sc b ->
            match pat_span b with
            | Some (lo, hi) when main && in_main lo ->
                let s = new_symbol ~kind ~qname:(pat_name b) ~ty:(ty_for b) (loc_of_span (lo, hi)) in
                local_syms := s :: !local_syms;
                (pat_name b, s) :: sc
            | _ -> sc)
          scope (pat_binders p)
      in
      let rec walk (scope : (string * symbol) list) (e : c_expr) : unit =
        match e with
        | EId (name, Some (lo, hi)) -> (
            let target =
              match List.assoc_opt name scope with
              | Some s -> Some s
              | None -> ( match Hashtbl.find_opt top_table name with Some s -> Some s | None -> builtin name)
            in
            match target with
            | Some s ->
                let t = ty_of_node e in
                if s.ty = "" && s.kind <> "builtin" then s.ty <- t;
                add_ref lo hi s (if t = "" then s.ty else t)
            | None -> ())
        | EId _ | EInt _ | EFloat _ | EBool _ | EString _ | EChar _ | EUnit | ENil -> ()
        | EFunction (p, _, body) ->
            let param_ty =
              match mono_of_node e with Some (FunctionType (a, _)) -> display_type a | _ -> ""
            in
            let sc = bind ~kind:"parameter" scope p (fun b -> if b == p then param_ty else "") in
            walk sc body
        | EBind (p, _, e1, e2, _) ->
            walk scope e1;
            let binds = T.NodeTbl.find_opt T.node_bindings e in
            let ty_for b =
              match Stdlib.Option.bind binds (List.assoc_opt (pat_name b)) with
              | Some sch when b == p -> (
                  match (sol, mono_of_node e1) with
                  | Some _, Some m -> display_type m
                  | _ -> display_scheme sch)
              | _ -> if b == p then ty_of_node e1 else ""
            in
            walk (bind ~kind:"variable" scope p ty_for) e2
        | EBindRec (p, _, e1, e2, _) ->
            let sc = bind ~kind:"variable" scope p (fun b -> if b == p then ty_of_node e1 else "") in
            walk sc e1;
            walk sc e2
        | EBindMutRec (bs, body) ->
            let sc =
              List.fold_left
                (fun sc (p, _, e1, _, _) -> bind ~kind:"variable" sc p (fun b -> if b == p then ty_of_node e1 else ""))
                scope bs
            in
            List.iter (fun (_, _, e1, _, _) -> walk sc e1) bs;
            walk sc body
        | ESwitch (s, brs) ->
            walk scope s;
            List.iter
              (fun (p, body) ->
                let sc = bind ~kind:"variable" scope p (fun b -> if b == p then ty_of_node s else "") in
                walk sc body)
              brs
        | EListComprehension (body, gens) ->
            let sc =
              List.fold_left
                (fun sc (p, src) ->
                  walk sc src;
                  bind ~kind:"variable" sc p (fun b ->
                      match mono_of_node src with
                      | Some (CListType t) when b == p -> display_type t
                      | _ -> ""))
                scope gens
            in
            walk sc body
        | EApp (a, b) | EBop (_, a, b) | EListEnumeration (a, b) ->
            walk scope a;
            walk scope b
        | EFieldAccess (b, _) -> walk scope b
        | ETernary (a, b, c) ->
            walk scope a;
            walk scope b;
            walk scope c
        | EVector es -> List.iter (walk scope) es
        | ERecordLit fs -> List.iter (fun (_, x) -> walk scope x) fs
        | ERecordUpdate (r, fs) ->
            walk scope r;
            List.iter (fun (_, x) -> walk scope x) fs
        | EBlock parts ->
            ignore
              (List.fold_left
                 (fun sc part ->
                   match part with
                   | Expr x ->
                       walk sc x;
                       sc
                   | Defn (CDefn (p, _, _, x, _, _)) ->
                       walk sc x;
                       bind ~kind:"variable" sc p (fun b -> if b == p then ty_of_node x else "")
                   | Defn (CDefnRec (p, _, _, x, _, _)) ->
                       let sc' = bind ~kind:"variable" sc p (fun b -> if b == p then ty_of_node x else "") in
                       walk sc' x;
                       sc'
                   | Defn _ -> sc)
                 scope parts)
      in
      (* Top-level binders. *)
      let top_binders = ref [] in
      let declare_top (p : c_pat) (rhs : c_expr) (annotated : bool) =
        List.iter
          (fun b ->
            let name = pat_name b in
            let span =
              match pat_span b with
              | Some sp -> Some sp
              | None -> Hashtbl.find_opt let_name_spans (last_segment name)
            in
            let loc = match span with Some sp -> loc_of_span sp | None -> None in
            let lo = match span with Some (lo, _) -> lo | None -> -1 in
            let kind = if b == p && peel_functions rhs then "function" else "value" in
            let s =
              new_symbol ~top:true ~annotated ~kind ~qname:name ~ty:""
                ~doc:(if lo >= 0 then doc_for_span lo else "")
                loc
            in
            top_binders := (s, lo) :: !top_binders;
            Hashtbl.replace top_table name s)
          (pat_binders p)
      in
      (match d with
      | CDefn (p, _, cto, x, rt, _) ->
          walk [] x;
          declare_top p x (cto <> None || rt <> None)
      | CDefnRec (p, _, cto, x, rt, _) ->
          declare_top p x (cto <> None || rt <> None);
          walk [] x
      | CDefnMutRec ds ->
          List.iter (fun (p, _, cto, x, rt, _) -> declare_top p x (cto <> None || rt <> None)) ds;
          List.iter (fun (_, _, _, x, _, _) -> walk [] x) ds
      | _ -> ());
      (* A local is visible to the end of its top-level definition. *)
      List.iter (fun (s, lo) -> if main && in_main lo then top_level_main := (s, lo) :: !top_level_main) !top_binders;
      (!top_binders, !local_syms)
    in
    let locals_by_defn : (int * symbol list) list ref = ref [] in
    List.iter
      (fun (d : c_defn) ->
        T.NodeTbl.reset T.node_types;
        T.NodeTbl.reset T.node_bindings;
        T.type_logging := true;
        let result =
          try Some (T.generate_defn !static_env !type_env d) with _ -> None
        in
        T.type_logging := false;
        (* Where the definition is: its first binder, or its first located name. *)
        let first_lo =
          let from_pat p = List.find_map (fun b -> Stdlib.Option.map fst (pat_span b)) (pat_binders p) in
          match d with
          | CDefn (p, _, _, x, _, _) | CDefnRec (p, _, _, x, _, _) -> (
              match from_pat p with Some lo -> Some lo | None -> Exhaustiveness.first_offset x)
          | CDefnMutRec ((p, _, _, x, _, _) :: _) -> (
              match from_pat p with Some lo -> Some lo | None -> Exhaustiveness.first_offset x)
          | _ -> None
        in
        let main = match first_lo with Some lo -> in_main lo | None -> false in
        let defn_loc () =
          match first_lo with
          | Some lo -> ( match loc_of_span (lo, lo + 1) with Some l -> l | None -> main_line_loc 0)
          | None -> main_line_loc 0
        in
        match result with
        | Some (T.Ok (nb, nte, nce)) ->
            let sol = Mono.solution !T.last_defn_equations in
            let cx = { Exhaustiveness.static_env = nb @ !static_env; ctor_env = nce @ !ctor_env; type_env = nte @ !type_env } in
            (if main then
               try
                 List.iter
                   (fun (w : Exhaustiveness.warning) ->
                     let loc =
                       match w.offset with
                       | Some lo -> ( match loc_of_span (lo, lo + 1) with Some l -> l | None -> defn_loc ())
                       | None -> defn_loc ()
                     in
                     add_diag 2 w.message loc)
                   (Exhaustiveness.check_defn cx d)
               with _ -> ());
            let tops, locals = walk_defn d (Some sol) main in
            List.iter
              (fun ((s : symbol), _) ->
                match List.assoc_opt s.qname nb with Some t -> s.ty <- display_scheme t | None -> ())
              tops;
            (match first_lo with Some lo when main -> locals_by_defn := (lo, locals) :: !locals_by_defn | _ -> ());
            static_env := nb @ !static_env;
            type_env := nte @ !type_env;
            ctor_env := nce @ !ctor_env
        | Some (T.Error e) ->
            T.last_type_clash := None;
            let msg = Diag.clean (T.string_of_type_check_error e) in
            (* The editor shows where: keep the clash, drop the echo of the
               definition. *)
            let msg =
              match String.index_opt msg '\n' with Some i -> String.sub msg 0 i | None -> msg
            in
            let prefix = "Type inference failed while checking definition: " in
            let msg =
              if String.starts_with ~prefix msg then
                String.sub msg (String.length prefix) (String.length msg - String.length prefix)
              else msg
            in
            add_diag 1 ("type error: " ^ msg) (defn_loc ());
            let tops, locals = walk_defn d None main in
            (match first_lo with Some lo when main -> locals_by_defn := (lo, locals) :: !locals_by_defn | _ -> ());
            (* Bind the failed names to a fresh type so that later definitions
               are not all reported as using an unbound name. *)
            List.iter
              (fun ((s : symbol), _) ->
                static_env := (s.qname, PolyType ("a", Mono (TypeVar "a"))) :: !static_env)
              tops
        | None ->
            let tops, _ = walk_defn d None main in
            List.iter
              (fun ((s : symbol), _) ->
                static_env := (s.qname, PolyType ("a", Mono (TypeVar "a"))) :: !static_env)
              tops)
      condensed;
    (* 5. Types of declared constructors, methods; references from the log. *)
    List.iter
      (fun (s : symbol) ->
        if s.ty = "" && (s.kind = "constructor" || s.kind = "method") then
          match List.assoc_opt s.qname !static_env with Some t -> s.ty <- display_scheme t | None -> ())
      !symbols;
    List.iter
      (fun (s : symbol) ->
        if s.kind = "type" then
          match Hover_query.find_type_definition_by_name condensed s.qname with
          | Some d -> s.ty <- Hover_query.hover_string_of_type_defn d
          | None -> ())
      !symbols;
    let add_main_ref lo hi (s : symbol) =
      if in_main lo then
        match loc_of_span (lo, hi) with Some l -> refs := { rloc = l; target = s.id; rty = s.ty } :: !refs | None -> ()
    in
    List.iter
      (fun (kind, name, lo, hi) ->
        let table = match kind with "type_ref" -> Some type_table | "trait_ref" -> Some trait_table | "ctor_ref" -> Some top_table | _ -> None in
        match table with
        | Some tbl -> ( match Hashtbl.find_opt tbl name with Some s -> add_main_ref lo hi s | None -> ())
        | None -> ())
      log;
    (* Traits after [impl], and macro invocations, from the main tokens. *)
    let rec scan_refs = function
      | { Lex.token_type = Lex.Impl; _ } :: ({ Lex.token_type = Lex.Id n; _ } as t) :: rest ->
          (match Hashtbl.find_opt trait_table n with Some s -> add_main_ref t.byte_start t.byte_end s | None -> ());
          scan_refs rest
      | ({ Lex.token_type = Lex.Id n; _ } as t) :: { Lex.token_type = Lex.Bang; _ } :: rest ->
          (match Hashtbl.find_opt macro_table n with Some s -> add_main_ref t.byte_start t.byte_end s | None -> ());
          scan_refs rest
      | _ :: rest -> scan_refs rest
      | [] -> ()
    in
    scan_refs full_tokens;
    (* 6. Extents: a top-level definition ends where the next one starts; its
       locals are visible until then. *)
    let tops = List.sort (fun (_, a) (_, b) -> compare a b) !top_level_main in
    let src_lines = Array.of_list (String.split_on_char '\n' source) in
    let indent_of (l : string) =
      let n = String.length l in
      let rec go i = if i < n && (l.[i] = ' ' || l.[i] = '\t') then go (i + 1) else i in
      go 0
    in
    let blank_or_comment (l : string) =
      let t = String.trim l in
      t = "" || (String.length t >= 2 && String.sub t 0 2 = "//")
    in
    (* A definition starting on line L (indented k) runs until the next line
       indented no deeper than k; its end is the end of the last non-blank
       line before that. *)
    let end_of (lo : int) : position =
      let start = (position_of source (lo - delta)).line in
      let k = indent_of src_lines.(start) in
      let n = Array.length src_lines in
      let rec next m = if m >= n then n else if (not (blank_or_comment src_lines.(m))) && indent_of src_lines.(m) <= k then m else next (m + 1) in
      let stop = next (start + 1) in
      let rec back m = if m > start && blank_or_comment src_lines.(m) then back (m - 1) else m in
      let last = back (stop - 1) in
      { line = last; col = String.length src_lines.(last) }
    in
    List.iter (fun ((s : symbol), lo) -> s.full_end <- Some (end_of lo)) tops;
    List.iter
      (fun (lo, locals) ->
        let e = end_of lo in
        List.iter
          (fun (s : symbol) ->
            match s.sloc with
            | Some l -> s.scope <- Some { file = src_path; start_pos = l.start_pos; end_pos = e }
            | None -> ())
          locals)
      !locals_by_defn;
    finish ()
  with
  | Exit -> finish ()
  | e ->
      Condense.clear_id_queue ();
      add_diag 1 ("internal error in the analysis: " ^ Printexc.to_string e) (main_line_loc 0);
      finish ()
