(** Standard prelude path resolution and source concatenation.

    When compiling or interpreting a user [.ls] file, the prelude text is
    prepended so canonical traits (e.g. [Monad]) are in scope. The REPL loads
    the prelude once at startup via its contents only — it does not prepend
    when evaluating that file again. *)

let prelude_basename = "prelude.ls"

(** When set (e.g. browser bundle), [contents] returns this string instead of reading the file. *)
let browser_embedded_prelude : string option ref = ref None

let set_browser_embedded_prelude (s : string) : unit = browser_embedded_prelude := Some s

let normalize_abs_path (path : string) : string =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path else path

let ancestor_dirs_from (start : string) : string list =
  let rec go acc dir =
    let dir = normalize_abs_path dir in
    let parent = Filename.dirname dir in
    if List.mem dir acc then List.rev acc
    else if String.equal parent dir then List.rev (dir :: acc)
    else go (dir :: acc) parent
  in
  go [] start

let dedupe_preserve_order (paths : string list) : string list =
  let rec go seen acc = function
    | [] -> List.rev acc
    | p :: rest ->
        if List.mem p seen then go seen acc rest
        else go (p :: seen) (p :: acc) rest
  in
  go [] [] paths

let path_candidates () : string list =
  let roots =
    [
      Sys.getcwd ();
      Filename.dirname (normalize_abs_path Sys.executable_name);
    ]
  in
  let dirs =
    roots |> List.map ancestor_dirs_from |> List.flatten |> dedupe_preserve_order
  in
  List.fold_right
    (fun dir acc ->
      Filename.concat dir ("prelude/" ^ prelude_basename)
      :: Filename.concat dir prelude_basename :: acc)
    dirs []

let first_existing (paths : string list) : string option =
  List.find_opt Sys.file_exists paths

let resolved_path () : string option = first_existing (path_candidates ())

let contents () : string =
  match !browser_embedded_prelude with
  | Some s -> s
  | None -> (
      match resolved_path () with
      | None -> ""
      | Some p ->
          let ic = open_in_bin p in
          Fun.protect
            ~finally:(fun () -> close_in ic)
            (fun () -> really_input_string ic (in_channel_length ic)))

let normalize_for_compare (path : string) : string =
  normalize_abs_path path

(** [true] if [path] is the resolved prelude file — avoid self-prepend when
    compiling the prelude. *)
let is_prelude_path (path : string) : bool =
  match resolved_path () with
  | None -> false
  | Some pre ->
      String.equal (normalize_for_compare path) (normalize_for_compare pre)

(** Prepend prelude to program source for compile / interpret. *)
let prepend_to_source ?(enabled = true) ~(src_path : string) (source : string) :
    string =
  if not enabled || is_prelude_path src_path then source
  else
    let p = contents () in
    if p = "" then source else p ^ "\n" ^ source

(** Number of top-level definitions when [frag] parses as a program (used for
    the exact [full_source] prefix [String.sub full_source 0 delta] so the
    prelude boundary matches lexer byte positions in that buffer — avoids a
    second read of the prelude file disagreeing with [prepend_to_source]. *)
let defn_count_for_source_fragment (frag : string) : int =
  if frag = "" then 0
  else
    let open Lex in
    let open Parser.ProgramParser in
    let tokens = lex (frag |> String.to_seq |> List.of_seq) in
    match program_parser (List.map (fun t -> t.token_type) tokens) with
    | Some (prog, _) -> List.length prog
    | None -> 0

(** Number of top-level definitions when the prelude text is parsed alone (with
    the same [p ^ "\\n"] separator as [prepend_to_source]). Used to turn on
    user-region id spans after the prelude block in [condense_program]. *)
let defn_count_when_parsed () : int =
  let p = contents () in
  if p = "" then 0 else defn_count_for_source_fragment (p ^ "\n")
