(** Standard prelude path resolution and source concatenation.

    When compiling or interpreting a user [.ls] file, the prelude text is
    prepended so canonical traits (e.g. [Monad]) are in scope. The REPL loads
    the prelude once at startup via its contents only — it does not prepend
    when evaluating that file again. *)

let prelude_basename = "prelude.ls"

let path_candidates () : string list =
  let cwd = Sys.getcwd () in
  let exe_dir = Filename.dirname Sys.executable_name in
  [
    Filename.concat cwd ("prelude/" ^ prelude_basename);
    Filename.concat cwd prelude_basename;
    Filename.concat (Filename.concat cwd "..") ("prelude/" ^ prelude_basename);
    Filename.concat exe_dir ("../prelude/" ^ prelude_basename);
    Filename.concat exe_dir ("prelude/" ^ prelude_basename);
  ]

let first_existing (paths : string list) : string option =
  List.find_opt Sys.file_exists paths

let resolved_path () : string option = first_existing (path_candidates ())

let contents () : string =
  match resolved_path () with
  | None -> ""
  | Some p ->
      let ic = open_in_bin p in
      Fun.protect
        ~finally:(fun () -> close_in ic)
        (fun () -> really_input_string ic (in_channel_length ic))

let normalize_for_compare (path : string) : string =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path else path

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
