(** One-shot hover helper for the Forge LSP: reads full source from stdin.

    Usage: [forge_hover path prelude line0 char0] < source.ls

    [prelude] is [1]/[true] to prepend the prelude (default for IDE), or
    [0]/[false] for raw buffer only.

    On success: prints [TYPE\t...] or [DEF\t...] (no trailing newline) to stdout.
    On failure: prints [ERROR: ...] to stdout and exits 1. *)

let read_all_stdin () =
  let b = Buffer.create 4096 in
  let chunk = Bytes.create 8192 in
  let rec loop () =
    let n = input stdin chunk 0 (Bytes.length chunk) in
    if n = 0 then Buffer.contents b
    else (
      Buffer.add_subbytes b chunk 0 n;
      loop ())
  in
  loop ()

let prelude_of_arg = function
  | "0" | "false" | "no" -> false
  | _ -> true

let () =
  if Array.length Sys.argv <> 5 then (
    Printf.eprintf
      "usage: forge_hover <path> <prelude:0|1> <line0> <char0> < source\n";
    exit 2);
  let src_path = Sys.argv.(1) in
  let prelude = prelude_of_arg Sys.argv.(2) in
  let line0 =
    try int_of_string Sys.argv.(3) with Failure _ ->
      Printf.eprintf "forge_hover: bad line\n";
      exit 2
  in
  let char0 =
    try int_of_string Sys.argv.(4) with Failure _ ->
      Printf.eprintf "forge_hover: bad character\n";
      exit 2
  in
  let source = read_all_stdin () in
  match Language.Hover_query.hover_for_position ~prelude ~src_path ~source ~line0 ~char0 with
  | Ok (Language.Hover_query.HoverType, s) -> Printf.printf "TYPE\t%s" s
  | Ok (Language.Hover_query.HoverDefinition, s) -> Printf.printf "DEF\t%s" s
  | Error msg ->
      Printf.printf "ERROR: %s" msg;
      exit 1
