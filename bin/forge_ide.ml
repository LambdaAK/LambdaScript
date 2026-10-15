(** Analysis for editors: [forge_ide path < source] prints the diagnostics,
    symbols and references of the buffer [source] (the contents of [path],
    possibly unsaved) as JSON. See [Language.Ide]. *)

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

let () =
  if Array.length Sys.argv <> 2 then (
    prerr_endline "usage: forge_ide <path> < source";
    exit 2);
  let source = read_all_stdin () in
  print_string (Language.Ide.analyze ~src_path:Sys.argv.(1) ~source)
