(* Print the parse tree of a Forge file: parse_file <file.forge>. *)

open Language

let () =
  if Array.length Sys.argv <> 2 then (
    prerr_endline "usage: parse_file <file.forge>";
    exit 2);
  let file = Sys.argv.(1) in
  let source =
    try Compile_pipeline.read_file file with Sys_error msg -> Diag.die ("cannot read " ^ msg)
  in
  match Diag.parse_file_source ~file source with
  | program -> List.iter (fun d -> print_endline (Tostring.string_of_defn d 0)) program
  | exception e -> (
      match Diag.message_of_exn ~file e with
      | Some msg -> Diag.die msg
      | None -> Diag.die ("internal error: " ^ Printexc.to_string e))
