(* Print the Min IR the native compiler produces for a .forge file. *)

let () =
  if Array.length Sys.argv <> 2 then (
    prerr_endline "usage: dump_min_ir <file.forge>";
    exit 2)
  else
    match Language.Compile_pipeline.min_ir_of_file Sys.argv.(1) with
    | Ok prog -> print_endline (Language.Min_ir.string_of_prog prog)
    | Error msg -> Language.Diag.die msg
