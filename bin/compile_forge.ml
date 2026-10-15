(* Forge native compiler driver: compile_forge <source.forge> [output]. *)

let () =
  if Array.length Sys.argv < 2 || Array.length Sys.argv > 3 then (
    prerr_endline "usage: compile_forge <source.forge> [output_executable]";
    prerr_endline "       (the default output is ./a.out)";
    exit 2);
  let src = Sys.argv.(1) in
  let out = if Array.length Sys.argv = 3 then Sys.argv.(2) else "a.out" in
  match Language.Compile_pipeline.compile src out with
  | Ok () -> ()
  | Error msg -> Language.Diag.die msg
