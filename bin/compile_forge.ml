let () =
  if Array.length Sys.argv < 2 || Array.length Sys.argv > 3 then (
    print_endline "Usage: compile_forge <source.ls> [output_executable]";
    print_endline "Default output: ./a.out";
    exit 1);
  let src = Sys.argv.(1) in
  let out =
    if Array.length Sys.argv = 3 then Sys.argv.(2) else "a.out"
  in
  try
    match Language.Compile_pipeline.compile src out with
    | Ok () -> ()
    | Error msg ->
        print_endline msg;
        exit 1
  with Failure msg ->
    print_endline msg;
    exit 1
