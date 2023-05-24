let main (): unit = 
  let dir: string = Sys.argv.(1) in
  let contents: string = Reader.read dir in
  print_endline contents


let () = main ()