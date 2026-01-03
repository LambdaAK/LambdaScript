open Language.Lex
open Language.Parser.ProgramParser
open Language.Condense
open Language.Js_codegen

let compile input_file output_file =
  let ch = open_in input_file in
  let content = really_input_string ch (in_channel_length ch) in
  close_in ch;

  let tokens = lex (String.to_seq content |> List.of_seq)
               |> List.map (fun t -> t.token_type) in

  match program_parser tokens with
  | Some (program, []) ->
      let condensed = List.map condense_defn program in
      let js = gen_program condensed in
      let out = open_out output_file in
      output_string out js;
      close_out out
  | _ -> failwith "Parse error"

let () =
  if Array.length Sys.argv = 3 then
    compile Sys.argv.(1) Sys.argv.(2)
  else
    Printf.eprintf "Usage: %s input.ls output.js\n" Sys.argv.(0)
