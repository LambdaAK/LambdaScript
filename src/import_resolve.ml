open Expr
open Lex
open Parser.ProgramParser

let path_is_abs (p : string) : bool =
  String.length p > 0 && p.[0] = '/'

let normalize_path (p : string) : string =
  let is_abs = path_is_abs p in
  let parts = String.split_on_char '/' p in
  let stack =
    List.fold_left
      (fun acc part ->
        match part with
        | "" | "." -> acc
        | ".." -> (
            match acc with [] -> [] | _ :: tl -> tl)
        | seg -> seg :: acc)
      [] parts
    |> List.rev
  in
  match (is_abs, stack) with
  | true, [] -> "/"
  | true, xs -> "/" ^ String.concat "/" xs
  | false, [] -> "."
  | false, xs -> String.concat "/" xs

let absolute_path (p : string) : string =
  if Filename.is_relative p then normalize_path (Filename.concat (Sys.getcwd ()) p)
  else normalize_path p

let is_regular_file (p : string) : bool =
  Sys.file_exists p
  &&
  try not (Sys.is_directory p) with _ -> false

let candidate_import_paths ~(base_dir : string) (raw : string) : string list =
  let with_exts =
    if Filename.check_suffix raw ".ls" || Filename.check_suffix raw ".forge" then
      [ raw ]
    else [ raw; raw ^ ".ls"; raw ^ ".forge" ]
  in
  with_exts
  |> List.map (fun p ->
         if Filename.is_relative p then Filename.concat base_dir p else p)
  |> List.map absolute_path
  |> List.sort_uniq String.compare

let resolve_import_target_exn ~(base_dir : string) (raw : string) : string =
  let candidates = candidate_import_paths ~base_dir raw in
  match List.find_opt is_regular_file candidates with
  | Some p -> p
  | None ->
      failwith
        ("forge: import not found: " ^ raw ^ " (looked in "
       ^ String.concat ", " candidates ^ ")")

let read_file (path : string) : string =
  In_channel.with_open_bin path In_channel.input_all

let parse_program_exn ~(path : string) (source : string) : defn list =
  let tokens =
    lex (source |> String.to_seq |> List.of_seq)
    |> List.map (fun t -> t.token_type)
  in
  match program_parser tokens with
  | Some (program, []) -> program
  | Some (_, rem) ->
      failwith
        (Printf.sprintf
           "forge: parse failed for import %s (%d trailing tokens)" path
           (List.length rem))
  | None -> failwith ("forge: parse failed for import " ^ path)

type resolver_state = {
  visiting : (string, unit) Hashtbl.t;
  cache : (string, defn list) Hashtbl.t;
  emitted : (string, unit) Hashtbl.t;
}

let make_state () : resolver_state =
  {
    visiting = Hashtbl.create 64;
    cache = Hashtbl.create 64;
    emitted = Hashtbl.create 64;
  }

let option_map f = function None -> None | Some x -> Some (f x)

let resolve_program ?root_file ~(base_dir : string) (program : defn list) :
    defn list =
  let base_dir = absolute_path base_dir in
  let st = make_state () in
  let root_file = option_map absolute_path root_file in
  (match root_file with
  | Some p -> Hashtbl.replace st.emitted p ()
  | None -> ());
  let rec resolve_file (path : string) : defn list =
    match Hashtbl.find_opt st.cache path with
    | Some cached -> cached
    | None ->
        if Hashtbl.mem st.visiting path then
          failwith ("forge: import cycle detected at " ^ path)
        else (
          Hashtbl.replace st.visiting path ();
          let source = read_file path in
          let parsed = parse_program_exn ~path source in
          let dir = absolute_path (Filename.dirname path) in
          let resolved = resolve_defns ~base_dir:dir parsed in
          Hashtbl.remove st.visiting path;
          Hashtbl.replace st.cache path resolved;
          resolved)
  and resolve_defns ~(base_dir : string) (defns : defn list) : defn list =
    List.concat_map
      (function
        | ImportDef raw_path ->
            let target = resolve_import_target_exn ~base_dir raw_path in
            if Hashtbl.mem st.visiting target then
              failwith ("forge: import cycle detected at " ^ target)
            else if Hashtbl.mem st.emitted target then []
            else (
              Hashtbl.replace st.emitted target ();
              resolve_file target)
        | ModDef (name, nested) ->
            [ ModDef (name, resolve_defns ~base_dir nested) ]
        | d -> [ d ])
      defns
  in
  resolve_defns ~base_dir program
