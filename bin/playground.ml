(** Long-lived playground runner: length-prefixed UTF-8 on stdin, one JSON line per
    response on stderr (stdout is reserved for [print] / [println] from user code). *)

open Language.Build_env
open Language.Repl_kernel
module Ceval = Language.Ceval

let json_escape (s : string) : string =
  let b = Buffer.create (String.length s + 16) in
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | '"' -> Buffer.add_string b "\\\""
    | '\\' -> Buffer.add_string b "\\\\"
    | '\b' -> Buffer.add_string b "\\b"
    | '\012' -> Buffer.add_string b "\\f"
    | '\n' -> Buffer.add_string b "\\n"
    | '\r' -> Buffer.add_string b "\\r"
    | '\t' -> Buffer.add_string b "\\t"
    | c ->
        let code = Char.code c in
        if code < 32 then Buffer.add_string b (Printf.sprintf "\\u%04x" code)
        else Buffer.add_char b c
  done;
  Buffer.contents b

let json_binding (name, value, typ) =
  Printf.sprintf {|{"name":"%s","value":"%s","type":"%s"}|} (json_escape name)
    (json_escape value) (json_escape typ)

let json_of_outcome (outcome : eval_outcome) : string =
  match outcome with
  | Ev_error msg ->
      Printf.sprintf {|{"ok":false,"error":"%s"}|} (json_escape msg)
  | Ev_expr { typ; value } ->
      Printf.sprintf {|{"ok":true,"kind":"expr","value":"%s","type":"%s"}|}
        (json_escape value) (json_escape typ)
  | Ev_defs { bindings } ->
      let inner = String.concat "," (List.map json_binding bindings) in
      Printf.sprintf {|{"ok":true,"kind":"defs","bindings":[%s]}|} inner

let respond line =
  Printf.eprintf "%s\n%!" line

let read_exact ic n =
  let buf = Bytes.create n in
  let rec loop pos left =
    if left = 0 then Bytes.to_string buf
    else
      let r = input ic buf pos left in
      if r = 0 then raise End_of_file
      else loop (pos + r) (left - r)
  in
  loop 0 n

let read_uint32_be ic =
  let s = read_exact ic 4 in
  (Char.code s.[0] lsl 24)
  lor (Char.code s.[1] lsl 16)
  lor (Char.code s.[2] lsl 8)
  lor Char.code s.[3]

let max_payload = 4 * 1024 * 1024

let init_session () =
  let static_env = build_full_static_env () in
  let dynamic_env = Ceval.initial_env () |> Ceval.unwrap_eval_result in
  let type_env = [] in
  match merge_prelude static_env dynamic_env type_env with
  | Ok triple -> triple
  | Error msg ->
      respond (json_of_outcome (Ev_error msg));
      exit 1

let rec loop se de te =
  let len = read_uint32_be stdin in
  if len > max_payload then (
    respond
      (json_of_outcome
         (Ev_error (Printf.sprintf "Payload too large: %d bytes (max %d)" len max_payload)));
    loop se de te)
  else
    let source = read_exact stdin len in
    try
      let outcome, se', de', te' = eval_user_input se de te source in
      respond (json_of_outcome outcome);
      loop se' de' te'
    with e ->
      let msg = Printexc.to_string e in
      respond (json_of_outcome (Ev_error msg));
      loop se de te

let () =
  Printexc.record_backtrace true;
  try
    let se, de, te = init_session () in
    loop se de te
  with End_of_file -> ()
