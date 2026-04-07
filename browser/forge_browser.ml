(** js_of_ocaml bundle: Forge evaluator for static hosting (no Node runner).

    Exposes [window.ForgePlayground.eval(code)] -> JSON string matching the
    playground HTTP API shape, and [reset()] to clear the session. *)

open Js_of_ocaml
open Language.Repl_kernel
open Language.Build_env
module Ceval = Language.Ceval

let print_acc = Buffer.create 256

let () =
  Sys_js.set_channel_flusher stdout (fun s -> Buffer.add_string print_acc s)

let session :
  (Language.Cexpr.static_env * Language.Cexpr.env * Language.Typecheck.type_env) option ref =
  ref None

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

let json_of_outcome ~printed (outcome : eval_outcome) : string =
  let p = json_escape printed in
  match outcome with
  | Ev_error msg ->
      Printf.sprintf {|{"ok":false,"error":"%s","printed":"%s"}|} (json_escape msg) p
  | Ev_expr { typ; value } ->
      Printf.sprintf
        {|{"ok":true,"kind":"expr","value":"%s","type":"%s","printed":"%s"}|}
        (json_escape value) (json_escape typ) p
  | Ev_defs { bindings } ->
      let inner = String.concat "," (List.map json_binding bindings) in
      Printf.sprintf {|{"ok":true,"kind":"defs","bindings":[%s],"printed":"%s"}|} inner p

let ensure_session () =
  match !session with
  | Some triple -> triple
  | None ->
      let static_env = build_full_static_env () in
      let dynamic_env = Ceval.initial_env () |> Ceval.unwrap_eval_result in
      let type_env = [] in
      (match merge_prelude static_env dynamic_env type_env with
      | Ok triple ->
          session := Some triple;
          triple
      | Error msg -> failwith ("Prelude failed: " ^ msg))

let eval_js (code_j : Js.js_string Js.t) : Js.js_string Js.t =
  let code = Js.to_string code_j in
  Buffer.clear print_acc;
  let se, de, te = ensure_session () in
  let outcome, se', de', te' = eval_user_input se de te code in
  (match outcome with Ev_error _ -> () | _ -> session := Some (se', de', te'));
  let printed = Buffer.contents print_acc in
  Js.string (json_of_outcome ~printed outcome)

let reset_js (_ : unit) : unit =
  session := None;
  Buffer.clear print_acc

let () =
  Language.Prelude.set_browser_embedded_prelude Prelude_data.prelude_ls;
  let obj =
    Js.Unsafe.obj
      [| ("eval", Js.Unsafe.inject (Js.wrap_callback eval_js));
         ("reset", Js.Unsafe.inject (Js.wrap_callback reset_js))
      |]
  in
  Js.Unsafe.set Js.Unsafe.global "ForgePlayground" obj
