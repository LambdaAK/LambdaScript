(** Diagnostics shared by the command-line drivers (interpreter, compiler,
    REPL, IR dumper): turn the exceptions raised by each stage into one-line
    messages for the user. *)

(** An error inside a specific file (used for imported files). *)
exception Located of string * int * string

let strip_prefix ~prefix s =
  if String.starts_with ~prefix s then
    String.sub s (String.length prefix) (String.length s - String.length prefix)
  else s

(** Remove internal prefixes such as ["forge: "] or ["Other error: "] that the
    individual stages put in front of their messages. *)
let clean (msg : string) : string =
  msg
  |> strip_prefix ~prefix:"forge: "
  |> strip_prefix ~prefix:"parser: "
  |> strip_prefix ~prefix:"Other error: "
  |> strip_prefix ~prefix:"Error: "
  |> String.trim

let located file line msg = Printf.sprintf "%s:%d: %s" file line (clean msg)

(** [message_of_exn ~file e] is the user-facing message for an exception
    raised while processing [file], or [None] for exceptions that indicate an
    internal bug (those are re-raised by the drivers). *)
let message_of_exn ~(file : string) (e : exn) : string option =
  match e with
  | Lex.Lex_error (msg, line) -> Some (located file line msg)
  | Parser.Parse_error (msg, line) -> Some (located file line msg)
  | Located (f, line, msg) -> Some (located f line msg)
  | Failure msg -> Some (clean msg)
  | Sys_error msg -> Some msg
  | Stack_overflow -> Some "stack overflow (recursion too deep)"
  | Out_of_memory -> Some "out of memory"
  | Typecheck.TypeFailure -> Some "type error: type inference failed"
  | _ -> None

let type_error_message (e : Typecheck.type_error) : string =
  "type error: " ^ clean (Typecheck.string_of_type_check_error e)

let runtime_error_message (e : Ceval.eval_error) : string =
  "runtime error: " ^ clean (Ceval.string_of_eval_error e)

(** Print a warning on stderr (after the program's output so far). *)
let warn (msg : string) : unit =
  flush stdout;
  prerr_endline ("warning: " ^ clean msg)

(** Print [error: msg] on stderr (after flushing the program's own output)
    and exit with status 1. *)
let die (msg : string) : 'a =
  flush stdout;
  prerr_endline ("error: " ^ msg);
  exit 1

(** Number of lines contributed by a prepended prefix (the prelude) of
    [prefix_bytes] bytes of [source]. *)
let lines_in_prefix (source : string) (prefix_bytes : int) : int =
  let n = ref 0 in
  for i = 0 to min prefix_bytes (String.length source) - 1 do
    if source.[i] = '\n' then incr n
  done;
  !n

(** Run [f] (which lexes/parses a buffer made of [prefix_lines] lines of
    prelude followed by [file]) and re-raise lexer/parser errors as
    [Located] errors pointing into the right file. *)
let with_source_lines ~(file : string) ?(prefix_lines = 0) (f : unit -> 'a) : 'a =
  let where line =
    if line <= prefix_lines then ("prelude.forge", line) else (file, line - prefix_lines)
  in
  try f () with
  | Lex.Lex_error (msg, line) ->
      let f', l' = where line in
      raise (Located (f', l', msg))
  | Parser.Parse_error (msg, line) ->
      let f', l' = where line in
      raise (Located (f', l', msg))

(** Lex and parse [source], the text of [file] (no prelude prefix). *)
let parse_file_source ~(file : string) (source : string) : Expr.program =
  with_source_lines ~file (fun () ->
      Parser.parse_program_tokens (Lex.lex (source |> String.to_seq |> List.of_seq)))
