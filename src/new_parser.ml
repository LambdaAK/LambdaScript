open Lex
open Expr
open Lazy

(* Idea - Make a module for each level of parser - Use a functor to combine them
   - Use another functor to then combine all of those into condensed parser

   Wrap each parser definition in lazy so that they can be mutually recursive
   without any problems Could also thunk as unit -> 'a parser, which would be
   fully evaluated already, so the mutual recursion would work *)

type 'a parser_result = 'a option
type 'a parser = token_type list -> ('a * token_type list) parser_result
type 'a lazy_parser = unit -> 'a parser

let ( <|> ) (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
 fun tokens ->
  match p1 tokens with
  | Some (result, remaining_tokens) -> Some (result, remaining_tokens)
  | None -> p2 tokens

let ( <* ) (parser_one : 'a parser) (parser_two : 'b parser) : 'b parser =
 fun tokens ->
  match parser_one tokens with
  | Some (_, remaining_tokens) -> parser_two remaining_tokens
  | None -> None

let ( *> ) parser_one parser_two =
 fun tokens ->
  match parser_one tokens with
  | Some (_, remaining_tokens) -> parser_two remaining_tokens
  | None -> None

let ( >>= ) (parser : 'a parser) (f : 'a -> 'b parser) : 'b parser =
 fun tokens ->
  match parser tokens with
  | Some (result, remaining_tokens) -> f result remaining_tokens
  | None -> None

(** * Parser is a parser for 'a * tokens is a list of tokens * Returns a parser
    that parses 'a while possible, and returns the list of 'a *)
let parse_several (parser : 'a parser) : 'a list parser =
  let rec parse_several' acc tokens =
    match parser tokens with
    | Some (result, remaining_tokens) ->
        parse_several' (result :: acc) remaining_tokens
    | None -> Some (List.rev acc, tokens)
  in
  parse_several' []

let parse_sep_delim (parser : 'a parser) (delim : token_type) : 'a list parser =
  let rec parse_sep_delim acc tokens =
    match parser tokens with
    | Some (result, remaining_tokens) -> (
        match remaining_tokens with
        | [] -> Some (List.rev (result :: acc), [])
        | _ -> (
            match List.hd remaining_tokens with
            | t when t = delim -> parse_sep_delim acc (List.tl remaining_tokens)
            | _ -> parse_sep_delim (result :: acc) remaining_tokens))
    | None -> Some (List.rev acc, tokens)
  in

  parse_sep_delim []

let ( let* ) = ( >>= )

let expect_token (expected : token_type) : unit parser =
 fun tokens ->
  match tokens with
  | [] -> None
  | token :: remaining_tokens ->
      if token = expected then Some ((), remaining_tokens) else None

let expect_token_get_data (pred : token_type -> 'a option) : 'a parser =
 fun tokens ->
  match tokens with
  | [] -> None
  | token :: remaining_tokens -> (
      match pred token with
      | Some data -> Some (data, remaining_tokens)
      | None -> None)

let return (result : 'a) : 'a parser = fun tokens -> Some (result, tokens)
let fail : 'a parser = fun _ -> None

let unimplemented_parser (parser_name : string) : 'a parser =
 fun _ -> failwith (parser_name ^ " is unimplemented")

(* factor parsers *)

let rec boolean_parser : factor parser =
  let* b =
    expect_token_get_data (function
      | Boolean b -> Some b
      | _ -> None)
  in
  return (Boolean b)

and string_parser : factor parser =
  let* s =
    expect_token_get_data (function
      | StringToken s -> Some s
      | _ -> None)
  in
  return (String s)

and unit_parser : factor parser =
  let* () = expect_token Unit in
  return Unit

and float_factor_parser : factor parser =
  let* f =
    expect_token_get_data (function
      | FloatToken f -> Some f
      | _ -> None)
  in
  return (FloatFactor f)

and id_parser : factor parser =
  let* id =
    expect_token_get_data (function
      | Id id -> Some id
      | _ -> None)
  in
  return (Id id)

and paren_factor_parser : factor parser =
  unimplemented_parser "paren_factor_parser"

and opposite_parser () : factor parser =
  let* () = expect_token Opposite in
  let* factor = factor_parser in
  return (Opposite factor)

and vector_parser () : factor parser =
  (* The first token should be left paren *)
  let* () = expect_token LParen in
  (* parse a list of expressions seperated by , *)
  let* exprs = parse_sep_delim expr_parser Comma in
  (* the last token should be right paren *)
  let* () = expect_token RParen in
  return (Vector exprs)

and nil_parser : factor parser =
  let* () = expect_token LBracket in
  let* () = expect_token RBracket in
  return Nil

and list_sugar_parser () : factor parser =
  let* () = expect_token LBracket in
  let* exprs = parse_sep_delim expr_parser Comma in
  let* () = expect_token RBracket in
  return (ListSugar exprs)

and list_enumeration_parser () : factor parser =
  unimplemented_parser "list_enumeration_parser"

and list_comprehension_parser () : factor parser =
  unimplemented_parser "list_comprehension_parser"

and factor_parser : factor parser = unimplemented_parser "factor_parser"
and expr_parser : expr parser = unimplemented_parser "expr_parser"
