type token_type =
  | Integer of int
  | FloatToken of float
  | Boolean of bool
  | CharToken of char
  | StringToken of string
  | Unit
  | Id of string
  | Assign
  | Fn
  | For
  | Arrow
  | If
  | Then
  | Else
  | LParen
  | RParen
  | Colon
  | SwitchArrow
  | Pipe
  | Switch
  | Case
  | Do
  | Plus
  | Minus
  | Times
  | Divide
  | Mod
  | Opposite
  | LT
  | GT
  | LE
  | GE
  | EQ
  | NE
  | AND
  | OR
  | IntegerType
  | BooleanType
  | StringType
  | CharType
  | UnitType
  | FloatType
  | LBracket
  | RBracket
  | In
  | BindArrow
  | LBrace
  | RBrace
  | Let
  | Rec
  | And
  | Comma
  | WildcardPattern
  | ConsToken
  | Semicolon
  | Enum
  | Equals
  | Relop of string (* start with = < or >, and are not = *)
  | Addop of string (* start with + or - *)
  | Mulop of string (* start with * / or % *)
  | Logop of string (* start with | or & *)
  | Type
  | LAngle
  | RAngle
  | Of
  | Dot
  | With
  | Inter
  | Trait
  | Impl
  | Val
  | Where
  | End
  | Requires

type token = {
  token_type : token_type;
  line : int;
}

let string_of_token_type : token_type -> string = function
  | Boolean b -> let s : string = string_of_bool b in

                 "<boolean: " ^ s ^ ">"
  | Integer n ->
      let s : string = string_of_int n in
      "<integer: " ^ s ^ ">"
  | CharToken c -> "<char: " ^ String.make 1 c ^ ">"
  | StringToken s -> "<string: " ^ s ^ ">"
  | FloatToken f ->
      let s : string = string_of_float f in
      "<float: " ^ s ^ ">"
  | Unit -> "<unit>"
  | FloatType -> "<float type>"
  | Id s -> "<id: " ^ s ^ ">"
  | Fn -> "<fn>"
  | For -> "<for>"
  | Arrow -> "<arrow>"
  | Assign -> "<assign>"
  | If -> "<if>"
  | Then -> "<then>"
  | Else -> "<else>"
  | LParen -> "<lparen>"
  | RParen -> "<rparen>"
  | Colon -> "<colon>"
  | SwitchArrow -> "<switch arrow>"
  | Plus -> "<plus>"
  | Minus -> "<minus>"
  | Times -> "<times>"
  | Divide -> "<divide>"
  | Mod -> "<mod>"
  | Opposite -> "<opposite>"
  | LT -> "<LT>"
  | GT -> "<GT>"
  | LE -> "<LE>"
  | GE -> "<GE>"
  | EQ -> "<EQ>"
  | NE -> "<NE>"
  | OR -> "<or>"
  | AND -> "<and>"
  | IntegerType -> "<integer type>"
  | BooleanType -> "<boolean type>"
  | StringType -> "<string type>"
  | CharType -> "<char type>"
  | UnitType -> "<unit type>"
  | LBracket -> "<left bracket>"
  | RBracket -> "<right bracket>"
  | BindArrow -> "<bind arrow>"
  | In -> "<in>"
  | LBrace -> "<lbrace>"
  | RBrace -> "<rbrace>"
  | Let -> "<let>"
  | Rec -> "<rec>"
  | And -> "<and>"
  | Comma -> "<comma>"
  | WildcardPattern -> "<wildcard pattern>"
  | ConsToken -> "<cons token>"
  | Pipe -> "<pipe>"
  | Switch -> "<switch>"
  | Case -> "<case>"
  | Do -> "<do>"
  | Semicolon -> "<semicolon>"
  | Enum -> "<enum>"
  | Relop s -> "<relop: " ^ s ^ ">"
  | Addop s -> "<addop: " ^ s ^ ">"
  | Mulop s -> "<mulop: " ^ s ^ ">"
  | Logop s -> "<logop: " ^ s ^ ">"
  | Equals -> "<equals>"
  | Type -> "<type>"
  | LAngle -> "<"
  | RAngle -> ">"
  | Of -> "<of>"
  | Dot -> "<dot>"
  | With -> "<with>"
  | Inter -> "<inter>"
  | Trait -> "<trait>"
  | Impl -> "<impl>"
  | Val -> "<val>"
  | Where -> "<where>"
  | End -> "<end>"
  | Requires -> "<requires>"
[@@coverage off]

let string_of_token : token -> string =
 fun { token_type; _ } -> string_of_token_type token_type

let list_of_string (s : string) = s |> String.to_seq |> List.of_seq

exception LexFailure

let is_num : char -> bool = function
  | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true
  | _ -> false

let int_from_char : char -> int = function
  | '0' -> 0
  | '1' -> 1
  | '2' -> 2
  | '3' -> 3
  | '4' -> 4
  | '5' -> 5
  | '6' -> 6
  | '7' -> 7
  | '8' -> 8
  | '9' -> 9
  | _ -> failwith "not an int passed to int_from_char"

(* [65, 90]: uppercase [97, 122]: lowercase 95: underscore *)
let is_letter : char -> bool =
 fun (c : char) ->
  let code : int = Char.code c in
  if (code >= 65 && code <= 90) || (code >= 97 && code <= 122) || code = 95 then
    true
  else false

let is_alpha_num (c : char) = is_letter c || is_num c

let is_bop_prefix : char -> bool = function
  | '+' | '-' | '*' | '/' | '%' | '<' | '>' | '=' | '^' | '|' | '!' -> true
  | _ -> false

let is_special =
  function
  | '+' | '-' | '*' | '/' | '%' | '<' | '>' | '=' | '&' | '|' | ':' | ';' | ',' | '^' | '!'
    -> true
  | _ -> false

let is_addop_prefix = function
  | '+' | '-' | '^' -> true
  | _ -> false

let is_mulop_prefix = function
  | '*' | '/' | '%' -> true
  | _ -> false

let is_relop_prefix = function
  | '<' | '>' | '=' | '!' -> true
  | _ -> false

let is_logop_prefix = function
  | '|' | '&' -> true
  | _ -> false

(* Try to parse a parenthesized operator like (++) or (+++)
   Returns Some (operator_string, remaining_chars) if successful, None otherwise *)
let try_parse_parenthesized_operator (lst : char list) : (string * char list) option =
  let rec collect_operator_chars (lst : char list) (acc : char list) : (char list * char list) option =
    match lst with
    | ')' :: t when acc <> [] -> Some (List.rev acc, t)  (* Found closing paren with operator chars *)
    | h :: t when is_bop_prefix h -> collect_operator_chars t (h :: acc)  (* Keep collecting operator chars *)
    | _ -> None  (* Not an operator or invalid *)
  in
  match lst with
  | h :: _ when is_bop_prefix h ->
      (match collect_operator_chars lst [] with
       | Some (op_chars, remaining) ->
           let op_string = List.fold_left (fun acc c -> acc ^ String.make 1 c) "" op_chars in
           Some (op_string, remaining)
       | None -> None)
  | _ -> None

(* whenever there's a special character, parse an operator *)

let bop_from_char_list (lst : char list) =
  let s : string = List.fold_left (fun acc c -> acc ^ String.make 1 c) "" lst in

  if s = "->" then Arrow
  else if s = "<-" then BindArrow
  else if s = "=>" then SwitchArrow
  else if s = "=" then Equals
  else if s = "|" then Pipe  (* Single | should be Pipe, not an operator *)
  else
    match lst with
    | h :: _ when is_addop_prefix h -> Addop s
    | h :: _ when is_mulop_prefix h -> Mulop s
    | h :: _ when is_relop_prefix h -> Relop s
    | h :: _ when is_logop_prefix h -> Logop s
    | _ -> failwith "invalid bop passed to bop_from_char_list"

let lex_bop (lst : char list) =
  let rec get_bop_chars (lst : char list) (acc : char list) :
      char list * char list =
    match lst with
    | h :: t when is_special h -> (
        (* Don't combine '>' with another '>' to allow nested type applications
           like Pair<Pair<int>> *)
        (* But DO combine if there's another operator char after, like >>= *)
        (* Also don't combine operators with standalone delimiters like , ; : *)
        match (acc, h) with
        | '>' :: _, '>' -> (
            (* Check if there's another operator character after the second > *)
            (* We want to allow >>= but not >>> (for nested types like Box<Box<Box<int>>>) *)
            match t with
            | next :: _ when is_special next && next <> '>' && next <> ',' && next <> ';' && next <> ':' ->
                (* There's another operator char (not >), so keep building (e.g., >>= ) *)
                get_bop_chars t (h :: acc)
            | _ ->
                (* No continuation or next is >, stop here to allow Pair<Pair<int>> *)
                (List.rev acc, lst))
        | _ :: _, (',' | ';' | ':') -> (List.rev acc, lst)
        | [], (',' | ';' | ':') -> (List.rev acc, lst)
        | _ -> get_bop_chars t (h :: acc))
    | _ ->
        (* no more chars are added to the bop *)
        (List.rev acc, lst)
  in
  let bop_chars, remaining_chars = get_bop_chars lst [] in
  (bop_from_char_list bop_chars, remaining_chars)

let string_of_char = String.make 1
let ( ^^ ) (s : string) (c : char) = s ^ string_of_char c

let keywords =
  [
    ("true", Boolean true);
    ("false", Boolean false);
    ("Int", IntegerType);
    ("Bool", BooleanType);
    ("String", StringType);
    ("Char", CharType);
    ("Unit", UnitType);
    ("if", If);
    ("then", Then);
    ("else", Else);
    ("in", In);
    ("let", Let);
    ("rec", Rec);
    ("and", And);
    ("fn", Fn);
    ("for", For);
    ("switch", Switch);
    ("case", Case);
    ("do", Do);
    ("enum", Enum);
    ("Float", FloatType);
    ("of", Of);
    ("type", Type);
    ("val", Val);
    ("with", With);
    (* [inter] shares a prefix with [in]; [in] must appear earlier so lexing
       retries and eventually matches [inter]. *)
    ("inter", Inter);
    ("trait", Trait);
    ("impl", Impl);
    ("where", Where);
    ("end", End);
    ("requires", Requires);
  ]
  |> List.map (fun (s, t) -> (list_of_string s, t))

let rec is_prefix (prefix : 'a list) (lst : 'a list) : bool * char list =
  match (prefix, lst) with
  (* if the prefix is [], return true, lst *)
  | [], _ -> (true, lst)
  (* if the prefix is not [], and the list is [], return false, [] *)
  | _, [] -> (false, [])
  | h1 :: t1, h2 :: t2 -> if h1 = h2 then is_prefix t1 t2 else (false, [])

let rec find_leading_keyword_if_it_exists (lst : char list) kw :
    token_type option * char list =
  (* a keyword is leading if the following holds

     it is a prefix of the list of chars the next character after the keyword is
     not a letter, number, or underscore *)
  match kw with
  | [] -> (None, [])
  | (keyword, token_type) :: t ->
      let is_prefix, remainder = is_prefix keyword lst in
      if is_prefix then
        match remainder with
        | [] -> (Some token_type, [])
        | h :: _ when is_alpha_num h ->
            (* Shorter keyword prefix (e.g. [in] vs [inter]); try next keyword. *)
            find_leading_keyword_if_it_exists lst t
        | _ -> (Some token_type, remainder)
      else find_leading_keyword_if_it_exists lst t

let rec lex_int (lst : char list) (acc : int) : token * char list =
  match lst with
  | n :: t when is_num n ->
      let n_int : int = int_from_char n in
      lex_int t ((acc * 10) + n_int)
  | _ -> ({ token_type = Integer acc; line = 0 }, lst)

let is_num_or_dot : char -> bool = function
  | '.' -> true
  | c -> is_num c

let rec lex_num (lst : char list) (acc : string) : token * char list =
  match lst with
  | n :: t when is_num_or_dot n ->
      let n_string : string = string_of_char n in
      lex_num t (acc ^ n_string)
  | _ ->
      (* if theres a ., it's an int if there is not a ., it's a float *)
      if String.contains acc '.' then
        ({ token_type = FloatToken (float_of_string acc); line = 0 }, lst)
      else ({ token_type = Integer (int_of_string acc); line = 0 }, lst)

let rec lex_string (lst : char list) (acc : string) : token * char list =
  match lst with
  | '"' :: t -> ({ token_type = StringToken acc; line = 0 }, t)
  | '\\' :: '"' :: t -> lex_string t (acc ^ "\"")
  | c :: t ->
      let char_string : string = string_of_char c in

      lex_string t (acc ^ char_string)
  | [] -> failwith "expected closing double quote in lexing string"

let decode_char_escape = function
  | '\\' -> '\\'
  | '\'' -> '\''
  | 'n' -> '\n'
  | 't' -> '\t'
  | 'r' -> '\r'
  | c -> c

let lex_char_literal (lst : char list) : token * char list =
  match lst with
  | '\\' :: escaped :: '\'' :: rest ->
      let ch = decode_char_escape escaped in
      ({ token_type = CharToken ch; line = 0 }, rest)
  | c :: '\'' :: rest -> ({ token_type = CharToken c; line = 0 }, rest)
  | _ -> failwith "expected closing single quote in lexing char"

let rec lex_id (lst : char list) (acc : string) : token * char list =
  (* the first char has to be a letter the following characters can be letters,
     numbers, or underscores *)
  match lst with
  | c :: t when is_letter c -> lex_id t (acc ^^ c) (* a letter *)
  | c :: t when is_num c && not (acc = "") ->
      lex_id t (acc ^^ c) (* a digit, and not the first character *)
  | _ -> ({ token_type = Id acc; line = 0 }, lst)

(* Helper functions for token creation and emission *)
let make_token line_number token_type = { token_type; line = line_number }

let emit_token line_number token_type remaining_chars lex_fn =
  make_token line_number token_type :: lex_fn remaining_chars

(* Multi-character sequences that need to be checked before single chars. Order
   matters: longer sequences should come before shorter ones. *)
let multi_char_sequences =
  [
    ([ '('; ')' ], Unit);
    ([ '.'; '.'; '.' ], Enum);
    ([ ':'; ':' ], ConsToken);
    ([ '|'; '|' ], OR);
    ([ '&'; '&' ], AND);
    ([ '~'; '-' ], Opposite);
  ]

(* Single-character tokens *)
let single_char_tokens =
  [
    (';', Semicolon);
    (':', Colon);
    (',', Comma);
    ('|', Pipe);
    ('[', LBracket);
    (']', RBracket);
    ('{', LBrace);
    ('}', RBrace);
    (')', RParen);
    ('_', WildcardPattern);
    ('.', Dot);
  ]

(* Try to match a multi-character sequence *)
let rec try_multi_char_sequence lst sequences =
  match sequences with
  | [] -> None
  | (char_seq, token_type) :: rest -> (
      let rec matches chars tokens =
        match (chars, tokens) with
        | [], remaining -> Some (token_type, remaining)
        | c :: cs, t :: ts when c = t -> matches cs ts
        | _ -> None
      in
      match matches char_seq lst with
      | Some result -> Some result
      | None -> try_multi_char_sequence lst rest)

(* Try to match a single-character token *)
let try_single_char_token lst tokens =
  match lst with
  | [] -> None
  | h :: t ->
      let rec find_token = function
        | [] -> None
        | (ch, token_type) :: rest ->
            if ch = h then Some (token_type, t) else find_token rest
      in
      find_token tokens

(* Skip single-line comment (// comment until end of line) *)
let rec skip_single_line_comment (lst : char list) : char list =
  match lst with
  | [] -> []
  | '\n' :: t -> '\n' :: t  (* Keep the newline for line counting *)
  | _ :: t -> skip_single_line_comment t

(* Skip multi-line comment (/* comment */)
   Returns (remaining_chars, newline_count) *)
let rec skip_multi_line_comment (lst : char list) (newlines : int) : char list * int =
  match lst with
  | [] -> failwith "Unclosed multi-line comment"
  | '*' :: '/' :: t -> (t, newlines)  (* End of multi-line comment *)
  | '\n' :: t -> skip_multi_line_comment t (newlines + 1)
  | _ :: t -> skip_multi_line_comment t newlines

let lex (lst : char list) : token list =
  let line_number : int ref = ref 1 in
  let rec lex (lst : char list) : token list =
    (* Check for keywords first *)
    match find_leading_keyword_if_it_exists lst keywords with
    | Some token_type, remainder ->
        make_token !line_number token_type :: lex remainder
    | _ -> (
        (* Try multi-character sequences first *)
        match try_multi_char_sequence lst multi_char_sequences with
        | Some (token_type, remaining) ->
            emit_token !line_number token_type remaining lex
        | None -> (
            match lst with
            | [] -> []
            (* Whitespace *)
            | ' ' :: t -> lex t
            | '\t' :: t -> lex t
            | '\n' :: t ->
                line_number := !line_number + 1;
                lex t
            (* Left parenthesis *)
            | '(' :: rest -> emit_token !line_number LParen rest lex
            (* Char literals: 'x' or '\n' etc. (type variables are plain ids: a, b, …) *)
            | '\'' :: tokens_after_single_quote -> (
                match tokens_after_single_quote with
                | c :: '\'' :: rest ->
                    let char_token =
                      { token_type = CharToken c; line = !line_number }
                    in
                    char_token :: lex rest
                | '\\' :: _ ->
                    let char_token, remainder = lex_char_literal tokens_after_single_quote in
                    { char_token with line = !line_number } :: lex remainder
                | _ ->
                    failwith
                      "Lex error: expected character literal 'c' or escape after single quote")
            (* String literals *)
            | '"' :: c :: t ->
                if c = '"' then emit_token !line_number (StringToken "") t lex
                else
                  let new_token, remainder = lex_string (c :: t) "" in
                  new_token :: lex remainder
            (* Comments and operators starting with / need special handling *)
            (* Check for // comment, but only if not followed by more operator chars *)
            | '/' :: '/' :: c :: _ when is_bop_prefix c ->
                (* This is an operator like ///, not a comment *)
                let bop, chars_after = lex_bop lst in
                make_token !line_number bop :: lex chars_after
            | '/' :: '/' :: t ->
                (* This is a comment *)
                lex (skip_single_line_comment t)
            (* Multi-line comment: /* */ *)
            | '/' :: '*' :: t ->
                let remaining, newlines = skip_multi_line_comment t 0 in
                line_number := !line_number + newlines;
                lex remaining
            (* Operators (including binary operators) *)
            | h :: _ when is_bop_prefix h ->
                let bop, chars_after = lex_bop lst in
                make_token !line_number bop :: lex chars_after
            (* Numbers (including floats) - only start with digits, not dots *)
            | n :: _ when is_num n ->
                let num_token, tail = lex_num lst "" in
                num_token :: lex tail
            (* Identifiers (also handles keywords, but those are checked
               earlier) *)
            | c :: _ when is_letter c ->
                let id_token, tail = lex_id lst "" in
                id_token :: lex tail
            (* Try single-character tokens *)
            | _ -> (
                match try_single_char_token lst single_char_tokens with
                | Some (token_type, remaining) ->
                    emit_token !line_number token_type remaining lex
                | None -> failwith "no token matched")))
  in

  let tokens = lex lst in

  tokens

let rec remove_line_numbers (tokens : token list) : token_type list =
  match tokens with
  | [] -> []
  | h :: t -> h.token_type :: remove_line_numbers t
[@@coverage off]

let rec print_tokens_list : token list -> unit = function
  | [] -> ()
  | token :: tail ->
      token |> string_of_token |> print_endline;
      print_tokens_list tail
