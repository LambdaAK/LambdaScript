type token_type =
  | Integer of int
  | FloatToken of float
  | Boolean of bool
  | StringToken of string
  | Unit
  | Id of string
  | TypeVar of string
  | Assign
  | Fn
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
  | End
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
  | UnitType
  | FloatType
  | LBracket
  | RBracket
  | Bind
  | In
  | BindArrow
  | LBrace
  | RBrace
  | Let
  | Rec
  | Comma
  | WildcardPattern
  | ConsToken
  | Semicolon
  | Enum
  | Equals
  | Relop of string (* start with = < or >, and are not = *)
  | Addop of string (* start with + or - *)
  | Mulop of string (* start with * / or % *)
  | Type
  | TypeVariable of string
  | LAngle
  | RAngle

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
  | StringToken s -> "<string: " ^ s ^ ">"
  | FloatToken f ->
      let s : string = string_of_float f in
      "<float: " ^ s ^ ">"
  | Unit -> "<unit>"
  | FloatType -> "<float type>"
  | Id s -> "<id: " ^ s ^ ">"
  | Fn -> "<fn>"
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
  | UnitType -> "<unit type>"
  | LBracket -> "<left bracket>"
  | RBracket -> "<right bracket>"
  | Bind -> "<bind>"
  | BindArrow -> "<bind arrow>"
  | In -> "<in>"
  | LBrace -> "<lbrace>"
  | RBrace -> "<rbrace>"
  | Let -> "<let>"
  | Rec -> "<rec>"
  | Comma -> "<comma>"
  | WildcardPattern -> "<wildcard pattern>"
  | TypeVar s -> "<type var: " ^ s ^ ">"
  | ConsToken -> "<cons token>"
  | Pipe -> "<pipe>"
  | Switch -> "<switch>"
  | End -> "<end>"
  | Semicolon -> "<semicolon>"
  | Enum -> "<enum>"
  | Relop s -> "<relop: " ^ s ^ ">"
  | Addop s -> "<addop: " ^ s ^ ">"
  | Mulop s -> "<mulop: " ^ s ^ ">"
  | Equals -> "<equals>"
  | Type -> "<type>"
  | TypeVariable s -> "<type variable: " ^ s ^ ">"
  | LAngle -> "<"
  | RAngle -> ">"
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
  | '+' | '-' | '*' | '/' | '%' | '<' | '>' | '=' -> true
  | _ -> false

let is_special =
  (* + - * / % < > = & | : ; , *)
  function
  | '+' | '-' | '*' | '/' | '%' | '<' | '>' | '=' | '&' | '|' | ':' | ';' | ','
    -> true
  | _ -> false

let is_addop_prefix = function
  | '+' | '-' -> true
  | _ -> false

let is_mulop_prefix = function
  | '*' | '/' | '%' -> true
  | _ -> false

let is_relop_prefix = function
  | '<' | '>' | '=' -> true
  | _ -> false

(* whenever there's a special character, parse an operator *)

let bop_from_char_list (lst : char list) =
  let s : string = List.fold_left (fun acc c -> acc ^ String.make 1 c) "" lst in

  if s = "->" then Arrow
  else if s = "<-" then BindArrow
  else if s = "=>" then SwitchArrow
  else if s = "=" then Equals
  else
    match lst with
    | h :: _ when is_addop_prefix h -> Addop s
    | h :: _ when is_mulop_prefix h -> Mulop s
    | h :: _ when is_relop_prefix h -> Relop s
    | _ -> failwith "invalid bop passed to bop_from_char_list"

let lex_bop (lst : char list) =
  let rec get_bop_chars (lst : char list) (acc : char list) :
      char list * char list =
    match lst with
    | h :: t when is_special h ->
        (* Don't combine '>' with another '>' to allow nested type applications like Pair<Pair<int>> *)
        (match (acc, h) with
        | '>' :: _, '>' -> (List.rev acc, lst)
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
    ("int", IntegerType);
    ("bool", BooleanType);
    ("str", StringType);
    ("unit", UnitType);
    ("if", If);
    ("then", Then);
    ("else", Else);
    ("in", In);
    ("let", Let);
    ("rec", Rec);
    ("bind", Bind);
    ("switch", Switch);
    ("end", End);
    ("enum", Enum);
    ("float", FloatType);
    ("type", Type);
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
        | h :: _ when is_alpha_num h -> (None, [])
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

let rec lex_id (lst : char list) (acc : string) : token * char list =
  (* the first char has to be a letter the following characters can be letters,
     numbers, or underscores *)
  match lst with
  | c :: t when is_letter c -> lex_id t (acc ^^ c) (* a letter *)
  | c :: t when is_num c && not (acc = "") ->
      lex_id t (acc ^^ c) (* a digit, and not the first character *)
  | _ -> ({ token_type = Id acc; line = 0 }, lst)

let is_lowercase : char -> bool = function
  | 'a'
  | 'b'
  | 'c'
  | 'd'
  | 'e'
  | 'f'
  | 'g'
  | 'h'
  | 'i'
  | 'j'
  | 'k'
  | 'l'
  | 'm'
  | 'n'
  | 'o'
  | 'p'
  | 'q'
  | 'r'
  | 's'
  | 't'
  | 'u'
  | 'v'
  | 'w'
  | 'x'
  | 'y'
  | 'z' -> true
  | _ -> false

let rec lex_type_var (tokens_after_single_quote : char list) (acc : string) :
    token * char list =
  match tokens_after_single_quote with
  | c :: t when is_lowercase c -> lex_type_var t (acc ^ string_of_char c)
  | _ -> ({ token_type = TypeVar acc; line = 0 }, tokens_after_single_quote)

(* Helper functions for token creation and emission *)
let make_token line_number token_type = { token_type; line = line_number }

let emit_token line_number token_type remaining_chars lex_fn =
  make_token line_number token_type :: lex_fn remaining_chars

(* Multi-character sequences that need to be checked before single chars.
   Order matters: longer sequences should come before shorter ones. *)
let multi_char_sequences =
  [
    (['('; ')'], Unit);
    (['.'; '.'; '.'], Enum);
    ([':'; ':'], ConsToken);
    (['|'; '|'], OR);
    (['&'; '&'], AND);
    (['~'; '-'], Opposite);
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
    ('\\', Fn);
  ]

(* Try to match a multi-character sequence *)
let rec try_multi_char_sequence lst sequences =
  match sequences with
  | [] -> None
  | (char_seq, token_type) :: rest ->
      let rec matches chars tokens =
        match (chars, tokens) with
        | [], remaining -> Some (token_type, remaining)
        | c :: cs, t :: ts when c = t -> matches cs ts
        | _ -> None
      in
      (match matches char_seq lst with
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
            (* Special case: LParen needs to check that next char is not ')' *)
            | '(' :: c :: t when c <> ')' ->
                emit_token !line_number LParen (c :: t) lex
            (* Type variables starting with single quote *)
            | '\'' :: tokens_after_single_quote ->
                let type_var_token, tokens_after_type_var =
                  lex_type_var tokens_after_single_quote ""
                in
                type_var_token :: lex tokens_after_type_var
            (* String literals *)
            | '"' :: c :: t ->
                if c = '"' then
                  emit_token !line_number (StringToken "") t lex
                else
                  let new_token, remainder = lex_string (c :: t) "" in
                  new_token :: lex remainder
            (* Operators (including binary operators) *)
            | h :: _ when is_bop_prefix h ->
                let bop, chars_after = lex_bop lst in
                make_token !line_number bop :: lex chars_after
            (* Numbers (including floats) *)
            | n :: _ when is_num_or_dot n ->
                let num_token, tail = lex_num lst "" in
                num_token :: lex tail
            (* Identifiers (also handles keywords, but those are checked earlier) *)
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
