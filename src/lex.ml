type token_type =
  | Integer of int
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

type token = {
  token_type : token_type;
  line : int;
}

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
let string_of_char = String.make 1
let ( ^^ ) (s : string) (c : char) = s ^ string_of_char c

let keywords : (string * token_type) list =
  [
    ("true", Boolean true);
    ("false", Boolean false);
    ("int", IntegerType);
    ("bool", BooleanType);
    ("string", StringType);
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
  ]

let rec lex_int (lst : char list) (acc : int) : token * char list =
  match lst with
  | n :: t when is_num n ->
      let n_int : int = int_from_char n in
      lex_int t ((acc * 10) + n_int)
  | _ -> ({ token_type = Integer acc; line = 0 }, lst)

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

(* A type variable has the following form 'x where x is an identifier *)

let lex_type_var (tokens_after_single_quote : char list) : token * char list =
  match lex_id tokens_after_single_quote "" with
  | { token_type = Id i; line = _ }, tokens_after_type_var ->
      ({ token_type = TypeVar i; line = 0 }, tokens_after_type_var)
  | _ -> failwith "expected an identifier after single quote"

let lex (lst : char list) : token list =
  let line_number : int ref = ref 1 in
  let rec lex (lst : char list) : token list =
    match lst with
    | [] -> []
    | ' ' :: t -> lex t (* ignore white space *)
    | '\n' :: t ->
        line_number := !line_number + 1;
        lex t (* ignore new lines, increment the line number *)
    | 's' :: 'w' :: 'i' :: 't' :: 'c' :: 'h' :: t ->
        let new_token : token = { token_type = Switch; line = !line_number } in
        new_token :: lex t
    | '.' :: '.' :: '.' :: t ->
        let new_token : token = { token_type = Enum; line = !line_number } in
        new_token :: lex t
    | 'e' :: 'n' :: 'd' :: t ->
        let new_token : token = { token_type = End; line = !line_number } in
        new_token :: lex t
    | 't' :: 'r' :: 'u' :: 'e' :: t ->
        { token_type = Boolean true; line = !line_number } :: lex t
    | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: t ->
        { token_type = Boolean false; line = !line_number } :: lex t
    | ':' :: ':' :: t ->
        let new_token : token =
          { token_type = ConsToken; line = !line_number }
        in
        new_token :: lex t
    | ';' :: t ->
        let new_token : token =
          { token_type = Semicolon; line = !line_number }
        in
        new_token :: lex t
    | 'i' :: 'n' :: 't' :: t ->
        { token_type = IntegerType; line = !line_number } :: lex t
    | 'b' :: 'o' :: 'o' :: 'l' :: t ->
        let new_token : token =
          { token_type = BooleanType; line = !line_number }
        in
        new_token :: lex t
    | 's' :: 't' :: 'r' :: t ->
        let new_token : token =
          { token_type = StringType; line = !line_number }
        in
        new_token :: lex t
    | 'u' :: 'n' :: 'i' :: 't' :: t ->
        let new_token : token =
          { token_type = UnitType; line = !line_number }
        in
        new_token :: lex t
    | '\'' :: tokens_after_single_quote ->
        let type_var_token, tokens_after_type_var =
          lex_type_var tokens_after_single_quote
        in
        type_var_token :: lex tokens_after_type_var
    | '_' :: t ->
        let new_token : token =
          { token_type = WildcardPattern; line = !line_number }
        in
        new_token :: lex t
    | 'i' :: 'n' :: t ->
        let new_token : token = { token_type = In; line = !line_number } in
        new_token :: lex t
    | 'r' :: 'e' :: 'c' :: t ->
        let new_token : token = { token_type = Rec; line = !line_number } in
        new_token :: lex t
    | 'b' :: 'i' :: 'n' :: 'd' :: t ->
        let new_token : token = { token_type = Bind; line = !line_number } in
        new_token :: lex t
    | 'l' :: 'e' :: 't' :: t ->
        let new_token : token = { token_type = Let; line = !line_number } in
        new_token :: lex t
    | '\\' :: t ->
        let new_token : token = { token_type = Fn; line = !line_number } in
        new_token :: lex t
    | 'i' :: 'f' :: t ->
        let new_token : token = { token_type = If; line = !line_number } in
        new_token :: lex t
    | 't' :: 'h' :: 'e' :: 'n' :: t ->
        let new_token : token = { token_type = Then; line = !line_number } in
        new_token :: lex t
    | 'e' :: 'l' :: 's' :: 'e' :: t ->
        let new_token : token = { token_type = Else; line = !line_number } in
        new_token :: lex t
    | '(' :: c :: t when c <> ')' ->
        let new_token : token = { token_type = LParen; line = !line_number } in
        new_token :: lex (c :: t)
    | ')' :: t ->
        let new_token : token = { token_type = RParen; line = !line_number } in
        new_token :: lex t
    | '=' :: '>' :: t ->
        let new_token : token =
          { token_type = SwitchArrow; line = !line_number }
        in
        new_token :: lex t
    | '-' :: '>' :: t ->
        let new_token : token = { token_type = Arrow; line = !line_number } in
        new_token :: lex t
    | '<' :: '-' :: t ->
        let new_token : token =
          { token_type = BindArrow; line = !line_number }
        in
        new_token :: lex t
    | ':' :: t ->
        let new_token : token = { token_type = Colon; line = !line_number } in
        new_token :: lex t
    | '(' :: ')' :: t ->
        let new_token : token = { token_type = Unit; line = !line_number } in
        new_token :: lex t
    | '+' :: t ->
        let new_token : token = { token_type = Plus; line = !line_number } in
        new_token :: lex t
    | '~' :: '-' :: t ->
        let new_token : token =
          { token_type = Opposite; line = !line_number }
        in
        new_token :: lex t
    | '-' :: t ->
        let new_token : token = { token_type = Minus; line = !line_number } in
        new_token :: lex t
    | '*' :: t ->
        let new_token : token = { token_type = Times; line = !line_number } in
        new_token :: lex t
    | '/' :: t ->
        let new_token : token = { token_type = Divide; line = !line_number } in
        new_token :: lex t
    | '%' :: t ->
        let new_token : token = { token_type = Mod; line = !line_number } in
        new_token :: lex t
    | '<' :: '=' :: t ->
        let new_token : token = { token_type = LE; line = !line_number } in
        new_token :: lex t
    | '>' :: '=' :: t ->
        let new_token : token = { token_type = GE; line = !line_number } in
        new_token :: lex t
    | ',' :: t ->
        let new_token : token = { token_type = Comma; line = !line_number } in
        new_token :: lex t
    | '<' :: t ->
        let new_token : token = { token_type = LT; line = !line_number } in
        new_token :: lex t
    | '>' :: t ->
        let new_token : token = { token_type = GT; line = !line_number } in
        new_token :: lex t
    | '=' :: '=' :: t ->
        let new_token : token = { token_type = EQ; line = !line_number } in
        new_token :: lex t
    | '!' :: '=' :: t ->
        let new_token : token = { token_type = NE; line = !line_number } in
        new_token :: lex t
    | '|' :: '|' :: t ->
        let new_token : token = { token_type = OR; line = !line_number } in
        new_token :: lex t
    | '|' :: t ->
        let new_token : token = { token_type = Pipe; line = !line_number } in
        new_token :: lex t
    | '&' :: '&' :: t ->
        let new_token : token = { token_type = AND; line = !line_number } in
        new_token :: lex t
    | '[' :: t ->
        let new_token : token =
          { token_type = LBracket; line = !line_number }
        in
        new_token :: lex t
    | ']' :: t ->
        let new_token : token =
          { token_type = RBracket; line = !line_number }
        in
        new_token :: lex t
    | '{' :: t ->
        let new_token : token = { token_type = LBrace; line = !line_number } in
        new_token :: lex t
    | '}' :: t ->
        let new_token : token = { token_type = RBrace; line = !line_number } in
        new_token :: lex t
    | n :: _ when is_num n ->
        let int_token, tail = lex_int lst 0 in
        int_token :: lex tail
    | c :: _ when is_letter c ->
        let id_token, tail = lex_id lst "" in
        id_token :: lex tail
    | '"' :: c :: t ->
        if c = '"' then
          let new_token : token =
            { token_type = StringToken ""; line = !line_number }
          in
          new_token :: lex t
        else
          let new_token, remainder = lex_string (c :: t) "" in
          new_token :: lex remainder
    | _ -> failwith "no token matched"
  in
  lex lst

let rec remove_line_numbers (tokens : token list) : token_type list =
  match tokens with
  | [] -> []
  | h :: t -> h.token_type :: remove_line_numbers t
[@@coverage off]

let string_of_token : token -> string =
 fun (tok : token) ->
  match tok.token_type with
  | Boolean b -> let s : string = string_of_bool b in

                 "<boolean: " ^ s ^ ">"
  | Integer n ->
      let s : string = string_of_int n in
      "<integer: " ^ s ^ ">"
  | StringToken s -> "<string: " ^ s ^ ">"
  | Unit -> "<unit>"
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
[@@coverage off]

let rec print_tokens_list : token list -> unit = function
  | [] -> ()
  | token :: tail ->
      token |> string_of_token |> print_endline;
      print_tokens_list tail
