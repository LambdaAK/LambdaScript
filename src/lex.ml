type token_type =
  | Integer of int
  | FloatToken of float
  | Boolean of bool
  | StringToken of string
  | Unit
  | Id of string (* starts with lowercase *)
  | Constructor of string (* starts with uppercase *)
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
  | Equals
  | LBrace
  | RBrace
  | Let
  | Rec
  | Comma
  | WildcardPattern
  | ConsToken
  | Semicolon
  | Enum
  | Type
  | Union
  | Relop of string (* start with = < or >, and are not = *)
  | Addop of string (* start with + or - *)
  | Mulop of string (* start with * / or % *)

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
  | Constructor s -> "<constructor: " ^ s ^ ">"
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
  | Equals -> "<equals>"
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
  | Type -> "<type>"
  | Union -> "<union>"
  | Relop s -> "<relop: " ^ s ^ ">"
  | Addop s -> "<addop: " ^ s ^ ">"
  | Mulop s -> "<mulop: " ^ s ^ ">"
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

let is_special = (* + - * / % < > = & | : ; , *)
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
    | h :: t when is_special h -> get_bop_chars t (h :: acc)
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
    ("Str", StringType);
    ("Unit", UnitType);
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
    ("Float", FloatType);
    ("type", Type);
    ("union", Union);
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

let is_lowercase : char -> bool = function
  | c when is_letter c -> Char.code c >= 97 && Char.code c <= 122
  | _ -> false

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

let rec lex_id_or_constructor (lst : char list) (acc : string) :
    token * char list =
  (* the first char has to be a letter the following characters can be letters,
     numbers, or underscores *)
  match lst with
  | c :: t when is_letter c -> lex_id_or_constructor t (acc ^^ c) (* a letter *)
  | c :: t when is_num c && not (acc = "") ->
      lex_id_or_constructor t (acc ^^ c)
      (* a digit, and not the first character *)
  | _ -> (
      let first_char : char = List.hd (list_of_string acc) in
      match is_lowercase first_char with
      | true -> ({ token_type = Id acc; line = 0 }, lst)
      | false -> ({ token_type = Constructor acc; line = 0 }, lst))

(* A type variable has the following form 'x where x is an identifier *)

let lex_type_var (tokens_after_single_quote : char list) : token * char list =
  match lex_id_or_constructor tokens_after_single_quote "" with
  | { token_type = Id i; line = _ }, tokens_after_type_var ->
      ({ token_type = TypeVar i; line = 0 }, tokens_after_type_var)
  | _ -> failwith "expected an identifier after single quote"

let lex (lst : char list) : token list =
  let line_number : int ref = ref 1 in
  let rec lex (lst : char list) : token list =
    (* check for leading keyword *)
    match find_leading_keyword_if_it_exists lst keywords with
    | Some token_type, remainder ->
        let new_token : token = { token_type; line = !line_number } in
        new_token :: lex remainder
    | _ -> (
        match lst with
        | [] -> []
        | ' ' :: t -> lex t (* ignore white space *)
        | '\n' :: t ->
            line_number := !line_number + 1;
            lex t (* ignore new lines, increment the line number *)
        | h :: _ when is_bop_prefix h ->
            let bop, chars_after = lex_bop lst in
            { token_type = bop; line = !line_number } :: lex chars_after
        | '.' :: '.' :: '.' :: t ->
            let new_token : token =
              { token_type = Enum; line = !line_number }
            in
            new_token :: lex t
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
        | '\\' :: t ->
            let new_token : token = { token_type = Fn; line = !line_number } in
            new_token :: lex t
        | '(' :: c :: t when c <> ')' ->
            let new_token : token =
              { token_type = LParen; line = !line_number }
            in
            new_token :: lex (c :: t)
        | ')' :: t ->
            let new_token : token =
              { token_type = RParen; line = !line_number }
            in
            new_token :: lex t
        | '-' :: '>' :: t ->
            let new_token : token =
              { token_type = Arrow; line = !line_number }
            in
            new_token :: lex t
        | '<' :: '-' :: t ->
            let new_token : token =
              { token_type = BindArrow; line = !line_number }
            in
            new_token :: lex t
        | ':' :: t ->
            let new_token : token =
              { token_type = Colon; line = !line_number }
            in
            new_token :: lex t
        | '(' :: ')' :: t ->
            let new_token : token =
              { token_type = Unit; line = !line_number }
            in
            new_token :: lex t
        | '~' :: '-' :: t ->
            let new_token : token =
              { token_type = Opposite; line = !line_number }
            in
            new_token :: lex t
        | '-' :: t ->
            let new_token : token =
              { token_type = Minus; line = !line_number }
            in
            new_token :: lex t
        | ',' :: t ->
            let new_token : token =
              { token_type = Comma; line = !line_number }
            in
            new_token :: lex t
        | '|' :: '|' :: t ->
            let new_token : token = { token_type = OR; line = !line_number } in
            new_token :: lex t
        | '|' :: t ->
            let new_token : token =
              { token_type = Pipe; line = !line_number }
            in
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
            let new_token : token =
              { token_type = LBrace; line = !line_number }
            in
            new_token :: lex t
        | '}' :: t ->
            let new_token : token =
              { token_type = RBrace; line = !line_number }
            in
            new_token :: lex t
        | n :: _ when is_num_or_dot n ->
            let int_token, tail = lex_num lst "" in
            int_token :: lex tail
        | c :: _ when is_letter c ->
            let id_token, tail = lex_id_or_constructor lst "" in
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
        | _ -> failwith "no token matched")
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
