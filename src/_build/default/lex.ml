type token_type = 
| Integer of int 
| Boolean of bool 
| StringToken of string 
| Nothing
| Id of string
| Assign
| Lam
| Arrow
| Let
| If
| Then
| Else
| LParen
| RParen
| Colon
| InArrow
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
| NothingType
| LBracket
| RBracket
| Bind
| In
| BindArrow

type token = {token_type: token_type; line: int}

let list_of_string (s: string) = s |> String.to_seq |> List.of_seq

let is_num: char -> bool = function
| '0'
| '1'
| '2'
| '3'
| '4'
| '5'
| '6'
| '7'
| '8'
| '9'
-> true
| _ -> false


let int_from_char: char -> int = function
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


let is_letter: char -> bool = fun (c: char) ->
  let code: int = Char.code c in
  if (code >= 65) && (code <= 122) then true else false


let string_of_char = String.make 1 


let ( ^^ ) (s: string) (c: char) = s ^ (string_of_char c)


let rec lex_int (lst: char list) (acc: int): token * char list = match lst with
| n :: t when is_num n ->
  let n_int: int = int_from_char n in
  lex_int t (acc * 10 + n_int)
| _ -> ({token_type = (Integer acc); line = 0}, lst)

let rec lex_string (lst: char list) (acc: string): token * char list = match lst with
| '"' :: t -> ({token_type = StringToken acc; line = 0}, t)
| '\\':: '"' :: t -> lex_string t (acc ^ "\"")
| c :: t -> 
  let char_string: string = string_of_char c in

  lex_string t (acc ^ char_string)

| [] -> failwith "expected closing double quote in lexing string"

let rec lex_id (lst: char list) (acc: string): token * char list = match lst with
| c :: t when is_letter c -> lex_id t (acc ^^ c)
| _ -> ({token_type = Id acc; line = 0}, lst)
  


let rec lex (lst: char list): token list =

  match lst with
  | [] -> []
  | ' ' :: t -> lex t (* ignore white space *)
  | '\n' :: t -> lex t (* ignore new lines *)
  | 't' :: 'r' :: 'u' :: 'e' :: t ->

    {token_type = (Boolean true); line = 0} :: (lex t)
  | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: t ->
      {token_type = (Boolean false); line = 0} :: (lex t)


  | 'i' :: 'n' :: 't' :: 'e' :: 'g' :: 'e' :: 'r' :: t ->
    let new_token: token = {token_type = IntegerType; line = 0} in
    new_token :: (lex t)

  | 'b' :: 'o' :: 'o' :: 'l' :: 'e' :: 'a' :: 'n' :: t ->
      let new_token: token = {token_type = BooleanType; line = 0} in
      new_token :: (lex t)

  | 's' :: 't' :: 'r' :: 'i' :: 'n' :: 'g' :: t ->
      let new_token: token = {token_type = StringType; line = 0} in
      new_token :: (lex t)

  | 'n' :: 'o' :: 't' :: 'h' :: 'i' :: 'n' :: 'g' :: t ->
      let new_token: token = {token_type = NothingType; line = 0} in
      new_token :: (lex t)

  | 'i' :: 'n' :: t ->
    let new_token: token = {token_type = In; line = 0} in
    new_token :: (lex t)

  | 'b' :: 'i' :: 'n' :: 'd' :: t ->
    let new_token: token = {token_type = Bind; line = 0} in
    new_token :: (lex t)
  
  | 'l' :: 'e' :: 't' :: t ->
    let new_token: token = {token_type = Let; line = 0} in
      new_token :: (lex t)


  | 'l' :: 'a' :: 'm' :: t ->
      let new_token: token = {token_type = Lam; line = 0} in
      new_token :: (lex t)

  | 'i' :: 'f' :: t ->
    let new_token: token = {token_type = If; line = 0} in
      new_token :: (lex t)

  | 't' :: 'h' :: 'e' :: 'n' :: t ->
      let new_token: token = {token_type = Then; line = 0} in
        new_token :: (lex t)

  | 'e' :: 'l' :: 's' :: 'e' :: t ->
      let new_token: token = {token_type = Else; line = 0} in
        new_token :: (lex t)


  | '(' :: c :: t when c <> ')' ->
    let new_token: token = {token_type = LParen; line = 0} in
        new_token :: (lex (c :: t))

  | ')' :: t ->
    let new_token: token = {token_type = RParen; line = 0} in
        new_token :: (lex t)

  | '=' :: '>' :: t ->
    let new_token: token = {token_type = InArrow; line = 0} in
    new_token :: (lex t)
  
  | '-' :: '>' :: t ->
    let new_token: token = {token_type = Arrow; line = 0} in
      new_token :: (lex t)

  | '<' :: '-' :: t ->
    let new_token: token = {token_type = BindArrow; line = 0} in
      new_token :: (lex t)
  
  | ':' :: t ->
    let new_token: token = {token_type = Colon; line = 0} in
      new_token :: (lex t)


  

  | '(' :: ')' :: t ->
    let new_token: token = {token_type = Nothing; line = 0} in
    new_token :: (lex t)
  
  | '+' :: t ->
    let new_token: token = {token_type = Plus; line = 0} in
    new_token :: (lex t)

  | '~' :: '-' :: t ->
    let new_token: token = {token_type = Opposite; line = 0} in
    new_token :: (lex t)

  | '-' :: t ->
    let new_token: token = {token_type = Minus; line = 0} in
    new_token :: (lex t)

  | '*' :: t ->
      let new_token: token = {token_type = Times; line = 0} in
      new_token :: (lex t)

  | '/' :: t ->
      let new_token: token = {token_type = Divide; line = 0} in
      new_token :: (lex t)
  
  | '%' :: t ->
      let new_token: token = {token_type = Mod; line = 0} in
      new_token :: (lex t)
    

  | '<' :: '=' :: t ->
    let new_token: token = {token_type = LE; line = 0} in
    new_token :: (lex t)

  | '>' :: '=' :: t ->
    let new_token: token = {token_type = GE; line = 0} in
    new_token :: (lex t)

  | '<' :: t ->
    let new_token: token = {token_type = LT; line = 0} in
    new_token :: (lex t)

  | '>' :: t ->
    let new_token: token = {token_type = GT; line = 0} in
    new_token :: (lex t)

  | '=' :: '=' :: t ->
    let new_token: token = {token_type = EQ; line = 0} in
    new_token :: (lex t)

  | '!' :: '=' :: t ->
    let new_token: token = {token_type = NE; line = 0} in
    new_token :: (lex t)
  
  | '|' :: '|' :: t ->
    let new_token: token = {token_type = OR; line = 0} in
    new_token :: (lex t)

  | '&' :: '&' :: t ->
    let new_token: token = {token_type = AND; line = 0} in
    new_token :: (lex t)


  | '[' :: t ->
    let new_token: token = {token_type = LBracket; line = 0} in
    new_token :: (lex t)


  | ']' :: t ->
      let new_token: token = {token_type = RBracket; line = 0} in
      new_token :: (lex t)

  | n :: _ when is_num n ->
    let int_token, tail = lex_int lst 0 in int_token :: (lex tail)


  | c :: _ when is_letter c ->
    let id_token, tail = lex_id lst "" in id_token :: (lex tail)


  | '"' :: c :: t ->
    
    if c = '"' then 
      let new_token: token = {token_type = (StringToken ""); line = 0} in
      new_token :: (lex t)
  else
    let new_token, remainder = lex_string (c :: t) "" in
    new_token :: (lex remainder)


  | _ -> failwith "no token matched"



let rec remove_line_numbers (tokens: token list): token_type list = match tokens with
| [] -> []
| h :: t -> h.token_type :: (remove_line_numbers t)




let string_of_token: token -> string = fun (tok: token) -> match tok.token_type with
| Boolean b -> let s: string = string_of_bool b in

"<boolean: " ^ s ^ ">"


| Integer n -> let s: string = string_of_int n in
"<integer: " ^ s ^ ">"
| StringToken s -> "<string: " ^ s ^ ">"
| Nothing -> "<nothing>"
| Id s -> "<id: " ^ s ^ ">"
| Lam -> "<lam>"
| Arrow -> "<arrow>"
| Assign -> "<assign>"
| Let -> "<let>"
| If -> "<if>"
| Then -> "<then>"
| Else -> "<else>"
| LParen -> "<lparen>"
| RParen -> "<rparen>"
| Colon-> "<colon>"
| InArrow -> "<in arrow>"
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
| NothingType -> "<nothing type>"
| LBracket -> "<left bracket>"
| RBracket -> "<right bracket>"
| Bind -> "<bind>"
| BindArrow -> "<bind arrow>"
| In -> "<in>"


let rec print_tokens_list: token list -> unit = function
| [] -> ()
| token :: tail ->
  token |> string_of_token |> print_endline; print_tokens_list tail