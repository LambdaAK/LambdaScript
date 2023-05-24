type token_type = Integer of int | Boolean of bool | StringToken of string
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


let string_of_char (c: char) = String.make 1 c



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




let rec lex (lst: char list): token list =

  match lst with
  | [] -> []
  | ' ' :: t -> lex t (* ignore white space *)
  | '\n' :: t -> lex t (* ignore new lines *)
  | 't' :: 'r' :: 'u' :: 'e' :: t ->

    {token_type = (Boolean true); line = 0} :: (lex t)
  | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: t ->

      {token_type = (Boolean false); line = 0} :: (lex t)


  | n :: _ when is_num n ->
    let int_token, tail = lex_int lst 0 in int_token :: (lex tail)


  | '"' :: c :: t ->
    
    if c = '"' then 
      let new_token: token = {token_type = (StringToken ""); line = 0} in
      new_token :: (lex t)
  else
    let new_token, remainder = lex_string (c :: t) "" in
    new_token :: (lex remainder)
    
      

  | _ -> failwith "no token matched"



let string_of_token: token -> string = function
| {token_type = Boolean b; line = _} -> let s: string = string_of_bool b in

"<boolean: " ^ s ^ ">"


| {token_type = Integer n; line = _} -> let s: string = string_of_int n in
"<integer: " ^ s ^ ">"
| {token_type = StringToken s; line = _} -> "<string: " ^ s ^ ">"
