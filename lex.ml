type token_type = Integer of int | Boolean of bool
type token = {token_type: token_type; line: int}


let list_of_string (s: string) = s |> String.to_seq |> List.of_seq

let rec lex (lst: char list): token list =

  match lst with
  | [] -> []
  | ' ' :: t -> lex t (* ignore white space *)
  | '\n' :: t -> lex t (* ignore new lines *)
  | 't' :: 'r' :: 'u' :: 'e' :: t ->

    {token_type = (Boolean true); line = 0} :: (lex t)
  | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: t ->

      {token_type = (Boolean false); line = 0} :: (lex t)


  | _ -> failwith "no token matched"



let string_of_token: token -> string = function
| {token_type = Boolean b; line = _} -> string_of_bool b
| _ -> failwith "string of token match failure"
