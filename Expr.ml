open Lex


exception ParseFailure
exception FactorParseFailure

type pat =
| IdPat of string
| NothingPat


and expr =
  | Function of pat * expr
  | Ternary of expr * expr * expr
  | ArithExpr of arith_expr


and arith_expr =
  | Plus of term * arith_expr
  | Minus of term * arith_expr
  | Term of term


and term = 
  | Mul of factor * term
  | Div of factor * term
  | Mod of factor * term
  | Factor of factor


and factor =
  | Boolean of bool
  | String of string
  | Nothing
  | Integer of int
  | Id of string
  | ParenFactor of expr
  | App of factor * factor


let string_of_pat: pat -> string =
  function
  | NothingPat -> "Nothing Pattern"
  | IdPat s -> "Id Pattern (" ^ s ^ ")"


let indentations (level: int) = String.make (2 * level) ' '

let indentations_with_newline (level: int) = "\n" ^ (indentations level)



let rec string_of_expr (e: expr) (level: int): string = match e with
| Ternary (e1, e2, e3) ->

  let e1_string: string = string_of_expr e1 (level + 1) in
  let e2_string: string = string_of_expr e2 (level + 1) in
  let e3_string: string = string_of_expr e3 (level + 1) in


  "Ternary ("
  ^ indentations_with_newline (level + 1)
  ^ e1_string
  ^ ","
  ^ indentations_with_newline (level + 1)
  ^ e2_string
  ^ ","
  ^ indentations_with_newline (level + 1)
  ^ e3_string
  ^ indentations_with_newline level
  ^ ")"


| Function (pattern, body) ->
  let pattern_string: string = string_of_pat pattern in
  let body_string: string = string_of_expr body (level + 1) in
  
  "Function ("
  ^ indentations_with_newline (level + 1)
  ^ pattern_string
  ^ ","
  ^ indentations_with_newline (level + 1)
  ^ body_string
  ^ indentations_with_newline level
  ^ ")"



| ArithExpr e ->
  string_of_arith_expr e (level + 1)


  
and string_of_arith_expr (ae: arith_expr) (level: int) = match ae with
| Plus (t, e) ->
  "Plus ("
  ^ indentations_with_newline (level + 1)
  ^ (string_of_arith_term t (level + 1))
  ^ indentations_with_newline (level + 1)
  ^ (string_of_arith_expr e (level + 1))
  ^ indentations_with_newline level
  ^ ")"

| Minus (t, e) ->
  "Minus ("
  ^ indentations_with_newline (level + 1)
  ^ (string_of_arith_term t (level + 1))
  ^ indentations_with_newline (level + 1)
  ^ (string_of_arith_expr e (level + 1))
  ^ indentations_with_newline level
  ^ ")"

| Term t ->
  string_of_arith_term t (level + 1)


and string_of_arith_term (at: term) (level: int) =
match at with
| Mul (f, t) ->
  "Mul ("
  ^ indentations_with_newline (level + 1)
  ^ (string_of_arith_factor f (level + 1))
  ^ indentations_with_newline (level + 1)
  ^ (string_of_arith_term t (level + 1))
  ^ indentations_with_newline level
  ^ ")"
| Div (f, t) ->
  "Div ("
  ^ indentations_with_newline (level + 1)
  ^ (string_of_arith_factor f (level + 1))
  ^ indentations_with_newline (level + 1)
  ^ (string_of_arith_term t (level + 1))
  ^ indentations_with_newline level
  ^ ")"
| Factor f ->
  string_of_arith_factor f (level + 1)

| _ -> failwith "unimplemented string_of_arith_term"



and string_of_arith_factor (af: factor) (level: int) =
match af with
| Integer n ->
  "Integer (" ^ (string_of_int n) ^ ")"
| Boolean b ->
  "Boolean (" ^ (string_of_bool b) ^ ")"
| String s ->
  "String (" ^ s ^ ")"
| Nothing ->
  "Nothing"
| Id s ->
  "Id (" ^ s ^ ")"
| ParenFactor e ->
  "Paren ("
  ^ indentations_with_newline (level + 1)
  ^ (string_of_expr e (level + 1))
  ^ indentations_with_newline level
  ^ ")"
| App (e1, e2) ->
  "App ("
  ^ indentations_with_newline (level + 1)
  ^ (string_of_arith_factor e1 (level + 1))
  ^ indentations_with_newline (level + 1)
  ^ (string_of_arith_factor e2 (level + 1))
  ^ indentations_with_newline level
  ^ ")"





let remove_head: 'a list -> 'a list = function
| [] -> failwith "cannot remove head from empty list"
| _ :: t -> t

let remove_last (lst: 'a list): 'a * 'a list =
  match List.rev lst with
  | [] -> failwith "cannot remove the last element of an empty list"
  | last :: rest ->
    last, List.rev rest


let rec parse_expr (tokens: token list) : expr * token list =
  match tokens with
  | {token_type = Lam; line = _} :: t ->
    (* parse a pattern *)
    let pattern, tokens_after_pattern = parse_pat t in
    (* remove the next token, which should be an arrow *)
    (
      match tokens_after_pattern with
      | {token_type = Arrow; line = _} :: tokens_after_arrow ->
        (* parse an expression from the remaining tokens *)
        let body, tokens_after_body = parse_expr tokens_after_arrow in
        Function (pattern, body), tokens_after_body
      | _ -> failwith "expected arrow"
    )
  | {token_type = If; line = _} :: t ->
    (* parse the guard *)
    let guard, tokens_after_guard = parse_expr t in
    (* the next token should be a Then *)
    let e1, tokens_after_e1 = parse_expr (remove_head tokens_after_guard) in
    (* the next token should be a Else *)
    let e2, tokens_after_e2 = parse_expr (remove_head tokens_after_e1) in
    Ternary (guard, e1, e2), tokens_after_e2
  | _ ->
    (* parse an arith_expr *)
    let (e, t): arith_expr * token list = parse_arith_expr tokens in
    ArithExpr e, t


    
  
and next_token_is_binop (tokens: token list): bool =
  match tokens with
  | {token_type = Plus; line = _} :: _
  | {token_type = Minus; line = _} :: _
  | {token_type = Times; line = _} :: _
  | {token_type = Divide; line = _} :: _ -> true
  | _ -> false


  

and parse_arith_expr (tokens: token list): arith_expr * token list =
  (* start by parsing a term *)
  let first, tokens_after_first = parse_term tokens in
  (* then check what the next symbol is *)
  (* if it's a PLUS or MINUS, parse another arith_expr and return the sum/difference *)
  match tokens_after_first with
  | {token_type = Plus; line = _} :: t ->

    (* parse another level_one expression *)
    (* return the sum of those expressions *)

    let second, tokens_after_second = parse_arith_expr t in
    
    Plus(first, second), tokens_after_second
  | {token_type = Minus; line = _} :: t ->

    (* parse another level_one expression *)
    (* return the sum of those expressions *)

    let second, tokens_after_second = parse_arith_expr t in
    
    Minus(first, second), tokens_after_second

  
  | _ -> Term first, tokens_after_first



and parse_term (tokens: token list): term * token list =
  (* start by parsing a factor *)
  let first, tokens_after_first = parse_factor tokens in
  (* check what the next token is *)
  (* if its a TIMES or DIVIDE, parse another term and return the product/quotient *)
  match tokens_after_first with
  | {token_type = Times; line = _} :: t ->
    let second, tokens_after_second = parse_term t in
    Mul(first, second), tokens_after_second

  | {token_type = Divide; line = _} :: t ->
    let second, tokens_after_second = parse_term t in
    Div(first, second), tokens_after_second

  | _ -> Factor first, tokens_after_first




and parse_factor (tokens: token list): factor * token list =
  let factors, tokens_after_factors = get_factor_list tokens [] in
  create_factor_app_chain_from_factor_list factors, tokens_after_factors
  
  


and parse_app (tokens: token list): factor * token list =
  let factor_list, remaining_tokens = get_factor_list tokens [] in
  create_factor_app_chain_from_factor_list factor_list, remaining_tokens



and parse_pat (tokens: token list): pat * token list = match tokens with
| [] -> failwith "empty list passed to parse_pat"
| {token_type = Nothing; line = _} :: t ->
  NothingPat, t
| {token_type = Id s; line = _} :: t ->
  IdPat s, t
| _ -> failwith "pattern match failed in parse_pat"


and parse_factor_not_app (tokens: token list): factor * token list =
  match tokens with
  | {token_type = Integer n; line = _} :: t ->
    Integer n, t
  | {token_type = Boolean b; line = _} :: t ->
    Boolean b, t
  | {token_type = StringToken s; line = _} :: t ->
    String s, t
  | {token_type = Nothing; line = _} :: t ->
    Nothing, t
  | {token_type = Id s; line = _} :: t ->
    Id s, t
  | {token_type = LParen; line = _} :: t ->
    (* parse an expr *)
    let e, tokens_after_e = parse_expr t in
    (* the next token should be a RPAREN *)
    (* remove the RPAREN with remove_head *)
    ParenFactor e, remove_head tokens_after_e
  | _ -> raise FactorParseFailure
  


and get_factor_list (tokens: token list) (acc: factor list) : factor list * token list =
try 
  let new_expr, remaining_tokens = parse_factor_not_app  tokens in
  get_factor_list remaining_tokens (new_expr :: acc)

with 
  | FactorParseFailure -> List.rev acc, tokens (* return no new exprs *)


and create_factor_app_chain_from_factor_list (factors: factor list): factor =
  match factors with
  | [] -> failwith "impossible"
  | f :: [] -> f
  | factors_list ->

    let last, factors_without_last = remove_last factors_list in
    App (create_factor_app_chain_from_factor_list factors_without_last, last)



and get_expr_list (tokens: token list) (acc: expr list): expr list * token list =
print_endline "getting expr list";
(*
match tokens with
| []
| {token_type = Then; line = _} :: _
| {token_type = Else; line = _} :: _ 
| _ when next_token_is_binop tokens
-> List.rev acc, tokens (* return no new exprs *)
| _ ->
  (* parse one more expr *)
  let new_expr, remaining_tokens = parse_expr ~can_be_app:false tokens in
  get_expr_list remaining_tokens (new_expr :: acc)
*)

try 
  let new_expr, remaining_tokens = parse_expr  tokens in
  get_expr_list remaining_tokens (new_expr :: acc)

with 
  | ParseFailure -> List.rev acc, tokens (* return no new exprs *)

(*
and construct_app_chain_from_expr_list (expressions: expr list): expr =
  print_endline "constructing app chain";
  match expressions with
  | [] -> failwith "impossible"
  | e :: [] -> e
  | expressions ->

    let last, expressions_without_last = remove_last expressions in
    App (construct_app_chain_from_expr_list expressions_without_last, last)


and parse_app (tokens: token list): expr * token list =
  let expressions, remaining_tokens = get_expr_list tokens [] in
  construct_app_chain_from_expr_list expressions, remaining_tokens
*)

(**
[split_tokens_list_by_parens t] is a tuple (a, b) where

a is a list of tokens between a pair of corresponding parenthesis
b is the list of tokens that comes after a

a @ b = t, except the two parenthesis are removed

Requires: the first token in t is a LParen, and there is a corresponding RParen in t

*)

and split_tokens_list_by_parens (tokens: token list): token list * token list =

  let balance: int ref = ref 0 in
  let tokens_list: token list ref = ref tokens in
  let accumulated_tokens: token list ref = ref [] in
  let active: bool ref = ref true in

  while !active do
    (match !tokens_list with
    | {token_type = LParen; line} :: remaining_tokens ->

      balance := !balance + 1;
      tokens_list := remaining_tokens;
      accumulated_tokens := !accumulated_tokens @ [{token_type = LParen; line}]

    | {token_type = RParen; line} :: remaining_tokens ->

      balance := ! balance - 1;
      tokens_list := remaining_tokens;
      accumulated_tokens := !accumulated_tokens @ [{token_type = LParen; line}]

    | h :: remaining_tokens ->
      tokens_list := remaining_tokens;
      accumulated_tokens := !accumulated_tokens @ [h]

    | [] -> active := false
    );
    if !balance = 0 then active := false

  done;
  (* at this point, accumulated tokens has both the opening and closing parens *)
  (* we need to remove them *)
  let a: token list = !accumulated_tokens |> remove_head |> remove_last |> snd in
  let b: token list = !tokens_list in
  a, b
