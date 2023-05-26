open Lex


type pat =
| IdPat of string
| NothingPat

type expr =
| IntegerExpr of int
| BooleanExpr of bool
| StringExpr of string
| NothingExpr
| IdExpr of string
| AppExpr of expr * expr
| TernaryExpr of expr * expr * expr
| Function of pat * expr



let string_of_pat: pat -> string =
  function
  | NothingPat -> "Nothing Pattern"
  | IdPat s -> "Id Pattern (" ^ s ^ ")"


let indentations (level: int) = String.make (2 * level) ' '

let indentations_with_newline (level: int) = "\n" ^ (indentations level)

let rec string_of_expr (e: expr) (level: int): string = match e with
| IntegerExpr n -> 
  "Integer (" ^ (string_of_int n) ^ ")"
| BooleanExpr b ->
  "Boolean (" ^ (string_of_bool b) ^ ")"
| StringExpr s ->
  "String (" ^ s ^ ")"
| NothingExpr -> "Nothing"
| IdExpr s ->
  "Id (" ^ s ^ ")"
| TernaryExpr (e1, e2, e3) ->

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

| AppExpr (e1, e2) ->
  let e1_string: string = string_of_expr e1 (level + 1) in
  let e2_string: string = string_of_expr e2 (level + 1) in

  "App ("
  ^ indentations_with_newline (level + 1)
  ^ e1_string
  ^ ","
  ^ indentations_with_newline (level + 1)
  ^ e2_string
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


let remove_head: 'a list -> 'a list = function
| [] -> failwith "cannot remove head from empty list"
| _ :: t -> t

let remove_last (lst: 'a list): 'a * 'a list =
  match List.rev lst with
  | [] -> failwith "cannot remove the last element of an empty list"
  | last :: rest ->
    last, List.rev rest


let rec parse_expr ?can_be_app:(can_be_app=true) (tokens: token list) : expr * token list = 
  let expr, remaining_tokens = (match tokens with
  | {token_type = Integer n; line = _} :: t -> IntegerExpr n, t
  | {token_type = Boolean b; line = _} :: t -> BooleanExpr b, t
  | {token_type = StringToken s; line = _} :: t -> StringExpr s, t
  | {token_type = Nothing; line = _} :: t -> NothingExpr, t
  | {token_type = Id s; line = _} :: t -> IdExpr s, t
  | {token_type = LParen; line = _} :: _ ->

    (* split *)
    let a, b = split_tokens_list_by_parens tokens in

    fst (parse_expr a), b


  | {token_type = If; line = _} :: t ->
    (* parse a ternary expression *)

    let e1, tokens_after_e1 = parse_expr t in (* t does not include the if *)
    let e2, tokens_after_e2 = parse_expr (remove_head tokens_after_e1) in (* remove the then *)
    let e3, tokens_after_e3 = parse_expr (remove_head tokens_after_e2) in (* remove the else *)

    TernaryExpr (e1, e2, e3), tokens_after_e3

  | {token_type = Lam; line = _} :: t ->
    (* parse a function *)

    (* the next token after Lam should be a pattern *)
    (* parse the pattern*)

    let pattern, tokens_after_parse_pat = parse_pat t in

    (* the next token should be an arrow *)
    (
      match tokens_after_parse_pat with
      | {token_type = Arrow; line = _} :: tokens_after_arrow ->
        let body, tokens_after_body = parse_expr tokens_after_arrow in
        Function (pattern, body), tokens_after_body
      | _ -> failwith "expected arrow after function"
    )

    
  | _ -> failwith "parsing error: no pattern matched"
  ) in 
  
  if not can_be_app then expr, remaining_tokens else

  match remaining_tokens with
  | [] -> expr, []
  | {token_type = Then; line = _} :: _ 
  | {token_type = Else; line = _} :: _ -> 
    expr, remaining_tokens
  | _ :: _ ->
    parse_app tokens


and parse_pat (tokens: token list): pat * token list = match tokens with
| [] -> failwith "empty list passed to parse_pat"
| {token_type = Nothing; line = _} :: t ->
  NothingPat, t
| {token_type = Id s; line = _} :: t ->
  IdPat s, t
| _ -> failwith "pattern match failed in parse_pat"

and get_expr_list (tokens: token list) (acc: expr list): expr list * token list =
match tokens with
| []
| {token_type = Then; line = _} :: _
| {token_type = Else; line = _} :: _ -> List.rev acc, tokens (* return no new exprs *)
| _ ->
  (* parse one more expr *)
  let new_expr, remaining_tokens = parse_expr ~can_be_app:false tokens in
  get_expr_list remaining_tokens (new_expr :: acc)


and construct_app_chain_from_expr_list (expressions: expr list): expr =
  match expressions with
  | [] -> failwith "impossible"
  | e :: [] -> e
  | expressions ->

    let last, expressions_without_last = remove_last expressions in
    AppExpr (construct_app_chain_from_expr_list expressions_without_last, last)


and parse_app (tokens: token list): expr * token list =
  let expressions, remaining_tokens = get_expr_list tokens [] in
  construct_app_chain_from_expr_list expressions, remaining_tokens


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
