open Lex

type expr =
| IntegerExpr of int
| BooleanExpr of bool
| StringExpr of string
| NothingExpr
| IdExpr of string
| AppExpr of expr * expr
| TernaryExpr of expr * expr * expr
| Function of expr * expr


let rec string_of_expr (e: expr): string = match e with
| IntegerExpr n -> "<integer: " ^ (string_of_int n) ^ ">"
| BooleanExpr b -> "<boolean: " ^ (string_of_bool b) ^ ">"
| StringExpr s -> "<string: " ^ s ^ ">"
| NothingExpr -> "<nothing>"
| IdExpr s -> "<id: " ^ s ^ ">"
| TernaryExpr (e1, e2, e3) ->

  let e1_string: string = string_of_expr e1 in
  let e2_string: string = string_of_expr e2 in
  let e3_string: string = string_of_expr e3 in


  "<ternary: if " ^ e1_string ^ " then " ^ e2_string ^ " else " ^ e3_string ^ ">"

| AppExpr (e1, e2) ->
  let e1_string: string = string_of_expr e1 in
  let e2_string: string = string_of_expr e2 in

  "<app: " ^ e1_string ^ " " ^ e2_string ^ ">"

| _ -> failwith "impossible"


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
  | {token_type = If; line = _} :: t ->
    (* parse a ternary expression *)

    let e1, tokens_after_e1 = parse_expr t in (* t does not include the if *)
    let e2, tokens_after_e2 = parse_expr (remove_head tokens_after_e1) in (* remove the then *)
    let e3, tokens_after_e3 = parse_expr (remove_head tokens_after_e2) in (* remove the else *)

    TernaryExpr (e1, e2, e3), tokens_after_e3
    

    
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
