open Lex

type expr =
| IntegerExpr of int
| BooleanExpr of bool
| StringExpr of string
| NothingExpr
| IdExpr of string
| AppExpr of expr * expr
| TernaryExpr of expr * expr * expr


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



| _ -> failwith "impossible"




let remove_head: 'a list -> 'a list = function
| [] -> failwith "cannot remove head from empty list"
| _ :: t -> t



let rec parse_expr (tokens: token list): expr * token list = 
  match tokens with
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