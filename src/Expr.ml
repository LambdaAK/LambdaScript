open Lex


exception ParseFailure
exception FactorParseFailure

type pat =
  | IdPat of string
  | NothingPat


and compound_type =
  | FunctionType of factor_type * compound_type
  | BasicType of factor_type


and factor_type =
  | IntegerType
  | StringType
  | BooleanType
  | NothingType
  | ParenFactorType of compound_type


type rel_op =
  | LT
  | GT
  | LE
  | GE

type eq_op =
  | EQ
  | NE

type expr =
  | Function of pat * compound_type option * expr
  | Ternary of expr * expr * expr
  | DisjunctionExpr of disjunction


and disjunction =
  | Disjunction of conjunction * disjunction
  | ConjunctionUnderDisjunction of conjunction


and conjunction =
  | Conjunction of eq_expr * conjunction
  | EqualityUnderConjunction of eq_expr

and eq_expr =
  | Equality of eq_op * rel_expr * eq_expr
  | RelationUnderEqExpr of rel_expr


and rel_expr =
  | Relation of rel_op * arith_expr * rel_expr
  | ArithmeticUnderRelExpr of arith_expr


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
  | Opposite of factor


let string_of_pat: pat -> string =
  function
  | NothingPat -> "Nothing Pattern"
  | IdPat s -> "Id Pattern (" ^ s ^ ")"


let string_of_rel_op: rel_op -> string =
  function
  | LT -> "LT"
  | GT -> "GT"
  | LE -> "LE"
  | GE -> "GE"

let string_of_eq_op: eq_op -> string =
  function
  | EQ -> "EQ"
  | NE -> "NE"




let indentations (level: int) = String.make (2 * level) ' '

let indentations_with_newline (level: int) = "\n" ^ (indentations level)


let rec string_of_basic_type (ft: factor_type) (level: int): string =
  match ft with
  | IntegerType -> "IntegerType"
  | BooleanType -> "BooleanType"
  | StringType -> "StringType"
  | NothingType -> "NothingType"
  | ParenFactorType c ->
    string_of_compound_type c level

and string_of_compound_type (ct: compound_type) (level: int) =
  match ct with
  | BasicType t ->
    string_of_basic_type t level
  | FunctionType (t1, t2) ->
    "FunctionType ("
    ^ indentations_with_newline (level + 1)
    ^ string_of_basic_type t1 level
    ^ ","
    ^ indentations_with_newline (level + 1)
    ^ (string_of_compound_type t2 (level + 1))
    ^ indentations_with_newline level
    ^ ")"


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


| Function (pattern, cto, body) ->
  let pattern_string: string = string_of_pat pattern in
  let body_string: string = string_of_expr body (level + 1) in
  
  "Function ("
  ^ indentations_with_newline (level + 1)
  ^ pattern_string
  ^ ","
  ^ (
    match cto with
    | None -> ""
    | Some ct ->
      (* add the type annotation *)
      let string_of_ct: string = string_of_compound_type ct (level + 1) in
      indentations_with_newline (level + 1)
      ^ string_of_ct
      ^ ","
    )
  ^ indentations_with_newline (level + 1)
  ^ body_string
  ^ indentations_with_newline level
  ^ ")"

| DisjunctionExpr e  ->
  string_of_disjunction e level


and string_of_disjunction (d: disjunction) (level: int): string =
  match d with
  | ConjunctionUnderDisjunction c ->
    string_of_conjunction c level
  | Disjunction (c, d) ->
    "Disjunction ("
    ^ indentations_with_newline (level + 1)
    ^ (string_of_conjunction c (level + 1))
    ^ ","
    ^ indentations_with_newline (level + 1)
    ^ (string_of_disjunction d (level + 1))
    ^ ","
    ^ indentations_with_newline level
    ^ ")"



and string_of_conjunction (c: conjunction) (level: int): string =
  match c with
  | EqualityUnderConjunction e ->
    string_of_eq_expr e level
  | Conjunction (e, c) ->
    "Conjunction ("
    ^ indentations_with_newline (level + 1)
    ^ (string_of_eq_expr e (level + 1))
    ^ ","
    ^ indentations_with_newline (level + 1)
    ^ (string_of_conjunction c (level + 1))
    ^ ","
    ^ indentations_with_newline level
    ^ ")"





and string_of_eq_expr (ee: eq_expr) (level: int): string =
  match ee with
  | RelationUnderEqExpr re ->
    string_of_rel_expr re level
  | Equality (op, re, ee) ->
    "Equality ("
    ^ indentations_with_newline (level + 1)
    ^ (string_of_eq_op op)
    ^ ","
    ^ indentations_with_newline (level + 1)
    ^ (string_of_rel_expr re (level + 1))
    ^ ","
    ^ indentations_with_newline (level + 1)
    ^ (string_of_eq_expr ee (level + 1))
    ^ indentations_with_newline level
    ^ ")"



and string_of_rel_expr (re: rel_expr) (level: int): string =
  match re with
  | ArithmeticUnderRelExpr ae ->
    string_of_arith_expr ae level
  | Relation (op, ae, re) ->
    "Relation ("
    ^ indentations_with_newline (level + 1)
    ^ (string_of_rel_op op)
    ^ ","
    ^ indentations_with_newline (level + 1)
    ^ (string_of_arith_expr ae (level + 1))
    ^ ","
    ^ indentations_with_newline (level + 1)
    ^ (string_of_rel_expr re (level + 1))
    ^ indentations_with_newline level
    ^ ")"


and string_of_arith_expr (ae: arith_expr) (level: int) =
match ae with
| Plus (t, e) ->
  "Plus ("
  ^ indentations_with_newline (level + 1)
  ^ (string_of_arith_term t (level + 1))
  ^ ","
  ^ indentations_with_newline (level + 1)
  ^ (string_of_arith_expr e (level + 1))
  ^ indentations_with_newline level
  ^ ")"

| Minus (t, e) ->
  "Minus ("
  ^ indentations_with_newline (level + 1)
  ^ (string_of_arith_term t (level + 1))
  ^ ","
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
  ^ ","
  ^ indentations_with_newline (level + 1)
  ^ (string_of_arith_term t (level + 1))
  ^ indentations_with_newline level
  ^ ")"
| Div (f, t) ->
  "Div ("
  ^ indentations_with_newline (level + 1)
  ^ (string_of_arith_factor f (level + 1))
  ^ ","
  ^ indentations_with_newline (level + 1)
  ^ (string_of_arith_term t (level + 1))
  ^ indentations_with_newline level
  ^ ")"
| Mod (f, t) ->
  "Mod ("
  ^ indentations_with_newline (level + 1)
  ^ (string_of_arith_factor f (level + 1))
  ^ ","
  ^ indentations_with_newline (level + 1)
  ^ (string_of_arith_term t (level + 1))
  ^ indentations_with_newline level
  ^ ")"
| Factor f ->
  string_of_arith_factor f (level + 1)



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
  ^ ","
  ^ indentations_with_newline (level + 1)
  ^ (string_of_arith_factor e2 (level + 1))
  ^ indentations_with_newline level
  ^ ")"
| Opposite f ->
  "Opposite ("
  ^ indentations_with_newline (level + 1)
  ^ string_of_arith_factor f (level + 1)
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


let rec parse_compound_type (tokens: token list): compound_type * token list =
  let left_type, tokens_after_left_type = parse_factor_type tokens in
  match tokens_after_left_type with
  (* check if the next token is an arrow *)
  | {token_type = Arrow; line = _} :: tokens_after_arrow ->
    (* parse a function type *)
    (* parse another compound type to the right of the arrow *)
    let right_type, tokens_after_right_type = parse_compound_type tokens_after_arrow in
    FunctionType (left_type, right_type), tokens_after_right_type
  | _ ->
    (* return the basic type *)
    BasicType left_type, tokens_after_left_type



and parse_factor_type (tokens: token list): factor_type * token list =
  match tokens with
  | {token_type = IntegerType; line = _} :: t ->
    IntegerType, t
  | {token_type = BooleanType; line = _} :: t ->
      BooleanType, t
  | {token_type = StringType; line = _} :: t ->
      StringType, t
  | {token_type = NothingType; line = _} :: t ->
      NothingType, t
  
  | {token_type = LParen; line = _} :: t ->

    let inside, tokens_after_inside = parse_compound_type t in
    ParenFactorType inside, (remove_head tokens_after_inside) (* remove the RParen here *)


  | _ -> failwith "no pattern matched in parse_factor_type"


and parse_expr (tokens: token list) : expr * token list =
  match tokens with
  | {token_type = Lam; line = _} :: t ->
    parse_function t
    
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
    let (e, t): disjunction * token list = parse_disjunction tokens in
    DisjunctionExpr e, t


and parse_function (tokens_without_lam: token list): expr * token list =
    let pattern, tokens_after_pattern = parse_pat tokens_without_lam in
    (* check if there is a left bracket for a type annotation *)
    match tokens_after_pattern with
    | {token_type = LBracket; line = _} :: tokens_after_l_bracket ->
      let ct, tokens_after_ct = parse_compound_type tokens_after_l_bracket in
      (* disregard the next r bracket and arrow *)
      let body_tokens: token list = tokens_after_ct |> remove_head |> remove_head in
      let body, tokens_after_body = parse_expr body_tokens in
      Function (pattern, Some ct, body), tokens_after_body
    | _ ->
      (* no type annotation *)
      (* in this case, the next token is arrow *)
      let body_tokens: token list = remove_head tokens_after_pattern in
      let body, tokens_after_body = parse_expr body_tokens in
      Function (pattern, None, body), tokens_after_body



and parse_disjunction (tokens: token list): disjunction * token list =
  let first, tokens_after_first = parse_conjunction tokens in
  match tokens_after_first with
  | {token_type = OR; line = _} :: t ->
    let second, tokens_after_second = parse_disjunction t in
    Disjunction (first, second), tokens_after_second

  | _ -> ConjunctionUnderDisjunction first, tokens_after_first 


and parse_conjunction (tokens: token list): conjunction * token list =
  let first, tokens_after_first = parse_eq_expr tokens in
  match tokens_after_first with
  | {token_type = AND; line = _} :: t ->
    let second, tokens_after_second = parse_conjunction t in
    Conjunction (first, second), tokens_after_second

  | _ -> EqualityUnderConjunction first, tokens_after_first 


and parse_eq_expr (tokens: token list): eq_expr * token list =
  let first, tokens_after_first = parse_rel_expr tokens in
  match tokens_after_first with
  | {token_type = EQ; line = _} :: t ->
    let second, tokens_after_second = parse_eq_expr t in
    Equality (EQ, first, second), tokens_after_second
  
  | {token_type = NE; line = _} :: t ->
    let second, tokens_after_second = parse_eq_expr  t in
    Equality (NE, first, second), tokens_after_second

  | _ -> RelationUnderEqExpr first, tokens_after_first


and parse_rel_expr (tokens: token list): rel_expr * token list =
  let first, tokens_after_first = parse_arith_expr tokens in
  match tokens_after_first with
  | {token_type = LT; line = _} :: t ->
    let second, tokens_after_second = parse_rel_expr t in
    Relation (LT, first, second), tokens_after_second

  | {token_type = GT; line = _} :: t ->
    let second, tokens_after_second = parse_rel_expr  t in
    Relation (GT, first, second), tokens_after_second

  | {token_type = LE; line = _} :: t ->
      let second, tokens_after_second = parse_rel_expr  t in
      Relation (LE, first, second), tokens_after_second

  | {token_type = GE; line = _} :: t ->
    let second, tokens_after_second = parse_rel_expr  t in
    Relation (GE, first, second), tokens_after_second
  | _ -> ArithmeticUnderRelExpr first, tokens_after_first

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

  | {token_type = Mod; line = _} :: t ->
    let second, tokens_after_second = parse_term t in
    Mod(first, second), tokens_after_second

  | _ -> Factor first, tokens_after_first




and parse_factor (tokens: token list): factor * token list =
  print_endline "printing tokens from parse_factor";
  print_tokens_list tokens;
  print_endline "finished";
  let factors, tokens_after_factors = get_factor_list tokens [] in
  if List.length factors = 0 then failwith "0 factors parsed in parse_factor" else
  if List.length factors > 1 then
    create_factor_app_chain_from_factor_list factors, tokens_after_factors
  else List.hd factors, tokens_after_factors
    
  


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
  | {token_type = Opposite; line = _} :: t ->
    let inside, remaining_tokens = parse_factor_not_app t in
    Opposite inside, remaining_tokens
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

