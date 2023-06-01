open Lex
open Expr



exception ParseFailure
exception FactorParseFailure



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
  | {token_type = Lam; line = _} :: t -> (* anonymous function *)
    parse_function t
    
  | {token_type = If; line = _} :: t -> (* ternary *)
    (* parse the guard *)
    let guard, tokens_after_guard = parse_expr t in
    (* the next token should be a Then *)
    let e1, tokens_after_e1 = parse_expr (remove_head tokens_after_guard) in
    (* the next token should be a Else *)
    let e2, tokens_after_e2 = parse_expr (remove_head tokens_after_e1) in
    Ternary (guard, e1, e2), tokens_after_e2
  
  | {token_type = Bind; line = _} :: t -> (* bind expression *)
    parse_bind t



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


and parse_bind (tokens_without_bind: token list): expr * token list =
  let pattern, tokens_after_pattern = parse_pat tokens_without_bind in
  (* check if there is a left bracket for a type annotation *)
  match tokens_after_pattern with
  | {token_type = LBracket; line = _} :: tokens_after_l_bracket ->
    let ct, tokens_after_ct = parse_compound_type tokens_after_l_bracket in
    (* disregard the next r bracket and bind arrow *)
    let tokens_after_bind_arrow = tokens_after_ct |> remove_head |> remove_head in
    let e1, tokens_after_e1 = parse_expr tokens_after_bind_arrow in
    (* the next token should be in *)
    (* remove it *)
    let tokens_after_in = remove_head tokens_after_e1 in
    let e2, tokens_after_e2 = parse_expr tokens_after_in in

    
    DisjunctionExpr (
      ConjunctionUnderDisjunction (
        EqualityUnderConjunction (
          RelationUnderEqExpr (
            ArithmeticUnderRelExpr (
              Term (
                Factor (
                  App (
                    ParenFactor (
                      Function (
                        pattern,
                        Some ct,
                        e2
                      )
                    ),
                    ParenFactor (e1)
                  )
                )
              )
            )
          )
        )
      )
    ), tokens_after_e2


  | _ ->
    (* there is no type annotation *)
    (* the next token should be the bind arrow *)
    let tokens_after_bind_arrow: token list = remove_head tokens_after_pattern in
    let e1, tokens_after_e1 = parse_expr tokens_after_bind_arrow in
    (* the next token should be in *)
    (* remove it *)
    let tokens_after_in = remove_head tokens_after_e1 in
    let e2, tokens_after_e2 = parse_expr tokens_after_in in

    DisjunctionExpr (
      ConjunctionUnderDisjunction (
        EqualityUnderConjunction (
          RelationUnderEqExpr (
            ArithmeticUnderRelExpr (
              Term (
                Factor (
                  App (
                    ParenFactor (
                      Function (
                        pattern,
                        None,
                        e2
                      )
                    ),
                    ParenFactor (e1)
                  )
                )
              )
            )
          )
        )
      )
    ), tokens_after_e2
    




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

    let second, tokens_after_second = parse_arith_expr t in
    (*
      if second is a Minus, the expression
      a - b - c
      has been incorrectly parsed as  
      a - (b - c)

      a - b - c is equivalent to the following
      (a - b) - c = a - b - c + a - (b + c)

      change it to that

      if second is a Plus, the expression
      a - b + c
      has been incorrectly parsed as
      a - (b + c)

      a - b + c is equivalent to
      a - b + c = a - (b - c)

      change it to that

      
    
     *)
      (
    match second with
    | Minus (b, c) ->
      Minus (first, Plus(b, c)), tokens_after_second
      (* Minus (arith_expr_to_term (Minus (first, Term b)), c), tokens_after_second *)
    | Plus (b, c) -> 
      Minus (first, Minus (b, c)), tokens_after_second
      (* Plus (arith_expr_to_term (Minus(first, Term b)), c), tokens_after_second *)
    | _ -> Minus (first, second), tokens_after_second
    
      )

  
  | _ -> Term first, tokens_after_first



and parse_term (tokens: token list): term * token list =
  (* start by parsing a factor *)
  let first, tokens_after_first = parse_factor tokens in
  let a = first in
  (* check what the next token is *)
  (* if its a TIMES or DIVIDE, parse another term and return the product/quotient *)
  match tokens_after_first with
  | {token_type = Times; line = _} :: t ->
    let second, tokens_after_second = parse_term t in
    (
    match second with
    | Div (b, c) ->
      (* a * (b / c) -> (a * b) / c *)
      Div (term_to_factor (Mul(a, Factor b)), c), tokens_after_second
    | Mod (b, c) ->
      (* a * (b / c) -> (a * b) / c *)
      Mod (term_to_factor (Mul(a, Factor b)), c), tokens_after_second
    | _ ->
      Mul (first, second), tokens_after_second
    )
  | {token_type = Divide; line = _} :: t ->
    let second, tokens_after_second = parse_term t in
    (
    match second with
    | Mul (b, c) ->
      (* a / (b * c) -> (a / b) * c *)
      Mul (term_to_factor c, Div(a, Factor b)), tokens_after_second
    | Div (b, c) ->
      (* a / (b / c) -> (a / b) / c *)
      Div (term_to_factor (Div (a, Factor b)), c), tokens_after_second
    | Mod (b, c) ->
      (* a / (b % c) -> (a / b) % c *)
      Mod (term_to_factor (Div (a, Factor b)), c), tokens_after_second
    | _ -> Div (first, second), tokens_after_second
    )
  | {token_type = Mod; line = _} :: t ->
    let second, tokens_after_second = parse_term t in
    (
      match second with
      | Mod (b, c) ->
        (* a % (b % c) -> (a % b) % c *)
        Mod (term_to_factor (Mod (a, Factor b)), c), tokens_after_second
      | Mul (b, c) ->
        (* a % (b * c) -> (a % b) * c *)
        Mul (term_to_factor c, Mod (a, Factor b)), tokens_after_second
      | Div (b, c) ->
        (* a % (b / c) -> (a % b) / c *)
        Div (term_to_factor (Mod(a, Factor b)), c), tokens_after_second
      | _ ->
        Mod (first, second), tokens_after_second
    )

  | _ -> Factor first, tokens_after_first




and parse_factor (tokens: token list): factor * token list =
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

  (*| {token_type = LBrace; line = _} :: t ->
    (* infix *)
    (* the first token in t is the infix operator *)
    let infix_op: token_type = (List.hd t).token_type in
    (* the next token is the first operand *)
    let tokens_after_infix_op_and_r_brace: token list = t |> remove_head |> remove_head in

    Infix (token_type_to_infix_op infix_op), tokens_after_infix_op_and_r_brace*)



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




and term_to_factor (t: term): factor =
ParenFactor (
  DisjunctionExpr (
    ConjunctionUnderDisjunction (
      EqualityUnderConjunction (
        RelationUnderEqExpr (
          ArithmeticUnderRelExpr (
            Term t
          )
        )
      )
    )
  )
)

and arith_expr_to_term (ae: arith_expr): term =
Factor (ParenFactor (
  DisjunctionExpr (
    ConjunctionUnderDisjunction (
      EqualityUnderConjunction (
        RelationUnderEqExpr (
          ArithmeticUnderRelExpr ae
        )
      )
    )
  )
)
)

