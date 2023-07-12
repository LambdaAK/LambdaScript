open Lex
open Expr

exception ParseFailure
exception FactorParseFailure


let remove_head: 'a list -> 'a list = function
  | [] -> raise ParseFailure
  | _ :: t -> t

let remove_last (lst: 'a list): 'a * 'a list =
  match List.rev lst with
  | [] -> raise ParseFailure
  | last :: rest ->
    last, List.rev rest


type mulop = Times | Div | Mod

exception UnexpectedToken of token_type * token_type option * int

let assert_next_token (tokens: token list) (expected_value: token_type) =
  match tokens with
  | [] -> raise (UnexpectedToken (expected_value, None, -1))
  | {token_type = t; line = line} :: _ ->
    if t = expected_value then ()
    else raise (UnexpectedToken (expected_value, Some t, line))


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

  | {token_type = TypeVar i; line = _} :: t ->
      TypeVarWritten i, t

  | {token_type = LBracket; line = _} :: t -> (* list type *)
    (* parse a compound type *)
    let compound_type, tokens_after_compound_type = parse_compound_type t in
    (* the next token should be a RBracket *)
    assert_next_token tokens_after_compound_type RBracket;
    ListType compound_type, remove_head tokens_after_compound_type
    
  | {token_type = LParen; line = _} :: t ->

    (* parse a list of compound types seperated by commas *)
    let compound_type_list, tokens_after_compound_type_list = parse_compound_type_list_seperated_by_commas t in
    (* the next token should be a RParen *)
    assert_next_token tokens_after_compound_type_list RParen;
    (* if the length of compound_type_list is 1, return a paren factor *)
    (* otherwise return a vector *)
    if List.length compound_type_list = 1 then
      ParenFactorType (List.hd compound_type_list), remove_head tokens_after_compound_type_list
    else
      VectorType compound_type_list, remove_head tokens_after_compound_type_list

  | _ -> raise ParseFailure


and parse_compound_type_list_seperated_by_commas (tokens: token list): compound_type list * token list =
  let first, tokens_after_first = parse_compound_type tokens in
  match tokens_after_first with (* if the next token is a comma, remove the comma and parse another type *)
  | {token_type = Comma; line = _} :: t ->
    let second, tokens_after_second = parse_compound_type_list_seperated_by_commas t in
    first :: second, tokens_after_second
  | _ -> [first], tokens_after_first


and parse_defn (tokens: token list): defn * token list =
  match tokens with
  | {token_type = Let; line = _} :: t ->
    (* let p <- e *)
    (* parse a pattern *)
    let pattern, tokens_after_pattern = parse_pat t in
    
    (* check if there's a type annotation after that *)
    (
      match tokens_after_pattern with
      | {token_type = LBracket; line = _} :: t ->
        (* parse a type *)
        let annotated_type, tokens_after_type = parse_compound_type t in
        let () = assert_next_token tokens_after_type RBracket in
        let tokens_after_r_bracket = remove_head tokens_after_type in

        (* the next token should be a bind arrow *)

        let () = assert_next_token tokens_after_r_bracket BindArrow in
        let tokens_after_bind_arrow = remove_head tokens_after_r_bracket in

        (* parse an expression *)

        let body_expression, tokens_after_body_expression = parse_expr tokens_after_bind_arrow in

        Defn (pattern, Some annotated_type, body_expression), tokens_after_body_expression

      | _ ->

        (* no type annotation *)

        (* the next token should be a bind arrow *)

        let () = assert_next_token tokens_after_pattern BindArrow in
        let tokens_after_bind_arrow = remove_head tokens_after_pattern in

        (* parse an expression *)

        let body_expression, tokens_after_body_expression = parse_expr tokens_after_bind_arrow in

        Defn (pattern, None, body_expression), tokens_after_body_expression

      )
    | _ -> raise ParseFailure


and parse_expressions_seperated_by_commas (tokens: token list): expr list * token list =
  let first, tokens_after_first = parse_expr tokens in
  match tokens_after_first with (* if the next token is a comma, remove the comma and parse another expression *)
  | {token_type = Comma; line = _} :: t ->
    let second, tokens_after_second = parse_expressions_seperated_by_commas t in
    first :: second, tokens_after_second
  | _ -> [first], tokens_after_first


and parse_switch_branches (tokens: token list): switch_branch list * token list =
    match tokens with
    | {token_type = Pipe; line = _} :: t ->
      (* | p1 -> e1 *)
      (* parse a pattern *)
      let pattern, tokens_after_pattern = parse_pat t in
      (* the next token should be an arrow *)
      assert_next_token tokens_after_pattern Arrow;
      let tokens_after_arrow = remove_head tokens_after_pattern in
      (* parse an expression *)
      let expression, tokens_after_expression = parse_expr tokens_after_arrow in
      (* if the next token is pipe, continue parsing switch branches *)
      (
        match tokens_after_expression with
        | {token_type = Pipe; line = _} :: _ ->
          let rest, tokens_after_rest = parse_switch_branches tokens_after_expression in
          (pattern, expression) :: rest, tokens_after_rest
        | _ -> (pattern, expression) :: [], tokens_after_expression
      )
    | _ -> raise ParseFailure


and parse_expr (tokens: token list) : expr * token list =
  match tokens with
  | {token_type = Lam; line = _} :: t -> (* anonymous function *)
    parse_function t
    
  | {token_type = If; line = _} :: t -> (* ternary *)
    (* parse the guard *)
    let guard, tokens_after_guard = parse_expr t in
    (* the next token should be a Then *)
    assert_next_token tokens_after_guard Then;
    let e1, tokens_after_e1 = parse_expr (remove_head tokens_after_guard) in
    (* the next token should be a Else *)
    assert_next_token tokens_after_e1 Else;
    let e2, tokens_after_e2 = parse_expr (remove_head tokens_after_e1) in
    Ternary (guard, e1, e2), tokens_after_e2
  
  | {token_type = Bind; line = _} :: {token_type = Rec; line = _} :: t -> (* bind rec expression *)
    parse_bind_rec t

  | {token_type = Bind; line = _} :: t -> (* bind expression *)
    parse_bind t

  | {token_type = Switch; line = _} :: t -> (* switch expression *)
    (*
    switch e =>
      | p1 -> e1
      | p2 -> e2
      ...
      | pn -> en
    end   
    *)

    (* parse e *)
    let e, tokens_after_e = parse_expr t in
    (* the next token should be a bind arrow *)
    assert_next_token tokens_after_e SwitchArrow;
    let tokens_after_arrow = remove_head tokens_after_e in
    (* parse a list of switch branches *)
    let switch_branches, tokens_after_switch_branches = parse_switch_branches tokens_after_arrow in
    (* the next token should be an end *)
    assert_next_token tokens_after_switch_branches End;
    Switch (e, switch_branches), remove_head tokens_after_switch_branches


  | _ -> let e, t = parse_cons tokens in ConsExpr e, t

    (*let (e, t): disjunction * token list = parse_disjunction tokens in
    DisjunctionExpr e, t*)
    

and parse_bind_rec (tokens_without_bind_rec: token list): expr * token list =

    let pattern, tokens_after_pattern = parse_pat tokens_without_bind_rec in


    let cto, tokens_after_annotated_type = (* if there is a type annotation, get it here *)
      (
      match tokens_after_pattern with
      | {token_type = LBracket; line = _} :: tokens_after_l_bracket ->
        let ct, tokens_after_ct = parse_compound_type tokens_after_l_bracket in
        (* the next token should be a r bracket *)
        assert_next_token tokens_after_ct RBracket;
        Some ct, remove_head tokens_after_ct
      | _ -> None, tokens_after_pattern
      ) in

    let parse_compound_type_if_possible (tokens: token list): compound_type option * token list =
      match tokens with
      | {token_type = LBracket; line = _} :: tokens_after_l_bracket ->
        let ct, tokens_after_ct = parse_compound_type tokens_after_l_bracket in
        (* the next token should be a r bracket *)
        assert_next_token tokens_after_ct RBracket;
        Some ct, remove_head tokens_after_ct
      | _ -> None, tokens

    in


    let rec parse_pats_while_next_token_is_not_bind_arrow
    (tokens: token list) (acc: (pat * compound_type option) list)
    : (pat * compound_type option) list * token list =
      match tokens with
      | {token_type = BindArrow; line = _ } :: _ -> acc, tokens (* I don't think you need List.rev here *)
      | _ ->
        let next_pat, tokens_after_next_pat = parse_pat tokens in
        (* parse a compound type if possible *)
        let cto, tokens_after_cto = parse_compound_type_if_possible tokens_after_next_pat in
        parse_pats_while_next_token_is_not_bind_arrow tokens_after_cto ((next_pat, cto) :: acc)

    in


    let pattern_list, tokens_after_pattern_list = parse_pats_while_next_token_is_not_bind_arrow tokens_after_annotated_type [] in

    let () = assert_next_token tokens_after_pattern_list BindArrow in

  (* the next token should be a bind arrow *)
  assert_next_token tokens_after_pattern_list BindArrow;
  let body_tokens: token list = remove_head tokens_after_pattern_list in
  let e1, tokens_after_body = parse_expr body_tokens in
  (* the next token should be in *)
  assert_next_token tokens_after_body In;
  let tokens_after_in: token list = remove_head tokens_after_body in
  let e2, tokens_after_e2 = parse_expr tokens_after_in in


  (* wrap e1 with functions using the patterns, right to left *)
  let rec wrap_e1_with_functions (e1: expr) (pattern_list: (pat * compound_type option) list): expr =
    match pattern_list with
    | [] -> e1
    | (pattern, cto) :: rest ->
      wrap_e1_with_functions (Function (pattern, cto, e1)) rest


  in

  let e1_wrapped = wrap_e1_with_functions e1 pattern_list in

  BindRec(pattern, cto, e1_wrapped, e2), tokens_after_e2




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
      assert_next_token tokens_after_pattern Arrow;
      let body_tokens: token list = remove_head tokens_after_pattern in
      let body, tokens_after_body = parse_expr body_tokens in
      Function (pattern, None, body), tokens_after_body


and parse_bind (tokens_without_bind: token list): expr * token list =
  let pattern, tokens_after_pattern = parse_pat tokens_without_bind in


  let cto, tokens_after_annotated_type = (* if there is a type annotation, get it here *)
    (
    match tokens_after_pattern with
    | {token_type = LBracket; line = _} :: tokens_after_l_bracket ->
      let ct, tokens_after_ct = parse_compound_type tokens_after_l_bracket in
      (* the next token should be a r bracket *)
      assert_next_token tokens_after_ct RBracket;
      Some ct, remove_head tokens_after_ct
    | _ -> None, tokens_after_pattern
    ) in

  
    let parse_compound_type_if_possible (tokens: token list): compound_type option * token list =
      match tokens with
      | {token_type = LBracket; line = _} :: tokens_after_l_bracket ->
        let ct, tokens_after_ct = parse_compound_type tokens_after_l_bracket in
        (* the next token should be a r bracket *)
        assert_next_token tokens_after_ct RBracket;
        Some ct, remove_head tokens_after_ct
      | _ -> None, tokens

    in


    let rec parse_pats_while_next_token_is_not_bind_arrow
    (tokens: token list) (acc: (pat * compound_type option) list)
    : (pat * compound_type option) list * token list =
      match tokens with
      | {token_type = BindArrow; line = _ } :: _ -> acc, tokens (* I don't think you need List.rev here *)
      | _ ->
        let next_pat, tokens_after_next_pat = parse_pat tokens in
        (* parse a compound type if possible *)
        let cto, tokens_after_cto = parse_compound_type_if_possible tokens_after_next_pat in
        parse_pats_while_next_token_is_not_bind_arrow tokens_after_cto ((next_pat, cto) :: acc)

    in

    let pattern_list, tokens_after_pattern_list = parse_pats_while_next_token_is_not_bind_arrow tokens_after_annotated_type [] in

    let () = assert_next_token tokens_after_pattern_list BindArrow in

  (* the next token should be a bind arrow *)
  assert_next_token tokens_after_pattern_list BindArrow;
  let body_tokens: token list = remove_head tokens_after_pattern_list in
  let e1, tokens_after_body = parse_expr body_tokens in
  (* the next token should be in *)
  assert_next_token tokens_after_body In;
  let tokens_after_in: token list = remove_head tokens_after_body in
  let e2, tokens_after_e2 = parse_expr tokens_after_in in


  (* wrap e1 with functions using the patterns, right to left *)
  let rec wrap_e1_with_functions (e1: expr) (pattern_list: (pat * compound_type option) list): expr =
    match pattern_list with
    | [] -> e1
    | (pattern, cto) :: rest ->
      wrap_e1_with_functions (Function (pattern, cto, e1)) rest


  in

  let e1_wrapped = wrap_e1_with_functions e1 pattern_list in
  ConsExpr(
    DisjunctionUnderCons (
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
                          cto,
                          e2
                        )
                      ),
                      ParenFactor (e1_wrapped)
                    )
                  )
                )
              )
            )
          )
        )
      )
    ), tokens_after_e2



and parse_cons (tokens: token list): cons_expr * token list =
  let first, tokens_after_first = parse_disjunction tokens in
  match tokens_after_first with
  | {token_type = Semicolon; line = _} :: t ->
    let second, tokens_after_second = parse_cons t in
    Cons (first, second), tokens_after_second
  | _ -> DisjunctionUnderCons first, tokens_after_first


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
      (a - b) - c = a - b - c = a - (b + c)

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
  let factor_list, mulop_list, tokens_after_factor_list = parse_factor_list tokens in
  let term = construct_term_from_factor_list_and_mulop_list_helper factor_list mulop_list in
  term, tokens_after_factor_list


and parse_factor_list (tokens: token list): factor list * mulop list * token list =
  let first, tokens_after_first = parse_factor tokens in
  let factor_list: (factor list) ref = ref [first] in
  let remaining_tokens: (token list) ref = ref tokens_after_first in
  let mulop_list: (mulop list) ref = ref [] in
  let guard: bool ref = ref true in

  while !guard do
    
    match !remaining_tokens with
    | {token_type = Times; line = _} :: t ->
      mulop_list := !mulop_list @ [Times];
      let next_factor, tokens_after_next_factor = parse_factor t in
      factor_list := !factor_list @ [next_factor];
      remaining_tokens := tokens_after_next_factor

    | {token_type = Divide; line = _} :: t ->
      mulop_list := !mulop_list @ [Div];
      let next_factor, tokens_after_next_factor = parse_factor t in
      factor_list := !factor_list @ [next_factor];
      remaining_tokens := tokens_after_next_factor

    | {token_type = Mod; line = _} :: t ->
      mulop_list := !mulop_list @ [Mod];
      let next_factor, tokens_after_next_factor = parse_factor t in
      factor_list := !factor_list @ [next_factor];
      remaining_tokens := tokens_after_next_factor

    | _ -> guard := false

  done;
  List.rev !factor_list, List.rev !mulop_list, !remaining_tokens
  

and construct_term_from_factor_list_and_mulop_list_helper (factor_list_reversed: factor list) (mulop_list_reversed: mulop list): term =
  match factor_list_reversed, mulop_list_reversed with
  | [], [] -> failwith "impossible"
  | f :: [], [] -> Factor f
  | f :: f_rest, m :: m_rest ->
    (
      match m with
      | Times -> Mul (construct_term_from_factor_list_and_mulop_list_helper f_rest m_rest, f)
      | Div -> Div (construct_term_from_factor_list_and_mulop_list_helper f_rest m_rest, f)
      | Mod -> Mod (construct_term_from_factor_list_and_mulop_list_helper f_rest m_rest, f)
    )
  | _ -> failwith "impossible"



and parse_factor (tokens: token list): factor * token list =
  let factors, tokens_after_factors = get_factor_list tokens [] in
  if List.length factors = 0 then raise ParseFailure else
  if List.length factors > 1 then
    create_factor_app_chain_from_factor_list factors, tokens_after_factors
  else List.hd factors, tokens_after_factors
    
  
and parse_pats_seperated_by_commas (tokens: token list): pat list * token list =
  let first, tokens_after_first = parse_pat tokens in
  match tokens_after_first with (* if the next token is a comma, remove the comma and parse another pat *)
  | {token_type = Comma; line = _} :: t ->
    let second, tokens_after_second = parse_pats_seperated_by_commas t in
    first :: second, tokens_after_second
  | _ -> [first], tokens_after_first

and parse_pat (tokens: token list): pat * token list = match tokens with
| [] -> failwith "empty list passed to parse_pat"
| {token_type = Nothing; line = _} :: t ->
  NothingPat, t
| {token_type = WildcardPattern; line = _} :: t ->
  WildcardPat, t
| {token_type = Id s; line = _} :: t ->
  IdPat s, t

| {token_type = Integer n; line = _} :: t ->
  IntPat n, t

| {token_type = Boolean b; line = _} :: t ->
  BoolPat b, t

| {token_type = StringToken s; line = _} :: t ->
  StringPat s, t

| {token_type = LParen; line = _} :: t ->
  (* parse a list of pats seperated by commas *)
  let pat_list, tokens_after_pat_list = parse_pats_seperated_by_commas t in
  (* the next token should be a RParen *)
  assert_next_token tokens_after_pat_list RParen;
  (* if the length of pat_list is 1, return a paren pat *)
  (* otherwise return a vector *)
  if List.length pat_list = 1 then
    (List.hd pat_list), remove_head tokens_after_pat_list
  else
    VectorPat pat_list, remove_head tokens_after_pat_list

| _ -> raise ParseFailure


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
  | {token_type = LBracket; line = _} :: {token_type = RBracket; line = _} :: t ->
    Nil, t
  | {token_type = LParen; line = _} :: t ->
    (* parse a list of expressions seperated by commas *)
    let expr_list, tokens_after_expr_list = parse_expressions_seperated_by_commas t in
    (* the next token should be a RParen *)
    assert_next_token tokens_after_expr_list RParen;
    (* if the length of expr_list is 1, return a paren factor *)
    (* otherwise return a vector *)
    if List.length expr_list = 1 then
      ParenFactor (List.hd expr_list), remove_head tokens_after_expr_list
    else
      Vector (expr_list), remove_head tokens_after_expr_list

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
