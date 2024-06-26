open Lex
open Expr

exception ParseFailure
exception FactorParseFailure

(** [remove_head lst] returns the list [lst] without the first element. If [lst]
    is empty, [ParseFailure] is raised. *)
let remove_head : 'a list -> 'a list = function
  | [] -> raise ParseFailure
  | _ :: t -> t

(** [remove_last lst] returns a tuple of the last element of [lst] and the list
    [lst] without the last element. If [lst] is empty, [ParseFailure] is raised. *)
let remove_last (lst : 'a list) : 'a * 'a list =
  match List.rev lst with
  | [] -> raise ParseFailure
  | last :: rest -> (last, List.rev rest)

(** [addop] represents the different types of addition operators *)
type addop =
  | AddopPlus
  | AddopMinus
  | AddopCustom of string

(** [mulop] represents the different types of multiplication operators *)
type mulop =
  | MulopTimes
  | MulopDiv
  | MulopMod
  | MulopCustom of string

(** [relop] represents the different types of relational operators *)
type relop =
  | EQ
  | NE
  | LT
  | GT
  | LE
  | GE
  | RelopCustom of string

(** [get_relop_if_exists tokens] returns a tuple of an optional [relop] and the
    list [tokens] without the first element if the first element of [tokens] is
    a [relop]. Otherwise, [None] is returned as the first element of the tuple. *)
let get_relop_if_exists (tokens : token list) =
  match tokens with
  | { token_type = Relop "=="; line = _ } :: t -> (Some EQ, t)
  | { token_type = Relop "<>"; line = _ } :: t -> (Some NE, t)
  | { token_type = Relop "<"; line = _ } :: t -> (Some LT, t)
  | { token_type = Relop ">"; line = _ } :: t -> (Some GT, t)
  | { token_type = Relop "<="; line = _ } :: t -> (Some LE, t)
  | { token_type = Relop ">="; line = _ } :: t -> (Some GE, t)
  | { token_type = Relop s; line = _ } :: t -> (Some (RelopCustom s), t)
  | _ -> (None, tokens)

(** [get_addop_if_exists tokens] returns a tuple of an optional [addop] and the
    list [tokens] without the first element if the first element of [tokens] is
    an [addop]. Otherwise, [None] is returned as the first element of the tuple. *)
let get_addop_if_exists (tokens : token list) =
  match tokens with
  | { token_type = Addop "+"; line = _ } :: t -> (Some AddopPlus, t)
  | { token_type = Addop "-"; line = _ } :: t -> (Some AddopMinus, t)
  | { token_type = Addop s; line = _ } :: t -> (Some (AddopCustom s), t)
  | _ -> (None, tokens)

(** [get_mulop_if_exists tokens] returns a tuple of an optional [mulop] and the
    list [tokens] without the first element if the first element of [tokens] is
    a [mulop]. Otherwise, [None] is returned as the first element of the tuple. *)
let get_mulop_if_exists (tokens : token list) =
  match tokens with
  | { token_type = Mulop "*"; line = _ } :: t -> (Some MulopTimes, t)
  | { token_type = Mulop "/"; line = _ } :: t -> (Some MulopDiv, t)
  | { token_type = Mulop "%"; line = _ } :: t -> (Some MulopMod, t)
  | _ -> (None, tokens)

(* parse a list of expressions seperated by seperators return the list of
   expressions, the list of seperators, and the remaining tokens *)
let rec parse_repeat parse_fun get_sep_if_exists tokens =
  (* parse the first one *)
  let first, tokens_after_first = parse_fun tokens in
  (* check if there's a seperator *)
  let sep_opt, tokens_after_sep = get_sep_if_exists tokens_after_first in
  match sep_opt with
  | None -> (* no seperator *) ([ first ], [], tokens_after_first)
  | Some sep ->
      (* sep is the seperator, we need to parse more *)
      let rest_exprs, rest_seperators, tokens_after_rest =
        parse_repeat parse_fun get_sep_if_exists tokens_after_sep
      in
      (rest_exprs @ [ first ], rest_seperators @ [ sep ], tokens_after_rest)

let combine_expressions exprs seps =
  let rec combine_expressions_aux exprs_rev seps_rev terminal_function
      combine_function =
    match (exprs_rev, seps_rev) with
    | [], [] -> failwith "impossible"
    | e :: [], [] -> terminal_function e
    | e :: e_rest, s :: s_rest ->
        combine_function
          (combine_expressions_aux e_rest s_rest terminal_function
             combine_function)
          e s
    | _ -> failwith "impossible"
  in
  combine_expressions_aux exprs seps

let combine_arith_exprs_into_rel_op arith_exprs relops =
  combine_expressions arith_exprs relops
    (fun arith_expr -> ArithmeticUnderRelExpr arith_expr)
    (fun rel_expr arith_expr relop ->
      match relop with
      | EQ -> Relation (EQ, rel_expr, arith_expr)
      | NE -> Relation (NE, rel_expr, arith_expr)
      | LT -> Relation (LT, rel_expr, arith_expr)
      | GT -> Relation (GT, rel_expr, arith_expr)
      | LE -> Relation (LE, rel_expr, arith_expr)
      | GE -> Relation (GE, rel_expr, arith_expr)
      | RelopCustom s -> CustomRelExpr (s, rel_expr, arith_expr))

let combine_terms_into_arith_expr terms addops =
  combine_expressions terms addops
    (fun t -> Term t)
    (fun arith_expr term addop ->
      match addop with
      | AddopPlus -> Plus (arith_expr, term)
      | AddopMinus -> Minus (arith_expr, term)
      | AddopCustom s -> CustomArithExpr (s, arith_expr, term))

let combine_factors_into_term factors mulops =
  combine_expressions factors mulops
    (fun f -> Factor f)
    (fun term factor mulop ->
      match mulop with
      | MulopTimes -> Mul (term, factor)
      | MulopDiv -> Div (term, factor)
      | MulopMod -> Mod (term, factor)
      | MulopCustom s -> CustomTerm (s, term, factor))

exception UnexpectedToken of token_type * token_type option * int

(** [assert_next_token tokens expected_value] asserts that the next token in
    [tokens] is [expected_value]. If it is not, [UnexpectedToken] is raised.
    Else, [()] is returned. *)
let assert_next_token (tokens : token list) (expected_value : token_type) =
  match tokens with
  | [] -> raise (UnexpectedToken (expected_value, None, -1))
  | { token_type = t; line } :: _ ->
      if t = expected_value then ()
      else raise (UnexpectedToken (expected_value, Some t, line))

(** [parse_compound_type tokens] parses a compound type from [tokens] and
    returns a tuple of the parsed compound type and the remaining tokens. *)
let rec parse_compound_type (tokens : token list) : compound_type * token list =
  let left_type, tokens_after_left_type = parse_factor_app_type tokens in
  match tokens_after_left_type with
  (* check if the next token is an arrow *)
  | { token_type = Arrow; line = _ } :: tokens_after_arrow ->
      (* parse a function type *)
      (* parse another compound type to the right of the arrow *)
      let right_type, tokens_after_right_type =
        parse_compound_type tokens_after_arrow
      in
      (FunctionType (left_type, right_type), tokens_after_right_type)
  | _ ->
      (* return the basic type *)
      (BasicType left_type, tokens_after_left_type)

(** [parse_sub_pat tokens] parses a sub pattern from [tokens] and returns a
    tuple of the parsed sub pattern and the remaining tokens. *)
and parse_factor_type_list (tokens : token list) : factor_type list * token list
    =
  let first, tokens_after_first = parse_factor_type tokens in
  if List.length tokens_after_first = 0 then ([ first ], tokens_after_first)
  else
    let next_token_type = (List.hd tokens_after_first).token_type in
    match next_token_type with
    | IntegerType
    | BooleanType
    | StringType
    | UnitType
    | FloatType
    | Constructor _
    | LParen
    | Id _
    | LBracket ->
        let second, tokens_after_second =
          parse_factor_type_list tokens_after_first
        in
        (first :: second, tokens_after_second)
    | _ -> ([ first ], tokens_after_first)

and combine_factor_types_into_factor_app_type (factor_types : factor_type list)
    =
  (* we have a b c d .... z which is parsed as (a b c d .... ) z *)
  let reversed_factor_types = List.rev factor_types in

  let rec combine_aux (types_reversed : factor_type list) =
    match types_reversed with
    | [] -> failwith "impossible"
    | [ last ] -> FactorType last
    | last :: before -> AppType (combine_aux before, last)
  in

  combine_aux reversed_factor_types

(** [parse_factor_app_type tokens] parses a factor application type from
    [tokens] and returns a tuple of the parsed factor application type and the
    remaining tokens. *)
and parse_factor_app_type (tokens : token list) : factor_app_type * token list =
  (* parse a list of factors *)
  let factors, tokens_after_factors = parse_factor_type_list tokens in
  (* combine the factors into a factor app type *)
  let factor_app_type = combine_factor_types_into_factor_app_type factors in
  (factor_app_type, tokens_after_factors)

(** [parse_sub_pat tokens] parses a compound type from [tokens], if possible,
    and returns a tuple of an option of parsed compound type and the remaining
    tokens. *)
and parse_compound_type_if_possible (tokens : token list) :
    compound_type option * token list =
  match tokens with
  | { token_type = LBracket; line = _ } :: tokens_after_l_bracket ->
      let ct, tokens_after_ct = parse_compound_type tokens_after_l_bracket in
      (* the next token should be a r bracket *)
      assert_next_token tokens_after_ct RBracket;
      (Some ct, remove_head tokens_after_ct)
  | _ -> (None, tokens)

(** Parses patterns while the next tokens is not "<-". Returns a tuple of the
    parsed patterns and the remaining tokens. *)
and parse_pats_while_next_token_is_not_bind_arrow (tokens : token list)
    (acc : (pat * compound_type option) list) :
    (pat * compound_type option) list * token list =
  match tokens with
  | { token_type = Equals; line = _ } :: _ ->
      (acc, tokens (* I don't think you need List.rev here *))
  | _ ->
      let next_pat, tokens_after_next_pat = parse_pat tokens in
      (* parse a compound type if possible *)
      let cto, tokens_after_cto =
        parse_compound_type_if_possible tokens_after_next_pat
      in
      parse_pats_while_next_token_is_not_bind_arrow tokens_after_cto
        ((next_pat, cto) :: acc)

(** [parse_factor_type tokens] parses a factor type from [tokens] and returns a
    tuple of the parsed factor type and the remaining tokens. *)
and parse_factor_type (tokens : token list) : factor_type * token list =
  match tokens with
  | { token_type = IntegerType; line = _ } :: t -> (IntegerType, t)
  | { token_type = BooleanType; line = _ } :: t -> (BooleanType, t)
  | { token_type = StringType; line = _ } :: t -> (StringType, t)
  | { token_type = UnitType; line = _ } :: t -> (UnitType, t)
  | { token_type = Id i; line = _ } :: t -> (TypeVarWritten i, t)
  | { token_type = Constructor c; _ } :: t -> (TypeName c, t)
  | { token_type = LBracket; line = _ } :: t ->
      (* list type *)
      (* parse a compound type *)
      let compound_type, tokens_after_compound_type = parse_compound_type t in
      (* the next token should be a RBracket *)
      assert_next_token tokens_after_compound_type RBracket;
      (ListType compound_type, remove_head tokens_after_compound_type)
  | { token_type = LParen; line = _ } :: t ->
      (* parse a list of compound types seperated by commas *)
      let compound_type_list, tokens_after_compound_type_list =
        parse_compound_type_list_seperated_by_commas t
      in
      (* the next token should be a RParen *)
      assert_next_token tokens_after_compound_type_list RParen;
      (* if the length of compound_type_list is 1, return a paren factor *)
      (* otherwise return a vector *)
      if List.length compound_type_list = 1 then
        ( ParenFactorType (List.hd compound_type_list),
          remove_head tokens_after_compound_type_list )
      else
        ( VectorType compound_type_list,
          remove_head tokens_after_compound_type_list )
  | _ -> raise ParseFailure

(** Parsses a list of compound types seperated by commas. Returns the list of
    compound types and the list of remaining tokens *)
and parse_compound_type_list_seperated_by_commas (tokens : token list) :
    compound_type list * token list =
  let first, tokens_after_first = parse_compound_type tokens in
  match tokens_after_first with
  (* if the next token is a comma, remove the comma and parse another type *)
  | { token_type = Comma; line = _ } :: t ->
      let second, tokens_after_second =
        parse_compound_type_list_seperated_by_commas t
      in
      (first :: second, tokens_after_second)
  | _ -> ([ first ], tokens_after_first)

(** Parses the contents of a definition. Returns a tuple of the parsed pattern,
    the optional type annotation, the body expression, and the remaining tokens. *)
and parse_defn_contents (t : token list) :
    pat * compound_type option * expr * token list =
  (* let p <- e *)
  (* parse a pattern *)
  let pattern, tokens_after_pattern = parse_pat t in

  (* check if there's a type annotation after that *)
  match tokens_after_pattern with
  | { token_type = LBracket; line = _ } :: t ->
      (* parse a type *)
      let annotated_type, tokens_after_type = parse_compound_type t in
      let () = assert_next_token tokens_after_type RBracket in
      let tokens_after_r_bracket = remove_head tokens_after_type in

      (* the next token should be a bind arrow *)
      let () = assert_next_token tokens_after_r_bracket Equals in

      let tokens_after_bind_arrow = remove_head tokens_after_r_bracket in

      (* parse an expression *)
      let body_expression, tokens_after_body_expression =
        parse_expr tokens_after_bind_arrow
      in

      ( pattern,
        Some annotated_type,
        body_expression,
        tokens_after_body_expression )
  | _ ->
      (* no type annotation *)

      (* the next token should be a bind arrow *)
      let () = assert_next_token tokens_after_pattern Equals in

      let tokens_after_bind_arrow = remove_head tokens_after_pattern in

      (* parse an expression *)
      let body_expression, tokens_after_body_expression =
        parse_expr tokens_after_bind_arrow
      in

      (pattern, None, body_expression, tokens_after_body_expression)

(** Parses a constructor from [tokens] and returns a tuple of the parsed
    constructor and the remaining tokens. *)
and parse_constructor (tokens : token list) : constructor * token list =
  (* 2 cases NullaryConstructor ParametricConstructor : type *)

  (* parse the constructor name *)
  let constructor_name, tokens_after_constructor_name =
    match tokens with
    | { token_type = Constructor s; _ } :: t -> (s, t)
    | _ ->
        print_endline "parse constructor failed";
        raise ParseFailure
  in

  (* check if theres a ( if there is, we need to parse the type that the
     constructor takes *)
  let constructor_type, tokens_after_constructor_type =
    match tokens_after_constructor_name with
    | { token_type = LParen; _ } :: _ ->
        (* parse the type that the constructor takes *)
        let constructor_type, tokens_after_constructor_type =
          parse_compound_type tokens_after_constructor_name
        in
        (Some constructor_type, tokens_after_constructor_type)
    | _ -> (None, tokens_after_constructor_name)
  in

  match constructor_type with
  | None -> (NullaryConstructor constructor_name, tokens_after_constructor_type)
  | Some constructor_type ->
      ( UnaryConstructor (constructor_name, constructor_type),
        tokens_after_constructor_type )

(** Parses a list of constructors from [tokens] and returns a tuple of the
    parsed list of constructors and the remaining tokens. *)
and parse_constructor_list (tokens : token list) (acc : constructor list) :
    constructor list * token list =
  match tokens with
  | { token_type = Pipe; _ } :: t ->
      let constructor, tokens_after_constructor = parse_constructor t in
      parse_constructor_list tokens_after_constructor (constructor :: acc)
  | _ -> (List.rev acc, tokens)

(** [wrap_type] is used to created a curried polymorphic type from a list of
    type variables and a type

    For example: wrap_type [a; b; c] t = PolymorphicType (a, PolymorphicType (b,
    PolymorphicType (c, t))) *)

(** Parses a definition. Returns a tuple of the parsed definition and the
    remaining tokens. *)
and parse_defn (tokens : token list) : defn * token list =
  match tokens with
  | { token_type = Type; line = _ } :: t -> (
      (* parse a type definition *)

      (* the next token should be an id *)
      let id, tokens_after_id =
        match t with
        | { token_type = Constructor s; line = _ } :: t -> (s, t)
        | _ ->
            print_endline "parse failure from parse_defn type definition";
            raise ParseFailure
      in

      (* parse type variables before the equals sign *)
      let rec parse_type_vars = function
        | { token_type = Id s; line = _ } :: t ->
            let type_vars, tokens_after_type_vars = parse_type_vars t in
            (s :: type_vars, tokens_after_type_vars)
        | tokens -> ([], tokens)
      in

      let type_vars, tokens_after_type_vars = parse_type_vars tokens_after_id in

      assert_next_token tokens_after_type_vars Equals;

      let tokens_after_bind_arrow = remove_head tokens_after_type_vars in

      (* if the next token is |, parse a sum type, otherwise parse a compound
         type *)
      match tokens_after_bind_arrow with
      | { token_type = Pipe; _ } :: _ ->
          (* parse a sum type *)
          let union_type, tokens_after_union_type =
            parse_constructor_list tokens_after_bind_arrow []
          in

          (* wrap the union type *)
          (UnionDefn (id, union_type, type_vars), tokens_after_union_type)
      | _ ->
          let body_expression, tokens_after_body_expression =
            parse_compound_type tokens_after_bind_arrow
          in

          (* replace written type vars with type vars *)
          ( TypeDefn (id, body_expression, type_vars),
            tokens_after_body_expression ))
  | _ ->
      let tokens_without_let, is_rec =
        match tokens with
        | { token_type = Let; line = _ } :: { token_type = Rec; line = _ } :: t
          -> (t, true)
        | { token_type = Let; line = _ } :: t -> (t, false)
        | _ ->
            print_endline "parse defn failed";
            (* print the tokens *)
            List.iter
              (fun t ->
                print_endline
                  ("token type: "
                  ^ string_of_token_type t.token_type
                  ^ " line: " ^ string_of_int t.line))
              tokens;
            raise ParseFailure
      in

      let pattern, tokens_after_pattern = parse_pat tokens_without_let in

      let cto, tokens_after_annotated_type =
        (* if there is a type annotation, get it here *)
        match tokens_after_pattern with
        | { token_type = LBracket; line = _ } :: tokens_after_l_bracket ->
            let ct, tokens_after_ct =
              parse_compound_type tokens_after_l_bracket
            in
            (* the next token should be a r bracket *)
            assert_next_token tokens_after_ct RBracket;
            (Some ct, remove_head tokens_after_ct)
        | _ -> (None, tokens_after_pattern)
      in

      let pattern_list, tokens_after_pattern_list =
        parse_pats_while_next_token_is_not_bind_arrow
          tokens_after_annotated_type []
      in

      let () = assert_next_token tokens_after_pattern_list Equals in
      assert_next_token tokens_after_pattern_list Equals;
      let body_tokens : token list = remove_head tokens_after_pattern_list in
      let e, tokens_after_body = parse_expr body_tokens in

      let rec wrap_e1_with_functions (e1 : expr)
          (pattern_list : (pat * compound_type option) list) : expr =
        match pattern_list with
        | [] -> e1
        | (pattern, cto) :: rest ->
            wrap_e1_with_functions (Function (pattern, cto, e1)) rest
      in

      let e_wrapped = wrap_e1_with_functions e pattern_list in

      if is_rec then (DefnRec (pattern, cto, e_wrapped), tokens_after_body)
      else (Defn (pattern, cto, e_wrapped), tokens_after_body)

(** Parses a list of expressions seperated by commas. Returns the list of
    expressions and the list of remaining tokens. *)
and parse_expressions_seperated_by_commas (tokens : token list) :
    expr list * token list =
  let first, tokens_after_first = parse_expr tokens in
  match tokens_after_first with
  (* if the next token is a comma, remove the comma and parse another
     expression *)
  | { token_type = Comma; line = _ } :: t ->
      let second, tokens_after_second =
        parse_expressions_seperated_by_commas t
      in
      (first :: second, tokens_after_second)
  | _ -> ([ first ], tokens_after_first)

(** Parses a list of switch branches. Returns a tuple of the parsed list of
    switch branches and the remaining tokens. *)
and parse_switch_branches (tokens : token list) :
    switch_branch list * token list =
  match tokens with
  | { token_type = Pipe; line = _ } :: t -> (
      (* | p1 -> e1 *)
      (* parse a pattern *)
      let pattern, tokens_after_pattern = parse_pat t in
      (* the next token should be an arrow *)
      assert_next_token tokens_after_pattern Arrow;
      let tokens_after_arrow = remove_head tokens_after_pattern in
      (* parse an expression *)
      let expression, tokens_after_expression = parse_expr tokens_after_arrow in
      (* if the next token is pipe, continue parsing switch branches *)
      match tokens_after_expression with
      | { token_type = Pipe; line = _ } :: _ ->
          let rest, tokens_after_rest =
            parse_switch_branches tokens_after_expression
          in
          ((pattern, expression) :: rest, tokens_after_rest)
      | _ -> ((pattern, expression) :: [], tokens_after_expression))
  | _ -> raise ParseFailure

(** Parses an expr. Returns a tuple of the parsed expr and the remaining tokens. *)
and parse_expr (tokens : token list) : expr * token list =
  match tokens with
  | { token_type = Fn; line = _ } :: t ->
      (* anonymous function *)
      parse_function t
  | { token_type = If; line = _ } :: t ->
      (* ternary *)
      (* parse the guard *)
      let guard, tokens_after_guard = parse_expr t in
      (* the next token should be a Then *)
      assert_next_token tokens_after_guard Then;
      let e1, tokens_after_e1 = parse_expr (remove_head tokens_after_guard) in
      (* the next token should be a Else *)
      assert_next_token tokens_after_e1 Else;
      let e2, tokens_after_e2 = parse_expr (remove_head tokens_after_e1) in
      (Ternary (guard, e1, e2), tokens_after_e2)
  | { token_type = Let; line = _ } :: { token_type = Rec; line = _ } :: t ->
      (* bind rec expression *)
      parse_bind_rec t
  | { token_type = Let; line = _ } :: t ->
      (* bind expression *)
      parse_bind t
  | { token_type = Switch; line = _ } :: t ->
      (* switch expression *)
      (* switch e => | p1 -> e1 | p2 -> e2 ... | pn -> en end  *)

      (* parse e *)
      let e, tokens_after_e = parse_expr t in
      (* the next token should be a bind arrow *)
      assert_next_token tokens_after_e SwitchArrow;
      let tokens_after_arrow = remove_head tokens_after_e in
      (* parse a list of switch branches *)
      let switch_branches, tokens_after_switch_branches =
        parse_switch_branches tokens_after_arrow
      in
      (* the next token should be an end *)
      assert_next_token tokens_after_switch_branches End;
      (Switch (e, switch_branches), remove_head tokens_after_switch_branches)
  | _ ->
      let e, t = parse_cons tokens in
      (ConsExpr e, t)

(** Parses a bind rec expression. Returns a tuple of the parsed bind rec
    expression and the remaining tokens. *)
and parse_bind_rec (tokens_without_bind_rec : token list) : expr * token list =
  let pattern, tokens_after_pattern = parse_pat tokens_without_bind_rec in

  let cto, tokens_after_annotated_type =
    (* if there is a type annotation, get it here *)
    match tokens_after_pattern with
    | { token_type = LBracket; line = _ } :: tokens_after_l_bracket ->
        let ct, tokens_after_ct = parse_compound_type tokens_after_l_bracket in
        (* the next token should be a r bracket *)
        assert_next_token tokens_after_ct RBracket;
        (Some ct, remove_head tokens_after_ct)
    | _ -> (None, tokens_after_pattern)
  in

  let pattern_list, tokens_after_pattern_list =
    parse_pats_while_next_token_is_not_bind_arrow tokens_after_annotated_type []
  in

  let () = assert_next_token tokens_after_pattern_list Equals in

  (* the next token should be a bind arrow *)
  assert_next_token tokens_after_pattern_list Equals;
  let body_tokens : token list = remove_head tokens_after_pattern_list in
  let e1, tokens_after_body = parse_expr body_tokens in
  (* the next token should be in *)
  assert_next_token tokens_after_body In;
  let tokens_after_in : token list = remove_head tokens_after_body in
  let e2, tokens_after_e2 = parse_expr tokens_after_in in

  (* wrap e1 with functions using the patterns, right to left *)
  let rec wrap_e1_with_functions (e1 : expr)
      (pattern_list : (pat * compound_type option) list) : expr =
    match pattern_list with
    | [] -> e1
    | (pattern, cto) :: rest ->
        wrap_e1_with_functions (Function (pattern, cto, e1)) rest
  in

  let e1_wrapped = wrap_e1_with_functions e1 pattern_list in

  (BindRec (pattern, cto, e1_wrapped, e2), tokens_after_e2)

(** Parses a function. Returns the function and the list of remaining tokens.*)
and parse_function (tokens_without_lam : token list) : expr * token list =
  let pattern, tokens_after_pattern = parse_pat tokens_without_lam in
  (* check if there is a left bracket for a type annotation *)
  match tokens_after_pattern with
  | { token_type = LBracket; line = _ } :: tokens_after_l_bracket ->
      let ct, tokens_after_ct = parse_compound_type tokens_after_l_bracket in
      (* disregard the next r bracket and arrow *)
      let body_tokens : token list =
        tokens_after_ct |> remove_head |> remove_head
      in
      let body, tokens_after_body = parse_expr body_tokens in
      (Function (pattern, Some ct, body), tokens_after_body)
  | _ ->
      (* no type annotation *)
      (* in this case, the next token is arrow *)
      assert_next_token tokens_after_pattern Arrow;
      let body_tokens : token list = remove_head tokens_after_pattern in
      let body, tokens_after_body = parse_expr body_tokens in
      (Function (pattern, None, body), tokens_after_body)

(** Parses a bind expression. Returns a tuple of the parsed bind expression and
    the list of remaining tokens. *)
and parse_bind (tokens_without_bind : token list) : expr * token list =
  let pattern, tokens_after_pattern = parse_pat tokens_without_bind in

  let cto, tokens_after_annotated_type =
    (* if there is a type annotation, get it here *)
    match tokens_after_pattern with
    | { token_type = LBracket; line = _ } :: tokens_after_l_bracket ->
        let ct, tokens_after_ct = parse_compound_type tokens_after_l_bracket in
        (* the next token should be a r bracket *)
        assert_next_token tokens_after_ct RBracket;
        (Some ct, remove_head tokens_after_ct)
    | _ -> (None, tokens_after_pattern)
  in

  let pattern_list, tokens_after_pattern_list =
    parse_pats_while_next_token_is_not_bind_arrow tokens_after_annotated_type []
  in

  let () = assert_next_token tokens_after_pattern_list Equals in

  (* the next token should be a bind arrow *)
  assert_next_token tokens_after_pattern_list Equals;
  let body_tokens : token list = remove_head tokens_after_pattern_list in
  let e1, tokens_after_body = parse_expr body_tokens in
  (* the next token should be in *)
  assert_next_token tokens_after_body In;
  let tokens_after_in : token list = remove_head tokens_after_body in
  let e2, tokens_after_e2 = parse_expr tokens_after_in in

  (* wrap e1 with functions using the patterns, right to left *)
  let rec wrap_e1_with_functions (e1 : expr)
      (pattern_list : (pat * compound_type option) list) : expr =
    match pattern_list with
    | [] -> e1
    | (pattern, cto) :: rest ->
        wrap_e1_with_functions (Function (pattern, cto, e1)) rest
  in

  let e1_wrapped = wrap_e1_with_functions e1 pattern_list in
  ( ConsExpr
      (DisjunctionUnderCons
         (ConjunctionUnderDisjunction
            (RelationUnderConjunction
               (ArithmeticUnderRelExpr
                  (Term
                     (Factor
                        (Application
                           ( FactorUnderApplication
                               (ParenFactor (Function (pattern, cto, e2))),
                             ParenFactor e1_wrapped )))))))),
    tokens_after_e2 )

(** Parses a cons expression. Returns a tuple of the parsed cons expression and
    the remaining tokens. *)
and parse_cons (tokens : token list) : cons_expr * token list =
  let first, tokens_after_first = parse_disjunction tokens in
  match tokens_after_first with
  | { token_type = ConsToken; line = _ } :: t ->
      let second, tokens_after_second = parse_cons t in
      (Cons (first, second), tokens_after_second)
  | _ -> (DisjunctionUnderCons first, tokens_after_first)

(** Parses a disjunction. Returns a tuple of the parsed disjunction and the
    remaining tokens. *)
and parse_disjunction (tokens : token list) : disjunction * token list =
  let first, tokens_after_first = parse_conjunction tokens in
  match tokens_after_first with
  | { token_type = OR; line = _ } :: t ->
      let second, tokens_after_second = parse_disjunction t in
      (Disjunction (first, second), tokens_after_second)
  | _ -> (ConjunctionUnderDisjunction first, tokens_after_first)

(** Parses a conjunction. Returns a tuple of the parsed conjunction and the
    remaining tokens. *)
and parse_conjunction (tokens : token list) : conjunction * token list =
  let first, tokens_after_first = parse_rel_expr tokens in
  match tokens_after_first with
  | { token_type = AND; line = _ } :: t ->
      let second, tokens_after_second = parse_conjunction t in
      (Conjunction (first, second), tokens_after_second)
  | _ -> (RelationUnderConjunction first, tokens_after_first)

(** Parses a relation expression. Returns a tuple of the parsed relation
    expression and the remaining tokens. *)
and parse_rel_expr (tokens : token list) : rel_expr * token list =
  let arith_expr_list, relop_list, tokens_after_arith_expr_list =
    parse_arith_expr_list tokens
  in
  let arith_expr = combine_arith_exprs_into_rel_op arith_expr_list relop_list in
  (arith_expr, tokens_after_arith_expr_list)

(** Parses an arithmetic expression. Returns a tuple of the parsed arithmetic
    expression and the remaining tokens. *)
and parse_arith_expr (tokens : token list) : arith_expr * token list =
  let term_list, addop_list, tokens_after_term_list = parse_term_list tokens in
  let term = combine_terms_into_arith_expr term_list addop_list in
  (term, tokens_after_term_list)

(** Parses a term. Returns a tuple of the parsed term and the remaining tokens. *)
and parse_term (tokens : token list) : term * token list =
  let factor_list, mulop_list, tokens_after_factor_list =
    parse_factor_list tokens
  in

  let term = combine_factors_into_term factor_list mulop_list in
  (term, tokens_after_factor_list)

(** Parses an app factor. Returns a tuple of the parsed app factor and the
    remaining tokens. *)
and parse_app_factor (tokens : token list) : app_factor * token list =
  let factors, tokens_after_factors = get_factor_list tokens [] in
  if List.length factors = 0 then raise ParseFailure
  else if List.length factors > 1 then
    (create_factor_app_chain_from_factor_list factors, tokens_after_factors)
  else (FactorUnderApplication (List.hd factors), tokens_after_factors)

(** Parses a list of factors seperated by mulops *)
and parse_factor_list (tokens : token list) =
  parse_repeat parse_app_factor get_mulop_if_exists tokens

(** Parses a list of terms seperated by addops *)
and parse_term_list (tokens : token list) =
  parse_repeat parse_term get_addop_if_exists tokens

(** Parses a list of arith_exprs seperated by relops *)
and parse_arith_expr_list (tokens : token list) =
  parse_repeat parse_arith_expr get_relop_if_exists tokens

(** Parses a list of patterns seperated by commas *)
and parse_pats_seperated_by_commas (tokens : token list) : pat list * token list
    =
  let first, tokens_after_first = parse_pat tokens in
  match tokens_after_first with
  (* if the next token is a comma, remove the comma and parse another pat *)
  | { token_type = Comma; line = _ } :: t ->
      let second, tokens_after_second = parse_pats_seperated_by_commas t in
      (first :: second, tokens_after_second)
  | _ -> ([ first ], tokens_after_first)

(** Checks if the current list of tokens allows for a sub_pat to be parsed.
    Returns true if it does, false otherwise. *)
and sub_pat_is_ahead (prev_pat : sub_pat) (tokens : token list) : bool =
  (* Look for the following things ( unit Nil id int float bool string *)
  match prev_pat with
  | ConstructorPat _ ->
      if List.length tokens = 0 then false
      else
        let first_token = (List.hd tokens).token_type in
        begin
          match first_token with
          | LParen
          | Unit
          | Id _
          | Integer _
          | FloatToken _
          | Boolean _
          | StringToken _
          | WildcardPattern -> true
          | _ -> false
        end
  | _ -> false

(** Parses a pat. Returns a tuple of the parsed pat and the remaining tokens. *)
and parse_pat (tokens : token list) : pat * token list =
  (* parse a sub_pat *)
  let sub_pat, tokens_after_sub_pat = parse_sub_pat tokens in

  (* print the next token *)

  (* check the next token *)
  match tokens_after_sub_pat with
  | { token_type = ConsToken; line = _ } :: t ->
      (* parse another pat *)
      (* return the cons *)
      let second_pat, tokens_after_second_pat = parse_pat t in
      (ConsPat (sub_pat, second_pat), tokens_after_second_pat)
  | _ when sub_pat_is_ahead sub_pat tokens_after_sub_pat ->
      (* parse another sub_pat, and we will have a constructor application *)
      let second_sub_pat, tokens_after_second_sub_pat =
        parse_sub_pat tokens_after_sub_pat
      in
      (AppPat (sub_pat, second_sub_pat), tokens_after_second_sub_pat)
  | _ -> (SubPat sub_pat, tokens_after_sub_pat)

(** Parses a sub_pat. Returns a tuple of the parsed sub_pat and the remaining
    tokens. *)
and parse_sub_pat (tokens : token list) : sub_pat * token list =
  match tokens with
  | [] -> failwith "empty list passed to parse_pat"
  | { token_type = Unit; line = _ } :: t -> (UnitPat, t)
  | { token_type = WildcardPattern; line = _ } :: t -> (WildcardPat, t)
  | { token_type = Id s; line = _ } :: t -> (IdPat s, t)
  | { token_type = Integer n; line = _ } :: t -> (IntPat n, t)
  | { token_type = Boolean b; line = _ } :: t -> (BoolPat b, t)
  | { token_type = StringToken s; line = _ } :: t -> (StringPat s, t)
  | { token_type = Constructor s; _ } :: t -> (ConstructorPat s, t)
  | { token_type = LBracket; line = _ }
    :: { token_type = RBracket; line = _ }
    :: t -> (NilPat, t)
  | { token_type = LParen; line = _ }
    :: { token_type = Addop s; line = _ }
    :: { token_type = RParen; line = _ }
    :: t
  | { token_type = LParen; line = _ }
    :: { token_type = Mulop s; line = _ }
    :: { token_type = RParen; line = _ }
    :: t
  | { token_type = LParen; line = _ }
    :: { token_type = Relop s; line = _ }
    :: { token_type = RParen; line = _ }
    :: t -> (InfixPat s, t)
  | { token_type = LParen; line = _ } :: t ->
      (* parse a list of pats seperated by commas *)
      let pat_list, tokens_after_pat_list = parse_pats_seperated_by_commas t in
      (* the next token should be a RParen *)
      assert_next_token tokens_after_pat_list RParen;
      (* if the length of pat_list is 1, return a paren pat *)
      (* otherwise return a vector *)
      if List.length pat_list = 1 then
        (Pat (List.hd pat_list), remove_head tokens_after_pat_list)
      else (VectorPat pat_list, remove_head tokens_after_pat_list)
  | _ -> raise ParseFailure

(** Parses a factor that is not an application. *)
and parse_factor_not_app (tokens : token list) : factor * token list =
  match tokens with
  | { token_type = LParen; line = _ }
    :: { token_type = Addop s; line = _ }
    :: { token_type = RParen; line = _ }
    :: t
  | { token_type = LParen; line = _ }
    :: { token_type = Mulop s; line = _ }
    :: { token_type = RParen; line = _ }
    :: t
  | { token_type = LParen; line = _ }
    :: { token_type = Relop s; line = _ }
    :: { token_type = RParen; line = _ }
    :: t -> (Id s, t)
  | { token_type = Integer n; line = _ } :: t -> (Integer n, t)
  | { token_type = FloatToken n; line = _ } :: t -> (FloatFactor n, t)
  | { token_type = Opposite; line = _ } :: t ->
      let inside, remaining_tokens = parse_factor_not_app t in
      (Opposite inside, remaining_tokens)
  | { token_type = Boolean b; line = _ } :: t -> (Boolean b, t)
  | { token_type = StringToken s; line = _ } :: t -> (String s, t)
  | { token_type = Unit; line = _ } :: t -> (Unit, t)
  | { token_type = Id s; line = _ } :: t -> (Id s, t)
  | { token_type = LBracket; line = _ }
    :: { token_type = RBracket; line = _ }
    :: t -> (Nil, t)
  | { token_type = LBracket; line = _ } :: t -> (
      (* list syntactic sugar *)

      (* need to decide whether to parse a listsugar, listenumeration, or
         listcomprehension

         we have [x ____

         parse x. if the token that follows is ..., then it's a listenumeration

         if the token that follows is |, then it's a listcomprehension

         otherwise, it's a listsugar *)

      (* parse a list of expressions seperated by ; *)
      let _, tokens_after_e1 = parse_expr t in
      (* check what the next token is *)
      match tokens_after_e1 with
      | [] -> raise FactorParseFailure
      | { token_type = Enum; line = _ } :: _ ->
          (* it's a list enumeration *)
          parse_list_enumeration t
      | { token_type = Pipe; line = _ } :: _ -> parse_list_comprehension t
      | _ ->
          (* it's a list sugar *)
          parse_list_sugar t)
  | { token_type = LParen; line = _ } :: t ->
      (* parse a list of expressions seperated by commas *)
      let expr_list, tokens_after_expr_list =
        parse_expressions_seperated_by_commas t
      in
      (* the next token should be a RParen *)
      assert_next_token tokens_after_expr_list RParen;
      (* if the length of expr_list is 1, return a paren factor *)
      (* otherwise return a vector *)
      if List.length expr_list = 1 then
        (ParenFactor (List.hd expr_list), remove_head tokens_after_expr_list)
      else (Vector expr_list, remove_head tokens_after_expr_list)
  | { token_type = Constructor n; _ } :: t -> (Constructor n, t)
  | _ -> raise FactorParseFailure

(** Parses a list syntactic sugar Ex: [1, 2, 3, 4, 5] *)
and parse_list_sugar (t : token list) : factor * token list =
  (* parse a list of expressions seperated by ; *)
  let inside_expressions, tokens_after_expr_list =
    parse_expressions_seperated_by_commas t
  in
  (* the next token should be a RBracket *)
  assert_next_token tokens_after_expr_list RBracket;

  let after_tokens : token list = remove_head tokens_after_expr_list in

  (ListSugar inside_expressions, after_tokens)

(** Parses a list enumeration. Ex: [1 ... 10] *)
and parse_list_enumeration (t : token list) : factor * token list =
  let e1, tokens_after_e1 = parse_expr t in
  (* the next token should be ... *)
  assert_next_token tokens_after_e1 Enum;
  let tokens_after_enum = remove_head tokens_after_e1 in
  let e2, tokens_after_e2 = parse_expr tokens_after_enum in
  (* the next token should be a RBracket *)
  assert_next_token tokens_after_e2 RBracket;
  let tokens_after_enumeration = remove_head tokens_after_e2 in
  (ListEnumeration (e1, e2), tokens_after_enumeration)

(** Parses a generator, which is something of the form p <- e *)
and parse_generator (tokens : token list) : generator * token list =
  let pattern, tokens_after_pattern = parse_pat tokens in
  (* next token should be a bind arrow *)
  assert_next_token tokens_after_pattern BindArrow;
  let tokens_after_bind_arrow = remove_head tokens_after_pattern in
  let e, tokens_after_e = parse_expr tokens_after_bind_arrow in
  let generator : generator = (pattern, e) in
  (generator, tokens_after_e)

(** Parses a list of generators seperated by commas. Returns the list of
    generators and the list of remaining tokens *)
and parse_generator_list (tokens : token list) : generator list * token list =
  let first, tokens_after_first = parse_generator tokens in
  match tokens_after_first with
  | { token_type = Comma; line = _ } :: t ->
      let rest, tokens_after_rest = parse_generator_list t in
      (first :: rest, tokens_after_rest)
  | _ -> ([ first ], tokens_after_first)

(** Parses a list comprehension. Returns the list comprehension and the list of
    remaining tokens. Ex: [[x * x | x <- [1, 2, 3, 4, 5]]] *)
and parse_list_comprehension (tokens : token list) : factor * token list =
  (* e | generators ] *)
  let e, tokens_after_e = parse_expr tokens in
  (* the next token should be a pipe *)
  assert_next_token tokens_after_e Pipe;
  let tokens_after_pipe = remove_head tokens_after_e in
  let generator_list, tokens_after_generator_list =
    parse_generator_list tokens_after_pipe
  in
  (* the next token should be a r bracket *)
  assert_next_token tokens_after_generator_list RBracket;
  let tokens_after_r_bracket = remove_head tokens_after_generator_list in
  (ListComprehension (e, generator_list), tokens_after_r_bracket)

(** Parses a list of factors while possible, and returns the list of factors and
    the remaining tokens. *)
and get_factor_list (tokens : token list) (acc : factor list) :
    factor list * token list =
  try
    let new_expr, remaining_tokens = parse_factor_not_app tokens in
    get_factor_list remaining_tokens (new_expr :: acc)
  with FactorParseFailure -> (List.rev acc, tokens (* return no new exprs *))

(** Chains a list of factors into an application *)
and create_factor_app_chain_from_factor_list (factors : factor list) :
    app_factor =
  match factors with
  | [] -> failwith "impossible"
  | f :: [] -> FactorUnderApplication f
  | factors_list ->
      let last, factors_without_last = remove_last factors_list in
      Application
        (create_factor_app_chain_from_factor_list factors_without_last, last)

(** Parses a program, which is a list of definitions *)
let rec parse_program = function
  | [] -> []
  | tokens ->
      let defn, tokens_after_defn = parse_defn tokens in
      defn :: parse_program tokens_after_defn
