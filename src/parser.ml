open Lex
open Expr

(* Idea - Make a module for each level of parser - Use a functor to combine them
   - Use another functor to then combine all of those into condensed parser

   Wrap each parser definition in lazy so that they can be mutually recursive
   without any problems Could also thunk as unit -> 'a parser, which would be
   fully evaluated already, so the mutual recursion would work

   Use recursive modules to organize the parsers for the different levels in the
   grammar

   Make sure each chain of <|> has the correct order of parsers. Larger parsers
   should usually come earlier in the chain.

   The reason the parser is very slow is because of <|>. We are not using
   lookahead in order to determine which parser to use. At leach level, we use
   at least 2 parsers, which compounds, and makes the time complexity really
   bad. *)

type 'a parser_result = 'a option
type 'a parser = token_type list -> ('a * token_type list) parser_result
type 'a lazy_parser = unit -> 'a parser

module ParserUtils = struct
  let ( <|> ) (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
   fun tokens ->
    match p1 tokens with
    | Some (result, remaining_tokens) -> Some (result, remaining_tokens)
    | None -> p2 tokens

  let ( <* ) (parser_one : 'a parser) (parser_two : 'b parser) : 'b parser =
   fun tokens ->
    match parser_one tokens with
    | Some (_, remaining_tokens) -> parser_two remaining_tokens
    | None -> None

  let ( *> ) parser_one parser_two =
   fun tokens ->
    match parser_one tokens with
    | Some (_, remaining_tokens) -> parser_two remaining_tokens
    | None -> None

  let ( >>= ) (parser : 'a parser) (f : 'a -> 'b parser) : 'b parser =
   fun tokens ->
    match parser tokens with
    | Some (result, remaining_tokens) -> f result remaining_tokens
    | None -> None

  (** * Parser is a parser for 'a * tokens is a list of tokens * Returns a
      parser that parses 'a while possible, and returns the list of 'a *)
  let parse_several (parser : 'a parser) =
    let rec parse_several' acc tokens =
      match parser tokens with
      | Some (result, remaining_tokens) ->
          parse_several' (result :: acc) remaining_tokens
      | None -> Some (List.rev acc, tokens)
    in
    parse_several' []

  (** * Parses a list of 'a, separated by delimiters. Takes: * - a parser for 'a
      * - a delimiter token_type * Returns a list of 'a (parsed results) * There
      will be one more 'a than delimiters if parsing succeeds. * First, parses
      an 'a, if possible. If not possible, returns [], [] * Otherwise, parses
      'a, and then (delim, 'a) pairs until there isn't a delimiter next *)
  let parse_sep_delim (parser : 'a parser) (delim : token_type) : 'a list parser
      =
    let rec helper acc_values tokens =
      match parser tokens with
      | Some (value, remaining_tokens) -> (
          match remaining_tokens with
          | delim_token :: rest when delim_token = delim ->
              helper (value :: acc_values) rest
          | _ -> Some (List.rev (value :: acc_values), remaining_tokens))
      | None -> Some (List.rev acc_values, tokens)
    in
    fun tokens -> helper [] tokens

  (** One or more [item] parses; an optional [Comma] may appear after each item
      (including between items). Stops when [item] fails; requires at least one
      successful parse. *)
  let parse_one_or_more_opt_commas (item : 'a parser) : 'a list parser =
    let rec go acc tokens =
      match item tokens with
      | Some (v, rest) ->
          let rest =
            match rest with
            | Comma :: t -> t
            | t -> t
          in
          go (v :: acc) rest
      | None ->
          if acc = [] then None else Some (List.rev acc, tokens)
    in
    go []

  (** Parses a list of 'a, separated by delimiters. Takes:
      - a parser for 'a
      - a predicate function to check if a token is a delimiter Returns a tuple
        of:
      - a list of 'a (parsed results)
      - a list of token_type (delimiters encountered) There will be one more 'a
        than delimiters if parsing succeeds. *)
  let parse_with_delims (parser : 'a parser) (is_delim : token_type -> bool) :
      ('a list * token_type list) parser =
    let rec helper acc_values acc_delims tokens =
      match parser tokens with
      | Some (value, remaining_tokens) -> (
          (* Check for a delimiter *)
          match remaining_tokens with
          | delim :: rest when is_delim delim ->
              (* Parse the delimiter and continue *)
              helper (value :: acc_values) (delim :: acc_delims) rest
          | _ ->
              (* No more delimiters, return results *)
              Some
                ( (List.rev (value :: acc_values), List.rev acc_delims),
                  remaining_tokens ))
      | None ->
          (* Parsing failed for the first item or subsequent values *)
          if acc_values = [] then None
          else Some ((List.rev acc_values, List.rev acc_delims), tokens)
    in
    fun tokens -> helper [] [] tokens

  let ( let* ) = ( >>= )

  let ( >== ) (x : 'a option) (f : 'a -> 'b option) : 'b option =
    match x with
    | Some x -> f x
    | None -> None

  let ( let= ) = ( >== )

  let expect_token (expected : token_type) : unit parser =
   fun tokens ->
    match tokens with
    | [] -> None
    | token :: remaining_tokens ->
        if token = expected then Some ((), remaining_tokens) else None

  let expect_token_get_data (pred : token_type -> 'a option) : 'a parser =
   fun tokens ->
    match tokens with
    | [] -> None
    | token :: remaining_tokens -> (
        match pred token with
        | Some data -> Some (data, remaining_tokens)
        | None -> None)

  let return (result : 'a) : 'a parser = fun tokens -> Some (result, tokens)
  let fail : 'a parser = fun _ -> None

  let combine_parsers (parsers : 'a parser list) : 'a parser =
    List.fold_left ( <|> ) fail parsers

  let unimplemented_parser (parser_name : string) : 'a parser =
    ignore parser_name;
    fail

  let remove_last (lst : 'a list) : 'a * 'a list =
    match List.rev lst with
    | [] -> failwith "remove_last: empty list"
    | last :: rest -> (last, List.rev rest)

  let combine_expressions exprs seps =
    (* note that the reversed lists are inputted into the aux function *)
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
    combine_expressions_aux (List.rev exprs) (List.rev seps)

  let combine_arith_exprs_into_rel_expr arith_exprs rel_ops =
    combine_expressions arith_exprs rel_ops
      (fun a -> ArithmeticUnderRelExpr a)
      (fun rel_expr arith_expr rel_op ->
        match rel_op with
        | Relop s when s = "==" -> Relation (EQ, rel_expr, arith_expr)
        | Relop s when s = "!=" -> Relation (NE, rel_expr, arith_expr)
        | Relop s when s = "<" -> Relation (LT, rel_expr, arith_expr)
        | Relop s when s = ">" -> Relation (GT, rel_expr, arith_expr)
        | Relop s when s = "<=" -> Relation (LE, rel_expr, arith_expr)
        | Relop s when s = ">=" -> Relation (GE, rel_expr, arith_expr)
        | Relop s -> CustomRelExpr (s, rel_expr, arith_expr)
        | _ -> failwith "impossible")

  let combine_factors_into_term factors mulops =
    combine_expressions factors mulops
      (fun f -> Factor f)
      (fun term factor mulop ->
        match mulop with
        | Mulop "*" -> Mul (term, factor)
        | Mulop "/" -> Div (term, factor)
        | Mulop "%" -> Mod (term, factor)
        | Mulop s -> CustomTerm (s, term, factor)
        | _ -> failwith "impossible")

  let combine_terms_into_arith_expr terms addops =
    combine_expressions terms addops
      (fun t -> Term t)
      (fun arith_expr term addop ->
        match addop with
        | Addop "+" -> Plus (arith_expr, term)
        | Addop "-" -> Minus (arith_expr, term)
        | Addop s -> CustomArithExpr (s, arith_expr, term)
        | _ -> failwith "impossible")

  let parse_print (msg : string) : unit parser =
    ignore msg;
    return ()

  (** [parse_print_tokens ()] is a parser utility function for debugging.

      When invoked, it prints all tokens currently available to the parser to
      standard output, one per line, prefixed by "Tokens:". It does not consume
      or modify the token stream, and always succeeds, returning
      [Some ((), tokens)].

      This is useful for inspecting the state of the token list at a particular
      point in the parsing process. *)
  let parse_print_tokens () : unit parser = fun tokens -> Some ((), tokens)

  (** [dispatch_parser dispatch_list default_parsers] creates a parser that
      selects an appropriate sub-parser based on the current input tokens.

      - [dispatch_list]: A list of predicate-parser pairs. Each predicate is a
        function of type [token_type list -> bool] that checks whether the
        current token list satisfies a condition. If the predicate returns
        [true], the corresponding parser is invoked.

      - [default_parsers]: A list of fallback parsers to try if none of the
        predicates in [dispatch_list] match. These parsers are combined using
        [<|>] (logical OR).

      If none of the predicates match and no [default_parsers] succeed, the
      combined parser will return [None].

      Example usage:
      {[
        let factor_parser : factor parser =
          dispatch_parser
            [
              (function
              | tokens -> (
                  match tokens with
                  | Boolean _ :: _ -> true
                  | _ -> (false, boolean_parser)));
              (function
              | tokens -> (
                  match tokens with
                  | Integer _ :: _ -> true
                  | _ -> (false, integer_parser)));
            ]
            [ fail ]
      ]}
      This creates a [factor_parser] that tries: 1. [boolean_parser] if the
      first token is a [Boolean]. 2. [integer_parser] if the first token is an
      [Integer]. 3. Falls back to [fail] if no predicates match. *)

  let dispatch_parser
      (dispatch_list : ((token_type list -> bool) * 'a parser) list)
      (default_parsers : 'a parser list) : 'a parser =
    let rec try_dispatch dispatch_list tokens =
      match dispatch_list with
      | [] ->
          (* None of the predicate functions matched; use default_parsers *)
          List.fold_left ( <|> ) fail default_parsers tokens
      | (predicate, parser) :: rest ->
          if predicate tokens then parser tokens else try_dispatch rest tokens
    in
    fun tokens -> try_dispatch dispatch_list tokens

  let check_tokens (next_token : token_type) : bool parser =
   fun tokens ->
    match tokens with
    | [] -> Some (false, tokens)
    | token :: _ -> Some (token = next_token, tokens)

  let int_to_expr (i : int) : expr =
    ConsExpr
      (DisjunctionUnderCons
         (ConjunctionUnderDisjunction
            (RelationUnderConjunction
               (ArithmeticUnderRelExpr
                  (Term (Factor (FactorUnderApplication (Integer i))))))))

  let id_to_expr (s : string) : expr =
    ConsExpr
      (DisjunctionUnderCons
         (ConjunctionUnderDisjunction
            (RelationUnderConjunction
               (ArithmeticUnderRelExpr
                  (Term (Factor (FactorUnderApplication (Id s))))))))

  (** Wraps an expression body in a series of function abstractions. Takes a
      body expression and a list of (pattern, type annotation) pairs, and
      returns the body wrapped in nested Function constructors. *)
  let rec wrap_e1_in_functions body
      (arg_pats_and_type_annotations : (pat * compound_type option) list) =
    match arg_pats_and_type_annotations with
    | [] -> body
    | (pat, cto) :: rest -> Function (pat, cto, wrap_e1_in_functions body rest)
end

open ParserUtils

(* factor parsers *)

module rec PatParser : sig
  val pat_parser : pat parser
end = struct
  module rec SubPatParser : sig
    val sub_pat_parser : sub_pat parser
  end = struct
    let is_uppercase_first (s : string) : bool =
      if String.length s = 0 then false
      else
        let c = String.get s 0 in
        c >= 'A' && c <= 'Z'

    let id_or_variant_pat_parser : sub_pat parser =
      (* next token should be id *)
      let* id =
        expect_token_get_data (function
          | Id id -> Some id
          | _ -> None)
      in

      (* Check if it starts with uppercase - if so, it's a constructor *)
      if is_uppercase_first id then
        (* Try to parse an optional payload pattern *)
        let* payload_option =
          (let* payload_sub_pat = SubPatParser.sub_pat_parser in
           let payload_pat = SubPat payload_sub_pat in
           return (Some payload_pat))
          <|> return None
        in
        return (VariantPat (id, payload_option))
      else
        (* Lowercase - it's a variable pattern *)
        return (IdPat id)

    let id_pat_parser : sub_pat parser = id_or_variant_pat_parser

    let unit_pat_parser : sub_pat parser =
      let* () = expect_token Unit in
      return UnitPat

    let bool_pat_parser : sub_pat parser =
      let* b =
        expect_token_get_data (function
          | Boolean b -> Some b
          | _ -> None)
      in

      return (BoolPat b)

    let int_pat_parser : sub_pat parser =
      let* i =
        expect_token_get_data (function
          | Integer i -> Some i
          | _ -> None)
      in

      return (IntPat i)

    let string_pat_parser : sub_pat parser =
      let* s =
        expect_token_get_data (function
          | StringToken s -> Some s
          | _ -> None)
      in

      return (StringPat s)

    let char_pat_parser : sub_pat parser =
      let* c =
        expect_token_get_data (function
          | CharToken c -> Some c
          | _ -> None)
      in
      return (CharPat c)

    let nil_pat_parser : sub_pat parser =
      let* () = expect_token LBracket in
      let* () = expect_token RBracket in
      return NilPat

    let infix_pat_parser : sub_pat parser =
      let* s =
        expect_token_get_data (function
          | Relop s | Addop s | Mulop s | Logop s -> Some s
          | _ -> None)
      in

      return (InfixPat s)

    let paren_infix_pat_parser : sub_pat parser =
      (* ( op ) *)
      let* () = expect_token LParen in
      let* s =
        expect_token_get_data (function
          | Relop s | Addop s | Mulop s | Logop s -> Some s
          | AND -> Some "&&"
          | OR -> Some "||"
          | _ -> None)
      in
      let* () = expect_token RParen in
      return (InfixPat s)

    let wildcard_pat_parser : sub_pat parser =
      let* () = expect_token WildcardPattern in
      return WildcardPat

    let vector_pat_parser : sub_pat parser =
      let* () = expect_token LParen in
      let* pats = parse_sep_delim PatParser.pat_parser Comma in
      let* () = expect_token RParen in
      return (VectorPat pats)

    let record_pat_parser : sub_pat parser =
      let* () = expect_token LBrace in
      let parse_field () =
        let* field_name =
          expect_token_get_data (function
            | Id id -> Some id
            | _ -> None)
        in
        let* () = expect_token Colon in
        let* p = PatParser.pat_parser in
        return (field_name, p)
      in
      let* fields = parse_sep_delim (parse_field ()) Comma in
      let* () = expect_token RBrace in
      return (RecordPat fields)

    let sub_pat_parser : sub_pat parser =
      combine_parsers
        [
          unit_pat_parser;
          bool_pat_parser;
          int_pat_parser;
          string_pat_parser;
          char_pat_parser;
          id_pat_parser;
          nil_pat_parser;
          infix_pat_parser;
          paren_infix_pat_parser;
          wildcard_pat_parser;
          vector_pat_parser;
          record_pat_parser;
        ]
  end

  and PatParser : sig
    val pat_parser : pat parser
  end = struct
    let rec pat_parser () : pat parser =
      let* sub_pat = SubPatParser.sub_pat_parser in
      (* check for :: *)
      let* next_cons = check_tokens ConsToken in
      match next_cons with
      | false -> return (SubPat sub_pat)
      | true ->
          let* () = expect_token ConsToken in
          let* pat = pat_parser () in
          return (ConsPat (sub_pat, pat))

    let pat_parser : pat parser = pat_parser ()
  end

  include PatParser
end

and FactorParser : sig
  val factor_parser : factor parser
end = struct
  open ExprParser

  let rec boolean_parser : factor parser =
    let* b =
      expect_token_get_data (function
        | Boolean b -> Some b
        | _ -> None)
    in
    return (Boolean b)

  and string_parser : factor parser =
    let* s =
      expect_token_get_data (function
        | StringToken s -> Some s
        | _ -> None)
    in
    return (String s)

  and char_parser : factor parser =
    let* c =
      expect_token_get_data (function
        | CharToken c -> Some c
        | _ -> None)
    in
    return (Char c)

  and unit_parser : factor parser =
    let* () = expect_token Unit in
    return Unit

  and integer_parser () : factor parser =
    let* i =
      expect_token_get_data (function
        | Integer i -> Some i
        | _ -> None)
    in
    return (Integer i)

  and float_factor_parser : factor parser =
    let* f =
      expect_token_get_data (function
        | FloatToken f -> Some f
        | _ -> None)
    in
    return (FloatFactor f)

  and id_parser : factor parser =
    let* id =
      expect_token_get_data (function
        | Id id -> Some id
        | _ -> None)
    in
    return (Id id)

  and type_as_id_parser : factor parser =
    (* Handle type tokens (BooleanType, IntegerType, etc.) as identifiers in
       expressions *)
    let* id =
      expect_token_get_data (function
        | BooleanType -> Some "bool"
        | IntegerType -> Some "int"
        | StringType -> Some "string"
        | CharType -> Some "char"
        | FloatType -> Some "float"
        | UnitType -> Some "unit"
        | _ -> None)
    in
    return (Id id)

  and infix_id_parser () : factor parser =
    (* ( op ) *)
    let* () = expect_token LParen in
    let* id =
      expect_token_get_data (function
        | Relop s | Addop s | Mulop s | Logop s -> Some s
        | AND -> Some "&&"
        | OR -> Some "||"
        | _ -> None)
    in

    let* () = expect_token RParen in
    return (Id id)

  and paren_factor_parser : factor parser =
    let* () = expect_token LParen in
    let* expr = expr_parser in
    let* () = expect_token RParen in
    return (ParenFactor expr)

  and opposite_parser () : factor parser =
    let* () = expect_token Opposite in
    let* factor = factor_parser () in
    return (Opposite factor)

  and vector_parser () : factor parser =
    (* The first token should be left paren *)
    let* () = expect_token LParen in
    (* parse a list of expressions seperated by , *)
    let* exprs = parse_sep_delim expr_parser Comma in
    (* the last token should be right paren *)
    let* () = expect_token RParen in
    return (Vector exprs)

  and nil_parser : factor parser =
    let* () = expect_token LBracket in
    let* () = expect_token RBracket in
    return Nil

  and list_sugar_parser () : factor parser =
    let* () = expect_token LBracket in
    let* exprs = parse_sep_delim expr_parser Comma in
    let* () = expect_token RBracket in
    return (ListSugar exprs)

  and list_enumeration_parser () : factor parser =
    (* [a ... b]

       is equivalent to

       [a, a + 1, a + 2, ..., b]

       where a and b are integers *)
    let* () = expect_token LBracket in
    let* a =
      expect_token_get_data (function
        | Integer i -> Some i
        | _ -> None)
    in

    let* () = expect_token Enum in

    let* b =
      expect_token_get_data (function
        | Integer i -> Some i
        | _ -> None)
    in

    let* () = expect_token RBracket in

    return (ListEnumeration (int_to_expr a, int_to_expr b))

  and list_comprehension_branch_parser : (pat * expr) parser =
    let* pat = PatParser.pat_parser in
    let* () = expect_token BindArrow in
    let* expr = expr_parser in
    return (pat, expr)

  and list_comprehension_parser () : factor parser =
    (* [e | p1 = e1, pn = pn] *)
    let* () = expect_token LBracket in
    let* expr = expr_parser in
    let* () = expect_token Pipe in
    let* branches = parse_sep_delim list_comprehension_branch_parser Comma in
    let* () = expect_token RBracket in
    return (ListComprehension (expr, branches))

  and record_update_parser () : factor parser =
    (* { expr with field1 = expr1, field2 = expr2, ... } *)
    let* () = expect_token LBrace in
    let* record_expr = expr_parser in
    let* () = expect_token With in
    let parse_field_update () =
      let* field_name = expect_token_get_data (function
        | Id id -> Some id
        | _ -> None) in
      let* () = expect_token Equals in
      let* field_expr = expr_parser in
      return (field_name, field_expr)
    in
    let* updates = parse_sep_delim (parse_field_update ()) Comma in
    let* () = expect_token RBrace in
    return (RecordUpdate (record_expr, updates))

  and record_lit_parser () : factor parser =
    (* {field1: expr1, field2: expr2, ...} *)
    let* () = expect_token LBrace in
    (* Parse comma-separated field:value pairs *)
    let parse_fields () =
      let* field_name = expect_token_get_data (function
        | Id id -> Some id
        | _ -> None) in
      let* () = expect_token Colon in
      let* field_expr = expr_parser in
      return (field_name, field_expr)
    in
    let* fields = parse_sep_delim (parse_fields ()) Comma in
    let* () = expect_token RBrace in
    return (RecordLit fields)

  and factor_parser () =
    dispatch_parser
      [
        ( (function
          | LBracket :: _ -> true
          | _ -> false),
          list_sugar_parser () <|> list_enumeration_parser ()
          <|> list_comprehension_parser ()
          <|> nil_parser );
        ( (function
          | LParen :: _ -> true
          | _ -> false),
          infix_id_parser () <|> paren_factor_parser <|> vector_parser () );
        ( (function
          | LBrace :: _ -> true
          | _ -> false),
          record_update_parser () <|> record_lit_parser () );
      ]
      [
        boolean_parser;
        string_parser;
        char_parser;
        unit_parser;
        integer_parser ();
        float_factor_parser;
        id_parser;
        type_as_id_parser;
        paren_factor_parser;
        opposite_parser ();
      ]

  (* Parse field access: factor.field.field... *)
  and field_access_parser () : factor parser =
    let rec parse_field_accesses base_factor =
      let* () = expect_token Dot in
      let* field_name = expect_token_get_data (function
        | Id id -> Some id
        | _ -> None) in
      let new_factor = FieldAccess (base_factor, field_name) in
      (* Try to parse more field accesses *)
      (parse_field_accesses new_factor) <|> return new_factor
    in
    let* base = factor_parser () in
    (parse_field_accesses base) <|> return base

  let factor_with_field_access_parser = field_access_parser ()
  let factor_parser = factor_with_field_access_parser
end

and AppFactorParser : sig
  val app_factor_parser : app_factor parser
end = struct
  open FactorParser

  let factor_under_application_parser : app_factor parser =
    let* factor = factor_parser in
    return (FactorUnderApplication factor)

  let application_parser : app_factor parser =
    (* parse a list of factors, then combine them *)
    let* factors = parse_several factor_parser in

    (* combine the factors into a single app factor *)
    let rec combine_factors factors : app_factor option =
      match factors with
      | [] -> None
      | [ factor ] -> Some (FactorUnderApplication factor)
      | factors_list -> (
          (* make the recursive call *)
          match combine_factors (List.tl factors_list) with
          | Some app_factor ->
              Some (Application (app_factor, List.hd factors_list))
          | None -> None)
    in

    let combine_factors factors = combine_factors (List.rev factors) in

    match combine_factors factors with
    | Some app_factor -> return app_factor
    | None -> fail

  let app_factor_parser = application_parser <|> factor_under_application_parser
end

and TermParser : sig
  val term_parser : term parser
end = struct
  open AppFactorParser

  let factor_parser : term parser =
    let* factor = app_factor_parser in
    return (Factor factor)

  let term_op_parser : term parser =
    (* first, parse app_factors separated by delimiters *)
    let* app_factors, seps =
      parse_with_delims app_factor_parser (function
        | Plus | Minus | Times | Mod | Divide | Mulop _ -> true
        | _ -> false)
    in

    (* combine the app factors into a single term *)
    return (combine_factors_into_term app_factors seps)

  (* combine them into a single term *)

  let term_parser : term parser = term_op_parser <|> factor_parser
end

and ArithExprParser : sig
  val arith_expr_parser : arith_expr parser
end = struct
  let term_parser : arith_expr parser =
    let* term = TermParser.term_parser in
    return (Term term)

  let arith_op_parser : arith_expr parser =
    let* terms, addops =
      parse_with_delims TermParser.term_parser (function
        | Plus | Minus | Addop _ -> true
        | _ -> false)
    in

    return (combine_terms_into_arith_expr terms addops)

  let arith_expr_parser : arith_expr parser = arith_op_parser <|> term_parser
end

and RelExprParser : sig
  val rel_expr_parser : rel_expr parser
end = struct
  let arith_expr_parser : rel_expr parser =
    let* arith_expr = ArithExprParser.arith_expr_parser in
    return (ArithmeticUnderRelExpr arith_expr)

  let relation_parser : rel_expr parser =
    (* parse a lit of arith_exprs *)
    let* arith_expr, rel_ops =
      parse_with_delims ArithExprParser.arith_expr_parser (function
        | LT | GT | LE | GE | EQ | NE | Relop _ -> true
        | _ -> false)
    in

    (* combine the arith_exprs and rel_ops into a rel_expr *)
    return (combine_arith_exprs_into_rel_expr arith_expr rel_ops)

  let rel_expr_parser () : rel_expr parser =
    relation_parser <|> arith_expr_parser

  let rel_expr_parser : rel_expr parser = rel_expr_parser ()
end

and ConjunctionParser : sig
  val conjunction_parser : conjunction parser
end = struct
  let rec conjunction_parser () : conjunction parser =
    let* rel_expr = RelExprParser.rel_expr_parser in
    let* next_and = check_tokens AND in
    match next_and with
    | false -> return (RelationUnderConjunction rel_expr)
    | true ->
        let* () = expect_token AND in
        let* conjunction = conjunction_parser () in
        return (Conjunction (rel_expr, conjunction))

  let conjunction_parser : conjunction parser = conjunction_parser ()
end

and DisjunctionParser : sig
  val disjunction_parser : disjunction parser
end = struct
  let rec disjunction_parser () : disjunction parser =
    let* conjunction = ConjunctionParser.conjunction_parser in
    let* next_or = check_tokens OR in
    match next_or with
    | false -> return (ConjunctionUnderDisjunction conjunction)
    | true ->
        let* () = expect_token OR in
        let* disjunction = disjunction_parser () in
        return (Disjunction (conjunction, disjunction))

  let disjunction_parser : disjunction parser = disjunction_parser ()
end

and ConsExprParser : sig
  val cons_expr_parser : cons_expr parser
end = struct
  let rec cons_expr_parser () : cons_expr parser =
    let* disjunction = DisjunctionParser.disjunction_parser in
    (* check the next token *)
    let* next_cons = check_tokens ConsToken in

    match next_cons with
    | false -> return (DisjunctionUnderCons disjunction)
    | true ->
        let* () = expect_token ConsToken in
        let* cons_expr = cons_expr_parser () in
        return (Cons (disjunction, cons_expr))

  let cons_expr_parser : cons_expr parser = cons_expr_parser ()
end

and ExprParser : sig
  val expr_parser : expr parser
  val pat_and_type_annotation_parser : (pat * compound_type option) parser
end = struct
  let rec cons_expr_parser : expr parser =
    let* cons_expr = ConsExprParser.cons_expr_parser in
    return (ConsExpr cons_expr)

  and block_parser () : expr parser =
    (* TODO: I'm pretty sure this code works. I think once we get rid of let
       expressions, it will be fine. *)
    let* () = expect_token LBrace in

    let* parts =
      parse_sep_delim ExprOrDefnParser.expr_or_defn_parser Semicolon
    in
    let* () = expect_token RBrace in
    return (Block parts)

  and function_parser () : expr parser =
    let* () = expect_token Fn in
    let* pat, type_annotation_option = pat_and_type_annotation_parser in

    let* () = expect_token Arrow in
    let* body = expr_parser () in
    return (Function (pat, type_annotation_option, body))

  and ternary_parser () : expr parser =
    (* parse if *)
    let* () = expect_token If in
    (* parse the condition *)
    let* condition = expr_parser () in
    (* parse then *)
    let* () = expect_token Then in
    (* parse the then branch *)
    let* then_branch = expr_parser () in
    (* parse else *)
    let* () = expect_token Else in
    (* parse the else branch *)
    let* else_branch = expr_parser () in
    return (Ternary (condition, then_branch, else_branch))

  and pat_and_type_annotation_parser =
    (* Try to parse (pat : type) first *)
    (let* () = expect_token LParen in
     let* pat = PatParser.pat_parser in
     let* () = expect_token Colon in
     let* ct = CompoundTypeParser.compound_type_parser in
     let* () = expect_token RParen in
     return (pat, Some ct))
    <|>
    (* Otherwise parse just pat without annotation *)
    (let* pat = PatParser.pat_parser in
     return (pat, None))

  (* Helper to parse a single binding component (pattern, args, type, body) for expressions *)
  and parse_single_bind_component () : (pat * compound_type option * expr * compound_type option * int) parser =
    let* pat, cto = pat_and_type_annotation_parser in
    (* parse argument patterns *)
    let* arg_pats_and_type_annotations : (pat * compound_type option) list =
      parse_several pat_and_type_annotation_parser
    in
    (* parse optional return type annotation : Type *)
    let* return_type_option : compound_type option =
      (let* () = expect_token Colon in
       let* ct = CompoundTypeParser.compound_type_parser in
       return (Some ct))
      <|> return None
    in
    let* () = expect_token Equals in
    let* e1 = expr_parser () in

    (* wrap body in functions *)
    let num_explicit_params = List.length arg_pats_and_type_annotations in
    return (pat, cto, wrap_e1_in_functions e1 arg_pats_and_type_annotations, return_type_option, num_explicit_params)

  and bind_rec_parser () : expr parser =
    let* () = expect_token Let in
    let* () = expect_token Rec in
    let* first_bind = parse_single_bind_component () in

    (* Try to parse 'and' clauses for mutual recursion *)
    let* and_binds =
      parse_several (
        let* () = expect_token And in
        parse_single_bind_component ()
      )
    in

    let* () = expect_token In in
    let* e2 = expr_parser () in

    (* If we have and clauses, return BindMutRec, otherwise BindRec *)
    match and_binds with
    | [] ->
        let (pat, cto, e1, return_type_option, _) = first_bind in
        return (BindRec (pat, cto, e1, e2, return_type_option))
    | _ ->
        return (BindMutRec (first_bind :: and_binds, e2))

  and bind_parser () : expr parser =
    let* () = expect_token Let in
    let* pat, cto = pat_and_type_annotation_parser in
    (* parse argument patterns *)
    let* arg_pats_and_type_annotations : (pat * compound_type option) list =
      parse_several pat_and_type_annotation_parser
    in
    (* parse optional return type annotation : Type *)
    let* return_type_option : compound_type option =
      (let* () = expect_token Colon in
       let* ct = CompoundTypeParser.compound_type_parser in
       return (Some ct))
      <|> return None
    in
    let* () = expect_token Equals in
    let* e1 = expr_parser () in
    let* () = expect_token In in
    (* TODO: Try printing what the remaining tokens are here *)
    let* e2 = expr_parser () in

    (* wrap body in functions *)
    return
      (Bind (pat, cto, wrap_e1_in_functions e1 arg_pats_and_type_annotations, e2, return_type_option))

  and branch_parser () : switch_branch parser =
    (* | pat -> expr *)
    let* () = expect_token Pipe in
    let* pat = PatParser.pat_parser in
    let* () = expect_token Arrow in
    let* expr = expr_parser () in
    return (pat, expr)

  and switch_parser () : expr parser =
    (* case e do branches *)
    (*
      Branches have the form
      | pat -> expr
    *)
    let* () = expect_token Case in
    let* e = expr_parser () in
    let* () = expect_token Do in
    (* parse the branches *)
    let* branches = parse_several (branch_parser ()) in
    return (Switch (e, branches))

  and expr_parser () : expr parser =
    block_parser () <|> function_parser () <|> bind_rec_parser ()
    <|> bind_parser () <|> switch_parser () <|> ternary_parser ()
    <|> cons_expr_parser

  let expr_parser : expr parser = expr_parser ()
end

and FactorTypeParser : sig
  val factor_type_parser : factor_type parser
end = struct
  let integer_type_parser : factor_type parser =
    let* () = expect_token IntegerType in
    return IntegerType

  let string_type_parser : factor_type parser =
    let* () = expect_token StringType in
    return StringType

  let boolean_type_parser : factor_type parser =
    let* () = expect_token BooleanType in
    return BooleanType

  let unit_type_parser : factor_type parser =
    let* () = expect_token UnitType in
    return UnitType

  let float_type_parser : factor_type parser =
    let* () = expect_token FloatType in
    return FloatType

  let char_type_parser : factor_type parser =
    let* () = expect_token CharType in
    return CharType

  let type_var_written_parser : factor_type parser =
    let* s =
      expect_token_get_data (function
        | TypeVar s -> Some s
        | _ -> None)
    in
    return (TypeVarWritten s)

  let type_name_parser : factor_type parser =
    let* s =
      expect_token_get_data (function
        | Id s -> Some s
        | _ -> None)
    in

    return (TypeName s)

  let paren_factor_type_parser : factor_type parser =
    let* () = expect_token LParen in
    let* ct = CompoundTypeParser.compound_type_parser in
    let* () = expect_token RParen in
    return (ParenFactorType ct)

  let vector_type_parser : factor_type parser =
    let* () = expect_token LParen in
    let* cts = parse_sep_delim CompoundTypeParser.compound_type_parser Comma in
    let* () = expect_token RParen in
    return (VectorType cts)

  let list_type_parser : factor_type parser =
    let* () = expect_token LBracket in
    let* ct = CompoundTypeParser.compound_type_parser in
    let* () = expect_token RBracket in
    return (ListType ct)

  let type_app_parser : factor_type parser =
    (* type_name < args > where args is comma-separated

       treat type_name as an id token each argument in args is a
       compound_type *)

    (* New type declaration syntax: type triple<a, b, c> = (a, b, c) type
       option<a> = Some a | None type either<a, b> = Left a | Right b *)
    let* name : string =
      expect_token_get_data (function
        | Id s -> Some s
        | _ -> None)
    in
    let* () =
      expect_token_get_data (function
        | Relop "<" -> Some ()
        | _ -> None)
    in
    let* args = parse_sep_delim CompoundTypeParser.compound_type_parser Comma in
    let* () =
      expect_token_get_data (function
        | Relop ">" -> Some ()
        | _ -> None)
    in
    return (TypeApp (name, args))

  let record_type_parser : factor_type parser =
    (* Record type: {field1: type1, field2: type2, ...} *)
    let* () = expect_token LBrace in
    let field_parser : (string * compound_type) parser =
      let* field_name =
        expect_token_get_data (function
          | Id s -> Some s
          | _ -> None)
      in
      let* () = expect_token Colon in
      let* field_type = CompoundTypeParser.compound_type_parser in
      return (field_name, field_type)
    in
    let* fields = parse_sep_delim field_parser Comma in
    let* () = expect_token RBrace in
    return (RecordTypeWritten fields)

  let factor_type_parser () : factor_type parser =
    integer_type_parser <|> string_type_parser <|> boolean_type_parser
    <|> unit_type_parser <|> float_type_parser <|> char_type_parser
    <|> type_var_written_parser <|> vector_type_parser
    <|> paren_factor_type_parser <|> list_type_parser <|> type_app_parser
    <|> record_type_parser <|> type_name_parser

  let factor_type_parser = factor_type_parser ()
end

and CompoundTypeParser : sig
  val compound_type_parser : compound_type parser
end = struct
  (* Parse [factor (-> compound_type)*] without committing to an arrow until we
     see one — so [impl Show for int {] does not consume [int] then fail. *)
  let rec compound_type_parser () : compound_type parser =
    let* ft = FactorTypeParser.factor_type_parser in
    (let* () = expect_token Arrow in
     let* ct = compound_type_parser () in
     return (FunctionType (ft, ct)))
    <|> return (BasicType ft)

  let compound_type_parser : compound_type parser = compound_type_parser ()
end

and DefnParser : sig
  val defn_parser : defn parser
end = struct
  let let_defn_parser () : defn parser =
    let* () = expect_token Let in
    let* pat, cto = ExprParser.pat_and_type_annotation_parser in
    (* parse argument patterns *)
    let* arg_pats_and_type_annotations : (pat * compound_type option) list =
      parse_several ExprParser.pat_and_type_annotation_parser
    in
    (* parse optional type annotation : Type *)
    let* type_annot_option : compound_type option =
      (let* () = expect_token Colon in
       let* ct = CompoundTypeParser.compound_type_parser in
       return (Some ct))
      <|> return None
    in
    let* () = expect_token Equals in
    let* e1 = ExprParser.expr_parser in

    (* Determine if this is a value type annotation or return type annotation *)
    let final_cto, return_type_option =
      match (arg_pats_and_type_annotations, cto, type_annot_option) with
      | [], None, Some t ->
          (* No arguments and no prior type annotation: this is a value type annotation *)
          (Some t, None)
      | _, _, Some t ->
          (* Has arguments or prior type annotation: this is a return type annotation *)
          (cto, Some t)
      | _, _, None ->
          (* No type annotation after arguments *)
          (cto, None)
    in

    (* wrap body in functions *)
    let num_explicit_params = List.length arg_pats_and_type_annotations in
    return
      (Defn (pat, final_cto, wrap_e1_in_functions e1 arg_pats_and_type_annotations, return_type_option, num_explicit_params))

  (* Helper to parse a single definition component (pattern, args, type, body) *)
  let parse_single_defn_component () : (pat * compound_type option * expr * compound_type option * int) parser =
    let* pat, cto = ExprParser.pat_and_type_annotation_parser in
    (* parse argument patterns *)
    let* arg_pats_and_type_annotations : (pat * compound_type option) list =
      parse_several ExprParser.pat_and_type_annotation_parser
    in
    (* parse optional type annotation : Type *)
    let* type_annot_option : compound_type option =
      (let* () = expect_token Colon in
       let* ct = CompoundTypeParser.compound_type_parser in
       return (Some ct))
      <|> return None
    in
    let* () = expect_token Equals in
    let* e1 = ExprParser.expr_parser in

    (* Determine if this is a value type annotation or return type annotation *)
    let final_cto, return_type_option =
      match (arg_pats_and_type_annotations, cto, type_annot_option) with
      | [], None, Some t ->
          (* No arguments and no prior type annotation: this is a value type annotation *)
          (Some t, None)
      | _, _, Some t ->
          (* Has arguments or prior type annotation: this is a return type annotation *)
          (cto, Some t)
      | _, _, None ->
          (* No type annotation after arguments *)
          (cto, None)
    in

    (* wrap body in functions *)
    let num_explicit_params = List.length arg_pats_and_type_annotations in
    return (pat, final_cto, wrap_e1_in_functions e1 arg_pats_and_type_annotations, return_type_option, num_explicit_params)

  let let_rec_defn_parser () : defn parser =
    let* () = expect_token Let in
    let* () = expect_token Rec in
    let* first_defn = parse_single_defn_component () in

    (* Try to parse 'and' clauses for mutual recursion *)
    let* and_defns =
      parse_several (
        let* () = expect_token And in
        parse_single_defn_component ()
      )
    in

    (* If we have and clauses, return DefnMutRec, otherwise DefnRec *)
    match and_defns with
    | [] ->
        let (pat, final_cto, body, return_type_option, num_explicit_params) = first_defn in
        return (DefnRec (pat, final_cto, body, return_type_option, num_explicit_params))
    | _ ->
        return (DefnMutRec (first_defn :: and_defns))

  let string_parser : string parser =
    let* s =
      expect_token_get_data (function
        | TypeVar s -> Some s
        | _ -> None)
    in
    return s

  let method_row_parser : (string * compound_type) parser =
    let* () = expect_token Val in
    let* name =
      expect_token_get_data (function
        | Id s -> Some s
        | _ -> None)
    in
    let* () = expect_token Colon in
    let* ct = CompoundTypeParser.compound_type_parser in
    return (name, ct)

  let class_defn_parser () : defn parser =
    let* () = expect_token Inter in
    let* class_name =
      expect_token_get_data (function
        | Id s -> Some s
        | _ -> None)
    in
    let* params =
      (let* () =
         expect_token_get_data (function
           | Relop "<" -> Some ()
           | _ -> None)
       in
       let* ps = parse_sep_delim string_parser Comma in
       let* () =
         expect_token_get_data (function
           | Relop ">" -> Some ()
           | _ -> None)
       in
       return ps)
      <|> return []
    in
    let* () = expect_token LBrace in
    let* methods = parse_one_or_more_opt_commas method_row_parser in
    let* () = expect_token RBrace in
    return (ClassDef (class_name, params, methods))

  let instance_defn_parser () : defn parser =
    let* () = expect_token Impl in
    let* cls =
      expect_token_get_data (function
        | Id s -> Some s
        | _ -> None)
    in
    let* () = expect_token For in
    let* head_ty = CompoundTypeParser.compound_type_parser in
    let* () = expect_token LBrace in
    let impl_row_parser =
      let* () = expect_token Let in
      let* (name, body_expr) =
        (* Desugar:
           let rec f x y = rhs
           into:
           let rec f x y = rhs in f
           so the method position can still be represented as an [expr]. *)
        (let* () = expect_token Rec in
         let* (pat, final_cto, body, return_type_option, _num_explicit_params) =
           parse_single_defn_component ()
         in
         let () =
           match (final_cto, return_type_option) with
           | None, None -> ()
           | _ ->
               failwith
                 "parser: type annotations are not allowed on impl let bindings \
                  (types come from the inter)"
         in
         let name =
           match pat with
           | SubPat (IdPat s) -> s
           | _ ->
               failwith
                 "parser: impl methods must use a simple name (e.g. let show = … \
                  or let mappend x y = …)"
         in
         let e2 = id_to_expr name in
         return
           (name, BindRec (pat, final_cto, body, e2, return_type_option)))
        <|>
        (let* (pat, final_cto, body, return_type_option, _num_explicit_params) =
           parse_single_defn_component ()
         in
         let () =
           match (final_cto, return_type_option) with
           | None, None -> ()
           | _ ->
               failwith
                 "parser: type annotations are not allowed on impl let bindings \
                  (types come from the inter)"
         in
         let name =
           match pat with
           | SubPat (IdPat s) -> s
           | _ ->
               failwith
                 "parser: impl methods must use a simple name (e.g. let show = … \
                  or let mappend x y = …)"
         in
         return (name, body))
      in
      return (name, body_expr)
    in
    let* impls = parse_one_or_more_opt_commas impl_row_parser in
    let* () = expect_token RBrace in
    return (InstanceDef (cls, head_ty, impls))

  let constructor_parser : (string * compound_type option) parser =
    let* () = expect_token Pipe in
    let* name =
      expect_token_get_data (function
        | Id s -> Some s
        | _ -> None)
    in
    (* Check if constructor name starts with uppercase *)
    let () =
      if
        name = ""
        || not (Char.uppercase_ascii (String.get name 0) = String.get name 0)
      then failwith ("Constructor name must start with uppercase: " ^ name)
    in
    (* Check for "of" keyword - if present, parse type, otherwise nullary *)
    let* payload_type =
      (let* () = expect_token Of in
       let* ct = CompoundTypeParser.compound_type_parser in
       return (Some ct))
      <|> return None
    in
    return (name, payload_type)

  let type_alias_defn_parser_no_args () : defn parser =
    let* () = expect_token Type in
    let* name =
      expect_token_get_data (function
        | Id s -> Some s
        | _ -> None)
    in
    let* () = expect_token Equals in
    (* Check if next token is Pipe - if so, it's a sum type, otherwise type
       alias *)
    let* is_sum_type = check_tokens Pipe in
    if is_sum_type then
      let* constructors = parse_several constructor_parser in
      return (SumTypeDef (name, [], constructors))
    else
      let* ct = CompoundTypeParser.compound_type_parser in
      return (TypeDef (name, [], ct))

  (* Helper to parse a single sum type component (name, args, constructors) *)
  let parse_single_sum_type_component () : (string * string list * (string * compound_type option) list) parser =
    let* name =
      expect_token_get_data (function
        | Id s -> Some s
        | _ -> None)
    in
    (* Try to parse type parameters <a, b, ...> *)
    let* args : string list =
      (let* () =
         expect_token_get_data (function
           | Relop "<" -> Some ()
           | _ -> None)
       in
       let* type_params = parse_sep_delim string_parser Comma in
       let* () =
         expect_token_get_data (function
           | Relop ">" -> Some ()
           | _ -> None)
       in
       return type_params)
      <|> return []
    in
    let* () = expect_token Equals in
    (* Parse constructors *)
    let* constructors = parse_several constructor_parser in
    return (name, args, constructors)

  let rec_sum_type_defn_parser_with_args () : defn parser =
    let* () = expect_token Type in
    let* () = expect_token Rec in
    let* first_type = parse_single_sum_type_component () in

    (* Try to parse 'and' clauses for mutual recursion *)
    let* and_types =
      parse_several (
        let* () = expect_token And in
        parse_single_sum_type_component ()
      )
    in

    (* If we have and clauses, return SumTypeDefMutRec, otherwise SumTypeDefRec *)
    match and_types with
    | [] ->
        let (name, args, constructors) = first_type in
        return (SumTypeDefRec (name, args, constructors))
    | _ ->
        return (SumTypeDefMutRec (first_type :: and_types))

  let rec_sum_type_defn_parser_no_args () : defn parser =
    let* () = expect_token Type in
    let* () = expect_token Rec in
    let* first_type = parse_single_sum_type_component () in

    (* Try to parse 'and' clauses for mutual recursion *)
    let* and_types =
      parse_several (
        let* () = expect_token And in
        parse_single_sum_type_component ()
      )
    in

    (* If we have and clauses, return SumTypeDefMutRec, otherwise SumTypeDefRec *)
    match and_types with
    | [] ->
        let (name, args, constructors) = first_type in
        return (SumTypeDefRec (name, args, constructors))
    | _ ->
        return (SumTypeDefMutRec (first_type :: and_types))

  let sum_type_defn_parser_with_args () : defn parser =
    let* () = expect_token Type in
    let* name =
      expect_token_get_data (function
        | Id s -> Some s
        | _ -> None)
    in
    let* () =
      expect_token_get_data (function
        | Relop "<" -> Some ()
        | _ -> None)
    in
    (* Parse a list of identifiers and store the strings *)
    let* args : string list = parse_sep_delim string_parser Comma in
    let* () =
      expect_token_get_data (function
        | Relop ">" -> Some ()
        | _ -> None)
    in
    let* () = expect_token Equals in
    (* Parse constructors *)
    let* constructors = parse_several constructor_parser in
    return (SumTypeDef (name, args, constructors))

  let sum_type_defn_parser_no_args () : defn parser =
    let* () = expect_token Type in
    let* name =
      expect_token_get_data (function
        | Id s -> Some s
        | _ -> None)
    in
    let* () = expect_token Equals in
    (* Parse constructors *)
    let* constructors = parse_several constructor_parser in
    return (SumTypeDef (name, [], constructors))

  let type_alias_defn_parser_with_args () : defn parser =
    let* () = expect_token Type in
    let* name =
      expect_token_get_data (function
        | Id s -> Some s
        | _ -> None)
    in
    let* () =
      expect_token_get_data (function
        | Relop "<" -> Some ()
        | _ -> None)
    in
    (* Parse a list of identifiers and store the strings *)
    let* args : string list = parse_sep_delim string_parser Comma in
    let* () =
      expect_token_get_data (function
        | Relop ">" -> Some ()
        | _ -> None)
    in
    let* () = expect_token Equals in
    (* Check if next token is Pipe - if so, it's a sum type, otherwise type
       alias *)
    let* is_sum_type = check_tokens Pipe in
    if is_sum_type then
      let* constructors = parse_several constructor_parser in
      return (SumTypeDef (name, args, constructors))
    else
      let* ct = CompoundTypeParser.compound_type_parser in
      return (TypeDef (name, args, ct))

  let defn_parser : defn parser =
    class_defn_parser ()
    <|> instance_defn_parser ()
    <|> type_alias_defn_parser_with_args ()
    <|> type_alias_defn_parser_no_args ()
    <|> rec_sum_type_defn_parser_with_args ()
    <|> rec_sum_type_defn_parser_no_args ()
    <|> sum_type_defn_parser_with_args ()
    <|> sum_type_defn_parser_no_args ()
    <|> let_rec_defn_parser () <|> let_defn_parser ()
end

and ExprOrDefnParser : sig
  val expr_or_defn_parser : expr_or_defn parser
end = struct
  let expr_or_defn_parser : expr_or_defn parser =
    (let* expr = ExprParser.expr_parser in
     return (Expr expr))
    <|>
    let* defn = DefnParser.defn_parser in
    return (Definition defn)
end

and ProgramParser : sig
  val program_parser : program parser
end = struct
  let program_parser : program parser = parse_several DefnParser.defn_parser
end
