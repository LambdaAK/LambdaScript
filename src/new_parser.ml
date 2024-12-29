open Lex
open Expr
open Tostring

(* Idea - Make a module for each level of parser - Use a functor to combine them
   - Use another functor to then combine all of those into condensed parser

   Wrap each parser definition in lazy so that they can be mutually recursive
   without any problems Could also thunk as unit -> 'a parser, which would be
   fully evaluated already, so the mutual recursion would work

   Use recursive modules to organize the parsers for the different levels in the
   grammar

   Make sure each chain of <|> has the correct order of parsers. Larger parsers
   should usually come earlier in the chain. *)

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
  let parse_several (parser : 'a parser) : 'a list parser =
    let rec parse_several' acc tokens =
      match parser tokens with
      | Some (result, remaining_tokens) ->
          parse_several' (result :: acc) remaining_tokens
      | None -> Some (List.rev acc, tokens)
    in
    parse_several' []

(**
    * Parses a list of 'a, separated by delimiters. Takes:
    * - a parser for 'a
    * - a delimiter token_type
    * Returns a list of 'a (parsed results)
    * There will be one more 'a than delimiters if parsing succeeds.
    * First, parses an 'a, if possible. If not possible, returns [], []
    * Otherwise, parses 'a, and then (delim, 'a) pairs until there isn't a delimiter next    

*)
  let parse_sep_delim (parser : 'a parser) (delim : token_type) : 'a list parser =
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
    print_endline ("WARNING! Unimplemented parser: " ^ parser_name);
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
    let* () = return () in
    print_endline msg;
    return ()
end

open ParserUtils

(* factor parsers *)

module rec FactorParser : sig
  val factor_parser : factor parser
end = struct
  open ExprParser

  let rec boolean_parser : factor parser =
    let* () = parse_print "boolean_parser" in
    let* b =
      expect_token_get_data (function
        | Boolean b -> Some b
        | _ -> None)
    in
    return (Boolean b)

  and string_parser : factor parser =
    let* () = parse_print "string_parser" in
    let* s =
      expect_token_get_data (function
        | StringToken s -> Some s
        | _ -> None)
    in
    return (String s)

  and unit_parser : factor parser =
    let* () = parse_print "unit_parser" in
    let* () = expect_token Unit in
    return Unit

  and integer_parser () : factor parser =
    let* () = parse_print "integer_parser" in
    let* i =
      expect_token_get_data (function
        | Integer i -> Some i
        | _ -> None)
    in
    return (Integer i)

  and float_factor_parser : factor parser =
    let* () = parse_print "float_factor_parser" in
    let* f =
      expect_token_get_data (function
        | FloatToken f ->
            print_endline "got float token";

            Some f
        | _ ->
            print_endline "didn't get float token";

            None)
    in
    return (FloatFactor f)

  and id_parser : factor parser =
    let* () = parse_print "id_parser" in
    let* id =
      expect_token_get_data (function
        | Id id -> Some id
        | _ -> None)
    in
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
    print_endline "parsing list sugar";
    let* exprs = parse_sep_delim expr_parser Comma in
    let* () = expect_token RBracket in
    return (ListSugar exprs)

  and list_enumeration_parser () : factor parser =
    unimplemented_parser "list_enumeration_parser"

  and list_comprehension_parser () : factor parser =
    unimplemented_parser "list_comprehension_parser"

  and factor_parser () =
    let factor_parsers =
      [
        list_sugar_parser ();
        boolean_parser;
        string_parser;
        unit_parser;
        integer_parser ();
        float_factor_parser;
        id_parser;
        paren_factor_parser;
        opposite_parser ();
        vector_parser ();
        nil_parser;
        list_enumeration_parser ();
        list_comprehension_parser ();
      ]
    in
    combine_parsers factor_parsers

  (* term parsers *)

  let factor_parser = factor_parser ()
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

    (* print the terms and addops *)
    print_endline "terms: ";
    List.iter (fun t -> print_endline (string_of_arith_term t 0)) terms;
    print_endline "addops: ";
    List.iter (fun a -> print_endline (string_of_token_type a)) addops;

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
  let rec relation_under_conjunction_parser : conjunction parser =
    let* rel_expr = RelExprParser.rel_expr_parser in
    return (RelationUnderConjunction rel_expr)

  and conjunction_branch_parser () : conjunction parser =
    let* rel_expr = RelExprParser.rel_expr_parser in
    let* () = expect_token AND in
    let* conjunction = conjunction_parser () in
    return (Conjunction (rel_expr, conjunction))

  and conjunction_parser () =
    relation_under_conjunction_parser <|> conjunction_branch_parser ()

  let conjunction_parser : conjunction parser = conjunction_parser ()
end

and DisjunctionParser : sig
  val disjunction_parser : disjunction parser
end = struct
  let rec conjunction_under_disjunction_parser : disjunction parser =
    let* conjunction = ConjunctionParser.conjunction_parser in
    return (ConjunctionUnderDisjunction conjunction)

  and disjunction_branch_parser () : disjunction parser =
    let* conjunction = ConjunctionParser.conjunction_parser in
    let* () = expect_token OR in
    let* disjunction = disjunction_parser () in
    return (Disjunction (conjunction, disjunction))

  and disjunction_parser () : disjunction parser =
    disjunction_branch_parser () <|> conjunction_under_disjunction_parser

  let disjunction_parser : disjunction parser = disjunction_parser ()
end

and ConsExprParser : sig
  val cons_expr_parser : cons_expr parser
end = struct
  let rec disjunction_under_cons_parser : cons_expr parser =
    let* disjunction = DisjunctionParser.disjunction_parser in
    return (DisjunctionUnderCons disjunction)

  and cons_branch_parser () : cons_expr parser =
    let* disjunction = DisjunctionParser.disjunction_parser in
    let* () = expect_token ConsToken in
    let* cons_expr = cons_expr_parser () in
    return (Cons (disjunction, cons_expr))

  and cons_expr_parser () : cons_expr parser =
    cons_branch_parser () <|> disjunction_under_cons_parser

  let cons_expr_parser : cons_expr parser = cons_expr_parser ()
end

and ExprParser : sig
  val expr_parser : expr parser
end = struct
  let expr_parser : expr parser =
    let* () = parse_print "expr_parser is not implemented properly yet!" in
    let* cons_expr = ConsExprParser.cons_expr_parser in
    return (ConsExpr cons_expr)
end
