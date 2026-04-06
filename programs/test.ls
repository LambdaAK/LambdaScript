type Token =
  | TNum of Int
  | TPlus
  | TTimes
  | TLParen
  | TRParen

// AST
type rec Expr =
  | ENum of Int
  | EAdd of (Expr, Expr)
  | EMul of (Expr, Expr)

type Parser<a> =
  List<Token> -> Option<(a, List<Token>)>

let empty_parser : Parser<a> = fn _ -> None

impl Functor for Parser where
  let fmap f p =
    fn tokens ->
      let result: Option<(a, List<Token>)> = p tokens in
      case result do
      | Some (result, remaining_tokens) -> Some (f result, remaining_tokens)
      | None -> None
end

impl Applicative for Parser where
  let pure x =
    fn tokens ->
      Some (x, tokens)
  let ap f p =
    fn tokens ->
      let result: Option<(a -> b, List<Token>)> = f tokens in 
      case result do
      | Some (f, remaining_tokens) -> p remaining_tokens
      | None -> None
end

impl Alternative for Parser where
  // Same as `empty_parser` but self-contained: forge dict bodies are lowered
  // before top-level names exist in the native env, so avoid referencing it here.
  let aempty = fn _ -> None
  let (<|>) p1 p2 =
    fn tokens ->
      // use both parsers and then <|> the results
      (p1 tokens) <|> (p2 tokens)
end

// grammar
// expr ::= term | term + expr
// term ::= factor | factor * term
// factor ::= number | ( expr )


// parser

let rec factor_parser : Parser<Expr> =
  fn tokens ->
    case tokens do

      // if the first token is a number, then parse it. That is the factor
        | TNum n :: rest -> Some (ENum n, rest)

        // otherwise, if the first token is a left parenthesis, then aprse the expression inside of it. That is the factor

        | TLParen :: rest ->
          let expr_result = expr_parser rest in
            (
              case expr_result do
                | Some (expr, TRParen :: tokens_after_r_paren) ->
                  Some (expr, tokens_after_r_paren)
                | _ -> None
            )

        // otherwise, parsing failed, so return None
        | _ -> None

and term_parser : Parser<Expr> = fn tokens ->
  // parse a factor

  let factor_result = factor_parser tokens in

  case factor_result do
    // if it failed, then the entire thing fails, so return None
    | None -> None
    // if it succeeded, and the next token is a times, then parse the term after the times
    | Some (factor, TTimes :: tokens_after_times) ->
      let term_result = term_parser tokens_after_times in
        (
          case term_result do
            | None -> None
            | Some (term, remaining_tokens) ->
              Some (EMul (factor, term), remaining_tokens)
        )

    // otherwise, we just return the factor and the remaining tokens
    | Some (factor, remaining_tokens) ->
        Some (factor, remaining_tokens)


and expr_parser : Parser<Expr> = fn tokens ->
  // parse a term

  let term_result = term_parser tokens in

  case term_result do
    | None -> None
    | Some (term, TPlus :: tokens_after_plus) ->
      let expr_result = expr_parser tokens_after_plus in
        (
          case expr_result do
            | None -> None
            | Some (expr, remaining_tokens) ->
              Some (EAdd (term, expr), remaining_tokens)
        )
    | Some (term, remaining_tokens) ->
        Some (term, remaining_tokens)

// evaluator

let rec eval: Expr -> Int = fn expr ->

  case expr do
    | ENum n -> n
    | EAdd (e1, e2) -> eval e1 + eval e2
    | EMul (e1, e2) -> eval e1 * eval e2

// write a bunch of tests and print whether they pass or fail

// tests

type Test =
  {
    tokens: List<Token>,
    expected_result: Int
  }

let tests: List<Test> = [
  {
    tokens : [TNum 1, TPlus, TNum 2],
    expected_result : 3
  },
  {
    tokens : [TLParen, TNum 1, TPlus, TNum 2, TRParen, TTimes, TNum 3],
    expected_result : 9
  },
  {
    // (1 + 2) * 3
    tokens: [TLParen, TNum 1, TPlus, TNum 2, TRParen, TTimes, TNum 3],
    expected_result : 9
  },
  {
    // (1 + 2) * 3 + 5
    tokens: [TLParen, TNum 1, TPlus, TNum 2, TRParen, TTimes, TNum 3, TPlus, TNum 5],
    expected_result : 14
  }

]

let run_test: Test -> Unit = fn test ->
  let result = expr_parser test.tokens in
  case result do
    | Some (expr, remaining_tokens) ->
      let eval_result = eval expr in
      if eval_result == test.expected_result then
        println "Test passed"
      else
        println "Test failed, not equal to expected result."
    | None ->
      println "Test failed, not parsed"

let rec run_tests: List<Test> -> Unit = fn tests ->
  case tests do
    | [] -> ()
    | test :: rest ->
      let () = run_test test in
      run_tests rest

let () = run_tests tests