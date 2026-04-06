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
  },
  {
    // 1 + 2 + 3 + 4 + 5
    tokens: [TNum 1, TPlus, TNum 2, TPlus, TNum 3, TPlus, TNum 4, TPlus, TNum 5],
    expected_result : 15
  },
  {
    // 1 + 2 + 3 + 4 + 5 + 6
    tokens: [TNum 1, TPlus, TNum 2, TPlus, TNum 3, TPlus, TNum 4, TPlus, TNum 5, TPlus, TNum 6],
    expected_result : 21
  },
  {
    // 1 + 2 + 3 + 4 + 5 + 6 + 7
    tokens: [TNum 1, TPlus, TNum 2, TPlus, TNum 3, TPlus, TNum 4, TPlus, TNum 5, TPlus, TNum 6, TPlus, TNum 7],
    expected_result : 28
  },
  {
    // 1 + (2 + 3) + 4
    tokens: [TNum 1, TPlus, TLParen, TNum 2, TPlus, TNum 3, TRParen, TPlus, TNum 4],
    expected_result : 10
  },
  {
    // 1 + 2 * 5
    tokens: [TNum 1, TPlus, TNum 2, TTimes, TNum 5],
    expected_result : 11
  },
  {
    // (1 + 2 + 3 + 4) * 5 + 1
    tokens: [TLParen, TNum 1, TPlus, TNum 2, TPlus, TNum 3, TPlus, TNum 4, TRParen, TTimes, TNum 5, TPlus, TNum 1],
    expected_result : 51
  },
  {
    // 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10
    tokens: [TNum 1, TPlus, TNum 2, TPlus, TNum 3, TPlus, TNum 4, TPlus, TNum 5, TPlus, TNum 6, TPlus, TNum 7, TPlus, TNum 8, TPlus, TNum 9, TPlus, TNum 10],
    expected_result : 55
  },
  {
    // 5 * 4 * 3 * 2 * 1
    tokens: [TNum 5, TTimes, TNum 4, TTimes, TNum 3, TTimes, TNum 2, TTimes, TNum 1],
    expected_result : 120
  },
  {
    // 2 * 3 + 4
    tokens: [TNum 2, TTimes, TNum 3, TPlus, TNum 4],
    expected_result : 10
  },
  {
    // (2 + 3) * (4 + 5)
    tokens: [TLParen, TNum 2, TPlus, TNum 3, TRParen, TTimes, TLParen, TNum 4, TPlus, TNum 5, TRParen],
    expected_result : 45
  },
  {
    // 10 * (2 + 3)
    tokens: [TNum 10, TTimes, TLParen, TNum 2, TPlus, TNum 3, TRParen],
    expected_result : 50
  },
  {
    // (10 + 5) * (3 + 2)
    tokens: [TLParen, TNum 10, TPlus, TNum 5, TRParen, TTimes, TLParen, TNum 3, TPlus, TNum 2, TRParen],
    expected_result : 75
  },
  {
    // 1 * 2 * 3 * 4
    tokens: [TNum 1, TTimes, TNum 2, TTimes, TNum 3, TTimes, TNum 4],
    expected_result : 24
  },
  {
    // (1 + 1) * (2 + 2) * (3 + 3)
    tokens: [TLParen, TNum 1, TPlus, TNum 1, TRParen, TTimes, TLParen, TNum 2, TPlus, TNum 2, TRParen, TTimes, TLParen, TNum 3, TPlus, TNum 3, TRParen],
    expected_result : 48
  },
  {
    // 7 + 3 * 2
    tokens: [TNum 7, TPlus, TNum 3, TTimes, TNum 2],
    expected_result : 13
  },
  {
    // (7 + 3) * 2
    tokens: [TLParen, TNum 7, TPlus, TNum 3, TRParen, TTimes, TNum 2],
    expected_result : 20
  },
  {
    // 6 * 6 + 6
    tokens: [TNum 6, TTimes, TNum 6, TPlus, TNum 6],
    expected_result : 42
  },
  {
    // (4 + 6) * (1 + 2 + 3)
    tokens: [TLParen, TNum 4, TPlus, TNum 6, TRParen, TTimes, TLParen, TNum 1, TPlus, TNum 2, TPlus, TNum 3, TRParen],
    expected_result : 60
  },
  {
    // 2 * 3 * 4 + 1
    tokens: [TNum 2, TTimes, TNum 3, TTimes, TNum 4, TPlus, TNum 1],
    expected_result : 25
  },
  {
    // 1 + 2 * 3 * 4
    tokens: [TNum 1, TPlus, TNum 2, TTimes, TNum 3, TTimes, TNum 4],
    expected_result : 25
  },
  {
    // (2 + 8) * (5 + 5)
    tokens: [TLParen, TNum 2, TPlus, TNum 8, TRParen, TTimes, TLParen, TNum 5, TPlus, TNum 5, TRParen],
    expected_result : 100
  },
  {
    // 3 * (4 + 5) + 2 * 6
    tokens: [TNum 3, TTimes, TLParen, TNum 4, TPlus, TNum 5, TRParen, TPlus, TNum 2, TTimes, TNum 6],
    expected_result : 39
  },
  {
    // ((2 + 3) * 4) + 1
    tokens: [TLParen, TLParen, TNum 2, TPlus, TNum 3, TRParen, TTimes, TNum 4, TRParen, TPlus, TNum 1],
    expected_result : 21
  },
  {
    // 2 * (3 + (4 * 5))
    tokens: [TNum 2, TTimes, TLParen, TNum 3, TPlus, TLParen, TNum 4, TTimes, TNum 5, TRParen, TRParen],
    expected_result : 46
  },
  {
    // (1 + 2 * 3) * (4 + 5 * 6)
    tokens: [TLParen, TNum 1, TPlus, TNum 2, TTimes, TNum 3, TRParen, TTimes, TLParen, TNum 4, TPlus, TNum 5, TTimes, TNum 6, TRParen],
    expected_result : 238
  },
  {
    // ((3 + 4) * 2 + 1) * ((5 + 1) * 3 + 2)
    tokens: [TLParen, TLParen, TNum 3, TPlus, TNum 4, TRParen, TTimes, TNum 2, TPlus, TNum 1, TRParen, TTimes, TLParen, TLParen, TNum 5, TPlus, TNum 1, TRParen, TTimes, TNum 3, TPlus, TNum 2, TRParen],
    expected_result : 300
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