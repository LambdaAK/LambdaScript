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
          let expr_result = expr_parser tokens in
            (
              case expr_result do
                | Some (expr, TRParen :: tokens_after_r_paren) ->
                  Some (expr, tokens_after_r_paren)
                | _ -> None
            )

        // otherwise, parsing failed, so return None
        | _ -> None

and term_parser : Parser<Expr> = fn tokens -> None

and expr_parser : Parser<Expr> = fn tokens -> None