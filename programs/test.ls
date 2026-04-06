type Token =
  | TInt of Int
  | TPlus
  | TTimes
  | TLParen
  | TRParen

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
