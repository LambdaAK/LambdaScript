type Parser<a> =
  List<Char> -> Option<(a, List<Char>)>

let empty_parser : Parser<a> = fn _ -> None

impl Functor for Parser where
  let fmap f p =
    fn tokens ->
      let result: Option<(a, List<Char>)> = p tokens in
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
      let result: Option<(a -> b, List<Char>)> = f tokens in
      case result do
      | Some (f, remaining_tokens) -> p remaining_tokens
      | None -> None
end

impl Alternative for Parser where
  let aempty = empty_parser
  let (<|>) p1 p2 =
    fn tokens ->
      (p1 tokens) <|> (p2 tokens)
end
