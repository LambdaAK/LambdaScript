(** Lexer tests: every token class, literal edge values, escapes, comments,
    line/byte positions, and malformed input (which must raise promptly, never
    hang). *)

open OUnit2
open Language.Lex

exception Timeout

(** Lexing is pure and allocates, so SIGALRM interrupts a runaway loop. *)
let with_timeout (secs : int) (f : unit -> 'a) : 'a =
  let old = Sys.signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise Timeout)) in
  ignore (Unix.alarm secs);
  Fun.protect
    ~finally:(fun () ->
      ignore (Unix.alarm 0);
      Sys.set_signal Sys.sigalrm old)
    f

let lex_str (s : string) : token list =
  with_timeout 5 (fun () -> lex (s |> String.to_seq |> List.of_seq))

let types (s : string) : token_type list = List.map (fun t -> t.token_type) (lex_str s)

let pp_types ts = String.concat " " (List.map string_of_token_type ts)

let t_tokens name src expected =
  name >:: fun _ ->
  assert_equal ~printer:pp_types ~msg:("lexing " ^ String.escaped src) expected (types src)

(** Malformed input must raise an exception (a lexer error), not hang or
    silently produce tokens. *)
let t_rejects name src =
  name >:: fun _ ->
  match lex_str src with
  | exception Timeout -> assert_failure ("lexer hung on " ^ String.escaped src)
  | exception _ -> ()
  | toks ->
      assert_failure
        (Printf.sprintf "lexer accepted %S as %s" src (pp_types (List.map (fun t -> t.token_type) toks)))

let keyword_tests =
  [
    ("true", Boolean true);
    ("false", Boolean false);
    ("Int", IntegerType);
    ("Bool", BooleanType);
    ("String", StringType);
    ("Char", CharType);
    ("Unit", UnitType);
    ("Float", FloatType);
    ("if", If);
    ("then", Then);
    ("else", Else);
    ("in", In);
    ("let", Let);
    ("rec", Rec);
    ("and", And);
    ("fn", Fn);
    ("for", For);
    ("switch", Switch);
    ("case", Case);
    ("do", Do);
    ("enum", Enum);
    ("of", Of);
    ("type", Type);
    ("val", Val);
    ("with", With);
    ("inter", Inter);
    ("trait", Trait);
    ("impl", Impl);
    ("where", Where);
    ("end", End);
    ("requires", Requires);
    ("mod", ModKw);
    ("use", Use);
    ("import", Import);
    ("macro_rules", MacroRules);
  ]
  |> List.map (fun (kw, tok) -> t_tokens ("keyword " ^ kw) kw [ tok ])

(* Identifiers that merely start with a keyword must stay identifiers. *)
let identifier_tests =
  [
    t_tokens "plain id" "x" [ Id "x" ];
    t_tokens "id with digits" "x1y2" [ Id "x1y2" ];
    t_tokens "leading underscore" "_tmp" [ Id "_tmp" ];
    t_tokens "snake case" "snake_case_name" [ Id "snake_case_name" ];
    t_tokens "keyword prefix if_" "if_x" [ Id "if_x" ];
    t_tokens "keyword prefix letter" "letter" [ Id "letter" ];
    t_tokens "keyword prefix inner" "inner" [ Id "inner" ];
    t_tokens "keyword prefix interval" "interval" [ Id "interval" ];
    t_tokens "keyword prefix done" "done" [ Id "done" ];
    t_tokens "keyword prefix types" "types" [ Id "types" ];
    t_tokens "keyword prefix endpoint" "endpoint" [ Id "endpoint" ];
    t_tokens "keyword prefix Integer" "Integer" [ Id "Integer" ];
    t_tokens "keyword prefix Strings" "Strings" [ Id "Strings" ];
    t_tokens "keyword prefix truex" "truex" [ Id "truex" ];
    t_tokens "keyword prefix fnord" "fnord" [ Id "fnord" ];
    t_tokens "keyword suffix x_in" "x_in" [ Id "x_in" ];
    t_tokens "lowercase int is an identifier" "int" [ Id "int" ];
    t_tokens "constructor-like id" "Some" [ Id "Some" ];
    t_tokens "two ids" "f x" [ Id "f"; Id "x" ];
    t_tokens "macro_rules prefix" "macro_rulesx" [ Id "macro_rulesx" ];
  ]

let number_tests =
  [
    t_tokens "zero" "0" [ Integer 0 ];
    t_tokens "small int" "42" [ Integer 42 ];
    t_tokens "leading zeros" "007" [ Integer 7 ];
    t_tokens "max native int" "4611686018427387903" [ Integer max_int ];
    t_tokens "int32 max" "2147483647" [ Integer 2147483647 ];
    t_tokens "float" "3.14" [ FloatToken 3.14 ];
    t_tokens "float zero" "0.0" [ FloatToken 0.0 ];
    t_tokens "float trailing dot" "2." [ FloatToken 2.0 ];
    t_tokens "range without spaces" "1...5" [ Integer 1; Enum; Integer 5 ];
    t_tokens "range with spaces" "1 ... 5" [ Integer 1; Enum; Integer 5 ];
    t_tokens "minus is an operator" "-5" [ Addop "-"; Integer 5 ];
    t_tokens "opposite" "~-5" [ Opposite; Integer 5 ];
    t_tokens "number then id" "3x" [ Integer 3; Id "x" ];
    t_rejects "integer literal too large" "99999999999999999999";
    t_rejects "two dots in float" "1.2.3";
  ]

let string_tests =
  [
    t_tokens "empty string" {|""|} [ StringToken "" ];
    t_tokens "simple string" {|"hello"|} [ StringToken "hello" ];
    t_tokens "string with spaces" {|"a b  c"|} [ StringToken "a b  c" ];
    t_tokens "escaped quote" {|"say \"hi\""|} [ StringToken {|say "hi"|} ];
    t_tokens "escaped backslash" {|"a\\b"|} [ StringToken {|a\b|} ];
    t_tokens "escaped backslash at end" {|"a\\"|} [ StringToken {|a\|} ];
    t_tokens "newline escape" {|"a\nb"|} [ StringToken "a\nb" ];
    t_tokens "tab escape" {|"a\tb"|} [ StringToken "a\tb" ];
    t_tokens "carriage return escape" {|"a\rb"|} [ StringToken "a\rb" ];
    t_tokens "keyword inside string" {|"let x = if"|} [ StringToken "let x = if" ];
    t_tokens "comment marker inside string" {|"// not a comment"|}
      [ StringToken "// not a comment" ];
    t_tokens "block comment marker inside string" {|"/* no */"|} [ StringToken "/* no */" ];
    t_tokens "utf8 inside string" "\"h\xc3\xa9\"" [ StringToken "h\xc3\xa9" ];
    t_tokens "two strings" {|"a" "b"|} [ StringToken "a"; StringToken "b" ];
    t_tokens "literal newline inside string" "\"a\nb\"" [ StringToken "a\nb" ];
    t_rejects "unterminated string" {|"abc|};
    t_rejects "unterminated after escape" {|"abc\"|};
  ]

let char_tests =
  [
    t_tokens "char" "'a'" [ CharToken 'a' ];
    t_tokens "char digit" "'7'" [ CharToken '7' ];
    t_tokens "char space" "' '" [ CharToken ' ' ];
    t_tokens "char newline escape" {|'\n'|} [ CharToken '\n' ];
    t_tokens "char tab escape" {|'\t'|} [ CharToken '\t' ];
    t_tokens "char backslash escape" {|'\\'|} [ CharToken '\\' ];
    t_tokens "char quote escape" {|'\''|} [ CharToken '\'' ];
    t_tokens "char double quote" {|'"'|} [ CharToken '"' ];
    t_tokens "chars in list" "['a', 'b']"
      [ LBracket; CharToken 'a'; Comma; CharToken 'b'; RBracket ];
    t_rejects "empty char" "''";
    t_rejects "two-char literal" "'ab'";
    t_rejects "unterminated char" "'a";
    t_rejects "lone quote" "'";
  ]

let operator_tests =
  [
    t_tokens "plus" "a + b" [ Id "a"; Addop "+"; Id "b" ];
    t_tokens "minus" "a - b" [ Id "a"; Addop "-"; Id "b" ];
    t_tokens "times" "a * b" [ Id "a"; Mulop "*"; Id "b" ];
    t_tokens "divide" "a / b" [ Id "a"; Mulop "/"; Id "b" ];
    t_tokens "mod" "a % b" [ Id "a"; Mulop "%"; Id "b" ];
    t_tokens "concat" "a ^ b" [ Id "a"; Addop "^"; Id "b" ];
    t_tokens "eq" "a == b" [ Id "a"; Relop "=="; Id "b" ];
    t_tokens "ne" "a != b" [ Id "a"; Relop "!="; Id "b" ];
    t_tokens "lt" "a < b" [ Id "a"; Relop "<"; Id "b" ];
    t_tokens "le" "a <= b" [ Id "a"; Relop "<="; Id "b" ];
    t_tokens "gt" "a > b" [ Id "a"; Relop ">"; Id "b" ];
    t_tokens "ge" "a >= b" [ Id "a"; Relop ">="; Id "b" ];
    t_tokens "and" "a && b" [ Id "a"; AND; Id "b" ];
    t_tokens "or" "a || b" [ Id "a"; OR; Id "b" ];
    t_tokens "cons" "a :: b" [ Id "a"; ConsToken; Id "b" ];
    t_tokens "pipeline" "a |> f" [ Id "a"; Pipeline; Id "f" ];
    t_tokens "arrow" "fn x -> x" [ Fn; Id "x"; Arrow; Id "x" ];
    t_tokens "bind arrow" "x <- xs" [ Id "x"; BindArrow; Id "xs" ];
    t_tokens "switch arrow" "() => 0" [ Unit; SwitchArrow; Integer 0 ];
    t_tokens "equals" "x = 1" [ Id "x"; Equals; Integer 1 ];
    t_tokens "bang" "m!(x)" [ Id "m"; Bang; LParen; Id "x"; RParen ];
    t_tokens "custom ++" "a ++ b" [ Id "a"; Addop "++"; Id "b" ];
    t_tokens "custom >>=" "m >>= f" [ Id "m"; Relop ">>="; Id "f" ];
    t_tokens "custom <|>" "a <|> b" [ Id "a"; Relop "<|>"; Id "b" ];
    t_tokens "custom <$>" "f <*> x" [ Id "f"; Relop "<*>"; Id "x" ];
    t_tokens "custom **" "a ** b" [ Id "a"; Mulop "**"; Id "b" ];
    (* [||], [&&], [|>] and [::] are fixed tokens matched before operator
       lexing (maximal munch is not applied to them), as in Rust. *)
    t_tokens "|| is a fixed token" "a ||| b" [ Id "a"; OR; Pipe; Id "b" ];
    t_tokens "operator section" "(+)" [ LParen; Addop "+"; RParen ];
    t_tokens "operator section cons" "(::)" [ LParen; ConsToken; RParen ];
    t_tokens "pipe" "| x" [ Pipe; Id "x" ];
    t_tokens "unit" "()" [ Unit ];
    t_tokens "unit with space" "( )" [ LParen; RParen ];
    t_tokens "nested generics split >>" "Pair<Pair<Int>>"
      [ Id "Pair"; Relop "<"; Id "Pair"; Relop "<"; IntegerType; Relop ">"; Relop ">" ];
    t_tokens "triple nested generics" "A<B<C<Int>>>"
      [
        Id "A"; Relop "<"; Id "B"; Relop "<"; Id "C"; Relop "<"; IntegerType; Relop ">";
        Relop ">"; Relop ">";
      ];
    t_tokens "generic then comma" "Pair<Int>, x"
      [ Id "Pair"; Relop "<"; IntegerType; Relop ">"; Comma; Id "x" ];
    t_tokens "comma not part of operator" "a +, b" [ Id "a"; Addop "+"; Comma; Id "b" ];
    t_tokens "colon" "x : Int" [ Id "x"; Colon; IntegerType ];
    t_tokens "semicolon" "a; b" [ Id "a"; Semicolon; Id "b" ];
    t_tokens "dot field" "p.x" [ Id "p"; Dot; Id "x" ];
    t_tokens "dollar" "$x" [ Dollar; Id "x" ];
    t_tokens "brackets braces" "[{}]" [ LBracket; LBrace; RBrace; RBracket ];
    t_tokens "wildcard" "_" [ Id "_" ];
    t_tokens "minus minus" "a - -b" [ Id "a"; Addop "-"; Addop "-"; Id "b" ];
  ]

let comment_tests =
  [
    t_tokens "line comment" "1 // two\n3" [ Integer 1; Integer 3 ];
    t_tokens "line comment at eof" "1 // no newline" [ Integer 1 ];
    t_tokens "block comment" "1 /* two */ 3" [ Integer 1; Integer 3 ];
    t_tokens "multi-line block comment" "1 /* a\nb\nc */ 3" [ Integer 1; Integer 3 ];
    t_tokens "empty block comment" "1 /**/ 3" [ Integer 1; Integer 3 ];
    t_tokens "block comment with stars" "1 /* ** * */ 3" [ Integer 1; Integer 3 ];
    t_tokens "comment only" "// nothing here" [];
    t_tokens "empty input" "" [];
    t_tokens "whitespace only" "  \n\t \n" [];
    t_tokens "crlf line endings" "let x = 1\r\nlet y = 2\r\n"
      [ Let; Id "x"; Equals; Integer 1; Let; Id "y"; Equals; Integer 2 ];
    t_tokens "comment with crlf" "1 // c\r\n2" [ Integer 1; Integer 2 ];
    t_rejects "unclosed block comment" "1 /* never closed";
  ]

let malformed_tests =
  [
    t_rejects "stray at sign" "let x = @";
    t_rejects "stray hash" "#x";
    t_rejects "stray backtick" "`x`";
    t_rejects "non-ascii identifier" "let caf\xc3\xa9 = 1";
    t_rejects "stray question mark" "x ? y";
  ]

let position_tests =
  [
    ( "line numbers advance on newlines" >:: fun _ ->
      let toks = lex_str "let x = 1\nlet y = 2\n\nlet z = 3" in
      let lines =
        List.filter_map
          (fun t -> match t.token_type with Id _ -> Some t.line | _ -> None)
          toks
      in
      assert_equal ~printer:(fun l -> String.concat "," (List.map string_of_int l)) [ 1; 2; 4 ] lines );
    ( "line numbers after multi-line comment" >:: fun _ ->
      let toks = lex_str "/* a\nb\nc */ x" in
      match toks with
      | [ t ] -> assert_equal ~printer:string_of_int 3 t.line
      | _ -> assert_failure "expected one token" );
    ( "line numbers after multi-line string" >:: fun _ ->
      let toks = lex_str "\"a\nb\" x" in
      match List.rev toks with
      | t :: _ -> assert_equal ~printer:string_of_int 2 t.line
      | [] -> assert_failure "expected tokens" );
    ( "byte spans of identifiers" >:: fun _ ->
      let toks = lex_str "let foo = barbaz" in
      let spans =
        List.filter_map
          (fun t -> match t.token_type with Id s -> Some (s, t.byte_start, t.byte_end) | _ -> None)
          toks
      in
      assert_equal [ ("foo", 4, 7); ("barbaz", 10, 16) ] spans );
    ( "byte spans after string literal" >:: fun _ ->
      let toks = lex_str {|"ab" x|} in
      match List.rev toks with
      | t :: _ ->
          assert_equal ~printer:string_of_int 5 t.byte_start;
          assert_equal ~printer:string_of_int 6 t.byte_end
      | [] -> assert_failure "expected tokens" );
    ( "byte spans after line comment" >:: fun _ ->
      let toks = lex_str "// hi\nx" in
      match toks with
      | [ t ] -> assert_equal ~printer:string_of_int 6 t.byte_start
      | _ -> assert_failure "expected one token" );
  ]

let performance_tests =
  [
    ( "lexing a 200 KB program is fast" >:: fun _ ->
      let line = "let value_number_x = 12345 + 678 * (9 - 10) // comment\n" in
      let buf = Buffer.create (String.length line * 3700) in
      for _ = 1 to 3700 do
        Buffer.add_string buf line
      done;
      let t0 = Unix.gettimeofday () in
      let toks = with_timeout 20 (fun () -> lex (Buffer.contents buf |> String.to_seq |> List.of_seq)) in
      let dt = Unix.gettimeofday () -. t0 in
      assert_equal ~printer:string_of_int (3700 * 12) (List.length toks);
      assert_bool (Printf.sprintf "lexing took %.2fs (should be well under 2s)" dt) (dt < 2.0) );
  ]

let suite =
  "lexer"
  >::: [
         "keywords" >::: keyword_tests;
         "identifiers" >::: identifier_tests;
         "numbers" >::: number_tests;
         "strings" >::: string_tests;
         "chars" >::: char_tests;
         "operators" >::: operator_tests;
         "comments_whitespace" >::: comment_tests;
         "malformed" >::: malformed_tests;
         "positions" >::: position_tests;
         "performance" >::: performance_tests;
       ]

let () = run_test_tt_main suite
