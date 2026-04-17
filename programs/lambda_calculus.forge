// Lambda calculus in Forge: lexer, parser, and small-step interpreter.
// Grammar:
//   expr := "^" ident "." expr | app
//   app  := atom atom*
//   atom := ident | "(" expr ")"

type LCToken =
  | TokLambda
  | TokDot
  | TokLParen
  | TokRParen
  | TokIdent of String

type LCExpr =
  | LCVar of String
  | LCLam of (String, LCExpr)
  | LCApp of (LCExpr, LCExpr)

let rec contains_string target xs =
  case xs do
  | [] -> false
  | h :: t -> if h == target then true else contains_string target t

let rec add_unique x xs = if contains_string x xs then xs else x :: xs

let rec union_strings xs ys =
  case xs do
  | [] -> ys
  | h :: t -> union_strings t (add_unique h ys)

let rec remove_string target xs =
  case xs do
  | [] -> []
  | h :: t ->
      if h == target then remove_string target t
      else h :: remove_string target t

let reverse_tokens xs =
  let rec go rest acc =
    case rest do
    | [] -> acc
    | h :: t -> go t (h :: acc)
  in
  go xs []

let rec free_vars e =
  case e do
  | LCVar x -> [x]
  | LCApp (a, b) -> union_strings (free_vars a) (free_vars b)
  | LCLam (x, body) -> remove_string x (free_vars body)

let rec rename_bound body old_name new_name =
  case body do
  | LCVar x -> if x == old_name then LCVar new_name else LCVar x
  | LCApp (a, b) ->
      LCApp
        (rename_bound a old_name new_name, rename_bound b old_name new_name)
  | LCLam (x, b) ->
      if x == old_name then LCLam (x, b)
      else LCLam (x, rename_bound b old_name new_name)

let rec fresh_name_from base used n =
  let candidate = if n == 0 then base else base ^ "_" ^ int_to_str n in
  if contains_string candidate used then fresh_name_from base used (n + 1)
  else candidate

let rec subst expr var_name replacement =
  case expr do
  | LCVar x -> if x == var_name then replacement else LCVar x
  | LCApp (a, b) ->
      LCApp (subst a var_name replacement, subst b var_name replacement)
  | LCLam (x, body) ->
      if x == var_name then LCLam (x, body)
      else if contains_string x (free_vars replacement) then
        let used = union_strings (free_vars body) (free_vars replacement) in
        let x_fresh = fresh_name_from x used 0 in
        let body_renamed = rename_bound body x x_fresh in
        LCLam (x_fresh, subst body_renamed var_name replacement)
      else LCLam (x, subst body var_name replacement)

let is_space ch =
  ch == " "

let is_delim ch =
  is_space ch || ch == "^" || ch == "." || ch == "(" || ch == ")"

let is_ident_char ch = not (is_delim ch)

let rec read_ident_end src len i =
  if i >= len then i
  else
    let ch = str_slice src i 1 in
    if is_ident_char ch then read_ident_end src len (i + 1) else i

let rec lex_lc_from src len i acc =
  if i >= len then Some (reverse_tokens acc)
  else
    let ch = str_slice src i 1 in
    if is_space ch then lex_lc_from src len (i + 1) acc
    else if ch == "^" then
      lex_lc_from src len (i + 1) (TokLambda :: acc)
    else if ch == "." then lex_lc_from src len (i + 1) (TokDot :: acc)
    else if ch == "(" then lex_lc_from src len (i + 1) (TokLParen :: acc)
    else if ch == ")" then lex_lc_from src len (i + 1) (TokRParen :: acc)
    else if is_ident_char ch then
      let j = read_ident_end src len (i + 1) in
      let ident = str_slice src i (j - i) in
      lex_lc_from src len j (TokIdent ident :: acc)
    else None

let lex_lc src = lex_lc_from src (str_length src) 0 []

let token_starts_atom t =
  case t do
  | TokIdent _ -> true
  | TokLParen -> true
  | _ -> false

let rec parse_expr toks =
  case toks do
  | TokLambda :: TokIdent x :: TokDot :: rest ->
      (case parse_expr rest do
      | Some (body, rest2) -> Some (LCLam (x, body), rest2)
      | None -> None)
  | _ -> parse_application toks

and parse_application toks =
  case parse_atom toks do
  | None -> None
  | Some (first, rest) -> parse_application_tail first rest

and parse_application_tail acc toks =
  case toks do
  | t :: _ ->
      if token_starts_atom t then
        (case parse_atom toks do
        | Some (next_expr, rest2) ->
            parse_application_tail (LCApp (acc, next_expr)) rest2
        | None -> Some (acc, toks))
      else Some (acc, toks)
  | [] -> Some (acc, [])

and parse_atom toks =
  case toks do
  | TokIdent x :: rest -> Some (LCVar x, rest)
  | TokLParen :: rest ->
      (case parse_expr rest do
      | Some (e, TokRParen :: rest2) -> Some (e, rest2)
      | _ -> None)
  | _ -> None

let parse_lc toks =
  case parse_expr toks do
  | Some (e, []) -> Some e
  | _ -> None

let rec step e =
  case e do
  | LCApp (LCLam (x, body), arg) -> Some (subst body x arg)
  | LCApp (f, a) ->
      (case step f do
      | Some f2 -> Some (LCApp (f2, a))
      | None ->
          case step a do
          | Some a2 -> Some (LCApp (f, a2))
          | None -> None)
  | LCLam (x, body) ->
      (case step body do
      | Some b2 -> Some (LCLam (x, b2))
      | None -> None)
  | LCVar _ -> None

let rec normalize fuel e =
  if fuel <= 0 then e
  else
    case step e do
    | Some e2 -> normalize (fuel - 1) e2
    | None -> e

let rec expr_to_string e =
  case e do
  | LCVar x -> x
  | LCLam (x, body) -> "(^" ^ x ^ "." ^ expr_to_string body ^ ")"
  | LCApp (a, b) -> "(" ^ expr_to_string a ^ " " ^ expr_to_string b ^ ")"

let interpret_lc src =
  case lex_lc src do
  | None -> "lex error"
  | Some toks ->
      case parse_lc toks do
      | None -> "parse error"
      | Some ast -> expr_to_string (normalize 200 ast)

let src1 = "(^x.x) (^y.y)"
let src2 = "(^x.^y.x) a b"
let src3 = "(^f.^x.f (f x)) (^n.n) z"
let () = println ("input : " ^ src1)
let () = println ("result: " ^ interpret_lc src1)
let () = println ""
let () = println ("input : " ^ src2)
let () = println ("result: " ^ interpret_lc src2)
let () = println ""
let () = println ("input : " ^ src3)
let () = println ("result: " ^ interpret_lc src3)
