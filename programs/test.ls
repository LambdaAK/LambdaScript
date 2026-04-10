// AST for a simple language

type rec Expr =
  | Lit of Int
  | Add of (Expr, Expr)
  | Sub of (Expr, Expr)
  | Mul of (Expr, Expr)
  | Div of (Expr, Expr)

impl Show for Expr where
  show e =
    case e do
    | Lit n -> show n
    | Add (e1, e2) -> "(" ^ (show e1) ^ " + " ^ (show e2) ^ ")"
    | Sub (e1, e2) -> "(" ^ (show e1) ^ " - " ^ (show e2) ^ ")"
    | Mul (e1, e2) -> "(" ^ (show e1) ^ " * " ^ (show e2) ^ ")"
    | Div (e1, e2) -> "(" ^ (show e1) ^ " / " ^ (show e2) ^ ")"
end

