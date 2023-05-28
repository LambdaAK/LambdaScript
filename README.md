# LambdaScript

# precedence

Grouping symbols (Highest precedence)
  + (
  + )

Function application
  + f x

Multiplicative arithmetic operators
  + \*
  + /

Additive arithmetic operators
  + \+
  + \-

Relational operators
  + <
  + \>
  + <=
  + \>=

Equality operators
  + ==
  + !=

Logical "and"
  + &&

Logical "or"
  + ||

Ternary conditional operator (if then else statement)
  + if e1 then e2 else e3
  
Function literal (lowest precedence)
  + lam pat -> e


# grammar

## general expression
expr ::= 

  | 'lam' pat '->' expr (* function *)

  | 'if' expr 'then' expr 'else' expr

  | arith_expr
<br><br>
## equality
eq_expr ::=

  | rel_expr '==' eq_expr

  | rel_expr '!=' eq_expr

  | rel_expr
<br><br>
## relation
rel_expr ::=

  | arith_expr '<' rel_expr

  | arith_expr '>' rel_expr

  | arith_expr '<=' rel_expr

  | arith_expr '>=' rel_expr

  | arith_expr
<br><br>
## arithmetic expressions
arith_expr ::= 

  | term '+' arith_expr 

  | term '-' arith_expr 

  | term
  <br><br>
## arithmetic term
term ::= 

  | factor '*' term 

  | factor '/' term 

  | factor
<br><br>
## arithmetic factor
factor ::= 

  | INT

  | BOOL

  | STRING

  | '()'

  | ID

  | '(' expr ')'

  | factor factor