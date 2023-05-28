# LambdaScript

# precedence

Grouping symbols (Highest precedence)
  1. (
  2. )

Function application
  1. f x
    - f is applied to x

multiplicative arithmetic operators
  1. *
  2. /

Additive arithmetic operators
  1. +
  2. -

Relational operators
  1. <
  2. >
  3. <=
  4. >=

Equality operators
  1. ==
  2. !=

Logical "and"
  1. &&

Logical "or"
  1. ||

Ternary conditional operator (if then else statement)
  1. if e1 then e2 else e3
  
Function literal (lowest precedence)
  1. lam pat -> e


# grammar

expr ::= 
  | 'lam' pat '->' expr (* function *)
  | 'if' expr 'then' expr 'else' expr
  | arith_expr

eq_expr ::=
  | rel_expr '==' eq_expr
  | rel_expr '!=' eq_expr
  | rel_expr

rel_expr ::=
  | arith_expr '<' rel_expr
  | arith_expr '>' rel_expr
  | arith_expr '<=' rel_expr
  | arith_expr '>=' rel_expr
  | arith_expr

arith_expr ::= 
  | term '+' arith_expr 
  | term '-' arith_expr 
  | term

term ::= 
  | factor '*' term 
  | factor '/' term 
  | factor

factor ::= 
  | INT
  | BOOL
  | STRING
  | NOTHING
  | ID
  | '(' expr ')'
  | factor factor (* function appliaction*)
