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
  + %

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

  | disjunction

<br><br>
## Disjunction
disjunction ::=

  | conjunction '||' disjunction
  
  | conjunction


<br><br>
## Conjunction
conjunction ::=

  | eq_expr '&&' conjunction

  | eq_expr


<br><br>
## Equality
eq_expr ::=

  | rel_expr '==' eq_expr

  | rel_expr '!=' eq_expr

  | rel_expr
<br><br>
## Relation
rel_expr ::=

  | arith_expr '<' rel_expr

  | arith_expr '>' rel_expr

  | arith_expr '<=' rel_expr

  | arith_expr '>=' rel_expr

  | arith_expr
<br><br>
## Arithmetic expressions
arith_expr ::= 

  | term '+' arith_expr 

  | term '-' arith_expr 

  | term
  <br><br>
## Arithmetic term
term ::= 

  | factor '*' term 

  | factor '/' term

  | factor '%' term 

  | factor
<br><br>
## Arithmetic factor
factor ::= 

  | INT

  | BOOL

  | STRING

  | '()'

  | ID

  | '(' expr ')'

  | factor factor