# LambdaScript

Currently in the early stages

Supports lexing for integers, booleans, and strings

expr ::= INT
| BOOL
| STRING
| NOTHING
| ID
| expr expr
| 'if' expr 'then' expr else 'expr'
| 'lam' pat '->' expr
| '(' expr ')'

# precedence

high -> low precedence

( )
function application (f x)
* /
+ -
Ternary conditional operator (if then else statement)
function arrow


# grammar

expr ::= 
  | 'lam' pat '->' expr (* function *)
  | 'if' expr 'then' expr 'else' expr
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


  # todo

  implement function application parsing