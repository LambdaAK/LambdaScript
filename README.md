# LambdaScript

# precedence

high -> low precedence

( )
function application (f x)
* /
+ -
relational operators
equality operators
Ternary conditional operator (if then else statement)
function arrow


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


  # todo

  implement function application parsing