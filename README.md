# LambdaScript

# todo

+ Fixing the numbers of type variables inside function types
+ Type aliases
+ definitions
+ recursive functions
+ Enum (a type of definition)
+ Sequence type
+ Vectors (tuples)
+ Error handling



<br>

# precedence

Grouping symbols (Highest precedence)
  + (
  + )

Unary operators
  + ~-

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
  
Function literal 
  + lam pat -> e

Bind expression
  + bind pat <- e1 in e2 (lowest precedence)


# grammar

<br><br>
## Type

compound_type ::=

  | factor_type '->' compound_type

  | factor_type

<br>
factor_type ::=

  | integer

  | boolean

  | string

  | '(' compound_type ')'
<br><br>
## Binding pattern

pat ::=

  | '()'

  | ID
  
<br><br>
## General expression
expr ::= 

  | 'lam' pat '->' expr (* function *)

  | 'lam' pat '[' compound_type ']' '->' expr (* function *)

  | bind pat <- expr 'in' expr

  | bind pat '[' compound_type ']'<- expr 'in' expr

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

  | '~-' factor

  | factor factor