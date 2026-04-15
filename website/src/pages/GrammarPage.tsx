import { Link } from 'react-router-dom';
import CodeBlock from '../components/CodeBlock';
import DocsLayout from '../components/DocsLayout';

const programGrammar = `program      ::= defn*

defn         ::= import_defn
               | use_defn
               | mod_defn
               | macro_defn
               | class_defn
               | instance_defn
               | type_defn
               | let_defn
               | let_rec_defn

import_defn  ::= "import" string_literal
use_defn     ::= "use" qualified_id
mod_defn     ::= "mod" id "where" defn* "end"`;

const defnGrammar = `let_defn     ::= "let" pat arg_pat* [":" type] "=" expr
let_rec_defn ::= "let" "rec" rec_binding ("and" rec_binding)*
rec_binding  ::= pat arg_pat* [":" type] "=" expr

arg_pat      ::= pat | "(" pat ":" type ")"

class_defn   ::= ("inter" | "trait") qualified_id type_params?
                 ("requires" super_list)? class_body
class_body   ::= "where" trait_item+ "end"
               | "{" trait_item+ "}"
trait_item   ::= "val" method_name ":" type
               | "let" method_name pat* "=" expr

instance_defn ::= "impl" qualified_id ("for" type | "<" type ">")
                  ("requires" super_list)? impl_body
impl_body      ::= "where" impl_row+ "end"
                 | "{" impl_row+ "}"
impl_row       ::= method_name pat* "=" expr`;

const exprGrammar = `expr         ::= block
               | function
               | bind_rec
               | bind
               | switch
               | if_then_else
               | cons_expr

block        ::= "{" (expr_or_defn (";" expr_or_defn)*)? "}"
function     ::= "fn" arg_pat "->" expr
bind         ::= "let" pat arg_pat* [":" type] "=" expr "in" expr
bind_rec     ::= "let" "rec" rec_binding ("and" rec_binding)* "in" expr
switch       ::= "case" expr "do" branch+
branch       ::= "|" pat "->" expr
if_then_else ::= "if" expr "then" expr "else" expr`;

const patGrammar = `pat          ::= sub_pat
               | sub_pat "::" pat

sub_pat      ::= id | "_" | literal
               | "()"
               | "[]"
               | "(" pat ("," pat)+ ")"
               | "{" record_pat_fields "}"
               | constructor [sub_pat]

record_pat_fields ::= record_pat_field ("," record_pat_field)*
record_pat_field  ::= id | id ":" pat`;

const typeGrammar = `type         ::= type_atom
               | type_atom "->" type

type_atom     ::= "int" | "float" | "bool" | "string" | "char" | "unit"
               | type_var
               | qualified_id
               | qualified_id "<" type_list ">"
               | "[" type "]"
               | "(" type_list ")"
               | "{" record_type_fields "}"

type_defn     ::= "type" id type_params? "=" type
               | "type" id type_params? "=" constructor+
               | "type" "rec" sum_type ("and" sum_type)*`;

const macroGrammar = `macro_defn   ::= "macro_rules" "!" id "{" macro_arm+ "}"
macro_arm    ::= matcher "=>" transcriber [";" | ","]
matcher      ::= delimited_token_trees
transcriber  ::= token_trees_until_arm_end

macro_call   ::= id "!" ("(" tt* ")" | "[" tt* "]" | "{" tt* "}")

fragment     ::= "expr" | "pat" | "ty" | "type" | "ident"
               | "item" | "tt" | "literal" | "path" | "block"

repeat       ::= "$" "(" matcher_seq ")" [separator] ("*" | "+")

builtins     ::= count_args!(...) | vec!(...) | stringify!(...)
               | concat!(...) | concat_str!(...)`;

function GrammarPage() {
  return (
    <DocsLayout>
      <div className="page-intro">
        <div className="breadcrumb">
          <Link to="/">Home</Link> / <Link to="/docs/">Docs</Link> /
          Grammar
        </div>
        <h1 className="page-title">Grammar (Practical EBNF)</h1>
        <p className="lead">
          This is a readable grammar guide for everyday use, aligned with the
          current parser shape.
        </p>
      </div>

      <h2>Program Structure</h2>
      <CodeBlock code={programGrammar} />

      <h2>Definitions</h2>
      <CodeBlock code={defnGrammar} />

      <h2>Expressions</h2>
      <CodeBlock code={exprGrammar} />

      <h2>Patterns</h2>
      <CodeBlock code={patGrammar} />

      <h2>Types</h2>
      <CodeBlock code={typeGrammar} />

      <h2>Macros</h2>
      <CodeBlock code={macroGrammar} />

      <div className="callout">
        <strong>Scope note:</strong> this page intentionally focuses on
        practical syntax. It is not a full formal semantics document.
      </div>
    </DocsLayout>
  );
}

export default GrammarPage;
