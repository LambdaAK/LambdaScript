import { Link } from 'react-router-dom';
import CodeBlock from '../components/CodeBlock';
import DocsLayout from '../components/DocsLayout';

const coreExpr = `let add = fn x -> fn y -> x + y
let rec factorial n =
  if n == 0 then 1
  else n * factorial (n - 1)

let p = { x: 10, y: 20 }
let p2 = { p with x = 100 }

let answer =
  case [1, 2, 3] do
  | [] -> 0
  | h :: _ -> h`;

const traitSnippet = `inter Show<a> {
  val show : a -> string
}

impl Show for Int where
  show = int_to_str
end`;

const macroSnippet = `macro_rules! choose {
  () => 0;
  ($x:expr) => $x;
}

macro_rules! collect_and_count {
  ($($x:expr),*) => count_args!($($x),*);
}

let a = choose!()
let b = choose!{42}
let n = collect_and_count!(10, 20, 30)`;

function LanguageFeaturesPage() {
  return (
    <DocsLayout>
      <div className="page-intro">
        <div className="breadcrumb">
          <Link to="/">Home</Link> / <Link to="/docs/">Docs</Link> /
          Language Features
        </div>
        <h1 className="page-title">Language Features</h1>
        <p className="lead">
          Forge is a statically typed functional language with both interpreter
          and native-compiler execution paths.
        </p>
      </div>

      <h2>Type System</h2>
      <ul>
        <li>Hindley-Milner style type inference</li>
        <li>
          Polymorphism with type parameters{' '}
          <code className="inline-code">type Option&lt;a&gt;</code>
        </li>
        <li>Algebraic data types and recursive types</li>
        <li>Records with field access and immutable update</li>
        <li>Optional type annotations on definitions and parameters</li>
      </ul>

      <h2>Core Expressions</h2>
      <CodeBlock code={coreExpr} />

      <h2>Traits / Typeclasses</h2>
      <p>
        Forge supports both <code className="inline-code">trait</code> and{' '}
        <code className="inline-code">inter</code> style declarations plus{' '}
        <code className="inline-code">impl</code> instances.
      </p>
      <CodeBlock code={traitSnippet} />

      <h2>Rust-Style Declarative Macros</h2>
      <p>
        <code className="inline-code">macro_rules!</code> expands before
        typechecking/evaluation/compilation. Matchers support fragment kinds and
        repetition.
      </p>
      <CodeBlock code={macroSnippet} />

      <h3>Macro Fragment Kinds</h3>
      <table className="info-table">
        <thead>
          <tr>
            <th>Kind</th>
            <th>Meaning</th>
          </tr>
        </thead>
        <tbody>
          <tr>
            <td>
              <code className="inline-code">expr</code>
            </td>
            <td>Expression</td>
          </tr>
          <tr>
            <td>
              <code className="inline-code">pat</code>
            </td>
            <td>Pattern</td>
          </tr>
          <tr>
            <td>
              <code className="inline-code">ty</code> /{' '}
              <code className="inline-code">type</code>
            </td>
            <td>Type syntax</td>
          </tr>
          <tr>
            <td>
              <code className="inline-code">ident</code>
            </td>
            <td>Identifier token</td>
          </tr>
          <tr>
            <td>
              <code className="inline-code">item</code>
            </td>
            <td>Top-level item fragment</td>
          </tr>
          <tr>
            <td>
              <code className="inline-code">tt</code>
            </td>
            <td>Single token tree</td>
          </tr>
          <tr>
            <td>
              <code className="inline-code">literal</code>
            </td>
            <td>Literal token</td>
          </tr>
          <tr>
            <td>
              <code className="inline-code">path</code>
            </td>
            <td>Qualified path</td>
          </tr>
          <tr>
            <td>
              <code className="inline-code">block</code>
            </td>
            <td>Block expression group</td>
          </tr>
        </tbody>
      </table>

      <h2>Built-in Macros</h2>
      <ul>
        <li>
          <code className="inline-code">count_args!(...)</code>
        </li>
        <li>
          <code className="inline-code">vec!(...)</code>
        </li>
        <li>
          <code className="inline-code">stringify!(...)</code>
        </li>
        <li>
          <code className="inline-code">concat!(...)</code> and{' '}
          <code className="inline-code">concat_str!(...)</code>
        </li>
      </ul>
    </DocsLayout>
  );
}

export default LanguageFeaturesPage;
