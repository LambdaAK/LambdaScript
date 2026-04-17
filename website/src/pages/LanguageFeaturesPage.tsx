import { Link } from 'react-router-dom';
import CodeBlock from '../components/CodeBlock';
import DocsLayout from '../components/DocsLayout';

const helloSnippet = `let () = println "Hello, Forge!"`;

const valuesSnippet = `// Values are immutable by default.
let project = "Forge"
let major = 1
let release_ready = false

let banner = str_concat "Language: " project
let () = println banner
let () = println (int_to_str major)`;

const functionSnippet = `let add x y = x + y

let rec sum xs =
  case xs do
  | [] -> 0
  | h :: t -> h + sum t

let () = println (int_to_str (add 20 22))
let () = println (int_to_str (sum [1, 2, 3, 4]))`;

const patternSnippet = `type Option<a> =
  | None
  | Some of a

let describe opt =
  case opt do
  | None -> "empty"
  | Some n -> str_concat "value=" (int_to_str n)

let () = println (describe (Some 7))`;

const recordSnippet = `type Point = { x: int, y: int }

let p1 : Point = { x: 4, y: 9 }
let p2 = { p1 with x = 100 }
let () = println (int_to_str (p2.x + p2.y))`;

const moduleSnippet = `mod Math where
  let square x = x * x
  let cube x = x * x * x
end

use Math
let () = println (int_to_str (Math.square 12))
let () = println (int_to_str (Math.cube 3))`;

const traitSnippet = `trait Render<a> where
  val render : a -> string
end

impl Render for Int where
  render x = int_to_str x
end

let () = println (render 42)`;

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

const annotationSnippet = `let answer : int = 42
let id x = x
let to_text (x : int) = int_to_str x

let () = println (to_text (id answer))`;

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

      <h2>Hello, Forge</h2>
      <p>
        A minimal Forge program is just top-level bindings plus effects through{' '}
        <code className="inline-code">let () = ...</code>.
      </p>
      <CodeBlock code={helloSnippet} />

      <h2>Values and Immutability</h2>
      <p>
        Bindings are immutable by default. You create new values rather than
        mutating old ones.
      </p>
      <CodeBlock code={valuesSnippet} />

      <h2>Functions and Recursion</h2>
      <p>
        Functions are first-class and recursion is explicit via{' '}
        <code className="inline-code">let rec</code>. Pattern matching over
        lists is a common style for recursive code.
      </p>
      <CodeBlock code={functionSnippet} />

      <h2>Algebraic Data Types and Pattern Matching</h2>
      <p>
        ADTs model domain states directly. Pattern matching with{' '}
        <code className="inline-code">case ... do</code> handles each case
        explicitly.
      </p>
      <CodeBlock code={patternSnippet} />

      <h2>Records</h2>
      <p>
        Records support field access and immutable update via{' '}
        <code className="inline-code">{'{ value with field = ... }'}</code>.
      </p>
      <CodeBlock code={recordSnippet} />

      <h2>Modules and Namespacing</h2>
      <p>
        Use <code className="inline-code">mod ... where ... end</code> to group
        definitions. Module names keep large projects organized and reduce
        naming collisions.
      </p>
      <CodeBlock code={moduleSnippet} />

      <h2>Traits and impls</h2>
      <p>
        Traits define interfaces, and <code className="inline-code">impl</code>{' '}
        supplies concrete behavior per type. This is the main abstraction
        mechanism for ad-hoc polymorphism in Forge.
      </p>
      <CodeBlock code={traitSnippet} />

      <h2>Type System</h2>
      <p>
        Forge uses Hindley-Milner style inference by default, with optional
        annotations where you want extra clarity.
      </p>
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
      <CodeBlock code={annotationSnippet} />

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

      <div className="callout">
        <strong>Next step:</strong> for end-to-end setup and commands, continue
        to <Link to="/docs/quickstart/">Quickstart</Link> and{' '}
        <Link to="/docs/examples/">Examples</Link>.
      </div>
    </DocsLayout>
  );
}

export default LanguageFeaturesPage;
