import { Link } from 'react-router-dom';
import CodeBlock from '../components/CodeBlock';
import DocsLayout from '../components/DocsLayout';

const listSnippet = `let rec length xs =
  case xs do
  | [] -> 0
  | _ :: t -> 1 + length t

let n = length [10, 20, 30, 40]
let () = println (int_to_str n)`;

const recordSnippet = `type Point = { x: int, y: int }

let p1 : Point = { x: 4, y: 9 }
let p2 = { p1 with x = 100 }
let () = println (int_to_str (p2.x + p2.y))`;

const traitSnippet = `inter Render<a> where
  val render : a -> string
end

impl Render for Int where
  render x = int_to_str x
end

let () = println (render 42)`;

const macroSnippet = `macro_rules! add where
  ($a:expr, $b:expr) => $a + $b
end

macro_rules! collect where
  ($($x:expr),*) => vec!($($x),*)
end

let sum = add!(20, 22)
let items = collect!(1, 2, 3, 4)
let () = println (int_to_str sum)
let () = println (int_to_str (list_length items))`;

function ExamplesPage() {
  return (
    <DocsLayout>
      <div className="page-intro">
        <div className="breadcrumb">
          <Link to="/">Home</Link> / <Link to="/docs/">Docs</Link> /
          Examples
        </div>
        <h1 className="page-title">Examples</h1>
        <p className="lead">
          These snippets are meant to be copied into local{' '}
          <code className="inline-code">.forge</code> files and run with the
          interpreter.
        </p>
      </div>

      <h2>Feature Snippets</h2>

      <h3>1) Pattern Matching + Lists</h3>
      <CodeBlock code={listSnippet} />

      <h3>2) Records</h3>
      <CodeBlock code={recordSnippet} />

      <h3>3) Traits / Typeclasses</h3>
      <CodeBlock code={traitSnippet} />

      <h3>4) Macro Rules</h3>
      <CodeBlock code={macroSnippet} />

      <h2>Run Existing Programs in the Repo</h2>
      <p>
        These are already included under{' '}
        <code className="inline-code">programs/</code>.
      </p>
      <table className="info-table">
        <thead>
          <tr>
            <th>Program</th>
            <th>Purpose</th>
            <th>Run Command</th>
          </tr>
        </thead>
        <tbody>
          <tr>
            <td>
              <code className="inline-code">programs/minimal.forge</code>
            </td>
            <td>Small baseline sanity program</td>
            <td>
              <code className="inline-code">
                dune exec ./bin/interpreter.exe programs/minimal.forge
              </code>
            </td>
          </tr>
          <tr>
            <td>
              <code className="inline-code">programs/record_update.forge</code>
            </td>
            <td>Record access and immutable update</td>
            <td>
              <code className="inline-code">
                dune exec ./bin/interpreter.exe programs/record_update.forge
              </code>
            </td>
          </tr>
          <tr>
            <td>
              <code className="inline-code">
                programs/red_black_tree_example.forge
              </code>
            </td>
            <td>Larger recursive ADT program</td>
            <td>
              <code className="inline-code">
                dune exec ./bin/interpreter.exe programs/red_black_tree_example.forge
              </code>
            </td>
          </tr>
          <tr>
            <td>
              <code className="inline-code">programs/test.forge</code>
            </td>
            <td>Macro rules sample</td>
            <td>
              <code className="inline-code">
                dune exec ./bin/interpreter.exe programs/test.forge
              </code>
            </td>
          </tr>
        </tbody>
      </table>

      <div className="callout">
        <strong>Tip:</strong> if you are evaluating the project quickly, run
        two interpreter programs (for example{' '}
        <code className="inline-code">minimal.forge</code> and{' '}
        <code className="inline-code">test.forge</code>) to see core language +
        macro behavior in under 2 minutes.
      </div>
    </DocsLayout>
  );
}

export default ExamplesPage;
