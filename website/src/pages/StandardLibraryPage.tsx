import { Link } from 'react-router-dom';
import CodeBlock from '../components/CodeBlock';
import DocsLayout from '../components/DocsLayout';

const sampleProgram = `let () = println (int_to_str (str_length "hello"))
let () = println (str_concat "hello" " world")
let () = println (str_slice "hello" 1 3)

let xs = [10, 20, 30]
let () = println (int_to_str (list_length xs))
let () = println (int_to_str (list_head xs))
let () = println (int_to_str (list_nth xs 2))

let evens = filter (fn x -> x % 2 == 0) [1, 2, 3, 4, 5, 6]
let total = reduce_left (+) 0 evens
let () = println (int_to_str total)`;

const runProgram = `cat > programs/stdlib_demo.ls <<'LS'
let () = println (int_to_str (str_length "hello"))
let () = println (str_concat "hello" " world")
let () = println (str_slice "hello" 1 3)
let xs = [10, 20, 30]
let () = println (int_to_str (list_length xs))
let () = println (int_to_str (list_head xs))
let () = println (int_to_str (list_nth xs 2))
let evens = filter (fn x -> x % 2 == 0) [1, 2, 3, 4, 5, 6]
let total = reduce_left (+) 0 evens
let () = println (int_to_str total)
LS

dune exec ./bin/interpreter.exe programs/stdlib_demo.ls`;

function StandardLibraryPage() {
  return (
    <DocsLayout>
      <div className="page-intro">
        <div className="breadcrumb">
          <Link to="/">Home</Link> / <Link to="/docs/">Docs</Link> /
          Standard Library
        </div>
        <h1 className="page-title">Standard Library</h1>
        <p className="lead">
          Forge loads a standard prelude that brings core data types, traits,
          and utility functions into scope.
        </p>
      </div>

      <h2>Prelude Behavior</h2>
      <ul>
        <li>Interpreter and compiler prepend the prelude source to user programs.</li>
        <li>REPL loads the prelude once at startup.</li>
        <li>
          Prelude provides canonical list/option style data definitions and
          trait instances.
        </li>
      </ul>

      <h2>Built-in Function Families</h2>
      <table className="info-table">
        <thead>
          <tr>
            <th>Category</th>
            <th>Functions</th>
          </tr>
        </thead>
        <tbody>
          <tr>
            <td>I/O</td>
            <td>
              <code className="inline-code">print</code>,{' '}
              <code className="inline-code">println</code>
            </td>
          </tr>
          <tr>
            <td>Type conversion</td>
            <td>
              <code className="inline-code">int_to_str</code>,{' '}
              <code className="inline-code">int_to_float</code>,{' '}
              <code className="inline-code">float_to_int</code>,{' '}
              <code className="inline-code">string_to_list</code>
            </td>
          </tr>
          <tr>
            <td>Strings</td>
            <td>
              <code className="inline-code">str_length</code>,{' '}
              <code className="inline-code">str_concat</code>,{' '}
              <code className="inline-code">str_slice</code>
            </td>
          </tr>
          <tr>
            <td>Lists</td>
            <td>
              <code className="inline-code">list_length</code>,{' '}
              <code className="inline-code">list_head</code>,{' '}
              <code className="inline-code">list_tail</code>,{' '}
              <code className="inline-code">list_nth</code>
            </td>
          </tr>
          <tr>
            <td>Tuples</td>
            <td>
              <code className="inline-code">tuple_fst</code>,{' '}
              <code className="inline-code">tuple_snd</code>
            </td>
          </tr>
          <tr>
            <td>Higher-order</td>
            <td>
              <code className="inline-code">map</code>,{' '}
              <code className="inline-code">filter</code>,{' '}
              <code className="inline-code">reduce_left</code>,{' '}
              <code className="inline-code">reduce_right</code>,{' '}
              <code className="inline-code">not</code>
            </td>
          </tr>
        </tbody>
      </table>

      <h2>Example: Built-ins in One Program</h2>
      <CodeBlock code={sampleProgram} />

      <h2>Run It</h2>
      <CodeBlock code={runProgram} />
    </DocsLayout>
  );
}

export default StandardLibraryPage;
