import { Link } from 'react-router-dom';
import CodeBlock from '../components/CodeBlock';
import DocsLayout from '../components/DocsLayout';

const cloneBuild = `git clone https://github.com/LambdaAK/LambdaScript
cd LambdaScript
make`;

const runInterpreter = `dune exec ./bin/interpreter.exe programs/minimal.ls`;

const replCmd = `make repl
# optional preload
make repl FILE=programs/simple_test.ls`;

const compileCmd = `make compile-ls FILE=programs/minimal.ls
./a.out`;

const compileOutCmd = `make compile-ls FILE=programs/minimal.ls OUT=./my_program
./my_program`;

const testCmd = `make suite
make compiler-suite
# everything dune knows about
dune test`;

function QuickstartPage() {
  return (
    <DocsLayout>
      <div className="page-intro">
        <div className="breadcrumb">
          <Link to="/">Home</Link> / <Link to="/docs/">Docs</Link> /
          Quickstart
        </div>
        <h1 className="page-title">Quickstart</h1>
        <p className="lead">
          Set up Forge locally, run a program in the interpreter, then compile
          one to native code.
        </p>
      </div>

      <h2>Prerequisites</h2>
      <ul>
        <li>OCaml 5.0 or newer</li>
        <li>Dune</li>
        <li>OPAM</li>
        <li>Clang (used by the native compiler pipeline)</li>
      </ul>

      <h2>Clone and Build</h2>
      <CodeBlock code={cloneBuild} />

      <h2>Run a Program (Interpreter)</h2>
      <CodeBlock code={runInterpreter} />

      <h2>Use the REPL</h2>
      <CodeBlock code={replCmd} />

      <h2>Compile to Native Code</h2>
      <CodeBlock code={compileCmd} />

      <h2>Optional Output Name</h2>
      <CodeBlock code={compileOutCmd} />

      <h2>Test Commands</h2>
      <CodeBlock code={testCmd} />

      <div className="callout">
        <strong>Note:</strong> Forge source files are commonly{' '}
        <code className="inline-code">.ls</code>. Some legacy examples also use{' '}
        <code className="inline-code">.txt</code>.
      </div>
    </DocsLayout>
  );
}

export default QuickstartPage;
