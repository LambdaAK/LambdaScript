import { Link } from 'react-router-dom';
import CodeBlock from '../components/CodeBlock';
import DocsLayout from '../components/DocsLayout';

const macToolchain = `# Homebrew + OPAM toolchain
brew install opam
brew install llvm clang

opam init -a --disable-sandboxing
eval "$(opam env)"
opam switch create 5.1.1
opam install dune`;

const ubuntuToolchain = `# Ubuntu / Debian toolchain
sudo apt update
sudo apt install -y opam m4 pkg-config libgmp-dev clang

opam init -a
eval "$(opam env)"
opam switch create 5.1.1
opam install dune`;

const verifyToolchain = `ocamlc -version
dune --version
clang --version`;

const cloneBuild = `git clone https://github.com/LambdaAK/Forge
cd Forge
make`;

const runInterpreter = `dune exec ./bin/interpreter.exe programs/minimal.forge`;

const replCmd = `make repl
# optional preload
make repl FILE=programs/simple_test.forge`;

const compileCmd = `make compile-ls FILE=programs/minimal.forge
./a.out`;

const compileOutCmd = `make compile-ls FILE=programs/minimal.forge OUT=./my_program
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
          Install the toolchain, build Forge, then run your first program in a
          few minutes.
        </p>
      </div>

      <h2>1) Install the Toolchain</h2>
      <p>Forge needs OCaml + Dune + OPAM + Clang.</p>

      <h3>macOS</h3>
      <CodeBlock code={macToolchain} language="plain" />

      <h3>Ubuntu / Debian</h3>
      <CodeBlock code={ubuntuToolchain} language="plain" />

      <h3>Verify Install</h3>
      <CodeBlock code={verifyToolchain} language="plain" />

      <h2>2) Clone and Build Forge</h2>
      <CodeBlock code={cloneBuild} language="plain" />

      <h2>3) Run a Program (Interpreter)</h2>
      <CodeBlock code={runInterpreter} language="plain" />

      <h2>4) Use the REPL</h2>
      <CodeBlock code={replCmd} language="plain" />

      <h2>5) Compile to Native Code</h2>
      <CodeBlock code={compileCmd} language="plain" />

      <h3>Optional Output Name</h3>
      <CodeBlock code={compileOutCmd} language="plain" />

      <h2>Test Commands</h2>
      <CodeBlock code={testCmd} language="plain" />

      <div className="callout">
        <strong>Note:</strong> Start with interpreter + REPL first for fastest
        feedback. Forge source files use{' '}
        <code className="inline-code">.forge</code>.
      </div>
    </DocsLayout>
  );
}

export default QuickstartPage;
