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

const dockerBuild = `docker build -t forge:local .`;

const dockerRunMinimal = `docker run --rm forge:local run /opt/forge/programs/minimal.forge`;

const dockerRepl = `docker run --rm -it forge:local repl`;

const dockerRunLocalFile = `docker run --rm -it -v "$PWD":/work -w /work forge:local run /work/hello.forge`;

const dockerCompileLocalFile = `# compile always writes /work/a.out (host: ./a.out)
docker run --rm -it -v "$PWD":/work -w /work forge:local compile /work/hello.forge
docker run --rm -v "$PWD":/work -w /work forge:local /work/a.out`;

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

const testCmd = `make suite
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
          Try Forge with Docker in minutes, or install the full local toolchain
          for development.
        </p>
      </div>

      <h2>0) Try With Docker (No Local Toolchain)</h2>
      <p>
        Docker mode avoids local installs of OPAM, OCaml, Dune, and Clang.
      </p>

      <h3>Build the Image</h3>
      <CodeBlock code={dockerBuild} language="plain" />

      <h3>Run the Minimal Program</h3>
      <CodeBlock code={dockerRunMinimal} language="plain" />

      <h3>Open REPL</h3>
      <CodeBlock code={dockerRepl} language="plain" />

      <h3>Run a Local File</h3>
      <CodeBlock code={dockerRunLocalFile} language="plain" />

      <h3>Compile a Local File</h3>
      <CodeBlock code={dockerCompileLocalFile} language="plain" />

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

      <h2>Test Commands</h2>
      <CodeBlock code={testCmd} language="plain" />

      <div className="callout">
        <strong>Note:</strong> Start with interpreter + REPL first for fastest
        feedback. Native compilation is currently experimental. Forge source
        files use{' '}
        <code className="inline-code">.forge</code>.
      </div>
    </DocsLayout>
  );
}

export default QuickstartPage;
