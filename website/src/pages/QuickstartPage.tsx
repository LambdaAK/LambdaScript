import { Link } from 'react-router-dom';
import CodeBlock from '../components/CodeBlock';
import DocsLayout from '../components/DocsLayout';

const cloneRepo = `git clone https://github.com/LambdaAK/Forge
cd Forge`;

const dockerBuild = `docker build -t forge:local .`;

const dockerRebuild = `docker build --no-cache -t forge:local .`;

const dockerHelp = `docker run --rm forge:local help`;

const dockerRunMinimal = `docker run --rm forge:local run /opt/forge/programs/minimal.forge`;

const dockerRepl = `docker run --rm -it forge:local repl`;

const dockerRunProgramsMount = `# from the repo root (mounts your working tree at /work)
docker run --rm -v "$PWD":/work -w /work forge:local run /work/programs/minimal.forge`;

const dockerCompileMount = `# native compile writes /work/a.out → ./a.out on the host
docker run --rm -it -v "$PWD":/work -w /work forge:local compile /work/hello.forge
docker run --rm -v "$PWD":/work -w /work forge:local /work/a.out`;

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
          Run Forge with Docker: clone the repository, build one image, then use
          the <code className="inline-code">forge</code> entrypoint to interpret,
          compile, or open a REPL. No local OCaml, opam, or Clang install required.
        </p>
      </div>

      <h2>1) Prerequisites</h2>
      <p>
        Install <a href="https://docs.docker.com/get-docker/">Docker</a> (Docker
        Desktop on macOS and Windows, or Docker Engine on Linux). You need a
        clone of the Forge repo for the Dockerfile and standard library layout.
      </p>

      <h2>2) Clone the Repository</h2>
      <p>Commands below assume your shell&apos;s current directory is the repo root.</p>
      <CodeBlock code={cloneRepo} language="plain" />

      <h2>3) Build the Image</h2>
      <p>
        From the repository root, build and tag the image as{' '}
        <code className="inline-code">forge:local</code> (first build compiles
        Forge inside the container and may take several minutes).
      </p>
      <CodeBlock code={dockerBuild} language="plain" />

      <h3>Rebuild after you change sources</h3>
      <p>
        Programs baked into the image come from the tree at build time. After
        you edit files, run the same build again. To ignore Docker layer cache
        (clean rebuild):
      </p>
      <CodeBlock code={dockerRebuild} language="plain" />

      <h2>4) Commands Inside the Image</h2>
      <p>
        The container entrypoint is <code className="inline-code">forge</code>.
        Default is <code className="inline-code">help</code>.
      </p>
      <CodeBlock code={dockerHelp} language="plain" />

      <h3>Run a program bundled in the image</h3>
      <p>
        Examples ship under <code className="inline-code">/opt/forge/programs/</code>{' '}
        (copied from <code className="inline-code">programs/</code> when the image
        was built).
      </p>
      <CodeBlock code={dockerRunMinimal} language="plain" />

      <h3>Interactive REPL</h3>
      <CodeBlock code={dockerRepl} language="plain" />

      <h3>Run a file from your checkout (no rebuild)</h3>
      <p>
        Mount the repo at <code className="inline-code">/work</code> and pass paths
        under <code className="inline-code">/work/...</code>. Example:
      </p>
      <CodeBlock code={dockerRunProgramsMount} language="plain" />

      <h3>Native compile a local file</h3>
      <p>
        <code className="inline-code">forge compile</code> always writes the
        executable to <code className="inline-code">/work/a.out</code> in the
        container. With <code className="inline-code">-v &quot;$PWD&quot;:/work</code>, that is{' '}
        <code className="inline-code">./a.out</code> in your mounted directory.
      </p>
      <CodeBlock code={dockerCompileMount} language="plain" />

      <div className="callout">
        <strong>Summary:</strong>{' '}
        <code className="inline-code">docker build -t forge:local .</code> once
        per change you want in the image; use{' '}
        <code className="inline-code">-v &quot;$PWD&quot;:/work -w /work</code> to
        run or compile files from your host without rebuilding. Forge sources use{' '}
        <code className="inline-code">.forge</code>.
      </div>

      <h2>Developing the Compiler Itself</h2>
      <p>
        Hacking on the OCaml implementation still uses a local opam/Dune setup.
        See the{' '}
        <a
          href="https://github.com/LambdaAK/Forge/blob/main/README.md"
          target="_blank"
          rel="noreferrer"
        >
          README on GitHub
        </a>{' '}
        for toolchain install and <code className="inline-code">dune</code> /{' '}
        <code className="inline-code">make</code> workflows.
      </p>
    </DocsLayout>
  );
}

export default QuickstartPage;
