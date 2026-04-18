import { Link } from 'react-router-dom';
import CodeBlock from '../components/CodeBlock';
import HighlightedCode from '../components/HighlightedCode';

const heroCode = `// Fibonacci with pattern matching
let rec fib n =
  case n do
  | 0 -> 0
  | 1 -> 1
  | n -> fib (n - 1) + fib (n - 2)

let () = println (int_to_str (fib 10))`;

const featureCards = [
  {
    icon: '⇢',
    iconClass: 'violet',
    title: 'Type inference',
    description:
      'Hindley-Milner style. Write less, express more, compiler figures out the types.',
  },
  {
    icon: '◇',
    iconClass: 'mint',
    title: 'Functional core',
    description:
      'ADTs, pattern matching, and records. Immutable by default, expressive by design.',
  },
  {
    icon: '△',
    iconClass: 'amber',
    title: 'Macros',
    description: 'Rust-style macros for zero-cost abstraction at compile time.',
  },
  {
    icon: '◌',
    iconClass: 'blue',
    title: 'Interpreter',
    description: 'Fast local feedback loop. Run scripts directly without a compile step.',
  },
  {
    icon: '~',
    iconClass: 'pink',
    title: 'Native compiler (exp)',
    description: 'LLVM backend under active development for native binaries.',
  },
  {
    icon: '▦',
    iconClass: 'gray',
    title: 'Scalability',
    description:
      'Built for larger codebases with polymorphism, modules, and traits/impls.',
  },
] as const;

const dockerClone = `git clone https://github.com/LambdaAK/Forge
cd Forge`;

const dockerBuild = `docker build -t forge:local .`;

const dockerRebuildClean = `docker build --no-cache -t forge:local .`;

const dockerHelp = `docker run --rm forge:local help`;

const dockerRunMinimal = `docker run --rm forge:local run /opt/forge/programs/minimal.forge`;

const dockerExpectedMinimal = `3`;

const dockerRepl = `docker run --rm -it forge:local repl`;

const dockerRunMountedHello = `docker run --rm -it -v "$PWD":/work -w /work forge:local run /work/hello.forge`;

const dockerRunMountedPrograms = `docker run --rm -v "$PWD":/work -w /work forge:local run /work/programs/minimal.forge`;

const dockerCompileNative = `docker run --rm -it -v "$PWD":/work -w /work forge:local compile /work/hello.forge
docker run --rm -v "$PWD":/work -w /work forge:local /work/a.out`;

function HomePage() {
  return (
    <div className="landing-frame">
      <section className="hero-section reveal">
        <div className="hero-copy">
          <h1>A functional language that scales</h1>
          <p>
            Forge is a statically typed functional language with Hindley-Milner
            type inference, Rust-style macros, and an experimental LLVM native
            compiler.
          </p>
          <div className="cta-row">
            <a className="btn btn-primary" href="#run-with-docker">
              Run with Docker
            </a>
            <Link className="btn" to="/docs/quickstart/">
              Docs quickstart
            </Link>
            <a
              className="btn"
              href="https://github.com/LambdaAK/Forge"
              target="_blank"
              rel="noreferrer"
            >
              View on GitHub
            </a>
          </div>
        </div>
        <div className="hero-panels">
          <aside className="hero-code">
            <div className="hero-code-head">
              <div className="window-dots" aria-hidden>
                <span />
                <span />
                <span />
              </div>
              <span>hello.forge</span>
            </div>
            <pre>
              <HighlightedCode code={heroCode} />
            </pre>
          </aside>
        </div>
      </section>

      <section className="section-divider reveal delay-1">
        <p className="section-kicker">Core features</p>
        <h2>Everything you need, nothing you don't.</h2>
        <p className="section-lead">
          A small, principled language focused on correctness, fast iteration,
          and high-performance native execution.
        </p>
        <div className="feature-grid">
          {featureCards.map((feature) => (
            <article key={feature.title} className="feature-card">
              <span className={`feature-icon ${feature.iconClass}`} aria-hidden>
                {feature.icon}
              </span>
              <h3>{feature.title}</h3>
              <p>{feature.description}</p>
            </article>
          ))}
        </div>
      </section>

      <section
        id="run-with-docker"
        className="home-docker-section reveal delay-2"
        aria-labelledby="home-docker-heading"
      >
        <div className="home-docker-panel">
          <p className="section-kicker">No local compiler toolchain</p>
          <h2 id="home-docker-heading">Run Forge with Docker</h2>
          <p className="section-lead home-docker-lead">
            The easiest way to try the language: build one image from the
            repository, then use the <code className="inline-code">forge</code>{' '}
            entrypoint to interpret programs, open a REPL, or produce a native
            binary. You do not need OCaml, opam, Dune, or Clang installed on your
            computer.
          </p>

          <div className="callout home-docker-callout">
            <strong>What you need:</strong> a working{' '}
            <a
              href="https://docs.docker.com/get-docker/"
              target="_blank"
              rel="noreferrer"
              className="home-docker-link"
            >
              Docker
            </a>{' '}
            install (Docker Desktop on macOS and Windows, or Docker Engine on
            Linux) and a terminal. Commands below assume your shell&apos;s current
            directory is the <strong>root of the Forge git clone</strong>.
          </div>

          <div className="home-docker-body">
            <h3 className="home-docker-h3">1. Clone the repository</h3>
            <p className="home-docker-p">
              If you do not have the source yet, clone it and enter the project
              folder. All later <code className="inline-code">docker</code>{' '}
              commands are run from this directory.
            </p>
            <CodeBlock code={dockerClone} language="plain" />

            <h3 className="home-docker-h3">2. Build the Docker image</h3>
            <p className="home-docker-p">
              This Dockerfile compiles the Forge interpreter, REPL, and native
              compiler inside the container, then copies a small runtime image.
              The <strong>first</strong> build can take several minutes; later
              rebuilds are faster when layers are cached.
            </p>
            <CodeBlock code={dockerBuild} language="plain" />
            <p className="home-docker-p">
              After you change Forge sources or example programs, run the same
              build again so the image picks up your tree. For a completely clean
              rebuild (ignore Docker layer cache):
            </p>
            <CodeBlock code={dockerRebuildClean} language="plain" />

            <h3 className="home-docker-h3">3. List built-in commands</h3>
            <p className="home-docker-p">
              The container&apos;s entrypoint is a small helper named{' '}
              <code className="inline-code">forge</code>. With no arguments it
              prints help:
            </p>
            <CodeBlock code={dockerHelp} language="plain" />

            <h3 className="home-docker-h3">4. Run a program bundled in the image</h3>
            <p className="home-docker-p">
              Example programs from the repo&apos;s{' '}
              <code className="inline-code">programs/</code> directory are copied
              into the image at{' '}
              <code className="inline-code">/opt/forge/programs/</code> when you
              build. This runs the minimal sanity check (prints the integer{' '}
              <code className="inline-code">3</code>):
            </p>
            <CodeBlock code={dockerRunMinimal} language="plain" />
            <p className="home-docker-caption">Expected output</p>
            <CodeBlock code={dockerExpectedMinimal} language="plain" />

            <h3 className="home-docker-h3">5. Open an interactive REPL</h3>
            <p className="home-docker-p">
              Use <code className="inline-code">-it</code> so Docker attaches your
              terminal to the REPL session (stdin/stdout). Exit the REPL with
              Ctrl+D or your usual EOF shortcut.
            </p>
            <CodeBlock code={dockerRepl} language="plain" />

            <h3 className="home-docker-h3">6. Run a file from your machine</h3>
            <p className="home-docker-p">
              Mount your project directory at <code className="inline-code">/work</code>{' '}
              and set the working directory there. Then pass paths under{' '}
              <code className="inline-code">/work/...</code>. Replace{' '}
              <code className="inline-code">hello.forge</code> with any{' '}
              <code className="inline-code">.forge</code> file you create next to
              your clone (for example <code className="inline-code">hello.forge</code>{' '}
              in the repo root):
            </p>
            <CodeBlock code={dockerRunMountedHello} language="plain" />
            <p className="home-docker-p">
              To run something under <code className="inline-code">programs/</code>{' '}
              without rebuilding the image after every edit:
            </p>
            <CodeBlock code={dockerRunMountedPrograms} language="plain" />
            <p className="home-docker-note">
              On Windows, use Docker Desktop&apos;s path conventions (often mount
              the current directory with <code className="inline-code">%cd%</code>{' '}
              in PowerShell or run these commands from Git Bash so{' '}
              <code className="inline-code">$PWD</code> behaves like a Unix path).
            </p>

            <h3 className="home-docker-h3">7. Native compile (optional)</h3>
            <p className="home-docker-p">
              The image can also ahead-of-time compile to a native executable.
              <code className="inline-code"> forge compile</code> always writes the
              binary to <code className="inline-code">/work/a.out</code> inside the
              container—which becomes <code className="inline-code">./a.out</code>{' '}
              on your host when <code className="inline-code">/work</code> is your
              mounted clone:
            </p>
            <CodeBlock code={dockerCompileNative} language="plain" />

            <div className="callout home-docker-callout">
              <strong>Rebuild vs mount:</strong> files under{' '}
              <code className="inline-code">/opt/forge/...</code> reflect the repo
              <em> at image build time</em>. To iterate on local{' '}
              <code className="inline-code">.forge</code> files without rebuilding,
              prefer the <code className="inline-code">-v &quot;$PWD&quot;:/work -w /work</code>{' '}
              pattern and paths starting with <code className="inline-code">/work/</code>.
            </div>

            <p className="home-docker-footer">
              More detail lives in the{' '}
              <Link to="/docs/quickstart/">documentation quickstart</Link>. To hack
              on the OCaml compiler itself, you will still want a local opam/Dune
              setup—see that README.
            </p>
          </div>
        </div>
      </section>
    </div>
  );
}

export default HomePage;
