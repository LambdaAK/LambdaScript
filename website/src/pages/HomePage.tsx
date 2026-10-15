import { Link } from 'react-router-dom';
import CodeBlock from '../components/CodeBlock';
import HighlightedCode from '../components/HighlightedCode';

const GITHUB = 'https://github.com/LambdaAK/Forge';

const heroCode = `// No type annotations: every type is inferred.
let rec fib n =
  case n do
  | 0 -> 0
  | 1 -> 1
  | n -> fib (n - 1) + fib (n - 2)

let () = println [fib n | n <- [1...10]]`;

const heroOutput = '[1, 1, 2, 3, 5, 8, 13, 21, 34, 55]';

const features = [
  {
    title: 'Type inference',
    description:
      'Hindley–Milner with let-polymorphism. Annotations are optional and checked when you write them.',
  },
  {
    title: 'Data and patterns',
    description:
      'Sum types, tuples, lists and records with nested patterns, plus warnings for cases you missed or can never reach.',
  },
  {
    title: 'Traits',
    description:
      'Type classes with superclasses, default methods, conditional instances and higher-kinded traits such as Functor and Monad.',
  },
  {
    title: 'Structural records',
    description:
      'A function that reads a field accepts any record that has it, through row polymorphism.',
  },
  {
    title: 'Macros and modules',
    description: 'macro_rules! macros by example, nested modules and file imports.',
  },
  {
    title: 'Two back ends',
    description:
      'An interpreter and REPL, and a native compiler through LLVM. 6,000 tests check that they agree.',
  },
];

const docs = [
  { to: '/docs/quickstart/', title: 'Quickstart', description: 'Run Forge with Docker, or build it from source.' },
  { to: '/docs/language-features/', title: 'The language', description: 'Values, functions, data types, patterns, records, modules, traits and macros.' },
  { to: '/docs/standard-library/', title: 'Standard library', description: 'Built-in functions and the prelude.' },
  { to: '/docs/examples/', title: 'Examples', description: 'Complete programs to read and run.' },
  { to: '/docs/grammar/', title: 'Grammar', description: 'The syntax of programs, expressions, patterns, types and macros.' },
  { to: '/docs/how-it-works/', title: 'Internals', description: 'From source text to native code: the pipeline behind both back ends.' },
];

const dockerSetup = `git clone https://github.com/LambdaAK/Forge && cd Forge
docker build -t forge:local .`;

const dockerRun = `docker run --rm forge:local run /opt/forge/programs/minimal.forge`;

const dockerRepl = `docker run --rm -it forge:local repl`;

function HomePage() {
  return (
    <>
      <section className="home-intro" aria-label="About Forge">
        <h1 className="t-page">
          Forge <span className="spark" aria-hidden>✻</span>
        </h1>
        <div className="t-sub" style={{ marginTop: 16 }}>
          <p>Forge is a small, statically typed functional language, written in OCaml.</p>
          <p>
            You write no type annotations: it infers every type, checks your traits and
            pattern matches, and then runs the program in an interpreter or compiles it to
            native code through LLVM.
          </p>
        </div>
        <p className="t-sub link-row">
          <Link className="ink" to="/docs/quickstart/">
            quickstart
          </Link>
          <Link className="ink" to="/docs/">
            docs
          </Link>
          <Link className="ink" to="/docs/examples/">
            examples
          </Link>
          <a className="ink" href={GITHUB} target="_blank" rel="noreferrer">
            github
          </a>
        </p>

        <figure className="panel">
          <div className="panel-head t-meta">
            <span>fib.forge</span>
            <span>interpreter or native</span>
          </div>
          <pre>
            <HighlightedCode code={heroCode} />
          </pre>
          <div className="panel-output">
            <p className="t-meta">output</p>
            <pre>{heroOutput}</pre>
          </div>
        </figure>
      </section>

      <section className="home-section" aria-labelledby="features">
        <h2 id="features" className="t-group">
          What&apos;s in it
        </h2>
        <ul className="feature-list">
          {features.map((f) => (
            <li key={f.title}>
              <p className="t-title">{f.title}</p>
              <p className="t-desc">{f.description}</p>
            </li>
          ))}
        </ul>
      </section>

      <section className="home-section" aria-labelledby="try">
        <h2 id="try" className="t-group">
          Try it with Docker
        </h2>
        <p className="t-desc">
          Nothing to install but{' '}
          <a className="ink" href="https://docs.docker.com/get-docker/" target="_blank" rel="noreferrer">
            Docker
          </a>
          . From a clone of the repository:
        </p>
        <div className="step" style={{ marginTop: 16 }}>
          <p className="t-title">Build the image</p>
          <CodeBlock code={dockerSetup} language="plain" />
        </div>
        <div className="step">
          <p className="t-title">Run a program</p>
          <CodeBlock code={dockerRun} language="plain" />
        </div>
        <div className="step">
          <p className="t-title">Open the REPL</p>
          <CodeBlock code={dockerRepl} language="plain" />
        </div>
        <p className="t-desc">
          <Link className="more" to="/docs/quickstart/">
            native compilation and building from source →
          </Link>
        </p>
      </section>

      <section className="home-section" aria-labelledby="learn">
        <h2 id="learn" className="t-group">
          Documentation
        </h2>
        <ul className="page-list">
          {docs.map((d) => (
            <li key={d.to}>
              <Link className="t-title ink" to={d.to}>
                {d.title}
              </Link>
              <p className="t-desc">{d.description}</p>
            </li>
          ))}
        </ul>
      </section>
    </>
  );
}

export default HomePage;
