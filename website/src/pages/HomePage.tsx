import { Link } from 'react-router-dom';

function HomePage() {
  const quickstart = `make
dune exec ./bin/interpreter.exe programs/minimal.ls
make compile-ls FILE=programs/minimal.ls
./a.out`;

  return (
    <>
      <section className="intro-shell reveal">
        <article className="intro-card intro-card--compact">
          <div className="intro-copy">
            <span className="eyebrow">Programming Language</span>
            <h1>Forge</h1>
            <p className="intro-role">
              Statically typed functional language with type inference,
              macro_rules, and native compilation.
            </p>
            <div className="cta-row">
              <Link className="btn btn-primary" to="/docs/quickstart/">
                Quickstart
              </Link>
              <Link className="btn" to="/docs/">
                Docs
              </Link>
            </div>
          </div>

          <aside className="hero-quickstart">
            <h3>Quick Start</h3>
            <pre>
              <code>{quickstart}</code>
            </pre>
          </aside>
        </article>
      </section>

      <section className="reveal delay-1">
        <h2>Core Features</h2>
        <div className="grid-3">
          <article className="card feature-card">
            <h3>Type Inference</h3>
            <p>Hindley-Milner style</p>
          </article>
          <article className="card feature-card">
            <h3>Functional Core</h3>
            <p>ADTs, pattern matching, records</p>
          </article>
          <article className="card feature-card">
            <h3>Macros</h3>
            <p>Rust-style macro_rules</p>
          </article>
          <article className="card feature-card">
            <h3>Interpreter</h3>
            <p>Fast local feedback loop</p>
          </article>
          <article className="card feature-card">
            <h3>Compiler</h3>
            <p>{'Min IR -> LLVM -> native'}</p>
          </article>
          <article className="card feature-card">
            <h3>Tooling</h3>
            <p>REPL, tests, docs</p>
          </article>
        </div>
      </section>

      <section className="reveal delay-2">
        <h2>Learn More</h2>
        <div className="grid-3">
          <article className="card">
            <h3>Documentation</h3>
            <p>Syntax and language guide.</p>
            <Link className="btn" to="/docs/">
              Open Docs
            </Link>
          </article>
          <article className="card">
            <h3>Examples</h3>
            <p>Copy and run real programs.</p>
            <Link className="btn" to="/docs/examples/">
              Browse Examples
            </Link>
          </article>
          <article className="card">
            <h3>How It Works</h3>
            <p>Lexer, parser, typechecker, compiler.</p>
            <Link className="btn" to="/docs/how-it-works/">
              View Internals
            </Link>
          </article>
        </div>
      </section>

      <section className="callout reveal delay-3">
        <strong>Frontend:</strong> minimalist React/Vite website, Netlify-ready.
      </section>
    </>
  );
}

export default HomePage;
