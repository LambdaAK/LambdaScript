import { Link } from 'react-router-dom';
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
    title: 'Native compiler',
    description: 'Min IR -> LLVM -> native binary. Real performance, no runtime overhead.',
  },
  {
    icon: '▦',
    iconClass: 'gray',
    title: 'Scalability',
    description:
      'Built for larger codebases with polymorphism, modules, and traits/impls.',
  },
] as const;

const quickstartSteps = [
  { title: 'Build the toolchain', command: 'make' },
  {
    title: 'Run the interpreter',
    command: 'dune exec ./bin/interpreter.exe programs/minimal.forge',
  },
  {
    title: 'Compile a program',
    command: 'make compile-ls FILE=programs/minimal.forge',
  },
  { title: 'Run the binary', command: './a.out' },
] as const;

function HomePage() {
  return (
    <div className="landing-frame">
      <section className="hero-section reveal">
        <div className="hero-copy">
          <h1>A functional language that scales</h1>
          <p>
            Forge is a statically typed functional language with Hindley-Milner
            type inference, Rust-style macros, and native compilation via LLVM.
          </p>
          <div className="cta-row">
            <Link className="btn btn-primary" to="/docs/quickstart/">
              Quickstart
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
      </section>

      <section className="section-divider reveal delay-1">
        <p className="section-kicker">Core features</p>
        <h2>Everything you need, nothing you don't.</h2>
        <p className="section-lead">
          A small, principled language designed for correctness, speed, and scalability.
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

      <section className="section-divider reveal delay-2">
        <p className="section-kicker">Quick start</p>
        <h2>Up and running in 4 steps.</h2>
        <div className="quickstart-grid">
          {quickstartSteps.map((step, index) => (
            <article key={step.title} className="quickstart-step">
              <div className="step-head">
                <span className="step-number">{index + 1}</span>
                <h3>{step.title}</h3>
              </div>
              <code className="step-command">{step.command}</code>
            </article>
          ))}
        </div>
      </section>
    </div>
  );
}

export default HomePage;
