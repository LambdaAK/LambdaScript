import { Link } from 'react-router-dom'

const workflows = [
  {
    title: 'Web IDE',
    blurb: 'Write Forge in a real multi-file browser IDE with syntax coloring, run targeting, and instant output.',
    cta: 'Open IDE',
    to: '/ide',
  },
  {
    title: 'Language Docs',
    blurb: 'Move from syntax to traits, ADTs, pattern matching, and prelude usage with practical references.',
    cta: 'Read Docs',
    to: '/docs',
  },
  {
    title: 'Install + Build',
    blurb: 'Use the repo compiler and runtime directly, with examples that mirror what runs in the IDE.',
    cta: 'Install Guide',
    to: '/docs/install',
  },
]

const capabilities = [
  'Static types with inference and algebraic data types',
  'Traits and implementations for reusable abstractions',
  'Pattern matching over rich recursive structures',
  'Browser evaluator plus local long-lived playground process',
  'Native compiler pipeline and interpreter in one repository',
  'VS Code syntax package and language tooling foundations',
]

export default function HomePage() {
  return (
    <div className="home-page">
      <section className="hero-panel">
        <div className="hero-copy">
          <p className="eyebrow">Forge Language</p>
          <h1>Industrial-strength functional programming, with the speed of a hackable local toolchain.</h1>
          <p className="lede">
            Forge gives you the feel of expressive typed FP while staying practical for real code: inference,
            ADTs, traits, pattern matching, and a prelude you can actually build with.
          </p>
          <div className="home-actions">
            <Link to="/ide" className="btn btn-primary">
              Launch IDE
            </Link>
            <Link to="/docs" className="btn">
              Explore Docs
            </Link>
          </div>
        </div>

        <div className="hero-console" aria-label="Forge command showcase">
          <div className="hero-console-title">Quickstart</div>
          <pre>
            <code>{`dune build bin/playground.exe
npm run dev --prefix website
npm run server --prefix website`}</code>
          </pre>
          <p>
            Run the site + playground API together, then open the IDE to execute individual files or full
            workspaces.
          </p>
        </div>
      </section>

      <section className="workflow-grid" aria-label="Forge workflows">
        {workflows.map((item) => (
          <article key={item.title} className="workflow-card">
            <h2>{item.title}</h2>
            <p>{item.blurb}</p>
            <Link to={item.to} className="workflow-link">
              {item.cta}
            </Link>
          </article>
        ))}
      </section>

      <section className="capabilities-panel" aria-label="Forge capabilities">
        <h2>What you can do with Forge right now</h2>
        <div className="capabilities-grid">
          {capabilities.map((line) => (
            <article key={line} className="capability-card">
              {line}
            </article>
          ))}
        </div>
      </section>

      <section className="showcase-panel">
        <div>
          <p className="eyebrow">Language Snapshot</p>
          <h2>Readable, typed code with minimal ceremony</h2>
          <p>
            Forge keeps source compact while preserving explicit type structure and predictable runtime
            behavior.
          </p>
        </div>
        <pre className="showcase-code" aria-label="Forge sample snippet">
          <code>{`type rec Tree = | Leaf of Int | Node of (Tree, Tree)

let rec sum_tree t =
  case t do
  | Leaf n -> n
  | Node (left, right) -> sum_tree left + sum_tree right

let _ = println (sum_tree (Node (Leaf 4, Node (Leaf 8, Leaf 15))))`}</code>
        </pre>
      </section>
    </div>
  )
}
