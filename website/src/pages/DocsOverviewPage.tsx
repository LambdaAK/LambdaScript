import { Link } from 'react-router-dom';
import DocsLayout from '../components/DocsLayout';

function DocsOverviewPage() {
  return (
    <DocsLayout>
      <div className="page-intro">
        <div className="breadcrumb">
          <Link to="/">Home</Link> / Docs
        </div>
        <h1 className="page-title">Forge Documentation</h1>
        <p className="lead">
          This docs set is optimized for three audiences: recruiters who want a
          quick signal, users who want to run code, and compiler learners who
          want implementation depth.
        </p>
      </div>

      <h2>Start Paths</h2>
      <div className="grid-3">
        <section className="card">
          <h3>Recruiters</h3>
          <p>
            Read project value quickly, then inspect architecture and sample
            programs.
          </p>
          <Link className="btn" to="/docs/how-it-works/">
            Open Architecture
          </Link>
        </section>
        <section className="card">
          <h3>Users</h3>
          <p>
            Install locally, run interpreter/compiler, then copy programs from
            examples.
          </p>
          <Link className="btn" to="/docs/quickstart/">
            Open Quickstart
          </Link>
        </section>
        <section className="card">
          <h3>Compiler Learners</h3>
          <p>
            Read parser/typechecker/compiler flow and grammar in intermediate
            depth.
          </p>
          <Link className="btn" to="/docs/grammar/">
            Open Grammar
          </Link>
        </section>
      </div>

      <h2>Docs Map</h2>
      <table className="info-table">
        <thead>
          <tr>
            <th>Section</th>
            <th>What You Get</th>
          </tr>
        </thead>
        <tbody>
          <tr>
            <td>
              <Link to="/docs/quickstart/">Quickstart</Link>
            </td>
            <td>Install prerequisites and run Forge programs locally in minutes.</td>
          </tr>
          <tr>
            <td>
              <Link to="/docs/language-features/">Language Features</Link>
            </td>
            <td>Types, pattern matching, traits/typeclasses, and macro_rules.</td>
          </tr>
          <tr>
            <td>
              <Link to="/docs/standard-library/">Standard Library</Link>
            </td>
            <td>Built-ins and prelude functions you can use immediately.</td>
          </tr>
          <tr>
            <td>
              <Link to="/docs/examples/">Examples</Link>
            </td>
            <td>Copyable snippets and complete local programs.</td>
          </tr>
          <tr>
            <td>
              <Link to="/docs/grammar/">Grammar</Link>
            </td>
            <td>Practical EBNF-style syntax guide.</td>
          </tr>
          <tr>
            <td>
              <Link to="/docs/how-it-works/">How It Works</Link>
            </td>
            <td>
              Lexer, parser, typechecker, evaluator, and compiler pipeline.
            </td>
          </tr>
          <tr>
            <td>
              <Link to="/docs/why-forge/">Why I Built It</Link>
            </td>
            <td>Project goals, timeline, and design choices.</td>
          </tr>
        </tbody>
      </table>

      <div className="callout">
        <strong>Website design decision:</strong> this documentation is
        static-only. Code examples are intended for local copy/run workflows.
      </div>
    </DocsLayout>
  );
}

export default DocsOverviewPage;
