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
          Everything you need to use and understand the Forge language:
          quickstart, syntax, examples, and implementation internals.
        </p>
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
            <td>Types, pattern matching, traits/typeclasses, and Rust-style macros.</td>
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
        </tbody>
      </table>

    </DocsLayout>
  );
}

export default DocsOverviewPage;
