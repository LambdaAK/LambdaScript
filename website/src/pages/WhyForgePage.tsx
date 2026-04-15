import { Link } from 'react-router-dom';
import DocsLayout from '../components/DocsLayout';

function WhyForgePage() {
  return (
    <DocsLayout>
      <div className="page-intro">
        <div className="breadcrumb">
          <Link to="/">Home</Link> / <Link to="/docs/">Docs</Link> / Why I
          Built It
        </div>
        <h1 className="page-title">Why I Built Forge</h1>
        <p className="lead">
          A full programming language project started in May 2023 to learn
          language implementation deeply through practice.
        </p>
      </div>

      <h2>Primary Goal</h2>
      <p>
        The motivation was simple: build a real language end-to-end instead of
        only reading about compilers and type systems. That meant implementing
        each stage directly and validating behavior with runnable programs and
        tests.
      </p>

      <h2>Project Timeline</h2>
      <ol className="timeline">
        <li>
          <strong>May 2023:</strong> initial parser/evaluator experiments and
          baseline syntax.
        </li>
        <li>
          <strong>Core language phase:</strong> expression forms, pattern
          matching, recursive definitions, and ADTs.
        </li>
        <li>
          <strong>Type-system phase:</strong> Hindley-Milner inference and
          polymorphic typing.
        </li>
        <li>
          <strong>Trait/typeclass phase:</strong> class declarations, instances,
          and dispatch integration.
        </li>
        <li>
          <strong>Compiler phase:</strong> lowering to Min IR, LLVM emission,
          and native output via Clang.
        </li>
        <li>
          <strong>Macro phase:</strong> Rust-style declarative macro_rules with
          token-tree matching and repetition.
        </li>
      </ol>

      <h2>What This Demonstrates</h2>
      <div className="grid-2">
        <section className="card">
          <h3>Language Design</h3>
          <p>
            Balancing expressiveness and implementation complexity across syntax,
            types, and semantics.
          </p>
        </section>
        <section className="card">
          <h3>Compiler Engineering</h3>
          <p>
            Building practical lowering/IR/codegen stages and connecting them to
            a runtime and toolchain.
          </p>
        </section>
        <section className="card">
          <h3>Tooling and Testing</h3>
          <p>
            Maintaining interpreter/compiler confidence through repeatable suites
            and targeted cases.
          </p>
        </section>
        <section className="card">
          <h3>Product Thinking</h3>
          <p>
            Presenting the system through clear docs, runnable examples, and
            deployable static website content.
          </p>
        </section>
      </div>

      <h2>Current Direction</h2>
      <ul>
        <li>Improve interpreter/compiler feature parity.</li>
        <li>Expand macro ergonomics while keeping error messages practical.</li>
        <li>
          Strengthen docs and examples for onboarding and external evaluation.
        </li>
      </ul>

      <div className="callout">
        <strong>Audience focus:</strong> this page is aimed at recruiters and
        compiler learners who want clear project motivation and execution scope.
      </div>
    </DocsLayout>
  );
}

export default WhyForgePage;
