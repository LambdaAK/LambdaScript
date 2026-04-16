import { Link } from 'react-router-dom';
import CodeBlock from '../components/CodeBlock';
import DocsLayout from '../components/DocsLayout';

const pipeline = `source text
  -> lexer (tokens)
  -> parser (AST)
  -> macro expansion (AST rewrite)
  -> typechecking + elaboration
  ->
     A) evaluator (interpreter path)
     B) lowering to Min IR -> LLVM IR -> clang -> native binary`;

function HowItWorksPage() {
  return (
    <DocsLayout>
      <div className="page-intro">
        <div className="breadcrumb">
          <Link to="/">Home</Link> / <Link to="/docs/">Docs</Link> /
          How It Works
        </div>
        <h1 className="page-title">How Forge Works</h1>
        <p className="lead">
          Intermediate-depth explanation of the language implementation
          pipeline.
        </p>
      </div>

      <h2>High-Level Pipeline</h2>
      <CodeBlock code={pipeline} language="plain" />

      <h2>Lexer</h2>
      <ul>
        <li>Converts raw source text into token stream.</li>
        <li>
          Recognizes identifiers, literals, operators, keywords, punctuation,
          and macro token-tree delimiters.
        </li>
        <li>
          Tracks positions so downstream tools can report useful diagnostics and
          support hover queries.
        </li>
      </ul>

      <h2>Parser</h2>
      <ul>
        <li>
          Builds AST nodes for expressions, patterns, type syntax, and top-level
          definitions.
        </li>
        <li>
          Supports language forms such as <code className="inline-code">let</code>,{' '}
          <code className="inline-code">let rec</code>,{' '}
          <code className="inline-code">fn</code>,{' '}
          <code className="inline-code">case ... do</code>, records, ADTs, and
          modules.
        </li>
        <li>
          Includes dedicated handling for{' '}
          <code className="inline-code">macro_rules!</code> definitions and
          macro invocation syntax.
        </li>
      </ul>

      <h2>Macro Expansion</h2>
      <ul>
        <li>
          Runs before typechecking so macro output is treated as normal language
          syntax.
        </li>
        <li>
          Matches token-tree patterns, supports fragment kinds, and repetition
          with <code className="inline-code">*</code>/
          <code className="inline-code">+</code>.
        </li>
        <li>
          Expands built-in declarative helpers like{' '}
          <code className="inline-code">count_args!</code> and{' '}
          <code className="inline-code">vec!</code>.
        </li>
      </ul>

      <h2>Typechecker</h2>
      <ul>
        <li>Constraint-based inference in Hindley-Milner style.</li>
        <li>
          Handles polymorphism, ADTs, records, recursive definitions, and
          pattern matching.
        </li>
        <li>
          Supports trait/typeclass constraints and elaborates dictionary-based
          method dispatch.
        </li>
      </ul>

      <h2>Evaluator (Interpreter Path)</h2>
      <ul>
        <li>
          Evaluates elaborated AST directly with lexical environments and
          closures.
        </li>
        <li>Implements built-ins and typeclass method dispatch at runtime.</li>
        <li>Works well for fast iteration and language behavior validation.</li>
      </ul>

      <h2>Compiler (Native Path)</h2>
      <ul>
        <li>
          Lowers elaborated programs into a simplified internal representation
          (Min IR).
        </li>
        <li>Emits LLVM IR from Min IR.</li>
        <li>
          Invokes Clang to produce assembly and linked executables with a
          lightweight C runtime.
        </li>
      </ul>

      <h2>Why Two Execution Paths?</h2>
      <table className="info-table">
        <thead>
          <tr>
            <th>Path</th>
            <th>Strength</th>
            <th>Best Use</th>
          </tr>
        </thead>
        <tbody>
          <tr>
            <td>Interpreter</td>
            <td>Fast feedback during language development</td>
            <td>Trying new syntax/features and validating semantics</td>
          </tr>
          <tr>
            <td>Compiler</td>
            <td>End-to-end native code generation</td>
            <td>
              Demonstrating systems/compiler depth and runnable binaries
            </td>
          </tr>
        </tbody>
      </table>

      <div className="callout">
        <strong>Design tradeoff:</strong> keeping both evaluator and compiler
        makes the codebase larger, but it gives clearer validation for language
        design decisions.
      </div>
    </DocsLayout>
  );
}

export default HowItWorksPage;
