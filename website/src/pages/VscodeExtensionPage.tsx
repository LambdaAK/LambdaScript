import { Link } from 'react-router-dom';
import CodeBlock from '../components/CodeBlock';
import DocsLayout from '../components/DocsLayout';

const buildExtension = `cd vscode-forge
npm install
npm run compile`;

const buildHover = `# from the Forge repository root (not vscode-forge)
dune build bin/forge_hover.exe`;

const vscePackage = `# from vscode-forge, after npm run compile (needs a recent Node.js)
npx @vscode/vsce package --allow-missing-repository`;

const installVsixCli = `# example filename — use the .vsix produced in vscode-forge/
code --install-extension forge-0.1.0.vsix`;

function VscodeExtensionPage() {
  return (
    <DocsLayout>
      <div className="page-intro">
        <div className="breadcrumb">
          <Link to="/">Home</Link> / <Link to="/docs/">Docs</Link> /
          VS Code extension
        </div>
        <h1 className="page-title">VS Code &amp; Cursor extension</h1>
        <p className="lead">
          The Forge editor support lives in the{' '}
          <code className="inline-code">vscode-forge/</code> directory of the
          repository. It provides syntax highlighting for{' '}
          <code className="inline-code">.forge</code> files and optional
          type-on-hover via the <code className="inline-code">forge_hover</code>{' '}
          tool from the main project. The extension is not published to the VS
          Marketplace yet—you install it from a local checkout.
        </p>
      </div>

      <h2>What you need</h2>
      <ul>
        <li>
          <strong>VS Code</strong> or <strong>Cursor</strong> (or another VS
          Code–compatible editor).
        </li>
        <li>
          <strong>Node.js</strong> with <code className="inline-code">npm</code>{' '}
          to compile the extension (Node 18+ is a reasonable baseline).
        </li>
        <li>
          A <strong>git clone</strong> of the Forge repository so you have both{' '}
          <code className="inline-code">vscode-forge/</code> and the OCaml
          toolchain project at the repo root.
        </li>
      </ul>

      <h2>1. Build the extension</h2>
      <p>
        From the <strong>Forge repository root</strong>, enter the extension
        package, install JavaScript dependencies, and compile TypeScript to{' '}
        <code className="inline-code">out/</code>:
      </p>
      <CodeBlock code={buildExtension} language="plain" />

      <p>
        Re-run <code className="inline-code">npm run compile</code> whenever you
        pull changes that touch <code className="inline-code">vscode-forge/src/</code>.
      </p>

      <h2>2. Try it in an Extension Development Host (recommended)</h2>
      <p>
        This is the usual workflow while developing or evaluating the extension—you
        do not need a <code className="inline-code">.vsix</code> file.
      </p>
      <ol>
        <li>
          In VS Code or Cursor, use <strong>File → Open Folder…</strong> and open
          the <code className="inline-code">vscode-forge</code> directory only
          (the folder that contains <code className="inline-code">package.json</code>
          ).
        </li>
        <li>
          Open the Run and Debug view and press <strong>F5</strong> (or{' '}
          <strong>Run → Start Debugging</strong>). If the editor asks you to pick an
          environment, choose <strong>VS Code Extension Development</strong> (or
          create a minimal <code className="inline-code">launch.json</code> that
          launches an extension host).
        </li>
        <li>
          A new window titled something like <strong>[Extension Development Host]</strong>{' '}
          opens with this build of the Forge extension enabled.
        </li>
        <li>
          In <em>that</em> window, open your full Forge clone (or any folder
          containing <code className="inline-code">.forge</code> files). Open a{' '}
          <code className="inline-code">.forge</code> file—you should see Forge
          syntax highlighting.
        </li>
      </ol>

      <div className="callout">
        <strong>Tip:</strong> keep two windows: one stays on{' '}
        <code className="inline-code">vscode-forge</code> for rebuilding (
        <code className="inline-code">npm run compile</code>) and hitting F5 again;
        use the Extension Development Host window for editing Forge programs.
      </div>

      <h2>3. Optional: install into your everyday editor profile</h2>
      <p>
        To load the extension without launching from the{' '}
        <code className="inline-code">vscode-forge</code> workspace, package it as
        a <code className="inline-code">.vsix</code> and install that file once.
      </p>
      <CodeBlock code={vscePackage} language="plain" />
      <p>
        If <code className="inline-code">npx @vscode/vsce</code> errors on an older
        Node.js install, upgrade Node or use the F5 workflow above instead.
      </p>
      <p>
        Then in VS Code: <strong>Extensions</strong> sidebar →{' '}
        <strong>⋯</strong> (Views and More Actions) →{' '}
        <strong>Install from VSIX…</strong> → choose the generated{' '}
        <code className="inline-code">.vsix</code> under{' '}
        <code className="inline-code">vscode-forge/</code>.
      </p>
      <p>From a terminal you can also run:</p>
      <CodeBlock code={installVsixCli} language="plain" />
      <p>
        In Cursor, use <strong>Extensions: Install from VSIX…</strong> from the
        Command Palette (<code className="inline-code">Cmd+Shift+P</code> /{' '}
        <code className="inline-code">Ctrl+Shift+P</code>) and pick the same file.
      </p>

      <h2>4. Type on hover (<code className="inline-code">forge_hover</code>)</h2>
      <p>
        Syntax highlighting works as soon as the extension is loaded. In-editor
        <strong> type hover</strong> requires the <code className="inline-code">forge_hover</code>{' '}
        executable from the OCaml project (see also{' '}
        <a
          className="home-docker-link"
          href="https://github.com/LambdaAK/Forge/blob/main/README.md#editor-support-lsp-hover"
          target="_blank"
          rel="noreferrer"
        >
          README: Editor support
        </a>
        ).
      </p>
      <p>From the Forge repository root:</p>
      <CodeBlock code={buildHover} language="plain" />
      <p>
        With a default layout, the extension searches upward from the open{' '}
        <code className="inline-code">.forge</code> file for{' '}
        <code className="inline-code">_build/default/bin/forge_hover</code> (or{' '}
        <code className="inline-code">forge_hover.exe</code> on Windows). If your
        build output lives elsewhere, set either:
      </p>
      <ul>
        <li>
          VS Code setting <code className="inline-code">forge.forgeHoverPath</code>{' '}
          to the <strong>absolute path</strong> of the{' '}
          <code className="inline-code">forge_hover</code> binary, or
        </li>
        <li>
          the environment variable{' '}
          <code className="inline-code">FORGE_HOVER_PATH</code> to that path before
          starting the editor.
        </li>
      </ul>

      <h2>5. Settings reference</h2>
      <table className="info-table">
        <thead>
          <tr>
            <th>Setting</th>
            <th>Meaning</th>
          </tr>
        </thead>
        <tbody>
          <tr>
            <td>
              <code className="inline-code">forge.forgeHoverPath</code>
            </td>
            <td>
              Absolute path to <code className="inline-code">forge_hover</code>.
              Leave empty to auto-discover from the workspace.
            </td>
          </tr>
        </tbody>
      </table>

      <p className="section-lead vscode-ext-footer">
        Back to <Link to="/docs/">documentation overview</Link> or the{' '}
        <Link to="/docs/quickstart/">Quickstart</Link>.
      </p>
    </DocsLayout>
  );
}

export default VscodeExtensionPage;
