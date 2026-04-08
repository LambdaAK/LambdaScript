import { spawn } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import {
  createConnection,
  ProposedFeatures,
  TextDocuments,
  TextDocumentSyncKind,
  type Hover,
  type InitializeParams,
  type Position,
} from "vscode-languageserver/node";
import { TextDocument } from "vscode-languageserver-textdocument";

const connection = createConnection(ProposedFeatures.all);
const documents = new TextDocuments(TextDocument);

let clientForgeHoverPath: string | undefined;

function uriToFsPath(uri: string): string | null {
  try {
    if (uri.startsWith("file:")) {
      return fileURLToPath(uri);
    }
  } catch {
    return null;
  }
  return null;
}

const HOVER_BIN_NAMES = ["forge_hover.exe", "forge_hover"] as const;

function isRunnablePath(p: string): boolean {
  try {
    const st = fs.statSync(p);
    if (!st.isFile()) {
      return false;
    }
    if (process.platform === "win32") {
      return true;
    }
    fs.accessSync(p, fs.constants.X_OK);
    return true;
  } catch {
    return false;
  }
}

function defaultBuildHoverCandidates(startDir: string): string[] {
  const out: string[] = [];
  let dir = startDir;
  for (let i = 0; i < 40; i++) {
    for (const name of HOVER_BIN_NAMES) {
      out.push(path.join(dir, "_build", "default", "bin", name));
    }
    const parent = path.dirname(dir);
    if (parent === dir) {
      break;
    }
    dir = parent;
  }
  return out;
}

function resolveForgeHoverExecutable(docUri: string): string | null {
  const configured = clientForgeHoverPath?.trim();
  if (configured) {
    if (isRunnablePath(configured)) {
      return configured;
    }
    connection.console.warn(
      `[forge] forge.forgeHoverPath is set but not executable: ${configured}`
    );
  }

  const fromEnv = process.env.FORGE_HOVER_PATH?.trim();
  if (fromEnv && isRunnablePath(fromEnv)) {
    return fromEnv;
  }

  const fsPath = uriToFsPath(docUri);
  if (fsPath) {
    for (const c of defaultBuildHoverCandidates(path.dirname(fsPath))) {
      if (isRunnablePath(c)) {
        return c;
      }
    }
  }

  return process.platform === "win32" ? "forge_hover.exe" : "forge_hover";
}

function runForgeHover(
  exe: string,
  srcPathForTool: string,
  line0: number,
  char0: number,
  source: string
): Promise<{ kind: "type" | "definition"; text: string } | null> {
  const args = [srcPathForTool, "1", String(line0), String(char0)];
  return new Promise((resolve) => {
    const child = spawn(exe, args, {
      stdio: ["pipe", "pipe", "pipe"],
    });

    let stdout = "";
    let stderr = "";
    child.stdout?.on("data", (d: Buffer) => {
      stdout += d.toString("utf8");
    });
    child.stderr?.on("data", (d: Buffer) => {
      stderr += d.toString("utf8");
    });
    child.on("error", (err) => {
      connection.console.warn(`[forge] forge_hover spawn: ${String(err)}`);
      resolve(null);
    });
    child.on("close", (code) => {
      const out = stdout;
      const trimmed = out.replace(/\r?\n$/, "");
      if (code !== 0 || trimmed.startsWith("ERROR:")) {
        if (stderr.trim()) {
          connection.console.info(`[forge] forge_hover stderr: ${stderr.trim()}`);
        }
        resolve(null);
        return;
      }
      if (trimmed.startsWith("TYPE\t")) {
        resolve({ kind: "type", text: trimmed.slice("TYPE\t".length) });
        return;
      }
      if (trimmed.startsWith("DEF\t")) {
        resolve({ kind: "definition", text: trimmed.slice("DEF\t".length) });
        return;
      }
      resolve({ kind: "type", text: trimmed });
    });
    const stdin = child.stdin;
    if (!stdin) {
      resolve(null);
      return;
    }
    stdin.end(source, "utf8");
  });
}

connection.onInitialize((params: InitializeParams) => {
  const opts = params.initializationOptions as
    | { forgeHoverPath?: string }
    | undefined;
  if (opts?.forgeHoverPath && typeof opts.forgeHoverPath === "string") {
    clientForgeHoverPath = opts.forgeHoverPath;
  }
  return {
    capabilities: {
      textDocumentSync: TextDocumentSyncKind.Incremental,
      hoverProvider: true,
    },
  };
});

connection.onHover(async (params): Promise<Hover | null> => {
  try {
    const doc = documents.get(params.textDocument.uri);
    if (!doc) {
      return null;
    }

    const exe = resolveForgeHoverExecutable(params.textDocument.uri);
    if (!exe) {
      return null;
    }

    const fsPath = uriToFsPath(params.textDocument.uri);
    const srcPathForTool = fsPath ?? "untitled.ls";

    const hoverInfo = await runForgeHover(
      exe,
      srcPathForTool,
      params.position.line,
      params.position.character,
      doc.getText()
    );
    if (!hoverInfo) {
      return null;
    }

    if (hoverInfo.kind === "definition") {
      const def = hoverInfo.text.replace(/```/g, "\\`\\`\\`");
      return {
        contents: {
          kind: "markdown",
          value: `**Definition:**\n\`\`\`forge\n${def}\n\`\`\``,
        },
      };
    }

    return {
      contents: {
        kind: "markdown",
        value: `**Type:** \`${hoverInfo.text.replace(/`/g, "\\`")}\``,
      },
    };
  } catch (err) {
    connection.console.warn(`[forge] onHover failed: ${String(err)}`);
    return null;
  }
});

documents.listen(connection);
connection.listen();
