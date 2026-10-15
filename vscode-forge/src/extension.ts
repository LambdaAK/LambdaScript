import * as fs from "fs";
import * as path from "path";
import * as vscode from "vscode";
import { LanguageClient, LanguageClientOptions, ServerOptions, TransportKind } from "vscode-languageclient/node";

let client: LanguageClient | undefined;
let extensionPath = "";

function config() {
  const c = vscode.workspace.getConfiguration("forge");
  return {
    toolchainPath: (c.get<string>("toolchainPath") ?? "").trim(),
    inlayHints: c.get<boolean>("inlayHints.enabled") ?? true,
    extensionPath,
  };
}

function isExecutable(p: string): boolean {
  try {
    if (!fs.statSync(p).isFile()) return false;
    if (process.platform !== "win32") fs.accessSync(p, fs.constants.X_OK);
    return true;
  } catch {
    return false;
  }
}

/** Same search as the server: settings, a Forge checkout above the file, the
 *  bundled copy. */
function findTool(name: string, nearFile: string | undefined): string | undefined {
  const dirs: string[] = [];
  const cfg = config();
  if (cfg.toolchainPath) dirs.push(cfg.toolchainPath);
  if (nearFile) {
    let dir = path.dirname(nearFile);
    for (let i = 0; i < 40; i++) {
      dirs.push(path.join(dir, "_build", "default", "bin"));
      const parent = path.dirname(dir);
      if (parent === dir) break;
      dir = parent;
    }
  }
  dirs.push(path.join(extensionPath, "server", "bin"));
  for (const d of dirs) for (const n of [name + ".exe", name]) if (isExecutable(path.join(d, n))) return path.join(d, n);
  return undefined;
}

function quote(s: string): string {
  return process.platform === "win32" ? `"${s}"` : `'${s.replace(/'/g, `'\\''`)}'`;
}

function terminal(): vscode.Terminal {
  const existing = vscode.window.terminals.find((t) => t.name === "Forge");
  return existing ?? vscode.window.createTerminal({ name: "Forge" });
}

async function targetFile(uri?: string | vscode.Uri): Promise<string | undefined> {
  const doc = uri
    ? await vscode.workspace.openTextDocument(typeof uri === "string" ? vscode.Uri.parse(uri) : uri)
    : vscode.window.activeTextEditor?.document;
  if (!doc || doc.languageId !== "forge") {
    vscode.window.showWarningMessage("Open a .forge file first.");
    return undefined;
  }
  if (doc.isDirty) await doc.save();
  return doc.uri.fsPath;
}

function missing(tool: string) {
  vscode.window.showErrorMessage(
    `Forge: cannot find ${tool}. Build Forge (\`dune build\`) or set "forge.toolchainPath" to its _build/default/bin directory.`
  );
}

export function activate(context: vscode.ExtensionContext): void {
  extensionPath = context.extensionPath;
  // Unzipping a .vsix can drop the executable bit of the bundled tools.
  const bundled = path.join(extensionPath, "server", "bin");
  if (process.platform !== "win32" && fs.existsSync(bundled))
    for (const f of fs.readdirSync(bundled))
      try {
        fs.chmodSync(path.join(bundled, f), 0o755);
      } catch {
        /* read-only install: leave it */
      }
  const serverModule = context.asAbsolutePath(path.join("out", "server.js"));
  const serverOptions: ServerOptions = {
    run: { module: serverModule, transport: TransportKind.ipc },
    debug: { module: serverModule, transport: TransportKind.ipc, options: { execArgv: ["--nolazy", "--inspect=6009"] } },
  };
  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "forge" }, { scheme: "untitled", language: "forge" }],
    initializationOptions: config(),
  };
  client = new LanguageClient("forgeLanguageServer", "Forge Language Server", serverOptions, clientOptions);
  void client.start();
  context.subscriptions.push(client);

  context.subscriptions.push(
    vscode.workspace.onDidChangeConfiguration((e) => {
      if (e.affectsConfiguration("forge")) void client?.sendNotification("forge/settings", config());
    }),
    vscode.commands.registerCommand("forge.run", async (uri?: string | vscode.Uri) => {
      const file = await targetFile(uri);
      if (!file) return;
      const exe = findTool("interpreter", file);
      if (!exe) return missing("the interpreter");
      const t = terminal();
      t.show(true);
      t.sendText(`${quote(exe)} ${quote(file)}`);
    }),
    vscode.commands.registerCommand("forge.compileAndRun", async (uri?: string | vscode.Uri) => {
      const file = await targetFile(uri);
      if (!file) return;
      const exe = findTool("compile_forge", file);
      if (!exe) return missing("compile_forge");
      const out = file.replace(/\.forge$/, "") + (process.platform === "win32" ? ".exe" : "");
      const t = terminal();
      t.show(true);
      t.sendText(`${quote(exe)} ${quote(file)} ${quote(out)} && ${quote(out)}`);
    }),
    vscode.commands.registerCommand("forge.openRepl", async (uri?: string | vscode.Uri) => {
      const file = uri || vscode.window.activeTextEditor?.document.languageId === "forge" ? await targetFile(uri) : undefined;
      const exe = findTool("repl", file);
      if (!exe) return missing("the REPL");
      const t = vscode.window.createTerminal({ name: "Forge REPL" });
      t.show();
      t.sendText(file ? `${quote(exe)} ${quote(file)}` : quote(exe));
    }),
    vscode.commands.registerCommand("forge.restartServer", async () => {
      await client?.restart();
    })
  );
}

export function deactivate(): Thenable<void> | undefined {
  return client?.stop();
}
