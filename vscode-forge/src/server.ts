// The Forge language server.
//
// Every edit (debounced) runs `forge_ide`, which prints the diagnostics,
// symbols and resolved references of the buffer as JSON (see src/ide.ml).
// All requests are answered from the latest analysis of each document.

import { spawn } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import {
  CodeLens,
  CompletionItem,
  CompletionItemKind,
  createConnection,
  Diagnostic,
  DiagnosticSeverity,
  DocumentHighlight,
  DocumentHighlightKind,
  DocumentSymbol,
  FoldingRange,
  Hover,
  InlayHint,
  InlayHintKind,
  InsertTextFormat,
  Location,
  MarkupKind,
  Position,
  ProposedFeatures,
  Range,
  SemanticTokensBuilder,
  SymbolInformation,
  SymbolKind,
  TextDocuments,
  TextDocumentSyncKind,
  TextEdit,
  WorkspaceEdit,
  type InitializeParams,
} from "vscode-languageserver/node";
import { TextDocument } from "vscode-languageserver-textdocument";

const connection = createConnection(ProposedFeatures.all);
const documents = new TextDocuments(TextDocument);

// ---------------------------------------------------------------- analysis

type Loc = { file: string; range: [number, number, number, number] };

type Sym = {
  id: number;
  name: string;
  qname: string;
  kind: string;
  type: string;
  doc: string;
  container: string;
  top: boolean;
  annotated: boolean;
  location?: Loc;
  fullEnd?: [number, number];
  scope?: Loc;
};

type Ref = { location: Loc; target: number; type: string };

type Analysis = {
  diagnostics: { severity: number; message: string; location: Loc }[];
  symbols: Sym[];
  references: Ref[];
  // derived
  byId: Map<number, Sym>;
  file: string;
};

type Settings = { toolchainPath: string; inlayHints: boolean; extensionPath: string };
let settings: Settings = { toolchainPath: "", inlayHints: true, extensionPath: "" };

const analyses = new Map<string, Analysis>(); // uri -> latest analysis
const lastGood = new Map<string, Analysis>(); // uri -> latest analysis that has symbols for the file
const pending = new Map<string, Promise<Analysis | null>>();
const timers = new Map<string, NodeJS.Timeout>();
const fileCache = new Map<string, { mtime: number; analysis: Analysis }>(); // workspace files not open
let workspaceRoots: string[] = [];

function uriToPath(uri: string): string | null {
  try {
    return uri.startsWith("file:") ? fileURLToPath(uri) : null;
  } catch {
    return null;
  }
}

function pathToUri(p: string): string {
  return pathToFileURL(p).toString();
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

/** A Forge tool: the configured toolchain directory, else the `_build` of a
 *  Forge checkout above the file, else the copy bundled with the extension. */
export function findTool(name: string, nearFile: string | null): string | null {
  const names = [name + ".exe", name];
  const dirs: string[] = [];
  if (settings.toolchainPath) dirs.push(settings.toolchainPath);
  if (nearFile) {
    let dir = path.dirname(nearFile);
    for (let i = 0; i < 40; i++) {
      dirs.push(path.join(dir, "_build", "default", "bin"));
      const parent = path.dirname(dir);
      if (parent === dir) break;
      dir = parent;
    }
  }
  if (settings.extensionPath) dirs.push(path.join(settings.extensionPath, "server", "bin"));
  for (const d of dirs) for (const n of names) if (isExecutable(path.join(d, n))) return path.join(d, n);
  return null;
}

function runIde(filePath: string, text: string): Promise<Analysis | null> {
  const exe = findTool("forge_ide", filePath);
  if (!exe) {
    connection.console.error("[forge] forge_ide not found: build it with `dune build` or set forge.toolchainPath");
    return Promise.resolve(null);
  }
  return new Promise((resolve) => {
    const child = spawn(exe, [filePath], { cwd: path.dirname(filePath), stdio: ["pipe", "pipe", "pipe"] });
    let out = "";
    let err = "";
    const timer = setTimeout(() => child.kill("SIGKILL"), 15000);
    child.stdout.on("data", (d: Buffer) => (out += d.toString("utf8")));
    child.stderr.on("data", (d: Buffer) => (err += d.toString("utf8")));
    child.on("error", (e) => {
      clearTimeout(timer);
      connection.console.error(`[forge] cannot run ${exe}: ${String(e)}`);
      resolve(null);
    });
    child.on("close", () => {
      clearTimeout(timer);
      try {
        const raw = JSON.parse(out);
        const byId = new Map<number, Sym>();
        for (const s of raw.symbols as Sym[]) byId.set(s.id, s);
        resolve({ ...raw, byId, file: filePath });
      } catch {
        if (err.trim()) connection.console.error(`[forge] forge_ide: ${err.trim()}`);
        resolve(null);
      }
    });
    child.stdin.end(text, "utf8");
  });
}

function analyzeNow(doc: TextDocument): Promise<Analysis | null> {
  const p = uriToPath(doc.uri) ?? path.join(workspaceRoots[0] ?? process.cwd(), "untitled.forge");
  const version = doc.version;
  const job = runIde(p, doc.getText()).then((a) => {
    if (a && documents.get(doc.uri)?.version === version) {
      analyses.set(doc.uri, a);
      if (a.symbols.some((s) => s.location?.file === a.file)) lastGood.set(doc.uri, a);
      publishDiagnostics(doc.uri, a);
    }
    if (pending.get(doc.uri) === job) pending.delete(doc.uri);
    return a;
  });
  pending.set(doc.uri, job);
  return job;
}

function schedule(doc: TextDocument, delay = 250) {
  const t = timers.get(doc.uri);
  if (t) clearTimeout(t);
  timers.set(
    doc.uri,
    setTimeout(() => {
      timers.delete(doc.uri);
      void analyzeNow(doc);
    }, delay)
  );
}

/** The latest analysis of an open document (waiting for one in flight). */
async function analysisFor(uri: string): Promise<Analysis | null> {
  const t = timers.get(uri);
  const doc = documents.get(uri);
  if (t && doc) {
    clearTimeout(t);
    timers.delete(uri);
    await analyzeNow(doc);
  } else if (pending.has(uri)) {
    await pending.get(uri);
  }
  return analyses.get(uri) ?? null;
}

/** Analyses of every .forge file in the workspace (open ones from memory). */
async function workspaceAnalyses(): Promise<Analysis[]> {
  const out: Analysis[] = [];
  const seen = new Set<string>();
  for (const doc of documents.all()) {
    const a = await analysisFor(doc.uri);
    if (a) out.push(a);
    const p = uriToPath(doc.uri);
    if (p) seen.add(p);
  }
  const files: string[] = [];
  const walk = (dir: string, depth: number) => {
    if (depth > 6 || files.length > 400) return;
    let entries: fs.Dirent[] = [];
    try {
      entries = fs.readdirSync(dir, { withFileTypes: true });
    } catch {
      return;
    }
    for (const e of entries) {
      if (e.name.startsWith(".") || e.name === "node_modules" || e.name === "_build") continue;
      const p = path.join(dir, e.name);
      if (e.isDirectory()) walk(p, depth + 1);
      else if (e.name.endsWith(".forge")) files.push(p);
    }
  };
  for (const r of workspaceRoots) walk(r, 0);
  for (const f of files) {
    if (seen.has(f)) continue;
    try {
      const mtime = fs.statSync(f).mtimeMs;
      const cached = fileCache.get(f);
      if (cached && cached.mtime === mtime) {
        out.push(cached.analysis);
        continue;
      }
      const a = await runIde(f, fs.readFileSync(f, "utf8"));
      if (a) {
        fileCache.set(f, { mtime, analysis: a });
        out.push(a);
      }
    } catch {
      /* unreadable file */
    }
  }
  return out;
}

// ----------------------------------------------------------------- helpers

function toRange(l: Loc): Range {
  return Range.create(l.range[0], l.range[1], l.range[2], l.range[3]);
}

function contains(l: Loc, p: Position): boolean {
  const [sl, sc, el, ec] = l.range;
  if (p.line < sl || p.line > el) return false;
  if (p.line === sl && p.character < sc) return false;
  if (p.line === el && p.character > ec) return false;
  return true;
}

function samePlace(a?: Loc, b?: Loc): boolean {
  return !!a && !!b && a.file === b.file && a.range.join() === b.range.join();
}

/** The symbol named at a position: a reference, or a definition site. */
function symbolAt(a: Analysis, p: Position): { sym: Sym; ref?: Ref } | null {
  for (const r of a.references) {
    if (r.location.file === a.file && contains(r.location, p)) {
      const sym = a.byId.get(r.target);
      if (sym) return { sym, ref: r };
    }
  }
  for (const s of a.symbols) if (s.location && s.location.file === a.file && contains(s.location, p)) return { sym: s };
  return null;
}

const kindLabel: Record<string, string> = {
  function: "function",
  value: "value",
  variable: "variable",
  parameter: "parameter",
  type: "type",
  constructor: "constructor",
  trait: "trait",
  method: "trait method",
  macro: "macro",
  module: "module",
  builtin: "built-in",
};

function symbolKind(k: string): SymbolKind {
  switch (k) {
    case "function":
    case "builtin":
      return SymbolKind.Function;
    case "value":
      return SymbolKind.Constant;
    case "variable":
      return SymbolKind.Variable;
    case "parameter":
      return SymbolKind.Variable;
    case "type":
      return SymbolKind.Enum;
    case "constructor":
      return SymbolKind.EnumMember;
    case "trait":
      return SymbolKind.Interface;
    case "method":
      return SymbolKind.Method;
    case "macro":
      return SymbolKind.Function;
    case "module":
      return SymbolKind.Module;
    default:
      return SymbolKind.Variable;
  }
}

function completionKind(k: string): CompletionItemKind {
  switch (k) {
    case "function":
    case "builtin":
      return CompletionItemKind.Function;
    case "value":
      return CompletionItemKind.Constant;
    case "variable":
    case "parameter":
      return CompletionItemKind.Variable;
    case "type":
      return CompletionItemKind.Enum;
    case "constructor":
      return CompletionItemKind.EnumMember;
    case "trait":
      return CompletionItemKind.Interface;
    case "method":
      return CompletionItemKind.Method;
    case "macro":
      return CompletionItemKind.Snippet;
    case "module":
      return CompletionItemKind.Module;
    default:
      return CompletionItemKind.Text;
  }
}

const hidden = (s: Sym) => s.qname.startsWith("__") || s.name.startsWith("__");

// --------------------------------------------------------------- lifecycle

const tokenTypes = ["namespace", "type", "interface", "enumMember", "function", "method", "macro", "variable", "parameter"];
const tokenModifiers = ["declaration", "readonly", "defaultLibrary"];

connection.onInitialize((params: InitializeParams) => {
  const opts = (params.initializationOptions ?? {}) as Partial<Settings>;
  settings = {
    toolchainPath: opts.toolchainPath ?? "",
    inlayHints: opts.inlayHints ?? true,
    extensionPath: opts.extensionPath ?? "",
  };
  workspaceRoots = (params.workspaceFolders ?? []).map((f) => uriToPath(f.uri)).filter((p): p is string => !!p);
  return {
    capabilities: {
      textDocumentSync: TextDocumentSyncKind.Incremental,
      hoverProvider: true,
      definitionProvider: true,
      typeDefinitionProvider: true,
      referencesProvider: true,
      documentHighlightProvider: true,
      renameProvider: { prepareProvider: true },
      documentSymbolProvider: true,
      workspaceSymbolProvider: true,
      completionProvider: { triggerCharacters: ["."], resolveProvider: false },
      inlayHintProvider: true,
      foldingRangeProvider: true,
      codeLensProvider: { resolveProvider: false },
      semanticTokensProvider: { legend: { tokenTypes, tokenModifiers }, full: true },
    },
  };
});

connection.onNotification("forge/settings", (s: Partial<Settings>) => {
  settings = { ...settings, ...s };
  for (const d of documents.all()) schedule(d, 0);
  connection.languages.inlayHint.refresh();
});

documents.onDidOpen((e) => schedule(e.document, 0));
documents.onDidChangeContent((e) => schedule(e.document));
documents.onDidSave((e) => {
  fileCache.clear();
  schedule(e.document, 0);
});
documents.onDidClose((e) => {
  analyses.delete(e.document.uri);
  lastGood.delete(e.document.uri);
  connection.sendDiagnostics({ uri: e.document.uri, diagnostics: [] });
});

function publishDiagnostics(uri: string, a: Analysis) {
  const diagnostics: Diagnostic[] = a.diagnostics
    .filter((d) => d.location.file === a.file)
    .map((d) => ({
      range: toRange(d.location),
      severity: d.severity === 1 ? DiagnosticSeverity.Error : DiagnosticSeverity.Warning,
      message: d.message,
      source: "forge",
    }));
  connection.sendDiagnostics({ uri, diagnostics });
}

// ------------------------------------------------------------------- hover

connection.onHover(async (params): Promise<Hover | null> => {
  const a = await analysisFor(params.textDocument.uri);
  if (!a) return null;
  const hit = symbolAt(a, params.position);
  if (!hit) return null;
  const { sym, ref } = hit;
  const label = kindLabel[sym.kind] ?? sym.kind;
  const type = ref && ref.type ? ref.type : sym.type;
  let code: string;
  if (sym.kind === "type") code = type || `type ${sym.name}`;
  else if (sym.kind === "trait") code = `trait ${sym.name}`;
  else if (sym.kind === "module") code = `mod ${sym.qname}`;
  else if (sym.kind === "macro") code = `macro_rules! ${sym.name}`;
  else code = type ? `${sym.qname} : ${type}` : sym.qname;
  const parts = [`*(${label})*`, "```forge\n" + code + "\n```"];
  if (ref && ref.type && sym.type && ref.type !== sym.type && sym.top) parts.push(`Defined as \`${sym.type}\``);
  if (sym.doc) parts.push(sym.doc);
  if (sym.location && sym.location.file !== a.file) parts.push(`*${path.basename(sym.location.file)}*`);
  return {
    contents: { kind: MarkupKind.Markdown, value: parts.join("\n\n") },
    range: ref ? toRange(ref.location) : sym.location ? toRange(sym.location) : undefined,
  };
});

// -------------------------------------------------- definition, references

connection.onDefinition(async (params): Promise<Location | null> => {
  const a = await analysisFor(params.textDocument.uri);
  const hit = a && symbolAt(a, params.position);
  if (!hit || !hit.sym.location) return null;
  return Location.create(pathToUri(hit.sym.location.file), toRange(hit.sym.location));
});

connection.onTypeDefinition(async (params): Promise<Location | null> => {
  const a = await analysisFor(params.textDocument.uri);
  const hit = a && symbolAt(a, params.position);
  if (!a || !hit) return null;
  const type = hit.ref?.type || hit.sym.type;
  // The first named type in the symbol's type.
  for (const m of type.matchAll(/\b([A-Z][A-Za-z0-9_]*)\b/g)) {
    const t = a.symbols.find((s) => s.kind === "type" && s.name === m[1] && s.location);
    if (t && t.location) return Location.create(pathToUri(t.location.file), toRange(t.location));
  }
  return null;
});

/** Every place (definition and uses) of the symbol at a position, across
 *  the workspace when the symbol is visible outside its file. */
async function occurrences(uri: string, pos: Position, includeDecl: boolean): Promise<Location[]> {
  const a = await analysisFor(uri);
  const hit = a && symbolAt(a, pos);
  if (!a || !hit) return [];
  const target = hit.sym;
  const out: Location[] = [];
  const key = new Set<string>();
  const push = (l: Loc) => {
    const k = l.file + ":" + l.range.join();
    if (!key.has(k)) {
      key.add(k);
      out.push(Location.create(pathToUri(l.file), toRange(l)));
    }
  };
  if (includeDecl && target.location) push(target.location);
  const sources = target.top && target.location ? await workspaceAnalyses() : [a];
  for (const b of sources) {
    for (const r of b.references) {
      const s = b.byId.get(r.target);
      if (!s) continue;
      const same = target.location ? samePlace(s.location, target.location) : s.qname === target.qname && b === a;
      if (same || (b === a && s.id === target.id)) push(r.location);
    }
  }
  return out;
}

connection.onReferences((params) =>
  occurrences(params.textDocument.uri, params.position, params.context.includeDeclaration)
);

connection.onDocumentHighlight(async (params): Promise<DocumentHighlight[]> => {
  const a = await analysisFor(params.textDocument.uri);
  const hit = a && symbolAt(a, params.position);
  if (!a || !hit) return [];
  const out: DocumentHighlight[] = [];
  if (hit.sym.location && hit.sym.location.file === a.file)
    out.push(DocumentHighlight.create(toRange(hit.sym.location), DocumentHighlightKind.Write));
  for (const r of a.references)
    if (r.target === hit.sym.id && r.location.file === a.file)
      out.push(DocumentHighlight.create(toRange(r.location), DocumentHighlightKind.Read));
  return out;
});

// ------------------------------------------------------------------- rename

function renameable(a: Analysis, s: Sym): string | null {
  if (!s.location) return "Built-in names cannot be renamed.";
  const inWorkspace = workspaceRoots.some((r) => s.location!.file.startsWith(r + path.sep)) || s.location.file === a.file;
  if (!inWorkspace || path.basename(s.location.file) === "prelude.forge") return "Names from the prelude cannot be renamed.";
  return null;
}

connection.onPrepareRename(async (params) => {
  const a = await analysisFor(params.textDocument.uri);
  const hit = a && symbolAt(a, params.position);
  if (!a || !hit) return null;
  const why = renameable(a, hit.sym);
  if (why) throw new Error(why);
  const loc = hit.ref ? hit.ref.location : hit.sym.location!;
  return { range: toRange(loc), placeholder: hit.sym.name };
});

connection.onRenameRequest(async (params): Promise<WorkspaceEdit | null> => {
  const newName = params.newName.trim();
  const a = await analysisFor(params.textDocument.uri);
  const hit = a && symbolAt(a, params.position);
  if (!a || !hit) return null;
  const why = renameable(a, hit.sym);
  if (why) throw new Error(why);
  const wantUpper = hit.sym.kind === "type" || hit.sym.kind === "constructor" || hit.sym.kind === "trait" || hit.sym.kind === "module";
  if (!/^[A-Za-z_][A-Za-z0-9_']*$/.test(newName) || /^[A-Z]/.test(newName) !== wantUpper)
    throw new Error(wantUpper ? "The new name must start with a capital letter." : "The new name must start with a lower-case letter or _.");
  const locs = await occurrences(params.textDocument.uri, params.position, true);
  const changes: Record<string, TextEdit[]> = {};
  for (const l of locs) (changes[l.uri] ??= []).push(TextEdit.replace(l.range, newName));
  return { changes };
});

// ------------------------------------------------------------------ symbols

connection.onDocumentSymbol(async (params): Promise<DocumentSymbol[]> => {
  const a = await analysisFor(params.textDocument.uri);
  if (!a) return [];
  const tops = a.symbols.filter((s) => s.top && s.location && s.location.file === a.file && !hidden(s));
  const make = (s: Sym): DocumentSymbol => {
    const sel = toRange(s.location!);
    const end = s.fullEnd ? Position.create(s.fullEnd[0], s.fullEnd[1]) : sel.end;
    return DocumentSymbol.create(s.name, s.type.split("\n")[0], symbolKind(s.kind), Range.create(sel.start, end), sel, []);
  };
  const result: DocumentSymbol[] = [];
  const modules = new Map<string, DocumentSymbol>();
  const typeSyms: { sym: DocumentSymbol; line: number }[] = [];
  const traits: { sym: DocumentSymbol; line: number }[] = [];
  const sorted = [...tops].sort((x, y) => x.location!.range[0] - y.location!.range[0] || x.location!.range[1] - y.location!.range[1]);
  for (const s of sorted) {
    const d = make(s);
    if (s.kind === "module") {
      modules.set(s.qname, d);
      const parent = modules.get(s.container);
      (parent ? parent.children! : result).push(d);
    } else if (s.kind === "constructor" && typeSyms.length) {
      const owner = [...typeSyms].reverse().find((t) => t.line <= s.location!.range[0]);
      (owner ? owner.sym.children! : result).push(d);
    } else if (s.kind === "method" && traits.length) {
      const owner = [...traits].reverse().find((t) => t.line <= s.location!.range[0]);
      (owner ? owner.sym.children! : result).push(d);
    } else {
      const parent = modules.get(s.container);
      (parent ? parent.children! : result).push(d);
      if (s.kind === "type") typeSyms.push({ sym: d, line: s.location!.range[0] });
      if (s.kind === "trait") traits.push({ sym: d, line: s.location!.range[0] });
    }
  }
  return result;
});

connection.onWorkspaceSymbol(async (params): Promise<SymbolInformation[]> => {
  const q = params.query.toLowerCase();
  const out: SymbolInformation[] = [];
  const seen = new Set<string>();
  for (const a of await workspaceAnalyses())
    for (const s of a.symbols) {
      if (!s.top || !s.location || hidden(s) || !s.name.toLowerCase().includes(q)) continue;
      const k = s.location.file + s.location.range.join();
      if (seen.has(k)) continue;
      seen.add(k);
      out.push(SymbolInformation.create(s.name, symbolKind(s.kind), toRange(s.location), pathToUri(s.location.file), s.container || path.basename(s.location.file)));
    }
  return out.slice(0, 500);
});

// --------------------------------------------------------------- completion

const keywords = ["let", "rec", "and", "in", "fn", "if", "then", "else", "case", "do", "type", "of", "trait", "impl", "for", "where", "end", "val", "requires", "mod", "use", "import", "macro_rules", "with", "true", "false"];

connection.onCompletion(async (params): Promise<CompletionItem[]> => {
  const doc = documents.get(params.textDocument.uri);
  const latest = await analysisFor(params.textDocument.uri);
  if (!doc || !latest) return [];
  // Text being typed rarely parses: use the last analysis that did, or else
  // analyse the buffer without the line being edited.
  let a: Analysis | null = latest.symbols.some((s) => s.location?.file === latest.file) ? latest : lastGood.get(doc.uri) ?? null;
  if (!a) {
    const lines = doc.getText().split("\n");
    lines[params.position.line] = "";
    a = await runIde(latest.file, lines.join("\n"));
  }
  if (!a) return [];
  const pos = params.position;
  const linePrefix = doc.getText(Range.create(pos.line, 0, pos.line, pos.character));
  const item = (s: Sym): CompletionItem => ({
    label: s.name,
    kind: completionKind(s.kind),
    detail: s.type ? s.type.split("\n")[0] : kindLabel[s.kind],
    documentation: s.doc ? { kind: MarkupKind.Markdown, value: s.doc } : undefined,
    sortText: (s.top ? "1" : "0") + s.name,
  });
  // Members of a module: `Mod.`
  const qualified = /([A-Z][A-Za-z0-9_]*(?:\.[A-Z][A-Za-z0-9_]*)*)\.([A-Za-z0-9_]*)$/.exec(linePrefix);
  if (qualified) {
    const mod = qualified[1];
    return a.symbols.filter((s) => s.top && s.container === mod && !hidden(s)).map(item);
  }
  const out: CompletionItem[] = [];
  const seen = new Set<string>();
  // Locals visible here.
  for (const s of a.symbols) {
    if (s.top || !s.scope || s.scope.file !== a.file || !contains(s.scope, pos)) continue;
    if (s.location && (s.location.range[0] > pos.line || (s.location.range[0] === pos.line && s.location.range[3] > pos.character))) continue;
    if (seen.has(s.name)) continue;
    seen.add(s.name);
    out.push(item(s));
  }
  // Top-level names: this file, imports, the prelude and the built-ins.
  for (const s of a.symbols) {
    if (!s.top || hidden(s) || seen.has(s.qname)) continue;
    if (s.container && s.location?.file !== a.file) continue; // offered after `Mod.`
    seen.add(s.qname);
    out.push({ ...item(s), label: s.container && s.location?.file === a.file ? s.name : s.qname });
  }
  for (const k of keywords)
    out.push({ label: k, kind: CompletionItemKind.Keyword, sortText: "2" + k });
  out.push({
    label: "case … do",
    kind: CompletionItemKind.Snippet,
    insertText: "case ${1:value} do\n| ${2:pattern} -> ${0:result}",
    insertTextFormat: InsertTextFormat.Snippet,
    sortText: "3case",
  });
  return out;
});

// -------------------------------------------------------------- inlay hints

connection.languages.inlayHint.on(async (params): Promise<InlayHint[]> => {
  if (!settings.inlayHints) return [];
  const a = await analysisFor(params.textDocument.uri);
  if (!a) return [];
  const out: InlayHint[] = [];
  for (const s of a.symbols) {
    if (!s.top || s.annotated || !s.type || !s.location || s.location.file !== a.file) continue;
    if (s.kind !== "function" && s.kind !== "value") continue;
    if (s.name === "()" || s.type === "Unit") continue;
    const [, , el, ec] = s.location.range;
    if (el < params.range.start.line || el > params.range.end.line) continue;
    out.push({ position: Position.create(el, ec), label: `: ${s.type}`, kind: InlayHintKind.Type, paddingLeft: true });
  }
  return out;
});

// ---------------------------------------------------------- folding, lenses

connection.onFoldingRanges(async (params): Promise<FoldingRange[]> => {
  const doc = documents.get(params.textDocument.uri);
  const a = await analysisFor(params.textDocument.uri);
  const out: FoldingRange[] = [];
  if (doc) {
    // Blocks closed by `end` (trait, impl, mod, macro_rules), and comments.
    const lines = doc.getText().split(/\r?\n/);
    lines.forEach((line, i) => {
      const m = /^(\s*)(trait|inter|impl|mod|macro_rules)\b/.exec(line);
      if (m) {
        for (let j = i + 1; j < lines.length; j++)
          if (lines[j].startsWith(m[1] + "end") && /^\s*end\b/.test(lines[j])) {
            if (j > i) out.push(FoldingRange.create(i, j));
            break;
          }
      }
      if (/^\s*\/\*/.test(line) && !line.includes("*/")) {
        for (let j = i + 1; j < lines.length; j++)
          if (lines[j].includes("*/")) {
            out.push(FoldingRange.create(i, j, undefined, undefined, "comment"));
            break;
          }
      }
      if (/^\s*\/\//.test(line) && (i === 0 || !/^\s*\/\//.test(lines[i - 1]))) {
        let j = i;
        while (j + 1 < lines.length && /^\s*\/\//.test(lines[j + 1])) j++;
        if (j > i) out.push(FoldingRange.create(i, j, undefined, undefined, "comment"));
      }
    });
  }
  if (!a) return out;
  for (const s of a.symbols)
    if (s.top && s.fullEnd && s.location && s.location.file === a.file && s.fullEnd[0] > s.location.range[0])
      out.push(FoldingRange.create(s.location.range[0], s.fullEnd[0]));
  return out;
});

connection.onCodeLens(async (params): Promise<CodeLens[]> => {
  const p = uriToPath(params.textDocument.uri);
  if (!p) return [];
  const r = Range.create(0, 0, 0, 0);
  return [
    { range: r, command: { title: "▶ Run", command: "forge.run", arguments: [params.textDocument.uri] } },
    { range: r, command: { title: "Compile & run", command: "forge.compileAndRun", arguments: [params.textDocument.uri] } },
    { range: r, command: { title: "REPL", command: "forge.openRepl", arguments: [params.textDocument.uri] } },
  ];
});

// ---------------------------------------------------------- semantic tokens

connection.languages.semanticTokens.on(async (params) => {
  const a = await analysisFor(params.textDocument.uri);
  const builder = new SemanticTokensBuilder();
  if (!a) return builder.build();
  const typeOf = (k: string): number => {
    switch (k) {
      case "module":
        return 0;
      case "type":
        return 1;
      case "trait":
        return 2;
      case "constructor":
        return 3;
      case "function":
      case "builtin":
        return 4;
      case "method":
        return 5;
      case "macro":
        return 6;
      case "parameter":
        return 8;
      default:
        return 7;
    }
  };
  const toks: [number, number, number, number, number][] = [];
  const add = (l: Loc, s: Sym, decl: boolean) => {
    if (l.file !== a.file || l.range[0] !== l.range[2]) return;
    let mods = 0;
    if (decl) mods |= 1;
    if (s.kind === "value" || s.kind === "variable") mods |= 2;
    if (s.kind === "builtin" || (s.location && path.basename(s.location.file) === "prelude.forge")) mods |= 4;
    // a value whose type is a function reads as a function
    let t = typeOf(s.kind);
    if ((s.kind === "value" || s.kind === "variable") && /->/.test(s.type.split("=>").pop() ?? "")) t = 4;
    toks.push([l.range[0], l.range[1], l.range[3] - l.range[1], t, mods]);
  };
  for (const s of a.symbols) if (s.location) add(s.location, s, true);
  for (const r of a.references) {
    const s = a.byId.get(r.target);
    if (s) add(r.location, s, false);
  }
  toks.sort((x, y) => x[0] - y[0] || x[1] - y[1]);
  let last = "";
  for (const t of toks) {
    const k = t[0] + ":" + t[1];
    if (k === last) continue;
    last = k;
    builder.push(t[0], t[1], t[2], t[3], t[4]);
  }
  return builder.build();
});

documents.listen(connection);
connection.listen();
