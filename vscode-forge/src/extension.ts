import * as path from "path";
import * as vscode from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";

let client: LanguageClient;

export function activate(context: vscode.ExtensionContext): void {
  const serverModule = context.asAbsolutePath(path.join("out", "server.js"));
  const serverOptions: ServerOptions = {
    run: { module: serverModule, transport: TransportKind.ipc },
    debug: {
      module: serverModule,
      transport: TransportKind.ipc,
      options: { execArgv: ["--nolazy", "--inspect=6009"] },
    },
  };

  const hoverPath = vscode.workspace
    .getConfiguration("forge")
    .get<string>("forgeHoverPath")
    ?.trim();

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "forge" }],
    initializationOptions:
      hoverPath && hoverPath.length > 0 ? { forgeHoverPath: hoverPath } : {},
  };

  client = new LanguageClient(
    "forgeLanguageServer",
    "Forge Language Server",
    serverOptions,
    clientOptions
  );

  client.start();
  context.subscriptions.push(client);
}

export function deactivate(): Thenable<void> | undefined {
  return client?.stop();
}
