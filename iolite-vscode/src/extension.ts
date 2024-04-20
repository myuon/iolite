import * as path from "path";
import * as net from "net";
import { workspace, type ExtensionContext } from "vscode";

import {
  LanguageClient,
  type LanguageClientOptions,
  type ServerOptions,
  TransportKind,
  StreamInfo,
} from "vscode-languageclient/node";

let client: LanguageClient;

export function activate(context: ExtensionContext) {
  const serverOptions = (): Promise<StreamInfo> => {
    let socket = net.connect(3030, "127.0.0.1");

    return Promise.resolve({
      writer: socket,
      reader: socket,
    });
  };

  client = new LanguageClient(
    "iolite",
    "Iolite Language Server",
    serverOptions,
    {
      documentSelector: [{ scheme: "file", language: "iolite" }],
      synchronize: {
        fileEvents: workspace.createFileSystemWatcher("**/.iolite"),
      },
    }
  );

  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
