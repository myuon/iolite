import * as net from "net";
import { type ExtensionContext } from "vscode";
import * as vscode from "vscode";
import { LanguageClient, StreamInfo } from "vscode-languageclient/node";

let client: LanguageClient;

export async function activate(context: ExtensionContext) {
  console.log("Iolite Language Server activated");
  context.subscriptions.push(
    vscode.commands.registerCommand("iolite.helloworld", helloWorld)
  );

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
    }
  );

  await client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}

export function helloWorld() {
  vscode.window.showInformationMessage("Hello, World!");
}
