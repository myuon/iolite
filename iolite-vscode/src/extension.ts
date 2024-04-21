import * as net from "net";
import { type ExtensionContext } from "vscode";
import * as vscode from "vscode";
import { LanguageClient, StreamInfo } from "vscode-languageclient/node";

const debugType = "iolite-debugger";
const debuggerDefaultConfig: vscode.DebugConfiguration = {
  name: "Iolite debugger",
  type: debugType,
  request: "launch",
};

let client: LanguageClient;

class IoliteDebugConfigurationProvider
  implements vscode.DebugConfigurationProvider
{
  provideDebugConfigurations(
    folder: vscode.WorkspaceFolder | undefined,
    token?: vscode.CancellationToken | undefined
  ): vscode.ProviderResult<vscode.DebugConfiguration[]> {
    return [debuggerDefaultConfig];
  }

  resolveDebugConfiguration(
    folder: vscode.WorkspaceFolder | undefined,
    debugConfiguration: vscode.DebugConfiguration,
    token?: vscode.CancellationToken | undefined
  ): vscode.ProviderResult<vscode.DebugConfiguration> {
    return {
      ...debuggerDefaultConfig,
      ...debugConfiguration,
    };
  }
}

class IoliteDebugAdapterDescriptorFactory
  implements vscode.DebugAdapterDescriptorFactory
{
  createDebugAdapterDescriptor(
    session: vscode.DebugSession,
    executable: vscode.DebugAdapterExecutable | undefined
  ): vscode.ProviderResult<vscode.DebugAdapterDescriptor> {
    console.debug("createDebugAdapterDescriptor", session, executable);

    return new vscode.DebugAdapterServer(3031);
  }
}

export async function activate(context: ExtensionContext) {
  console.debug("Iolite extesion activated");

  context.subscriptions.push(
    vscode.commands.registerCommand("iolite.helloworld", helloWorld)
  );
  context.subscriptions.push(
    vscode.debug.registerDebugConfigurationProvider(
      debugType,
      new IoliteDebugConfigurationProvider()
    )
  );
  context.subscriptions.push(
    vscode.debug.registerDebugAdapterDescriptorFactory(
      debugType,
      new IoliteDebugAdapterDescriptorFactory()
    )
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
