import { ChildProcessWithoutNullStreams, exec, spawn } from "child_process";
import * as net from "net";
import { type ExtensionContext } from "vscode";
import * as vscode from "vscode";
import { LanguageClient, StreamInfo } from "vscode-languageclient/node";

const debugType = "iolite-debugger";
const debuggerDefaultConfig: vscode.DebugConfiguration = {
  name: "Iolite debugger",
  type: debugType,
  request: "launch",
  sourceFile: "${file}",
  cwd: "${fileDirname}",
};

let client: LanguageClient;
let lspProcess: ChildProcessWithoutNullStreams;

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
    console.debug(
      "createDebugAdapterDescriptor",
      JSON.stringify(session),
      JSON.stringify(executable)
    );
    if (session.configuration["noDebug"]) {
      console.debug("noDebug mode");
    }

    console.debug("Starting dap");
    exec("iolite dap", (err, stdout, stderr) => {
      if (err) {
        console.debug("err", err);
      }
      if (stdout) {
        console.debug("stdout", stdout);
      }
      if (stderr) {
        console.debug("stderr", stderr);
      }
    });
    console.debug("Started dap");

    return new vscode.DebugAdapterNamedPipeServer("3031");
  }
}

const getRandomPort = (min = 1024, max = 65535) => {
  return Math.floor(Math.random() * (max - min + 1)) + min;
};
const sleep = (ms: number) => new Promise((resolve) => setTimeout(resolve, ms));

export async function activate(context: ExtensionContext) {
  console.debug("Iolite extesion activated");

  context.subscriptions.push(
    vscode.commands.registerCommand("iolite.run", (resource: vscode.Uri) => {
      let targetResource = resource;
      if (!targetResource && vscode.window.activeTextEditor) {
        targetResource = vscode.window.activeTextEditor.document.uri;
      }
      if (targetResource) {
        const runProcess = spawn("iolite", ["run", targetResource.fsPath]);
        runProcess.stdout.on("data", (data) => {
          console.debug("stdout", data.toString());
        });
        runProcess.stderr.on("data", (data) => {
          console.debug("stderr", data.toString());
        });
        runProcess.on("close", (code) => {
          console.debug(`child process exited with code ${code}`);
        });
      }
    })
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

  const serverOptions = async (): Promise<StreamInfo> => {
    const port = getRandomPort();

    if (!lspProcess) {
      console.debug("Starting LSP");
      const iolite_lsp = spawn("iolite", ["lsp", "--port", port.toString()]);
      iolite_lsp.stdout.on("data", (data) => {
        console.debug("stdout", data.toString());
      });
      iolite_lsp.stderr.on("data", (data) => {
        console.debug("stderr", data.toString());
      });
      iolite_lsp.on("close", (code) => {
        console.debug(`child process exited with code ${code}`);
      });

      lspProcess = iolite_lsp;
      console.debug("Started LSP");
    }

    let socket: net.Socket;
    let connected = false;
    while (!connected) {
      console.debug("Trying to connect to LSP...");
      socket = net.connect(port, "127.0.0.1");
      socket.on("connect", () => {
        connected = true;
      });
      socket.on("error", (err) => {
        console.error("Error:", err);
      });

      await sleep(250);
    }

    console.debug("Connected to LSP");

    return Promise.resolve({
      writer: socket!,
      reader: socket!,
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

  if (lspProcess) {
    lspProcess.kill();
  }

  return client.stop();
}

export function helloWorld() {
  vscode.window.showInformationMessage("Hello, World!");
}
