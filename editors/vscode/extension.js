const path = require('path');
const vscode = require('vscode');
const { LanguageClient, TransportKind } = require('vscode-languageclient/node');

let client = null;
let serverWatcher = null;
let restartPromise = null;

const settings = () => {
  const config = vscode.workspace.getConfiguration('sqlgg');
  return { command: config.get('serverPath'), args: config.get('serverArgs') };
};

const makeClient = ({ command, args }) => {
  const server = { command, args, transport: TransportKind.stdio };
  const clientOptions = {
    documentSelector: [{ scheme: 'file', language: 'sql' }],
    outputChannelName: 'sqlgg',
  };
  return new LanguageClient('sqlgg', 'sqlgg', { run: server, debug: server }, clientOptions);
};

const watchServer = ({ command }) => {
  serverWatcher?.dispose();
  serverWatcher = null;
  if (!path.isAbsolute(command)) return;
  const pattern = new vscode.RelativePattern(vscode.Uri.file(path.dirname(command)), path.basename(command));
  serverWatcher = vscode.workspace.createFileSystemWatcher(pattern);
  serverWatcher.onDidChange(restart);
  serverWatcher.onDidCreate(restart);
};

const start = async () => {
  const config = settings();
  watchServer(config);
  const nextClient = makeClient(config);
  client = nextClient;
  try {
    await nextClient.start();
  } catch (err) {
    nextClient.outputChannel.appendLine(`could not start "${config.command}": ${err.stack || err}`);
    vscode.window.showErrorMessage(
      `sqlgg: could not start "${config.command}". Set sqlgg.serverPath to the sqlgg-lsp executable. (${err.message})`
    );
  }
};

const deactivate = async () => {
  serverWatcher?.dispose();
  serverWatcher = null;
  const stoppingClient = client;
  client = null;
  if (stoppingClient?.needsStop()) await stoppingClient.stop();
};

const restart = () => {
  restartPromise ??= deactivate().then(start).finally(() => { restartPromise = null; });
  return restartPromise;
};

const activate = (context) => {
  context.subscriptions.push(
    vscode.commands.registerCommand('sqlgg.restartServer', restart),
    vscode.workspace.onDidChangeConfiguration((e) => {
      if (e.affectsConfiguration('sqlgg')) return restart();
    })
  );
  return start();
};

module.exports = { activate, deactivate };
