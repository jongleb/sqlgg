The server exits after the LSP shutdown handshake without waiting for stdin to
close:

  $ ../protocol.exe ../../lsp_main.exe shutdown

Closing a document clears its diagnostics and removes its in-memory state:

  $ ../protocol.exe ../../lsp_main.exe close-clears-diagnostics
  $ ../protocol.exe ../../lsp_main.exe close-forgets-document

Diagnostics identify the document version they were computed from:

  $ ../protocol.exe ../../lsp_main.exe change-versions-diagnostics

Hover over the wire, so everything in lsp_main is exercised: position encoding,
the line index cache and the document cache:

  $ ../protocol.exe ../../lsp_main.exe hover-throughput
