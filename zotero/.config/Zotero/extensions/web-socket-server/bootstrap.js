const { Services } = ChromeUtils.import("resource://gre/modules/Services.jsm");

var WebSocketServer = null;

function startup() {
  var WebSocketServer = {
    server: null,
    start: function () {
      try {
        // Create a server socket that listens on port 8080
        let serverSocket = Cc[
          "@mozilla.org/network/server-socket;1"
        ].createInstance(Ci.nsIServerSocket);
        serverSocket.init(8080, 0, -1); // Port 8080, no blocking, allow reuse

        serverSocket.asyncListen({
          onSocketAccepted: function (aSocket, aTransport) {
            // Create a stream reader
            let inputStream = aTransport.openInputStream(0, 0, 0);

            let outputStream = aTransport.openOutputStream(0, 0, 0);

            let inputStreamReader = Cc[
              "@mozilla.org/binaryinputstream;1"
            ].createInstance(Ci.nsIBinaryInputStream);
            inputStreamReader.setInputStream(inputStream);

            let script = "";

            // Create a pump for asynchronous reading
            let pump = Cc[
              "@mozilla.org/network/input-stream-pump;1"
            ].createInstance(Ci.nsIInputStreamPump);

            // Pass null for aBaseOffset to avoid the conversion issue
            pump.init(inputStream, 0, -1, null, null, false);

            var dataListener = {
              onStartRequest: function (aRequest) {
                // Called when reading starts
              },
              onStopRequest: function (aRequest, aContext, aStatusCode) {
                // Called when reading finishes
                if (script) {
                  try {
                    eval(script); // Execute the received JavaScript
                  } catch (e) {
                    Zotero.debug("Error executing script: " + e);
                  }
                }
              },
              onDataAvailable: function (
                aRequest,
                aContext,
                aStream,
                aOffset,
                aCount,
              ) {
                let available = inputStream.available();
                let chunk = inputStreamReader.readBytes(available);
                script += chunk;
                aRequest.cancel(Components.results.NS_BINDING_ABORTED);
              },
            };

            pump.asyncRead(dataListener, null); // Start the read pump
          },
          onStopListening: function (aSocket) {
            // Handle server stopping
            Zotero.debug("WebSocket server stopped.");
          },
        });

        Zotero.debug("WebSocket server started on port 8080");
      } catch (e) {
        Zotero.debug("Error starting WebSocket server: " + e);
      }
    },
    stop: function () {
      if (this.server) {
        this.server.close();
        Zotero.debug("WebSocket server stopped");
      }
    },
  };

  WebSocketServer.start();
}

function shutdown() {
  if (WebSocketServer) {
    WebSocketServer.stop();
  }
}
