/* global Zotero, Services */

let observerID = null;
let pluginRootURI = null;

function startup({ id, version, rootURI }, reason) {
  pluginRootURI = rootURI;

  Services.scriptloader.loadSubScript(pluginRootURI + "mirror.js", globalThis);

  if (!globalThis.ZoteroMirror) {
    throw new Error("ZoteroMirror failed to load");
  }

  globalThis.ZoteroMirror.start();

  Zotero.debug("Starting Zotero Mirror");

  observerID = Zotero.Notifier.registerObserver(
    {
      notify(action, type, ids, extraData) {
        if (
          type === "collection"
          || type === "collection-item"
          || type === "item"
          || type === "item-tag"
        ) {
          globalThis.ZoteroMirror.queueChange(type, ids);
        }
      },
    },
    ["collection", "collection-item", "item", "item-tag"],
    "zotero-mirror",
    50,
  );
}

function shutdown(data, reason) {
  if (observerID) {
    Zotero.Notifier.unregisterObserver(observerID);
    observerID = null;
  }

  if (globalThis.ZoteroMirror) {
    globalThis.ZoteroMirror.stop();
  }
}

function install() {}
function uninstall() {}
