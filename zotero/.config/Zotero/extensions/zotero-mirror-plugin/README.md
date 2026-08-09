# Zotero mirror skeleton

Files:

- `bootstrap.js` — registers the Zotero notifier and schedules refreshes
- `mirror.js` — computes desired collection output, writes the manifest, and reconciles the folder tree

Generated file:

- `.zotero-mirror-manifest.json` in the mirror root

Expected flow:

1. Zotero collection/item change fires notifier
2. `bootstrap.js` queues a refresh
3. `mirror.js` recomputes changed collections
4. Mirror folder is reconciled
5. Syncthing syncs the mirror to the tablet

You need to wire the file-bridge methods if your Zotero environment does not expose `IOUtils` or `Zotero.File` methods used in `mirror.js`.
