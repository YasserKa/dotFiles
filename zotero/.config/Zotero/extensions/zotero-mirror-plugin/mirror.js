/*
 * mirror.js
 *
 * Event-driven collection mirror builder.
 *
 * Output model:
 *   mirrorRoot/
 *     Collection A/
 *       AuthorYear - Title.pdf
 *     Collection B/
 *       AuthorYear - Title.pdf
 *
 * The mirror is treated as generated output. It is reconciled from a manifest.
 */

/* global Zotero, IOUtils */

(() => {
  const Mirror = {
    // Configure these from prefs or your bridge.
    mirrorRoot: "/home/yasser/Documents/zotero_papers_mirror",
    manifestPath: "/home/yasser/Documents/zotero_papers_mirror/.zotero-mirror-manifest.json",

    debounceMs: 2500,
    running: false,
    timer: null,
    dirty: false,
    pendingCollectionIDs: new Set(),
    pendingItemIDs: new Set(),

    start() {
      this.log("start");
      this.rebuildAll().catch((e) => this.error(e));
    },

    stop() {
      this.log("stop");
      if (this.timer) {
        clearTimeout(this.timer);
        this.timer = null;
      }
    },

    queueChange(type, ids) {
      if (type === "collection") {
        for (const id of ids) this.pendingCollectionIDs.add(Number(id));
      } else {
        for (const id of ids) this.pendingItemIDs.add(Number(id));
      }
      this.log(`queueChange: ${type} [${ids.join(", ")}]`);

      this.dirty = true;
      this.scheduleRefresh(`notify:${type}`);
    },

    scheduleRefresh(reason = "manual") {
      if (this.timer) {
        clearTimeout(this.timer);
      }

      this.timer = setTimeout(() => {
        this.timer = null;
        this.refresh(reason).catch((e) => this.error(e));
      }, this.debounceMs);
    },

    async refresh(reason = "manual") {
      if (this.running) {
        this.dirty = true;
        return;
      }

      if (!this.dirty) {
        return;
      }

      this.running = true;
      this.dirty = false;

      try {
        this.log(`refresh: ${reason}`);

        await this.rebuildAll();
      } catch (e) {
        this.error(e);
      } finally {
        this.running = false;

        // Something changed while rebuildAll() was running
        if (this.dirty) {
          this.scheduleRefresh("coalesced");
        }
      }
    },

    async rebuildAll() {
      const collections = Zotero.Collections.getLoaded();
      const allIDs = collections.map((c) => c.id);

      this.log(`rebuildAll: ${allIDs.length} collections`);

      await this.reconcileCollections(allIDs);
    },

    async computeAffectedCollections(changedCollectionIDs, changedItemIDs) {
      const affected = new Set();

      for (const collectionID of changedCollectionIDs) {
        affected.add(Number(collectionID));
      }

      for (const itemID of changedItemIDs) {
        const item = Zotero.Items.get(Number(itemID));
        if (!item) continue;

        const isAttachment = typeof item.isAttachment === "function"
          ? item.isAttachment()
          : !!item.isAttachment;

        if (typeof item.getCollections === "function") {
          for (const collectionID of item.getCollections()) {
            affected.add(Number(collectionID));
          }
        }

        // If an attachment changed, also include the parent item collections.
        if (isAttachment && typeof item.getSource === "function") {
          const parentID = item.getSource();
          if (parentID) {
            const parent = Zotero.Items.get(Number(parentID));
            if (parent && typeof parent.getCollections === "function") {
              for (const collectionID of parent.getCollections()) {
                affected.add(Number(collectionID));
              }
            }
          }
        }
      }

      return [...affected];
    },
    async reconcileCollections(collectionIDs) {
      const previousManifest = await this.loadManifest();
      const nextManifest = {
        version: 1,
        generatedAt: new Date().toISOString(),
        mirrorRoot: this.mirrorRoot,
        entries: {},
      };

      for (const collectionID of collectionIDs) {
        const collection = Zotero.Collections.get(Number(collectionID));
        if (!collection) continue;

        const collectionPath = this.getCollectionPath(collection);
        const desired = await this.buildDesiredEntriesForCollection(
          collection,
          collectionPath,
        );

        Zotero.debug(`[mirror] applying manifest collection ${collectionPath}`);
        Zotero.debug(
          `[mirror] applying manifest desired ${JSON.stringify(desired, null, 2)}`,
        );
        for (const entry of desired) {
          nextManifest.entries[entry.destPath] = entry;
        }
      }

      await this.applyManifestDiff(previousManifest, nextManifest);
      await this.saveManifest(nextManifest);
    },

    async buildDesiredEntriesForCollection(collection, collectionPath) {
      const desired = [];
      const childItemIDs = typeof collection.getChildItems === "function"
        ? collection.getChildItems(true)
        : [];

      for (const itemID of childItemIDs) {
        const item = Zotero.Items.get(Number(itemID));
        if (!item) continue;

        const isAttachment = typeof item.isAttachment === "function"
          ? item.isAttachment()
          : !!item.isAttachment;

        if (isAttachment) {
          const attachmentFile = await this.resolveAttachmentFile(item);
          if (attachmentFile) {
            desired.push(
              this.makeEntry(collectionPath, item, item, attachmentFile),
            );
          }
          continue;
        }

        const attachmentIDs = typeof item.getAttachments === "function"
          ? item.getAttachments()
          : [];

        for (const attID of attachmentIDs) {
          const attachment = Zotero.Items.get(Number(attID));
          if (!attachment) continue;

          const attachmentIsAttachment = typeof attachment.isAttachment === "function"
            ? attachment.isAttachment()
            : !!attachment.isAttachment;

          if (!attachmentIsAttachment) continue;

          const attachmentFile = await this.resolveAttachmentFile(attachment);
          if (!attachmentFile) continue;

          desired.push(
            this.makeEntry(collectionPath, item, attachment, attachmentFile),
          );
        }
      }

      return desired;
    },

    makeEntry(collectionPath, parentItem, attachmentItem, sourcePath) {
      const displayName = this.makeDisplayName(
        parentItem,
        attachmentItem,
        sourcePath,
      );
      const destPath = this.joinPath(
        this.mirrorRoot,
        collectionPath,
        displayName,
      );

      return {
        destPath,
        sourcePath,
        parentItemID: parentItem.id || null,
        attachmentID: attachmentItem.id || null,
        attachmentKey: attachmentItem.key || null,
        collectionPath,
        displayName,
      };
    },

    async applyManifestDiff(previousManifest, nextManifest) {
      const previous = previousManifest.entries || {};
      const next = nextManifest.entries || {};
      Zotero.debug(
        `[mirror] applying manifest: prev ${JSON.stringify(previous, null, 2)}`,
      );
      Zotero.debug(
        `[mirror] applying manifest: next ${JSON.stringify(next, null, 2)}`,
      );

      // Delete stale destinations.
      for (const destPath of Object.keys(previous)) {
        if (!next[destPath]) {
          await this.removePathIfExists(destPath);
        }
      }

      // Create or update desired destinations.
      for (const [destPath, entry] of Object.entries(next)) {
        Zotero.debug(`[mirror] applying manifest: entry ${entry}`);
        const prev = previous[destPath];
        const needsWrite = !prev
          || prev.sourcePath !== entry.sourcePath
          || prev.displayName !== entry.displayName;

        if (needsWrite) {
          await this.ensureParentDir(destPath);
          Zotero.debug(`[mirror] applying manifest: from ${entry.sourcePath}`);
          Zotero.debug(`[mirror] applying manifest: to ${destPath}`);
          await this.copyFile(entry.sourcePath, destPath);
        }
      }
    },

    async loadManifest() {
      try {
        const text = await this.readText(this.manifestPath);
        return JSON.parse(text);
      } catch (e) {
        return {
          version: 1,
          generatedAt: null,
          mirrorRoot: this.mirrorRoot,
          entries: {},
        };
      }
    },

    async saveManifest(manifest) {
      await this.ensureParentDir(this.manifestPath);
      await this.writeText(
        this.manifestPath,
        JSON.stringify(manifest, null, 2),
      );
    },

    async resolveAttachmentFile(item) {
      const isAttachment = typeof item?.isAttachment === "function"
        ? item.isAttachment()
        : !!item?.isAttachment;

      if (!isAttachment) {
        return null;
      }

      const path = await item.getFilePathAsync();
      return path || null;
    },

    getCollectionPath(collection) {
      const parts = [];
      let current = collection;

      while (current) {
        parts.unshift(this.sanitize(current.name));
        const parentID = current.parentID ?? current.parentCollectionID ?? null;
        current = parentID ? Zotero.Collections.get(Number(parentID)) : null;
      }

      return parts.join("/");
    },

    makeDisplayName(parentItem, attachmentItem, sourcePath) {
      const title = (typeof parentItem.getField === "function"
        && parentItem.getField("title"))
        || parentItem.title
        || "Untitled";

      const dateValue = (typeof parentItem.getField === "function"
        && parentItem.getField("date"))
        || parentItem.date
        || "";

      const base = [
        this.sanitize(title),
        this.sanitize(this.shortYear(dateValue)),
      ]
        .filter(Boolean)
        .join(" - ");

      const ext = this.extensionOf(sourcePath);
      return base ? `${base}${ext}` : this.basename(sourcePath);
    },

    shortYear(dateValue) {
      const m = String(dateValue || "").match(/\b(19|20)\d{2}\b/);
      return m ? m[0] : "";
    },

    basename(path) {
      const s = String(path || "");
      const idx = Math.max(s.lastIndexOf("/"), s.lastIndexOf("\\"));
      return idx >= 0 ? s.slice(idx + 1) : s;
    },

    extensionOf(path) {
      const name = this.basename(path);
      const idx = name.lastIndexOf(".");
      return idx > 0 ? name.slice(idx) : "";
    },

    sanitize(value) {
      return String(value || "")
        .replace(/[<>:"/\\|?*\u0000-\u001F]/g, "_")
        .replace(/\s+/g, " ")
        .trim();
    },

    joinPath(...parts) {
      return parts
        .map((p) => String(p).replace(/[\\/]+$/g, ""))
        .filter(Boolean)
        .join("/");
    },

    async ensureParentDir(path) {
      const parent = this.dirname(path);
      if (!parent) return;
      await this.ensureDir(parent);
    },

    dirname(path) {
      const s = String(path || "");
      const idx = Math.max(s.lastIndexOf("/"), s.lastIndexOf("\\"));
      return idx >= 0 ? s.slice(0, idx) : "";
    },

    async ensureDir(dirPath) {
      if (globalThis.IOUtils && typeof IOUtils.makeDirectory === "function") {
        await IOUtils.makeDirectory(dirPath, { createAncestors: true });
        return;
      }
      if (
        Zotero.File
        && typeof Zotero.File.createDirectoryIfMissingAsync === "function"
      ) {
        await Zotero.File.createDirectoryIfMissingAsync(dirPath);
        return;
      }
      throw new Error(`No directory creation API available for ${dirPath}`);
    },

    async writeText(path, text) {
      if (globalThis.IOUtils && typeof IOUtils.writeUTF8 === "function") {
        await IOUtils.writeUTF8(path, text);
        return;
      }
      if (Zotero.File && typeof Zotero.File.putContentsAsync === "function") {
        await Zotero.File.putContentsAsync(path, text);
        return;
      }
      throw new Error(`No text write API available for ${path}`);
    },

    async readText(path) {
      if (globalThis.IOUtils && typeof IOUtils.readUTF8 === "function") {
        return await IOUtils.readUTF8(path);
      }
      if (Zotero.File && typeof Zotero.File.getContentsAsync === "function") {
        return await Zotero.File.getContentsAsync(path);
      }
      throw new Error(`No text read API available for ${path}`);
    },

    async removePathIfExists(path) {
      try {
        if (globalThis.IOUtils && typeof IOUtils.remove === "function") {
          await IOUtils.remove(path, { recursive: false });
          return;
        }
        if (Zotero.File && typeof Zotero.File.remove === "function") {
          await Zotero.File.remove(path);
          return;
        }
      } catch (e) {
        // Ignore missing files and continue.
        return;
      }
    },

    async copyFile(src, dst) {
      Zotero.debug("[mirror] copyfile");
      if (Zotero.File && typeof Zotero.File.copyFile === "function") {
        await Zotero.File.copyFile(src, dst);
        return;
      }
      if (globalThis.IOUtils && typeof IOUtils.copy === "function") {
        await IOUtils.copy(src, dst);
        return;
      }
      throw new Error(`No file copy API available for ${src} -> ${dst}`);
    },

    log(msg) {
      Zotero.debug(`[ZoteroMirror] ${msg}`);
    },

    error(e) {
      Zotero.logError(e);
    },
  };

  globalThis.ZoteroMirror = Mirror;
})();
