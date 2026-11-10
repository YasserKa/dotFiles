;;; org-backlinks.el --- ID/CUSTOM_ID backlinks via universal-sidecar and consult -*- lexical-binding: t; -*-

;; Requires: Emacs 28+, Org, consult, universal-sidecar.

;;; Commentary:
;;
;; Finds backlinks to an Org node and to the headings it owns:
;;   - [[id:UUID]] and [[id:UUID::search]]
;;   - [[file:x.org]] / [[./x.org]]            (the file-level node)
;;   - [[file:x.org::#custom-id]]              (heading with CUSTOM_ID)
;;   - [[#custom-id]]                          (same-file internal link)
;;
;; "Node" follows the org-roam convention: the closest heading (self or
;; ancestor) with an :ID:, or the file-level node.  A node owns its own ID
;; and the CUSTOM_IDs of headings inside it that do not have their own ID.
;;
;; Keys (Org buffers):
;;   C-c b      Sidecar: backlinks for the node at point (pinned)
;;   C-c B      Sidecar: backlinks for all nodes in the file
;;   C-c n b    Consult: node at point; with C-u, all nodes in the file
;;
;; Setup:
;;   (setq my/org-bl-directories '("~/org/"))
;;   (add-to-list 'universal-sidecar-sections 'org-custom-id-backlinks-section)

;;; Code:

(require 'org)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'consult)
(require 'universal-sidecar)

;;;; Customization ----------------------------------------------------------

(defgroup my/org-bl nil
  "Org backlinks for ID and CUSTOM_ID targets."
  :group 'org)

(defcustom my/org-bl-directories nil
  "Directories searched for backlinks.  Defaults to `org-directory'."
  :type '(repeat directory)
  :group 'my/org-bl)

(defface my/org-bl-node
  '((t :inherit font-lock-function-name-face :weight bold))
  "Referenced node (sidecar group header)."
  :group 'my/org-bl)

(defface my/org-bl-count
  '((t :inherit shadow))
  "Reference count next to a node."
  :group 'my/org-bl)

(defface my/org-bl-location
  '((t :inherit font-lock-comment-face))
  "file:line of a reference."
  :group 'my/org-bl)

(defface my/org-bl-source-heading
  '((t :inherit font-lock-keyword-face))
  "Heading containing the reference."
  :group 'my/org-bl)

(defface my/org-bl-reference
  '((t :inherit org-link :weight bold))
  "The link that references the node."
  :group 'my/org-bl)

(defface my/org-bl-other-link
  '((t :inherit org-link :weight normal))
  "Other links on the same line as a reference."
  :group 'my/org-bl)

;;;; State ------------------------------------------------------------------

(defcustom my/org-bl-scope 'file
  "What the sidecar shows, for all Org buffers.
`node': backlinks to the node at point (follows point, see
`my/org-bl--maybe-refresh').  `file': backlinks to all nodes in the file.
Set by `my/org-backlinks-node' and `my/org-backlinks-file'."
  :type '(choice (const :tag "Node at point" node)
                 (const :tag "All nodes in file" file))
  :group 'my/org-bl)

(defvar my/org-bl--index (make-hash-table :test 'equal)
  "File -> (STAMP . ENTRIES), the cached link index.")

(defconst my/org-bl--index-version 2
  "Bump when the shape of index entries changes.")

;;;; Indexing ---------------------------------------------------------------

(defun my/org-bl--classify (link dir)
  "Classify the Org LINK path string.  DIR resolves relative file paths.
Return a plist starting with :kind (id, internal or file), or nil."
  (cond
   ((string-match "\\`id:\\([^:]+\\)\\(?:::.*\\)?\\'" link)
    (list :kind 'id :key (match-string 1 link)))
   ((string-match "\\`#\\(.+\\)\\'" link)
    (list :kind 'internal :cid (match-string 1 link)))
   ((and (or (string-prefix-p "file:" link)
             (not (string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]+:" link)))
         (string-match "\\`\\(?:file:\\)?\\(.+?\\.org\\)\\(?:::\\(.*\\)\\)?\\'" link))
    (let ((path (match-string 1 link))
          (search (match-string 2 link)))
      (list :kind 'file
            :file (file-truename (expand-file-name path dir))
            :search search
            :cid (and search (string-prefix-p "#" search) (substring search 1)))))))

(defun my/org-bl--scan-buffer (dir)
  "Index all links in the current buffer.  DIR is the file's directory."
  (let (entries)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward "\\[\\[\\([^]\n]+\\)\\]" nil t)
          (let* ((pos (match-beginning 0))
                 (link (match-string-no-properties 1))
                 (info (my/org-bl--classify link dir)))
            (when info
              (let* ((lb (line-beginning-position))
                     (line (buffer-substring-no-properties lb (line-end-position)))
                     (lead (- (length line) (length (string-trim-left line))))
                     (raw (string-trim line)))
                (push (append
                       info
                       (list :pos pos
                             :line (line-number-at-pos pos)
                             :raw raw
                             :col (- pos lb lead)
                             :heading (save-excursion
                                        (goto-char pos)
                                        (when (re-search-backward "^\\*+ +\\(.*\\)$" nil t)
                                          (match-string-no-properties 1)))))
                      entries)))
            (end-of-line)))))
    (nreverse entries)))

(defun my/org-bl--entries (file)
  "Return the (cached) link entries of FILE."
  (let* ((buf (find-buffer-visiting file))
         (stamp (cons my/org-bl--index-version
                      (if buf
                          (with-current-buffer buf (buffer-chars-modified-tick))
                        (file-attribute-modification-time (file-attributes file)))))
         (cached (gethash file my/org-bl--index)))
    (if (and cached (equal (car cached) stamp))
        (cdr cached)
      (let* ((dir (file-name-directory file))
             (entries (if buf
                          (with-current-buffer buf (my/org-bl--scan-buffer dir))
                        (with-temp-buffer
                          (insert-file-contents file)
                          (my/org-bl--scan-buffer dir)))))
        (puthash file (cons stamp entries) my/org-bl--index)
        entries))))

(defun my/org-bl--files (self)
  "Return all Org files to search, always including SELF."
  (let ((dirs (or my/org-bl-directories (list org-directory)))
        files)
    (dolist (d dirs)
      (when (file-directory-p d)
        (dolist (f (directory-files-recursively d "\\.org\\'"))
          (unless (string-match-p "\\(?:\\`\\|/\\)\\." (file-relative-name f d))
            (push (file-truename f) files)))))
    (push self files)
    (delete-dups files)))

;;;; Targets ----------------------------------------------------------------

(defun my/org-bl--owner (pos)
  "Return the start of the node owning POS, or nil for the file-level node.
The owner is the closest heading (self or ancestor) with an ID property."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char pos)
      (unless (org-before-first-heading-p)
        (org-back-to-heading t)
        (catch 'found
          (while t
            (when (org-entry-get nil "ID") (throw 'found (point)))
            (unless (org-up-heading-safe) (throw 'found nil))))))))

(defun my/org-bl--file-title ()
  "Title of the current Org file: #+TITLE or the file name."
  (or (cadr (assoc "TITLE" (org-collect-keywords '("TITLE"))))
      (file-name-nondirectory (or (buffer-file-name) "file"))))

(defun my/org-bl--targets (pin)
  "Collect link targets in the current buffer.
PIN nil means every ID/CUSTOM_ID in the file.  Otherwise PIN is a position and
only the node owning it counts: its own ID and the CUSTOM_IDs of headings that
belong to it (not to nested nodes with their own ID).
Return (:ids ALIST :cids ALIST :whole BOOL :title STRING :filenode NODE).
ALISTs map ID/CUSTOM_ID to a NODE, which is (POS . LABEL); the file-level
node has POS 0 and the file title as LABEL."
  (let (ids cids whole beg end owner title filetitle)
    (save-excursion
      (save-restriction
        (widen)
        (setq filetitle (my/org-bl--file-title)
              owner (and pin (my/org-bl--owner pin)))
        (cond
         ((and pin owner)
          (goto-char owner)
          (setq beg owner
                end (progn (org-end-of-subtree t) (point))
                whole nil
                title (org-get-heading t t t t)))
         (t
          (setq beg (point-min) end (point-max) whole t
                title filetitle)))
        (goto-char beg)
        (let ((case-fold-search t))
          (while (re-search-forward
                  "^[ \t]*:\\(ID\\|CUSTOM_ID\\):[ \t]+\\(.+?\\)[ \t]*$" end t)
            (let* ((mb (match-beginning 0))
                   (kind (upcase (match-string-no-properties 1)))
                   (val (match-string-no-properties 2)))
              (when (or (null pin) (eql (my/org-bl--owner mb) owner))
                (let ((node (save-excursion
                              (goto-char mb)
                              (if (org-before-first-heading-p)
                                  (cons 0 filetitle)
                                (org-back-to-heading t)
                                (cons (point) (or (org-get-heading t t t t) ""))))))
                  (if (string= kind "ID")
                      (push (cons val node) ids)
                    (push (cons val node) cids)))))))))
    (list :ids ids :cids cids :whole whole :title title
          :filenode (cons 0 filetitle))))

;;;; Collecting -------------------------------------------------------------

(defun my/org-bl--collect (pin)
  "Return (:rows ROWS :title TITLE) for the current buffer.
ROWS is a list of (FILE ENTRY NODE), NODE being (POS . LABEL) of the
referenced node.  PIN is nil or a buffer position."
  (let* ((self (file-truename (buffer-file-name (buffer-base-buffer))))
         (targets (my/org-bl--targets pin))
         (ids (plist-get targets :ids))
         (cids (plist-get targets :cids))
         (whole (plist-get targets :whole))
         (filenode (plist-get targets :filenode))
         rows)
    (dolist (f (my/org-bl--files self))
      (dolist (e (my/org-bl--entries f))
        (let ((node
               (pcase (plist-get e :kind)
                 ('id (cdr (assoc (plist-get e :key) ids)))
                 ('internal (and (string= f self)
                                 (cdr (assoc (plist-get e :cid) cids))))
                 ('file (when (string= (plist-get e :file) self)
                          (if-let* ((cid (plist-get e :cid)))
                              (cdr (assoc cid cids))
                            (and whole filenode)))))))
          (when node
            (push (list f e node) rows)))))
    (list :rows (nreverse rows) :title (plist-get targets :title))))

;;;; Opening files quietly ---------------------------------------------------

(defun my/org-bl-find-file-quietly (file)
  "Return a buffer visiting FILE without echoing anything.
Opening Org files in the background runs mode hooks and file checks that
print messages (\"has auto save data\", \"Indentation setup for shell
type ...\").  While a minibuffer prompt is open, those show up inside the
input line, so they are silenced here: NOWARN suppresses the file warnings
and `inhibit-message' the rest."
  (or (find-buffer-visiting file)
      (let ((inhibit-message t)
            (message-log-max nil))
        (find-file-noselect file t))))

;;;; Rendering helpers ------------------------------------------------------

(defun my/org-bl--visit (file pos)
  "Open FILE in another window at POS and reveal context."
  (find-file-other-window file)
  (goto-char pos)
  (org-reveal)
  (recenter))

(defun my/org-bl--link-label (path desc)
  "Short display label for a link with PATH and optional DESC."
  (cond
   (desc desc)
   ((string-match "\\`id:\\(.\\{8\\}\\)" path)
    (concat "id:" (match-string 1 path) "…"))
   (t (replace-regexp-in-string "\\`file:" "" path))))

(defun my/org-bl--render-text (raw col)
  "Return RAW with Org links shown as their description, fontified.
The link starting at COL gets `my/org-bl-reference'."
  (let ((raw (or raw ""))
        (col (or col -1))
        (out "") (i 0))
    (while (string-match
            "\\[\\[\\([^]\n]+\\)\\]\\(?:\\[\\([^]\n]+\\)\\]\\)?\\]" raw i)
      (let ((ms (match-beginning 0))
            (me (match-end 0))
            (label (my/org-bl--link-label (match-string 1 raw)
                                          (match-string 2 raw))))
        (setq out (concat out
                          (substring raw i ms)
                          (propertize label 'face
                                      (if (= ms col)
                                          'my/org-bl-reference
                                        'my/org-bl-other-link)))
              i me)))
    (truncate-string-to-width (concat out (substring raw i)) 110 nil nil "…")))

;;;; Sidecar ----------------------------------------------------------------

(defvar my/org-bl-link-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") #'my/org-bl-follow)
    (define-key m [mouse-1] #'my/org-bl-follow)
    (define-key m [follow-link] 'mouse-face)
    m)
  "Keymap for clickable rows in the sidecar.")

(defun my/org-bl-follow (&optional event)
  "Visit the location stored on the text at point or clicked via EVENT."
  (interactive (list last-nonmenu-event))
  (let* ((mouse (mouse-event-p event))
         (pos (if mouse (posn-point (event-end event)) (point)))
         (win (and mouse (posn-window (event-end event))))
         (target (get-text-property pos 'my-org-bl-target
                                    (and win (window-buffer win)))))
    (when target
      (my/org-bl--visit (car target) (cdr target)))))

(defun my/org-bl--mirror-faces (string)
  "Copy the `face' property of STRING to `font-lock-face' and return STRING.
Some sidecar buffers run `font-lock-mode', which strips plain `face'
properties.  `font-lock-face' survives that, and plain `face' still works when
font-lock is off, so both are set."
  (let ((i 0) (len (length string)))
    (while (< i len)
      (let ((next (or (next-single-property-change i 'face string) len))
            (face (get-text-property i 'face string)))
        (when face
          (put-text-property i next 'font-lock-face face string))
        (setq i next))))
  string)

(defun my/org-bl--clickable (string file pos help)
  "Make STRING jump to FILE at POS.  Existing faces are kept."
  (add-text-properties
   0 (length string)
   (list 'keymap my/org-bl-link-map
         'mouse-face 'highlight
         'follow-link t
         'help-echo help
         'my-org-bl-target (cons file pos))
   string)
  (my/org-bl--mirror-faces string))

(universal-sidecar-define-section org-custom-id-backlinks-section
    (title)
    (:major-modes org-mode :predicate (buffer-file-name))
  (let* ((pos (and (eq my/org-bl-scope 'node)
                   (if-let* ((w (get-buffer-window buffer)))
                       (window-point w)
                     (with-current-buffer buffer (point)))))
         (res (with-current-buffer buffer (my/org-bl--collect pos)))
         (rows (plist-get res :rows))
         (src (with-current-buffer buffer
                (buffer-file-name (buffer-base-buffer)))))
    (when rows
      (universal-sidecar-insert-section org-custom-id-backlinks-section
          (format "%s (%d)"
                  (or title
                      (if pos
                          (format "Backlinks: %s" (plist-get res :title))
                        "Backlinks: all nodes"))
                  (length rows))
        (dolist (group (sort (seq-group-by (lambda (r) (car (nth 2 r))) rows)
                             (lambda (a b) (< (car a) (car b)))))
          (let ((node-pos (car group))
                (label (cdr (nth 2 (cadr group)))))
            ;; Referenced node
            (insert (my/org-bl--clickable
                     (propertize label 'face 'my/org-bl-node)
                     src (max 1 node-pos) "Visit this node")
                    (my/org-bl--mirror-faces
                     (propertize (format " (%d)" (length (cdr group)))
                                 'face 'my/org-bl-count))
                    "\n")
            ;; Where it is referenced
            (dolist (row (cdr group))
              (pcase-let* ((`(,file ,e ,_) row))
                (insert "  "
                        (my/org-bl--clickable
                         (concat
                          (propertize (format "%s:%d"
                                              (file-name-nondirectory file)
                                              (plist-get e :line))
                                      'face 'my/org-bl-location)
                          " "
                          (if-let* ((h (plist-get e :heading)))
                              (propertize (format "[%s] " h)
                                          'face 'my/org-bl-source-heading)
                            "")
                          (my/org-bl--render-text (plist-get e :raw)
                                                  (plist-get e :col)))
                         file (plist-get e :pos) "Jump to reference")
                        "\n"))))
          (insert "\n"))))))

(defun my/org-bl--sidecar-visible-p ()
  "Non-nil if a sidecar window is displayed."
  (seq-some (lambda (w)
              (string-match-p "\\*sidecar\\*" (buffer-name (window-buffer w))))
            (window-list)))

(defun my/org-bl--show ()
  "Show the sidecar if hidden, otherwise refresh it."
  (if (my/org-bl--sidecar-visible-p)
      (universal-sidecar-refresh)
    (universal-sidecar-toggle)))

;;;###autoload
(defun my/org-backlinks-node ()
  "Show backlinks for the node at point in the sidecar (all Org buffers).
The node is the closest heading with an ID, or the file if there is none."
  (interactive)
  (unless (derived-mode-p 'org-mode) (user-error "Not in an Org buffer"))
  (setq my/org-bl-scope 'node)
  (my/org-bl--show))

;;;###autoload
(defun my/org-backlinks-file ()
  "Show backlinks for all nodes of the file in the sidecar (all Org buffers)."
  (interactive)
  (unless (derived-mode-p 'org-mode) (user-error "Not in an Org buffer"))
  (setq my/org-bl-scope 'file)
  (my/org-bl--show))

;;;; Follow point ----------------------------------------------------------

(defvar-local my/org-bl--last-owner 'none
  "Owner node of point at the last check, to detect node changes.")

(defun my/org-bl--maybe-refresh ()
  "In `node' scope, refresh the sidecar when point moves to another node."
  (when (and (eq my/org-bl-scope 'node)
             (derived-mode-p 'org-mode)
             (buffer-file-name)
             (not (minibufferp))
             (my/org-bl--sidecar-visible-p))
    (let ((owner (my/org-bl--owner (point))))
      (unless (equal owner my/org-bl--last-owner)
        (setq my/org-bl--last-owner owner)
        (universal-sidecar-refresh)))))

(add-hook 'post-command-hook #'my/org-bl--maybe-refresh)

;;;; Consult ----------------------------------------------------------------

(defun my/org-bl--consult (buf pin)
  "Search backlinks with `consult' for the node owning PIN in BUF.
PIN is a position in BUF; nil means all nodes of the file visited by BUF."
  (let* ((res (with-current-buffer buf (my/org-bl--collect pin)))
         (rows (plist-get res :rows))
         (title (plist-get res :title))
         (bufs (make-hash-table :test 'equal))
         cands)
    (unless rows (user-error "No backlinks found"))
    (pcase-dolist (`(,file ,e ,node) rows)
      (let* ((fbuf (or (gethash file bufs)
                       (puthash file (my/org-bl-find-file-quietly file)
                                bufs)))
             (marker (with-current-buffer fbuf
                       (copy-marker (plist-get e :pos)))))
        (push (my/org-bl--mirror-faces
               (propertize
                (format "%s:%d %s %s%s"
                        (file-name-nondirectory file)
                        (plist-get e :line)
                        (propertize (format "→ %s" (cdr node))
                                    'face 'my/org-bl-node)
                        (if-let* ((h (plist-get e :heading)))
                            (propertize (format "[%s] " h)
                                        'face 'my/org-bl-source-heading)
                          "")
                        (my/org-bl--render-text (plist-get e :raw)
                                                (plist-get e :col)))
                'consult--candidate marker
                'my-file file
                'my-target (cdr node)))
              cands)))
    (consult--jump
     (consult--read
      (nreverse cands)
      :prompt (if pin
                  (format "Backlinks (%s): " title)
                "Backlinks (all nodes): ")
      :require-match t
      :sort nil
      :category 'org-backlink
      :group (lambda (cand transform)
               (if transform
                   cand
                 (get-text-property 0 'my-target cand)))
      :lookup #'consult--lookup-candidate
      :state (consult--jump-state)))))

;;;###autoload
(defun my/org-backlinks (&optional whole-file)
  "Search backlinks with `consult'.
By default, show backlinks to the node at point: the closest heading with an
ID (or the file) plus the CUSTOM_ID headings it owns.  With prefix arg
WHOLE-FILE, show backlinks to all nodes in this file."
  (interactive "P")
  (unless (derived-mode-p 'org-mode) (user-error "Not in an Org buffer"))
  (unless (buffer-file-name (buffer-base-buffer))
    (user-error "Buffer is not visiting a file"))
  (my/org-bl--consult (current-buffer) (unless whole-file (point))))

;;;; Keys -------------------------------------------------------------------

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c b") #'my/org-backlinks-node)
  (define-key org-mode-map (kbd "C-c B") #'my/org-backlinks-file)
  (define-key org-mode-map (kbd "C-c n b") #'my/org-backlinks))

(provide 'org-backlinks)
;;; org-backlinks.el ends here
