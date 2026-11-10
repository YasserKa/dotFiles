;;; org-link-maint.el --- Maintenance tools for Org links  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Maintenance utilities for links in your Org notes.
;;
;; A "node" is anything with an ID: a file (ID in the top property
;; drawer) or a heading.  Its title is #+TITLE (or the file name) for a
;; file node, the heading text for a heading node.
;;
;;  1. Keep id: link descriptions in sync with node titles.
;;       M-x org-link-maint-update-backlinks     after renaming the node at point
;;                                               (the nearest heading with an ID,
;;                                               else the file node)
;;       M-x org-link-maint-update-descriptions  fix every stale description
;;                                               (asks y/n/!/q; C-u to replace
;;                                               all without asking)
;;
;;  2. org-link-maint-mode (global minor mode):
;;     - broken id: links are shown red with strike-through;
;;     - saving an Org file checks its links and tells you when new
;;       ones broke (instant: only that file is checked);
;;     - after `org-link-maint-background-idle' seconds idle, a full
;;       scan runs in a separate low-priority Emacs, at most every
;;       `org-link-maint-background-interval' seconds.  Emacs never
;;       freezes for it.  M-x org-link-maint-scan-async starts one now.
;;     For scans while Emacs is closed, run `org-link-maint-batch-scan'
;;     from cron or a systemd timer (its docstring has the command).
;;
;;  3. M-x org-link-maint-consult-broken: pick any broken link in all
;;     your Org files with consult, with live preview.  Checks id:,
;;     file:, file+sys: and file+emacs: links (and bare paths such as
;;     [[./x.org]]).  For links into Org files the "::" part is checked
;;     too: ::*Heading, ::#custom-id, ::42 (line) and ::target.
;;     Custom link types: `org-link-maint-command-link-types' asks the
;;     program that opens the link (run as PROGRAM --check TARGET [POS]):
;;       (setq org-link-maint-command-link-types '(("doc" . "open_document")))
;;     `org-link-maint-link-checkers' takes Lisp functions instead.
;;     It opens from the last full scan, rechecking only files changed
;;     since, so it is fast.  C-u forces a full scan first.
;;     "All Org files" = every Org file under `my/notes-dir', Org's ID
;;     database, `org-agenda-files' and every open Org buffer (unsaved
;;     text included).
;;
;; Setup:
;;
;;   (with-eval-after-load 'org
;;     (require 'org-link-maint)
;;     (org-link-maint-mode 1))

;;; Code:

(require 'org)
(require 'org-id)
(require 'org-element)
(require 'cl-lib)
(require 'subr-x)
(require 'seq)

(defgroup org-link-maint nil
  "Maintenance tools for Org links."
  :group 'org-id)

(defvar my/notes-dir)                   ; defined in your config

(defcustom org-link-maint-save-buffers t
  "When non-nil, save buffers whose link descriptions were updated."
  :type 'boolean)

(defface org-link-maint-broken-link
  '((t :inherit org-link :foreground "red" :strike-through t))
  "Face for ID links whose target cannot be found.")

;;;; Files and ID database

(defun org-link-maint--dir-files (dir)
  "Return Org files under DIR, recursively, skipping hidden files and dirs."
  (when (and dir (file-directory-p dir))
    (directory-files-recursively
     (expand-file-name dir)
     "\\`[^.#].*\\.org\\'"              ; skip lock files (.#foo.org)
     nil
     (lambda (d) (not (string-prefix-p "." (file-name-nondirectory d)))))))

(defun org-link-maint-files ()
  "Return every Org file we know about, as absolute file names.
This is the union of:
- files under `my/notes-dir' (recursively),
- every file in Org's ID database,
- `org-agenda-files',
- every file currently open in an Org buffer."
  (unless (hash-table-p org-id-locations)
    (org-id-locations-load))
  (let ((files (append
                (and (boundp 'my/notes-dir)
                     (org-link-maint--dir-files my/notes-dir))
                (copy-sequence org-id-files)
                (and (hash-table-p org-id-locations)
                     (let (fs)
                       (maphash (lambda (_id f) (push f fs)) org-id-locations)
                       fs))
                (ignore-errors (org-agenda-files t))
                (delq nil
                      (mapcar (lambda (b)
                                (with-current-buffer b
                                  (and (derived-mode-p 'org-mode)
                                       buffer-file-name)))
                              (buffer-list))))))
    (cl-remove-if-not
     #'file-exists-p
     (delete-dups (mapcar #'expand-file-name (delq nil files))))))

(defun org-link-maint-refresh (&optional files)
  "Rescan FILES (default: `org-link-maint-files') for IDs and refontify."
  (interactive)
  (org-id-update-id-locations (or files (org-link-maint-files)) t)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'org-mode)
        (font-lock-flush))))
  (when (called-interactively-p 'interactive)
    (message "Org ID locations refreshed (%d IDs)"
             (hash-table-count org-id-locations))))

(defun org-link-maint--id-file (id)
  "Return the file that contains ID according to the database, or nil.
Does not trigger a rescan, so it is cheap enough for font-lock."
  (unless (hash-table-p org-id-locations)
    (org-id-locations-load))
  (let ((file (and (hash-table-p org-id-locations)
                   (gethash id org-id-locations))))
    (and file (file-exists-p file) file)))

(defun org-link-maint--link-id (link)
  "Return the ID in LINK (\"id:XXX\" or \"id:XXX::search\"), or nil."
  (when (string-prefix-p "id:" link)
    (car (split-string (substring link 3) "::"))))

;;;; Titles

(defun org-link-maint--clean (string)
  "Make STRING safe as a link description (no square brackets)."
  (replace-regexp-in-string
   "\\]" "}" (replace-regexp-in-string "\\[" "{" string)))

(defun org-link-maint--title (id)
  "Return the current title of the node with ID, or nil if not found.
For a file-level ID, return #+TITLE (or the file name)."
  (let* ((file (org-link-maint--id-file id))
         (loc (and file (org-id-find-id-in-file id file))))
    (when loc
      (with-current-buffer (find-file-noselect (car loc))
        (org-with-wide-buffer
         (goto-char (cdr loc))
         (org-link-maint--clean
          (if (org-before-first-heading-p)
              (or (cadr (assoc "TITLE" (org-collect-keywords '("TITLE"))))
                  (file-name-base (car loc)))
            (org-link-display-format (org-get-heading t t t t)))))))))

(defun org-link-maint--cached-title (id cache)
  "Return title for ID, memoised in hash table CACHE."
  (let ((hit (gethash id cache 'none)))
    (if (not (eq hit 'none))
        hit
      (puthash id (org-link-maint--title id) cache))))

;;;; Updating descriptions

(defvar org-link-maint--replace-all nil
  "Non-nil once the user answered `!' during a query.")

(defun org-link-maint--ask (old new)
  "Show the current match and ask whether to replace OLD with NEW.
Return ?y, ?n, ?! or ?q."
  (let* ((beg (match-beginning 0))
         (end (match-end 0))
         (ov (make-overlay beg end)))
    (overlay-put ov 'face 'query-replace)
    (save-excursion (goto-char beg) (org-reveal))
    (set-window-point (display-buffer (current-buffer)) beg)
    (unwind-protect
        (read-char-choice
         (format "Replace \"%s\" → \"%s\"? (y/n/!/q) " old new)
         '(?y ?n ?! ?q))
      (delete-overlay ov))))

(defun org-link-maint--update-file (file cache &optional only-id ask)
  "Update stale ID link descriptions in FILE.
CACHE memoises titles.  If ONLY-ID, touch only links to that ID.
If ASK, query before each replacement.
Return (COUNT . QUIT)."
  (with-current-buffer (find-file-noselect file)
    (let ((count 0) (quit nil))
      (org-with-wide-buffer
       (goto-char (point-min))
       (while (and (not quit) (re-search-forward org-link-bracket-re nil t))
         (let* ((link (match-string-no-properties 1))
                (desc (match-string-no-properties 2))
                (id (org-link-maint--link-id link)))
           (when (and id desc (or (null only-id) (equal id only-id)))
             (let ((title (save-match-data
                            (org-link-maint--cached-title id cache))))
               (when (and title (not (equal title desc)))
                 (let ((answer (if (or (not ask) org-link-maint--replace-all)
                                   ?y
                                 (save-match-data
                                   (org-link-maint--ask desc title)))))
                   (when (eq answer ?!)
                     (setq org-link-maint--replace-all t
                           answer ?y))
                   (pcase answer
                     (?y (replace-match title t t nil 2)
                         (cl-incf count))
                     (?q (setq quit t))))))))))
      (when (and (> count 0) org-link-maint-save-buffers)
        (save-buffer))
      (cons count quit))))

(defun org-link-maint--update (files &optional only-id ask)
  "Run `org-link-maint--update-file' over FILES; return total count."
  (let ((cache (make-hash-table :test #'equal))
        (total 0)
        (org-link-maint--replace-all nil))
    (cl-loop for file in files
             for (count . quit) = (org-link-maint--update-file file cache only-id ask)
             do (cl-incf total count)
             until quit)
    total))

;;;###autoload
(defun org-link-maint-update-descriptions (&optional no-query)
  "Update every ID link description that differs from its target's title.
Asks for each one (y = yes, n = no, ! = all remaining, q = quit).
With prefix argument NO-QUERY, replace everything without asking.

Note: this also rewrites descriptions you changed on purpose,
which is why it asks by default."
  (interactive "P")
  (let ((files (org-link-maint-files)))
    (org-link-maint-refresh files)
    (message "Updated %d link description(s)"
             (org-link-maint--update files nil (not no-query)))))

(defun org-link-maint--node-id-at-point ()
  "Return the ID of the node containing point, or nil.
That is the nearest heading (this one or an ancestor) with an ID,
falling back to the file-level ID.  Headings without an ID are skipped."
  (org-with-wide-buffer
   (let (id)
     (unless (org-before-first-heading-p)
       (org-back-to-heading t)
       (while (and (not (setq id (org-entry-get nil "ID")))
                   (org-up-heading-safe))))
     (or id
         (progn (goto-char (point-min))
                (org-entry-get nil "ID"))))))

;;;###autoload
(defun org-link-maint-update-backlinks ()
  "Update descriptions of all links pointing to the node at point.
The node is the nearest heading with an ID, or else the file itself."
  (interactive)
  (let ((id (org-link-maint--node-id-at-point)))
    (unless id (user-error "Point is not inside a node (no ID found)"))
    (when (and (buffer-modified-p) buffer-file-name) (save-buffer))
    (let ((files (org-link-maint-files))
          (title nil))
      (org-link-maint-refresh files)
      (setq title (org-link-maint--title id))
      (message "Updated %d backlink(s) to \"%s\""
               (org-link-maint--update files id) title))))

;;;; Finding broken links (id:, file:, file+sys:, file+emacs:)

(defconst org-link-maint--file-types '("file" "file+sys" "file+emacs")
  "Link types whose path is a file name.")

;;;;; Custom link types

(defcustom org-link-maint-command-link-types nil
  "Custom link types checked by asking an external program.
An alist of (TYPE . PROGRAM).  For a link TYPE:TARGET::POS this runs
  PROGRAM --check TARGET [POS]
from the note's folder (the same split and folder the link uses when
followed).  Exit status 0 means the link is fine, 1 means broken, with
the reason as the first line of output.  Any other status is reported
as a failed check.  Example:
  ((\"doc\" . \"open_document\"))"
  :type '(alist :key-type string :value-type string))

(defcustom org-link-maint-link-checkers nil
  "Your own Lisp checkers for other custom link types.
An alist of (TYPE . FUNCTION).  FUNCTION is called with the link's
path (text after \"TYPE:\") and the file containing the link, and
returns nil when the link is fine or a short reason string when not.
Background scans run in a separate Emacs that only loads this file,
so FUNCTION must be a lambda that uses nothing from your config."
  :type '(alist :key-type string :value-type function))

(defcustom org-link-maint-check-on-save-command-types nil
  "When non-nil, saving also checks `org-link-maint-command-link-types' links.
Off by default, since the program may be slow (searching your home
folder, say).  Background scans and the picker always check them."
  :type 'boolean)

(defcustom org-link-maint-command-jobs 4
  "How many link-checking programs may run at the same time."
  :type 'integer)

(defcustom org-link-maint-command-timeout 30
  "Seconds before a link-checking program is given up on."
  :type 'number)

(defvar org-link-maint--skip-command-types nil
  "Non-nil while checking links without running external programs.")

(defun org-link-maint--split-pos (path)
  "Split PATH at the first \"::\" after any \"://\" into (TARGET POS)."
  (let ((start (if (string-match "://" path) (match-end 0) 0)))
    (if (string-match "::" path start)
        (list (substring path 0 (match-beginning 0))
              (substring path (match-end 0)))
      (list path nil))))

(defun org-link-maint--parse-link (link)
  "Split LINK into (TYPE PATH SEARCH), or nil if it is not a checked type.
Bare paths such as [[./foo.org]] or [[~/x.org::*H]] count as \"file\".
For id:, file and command types SEARCH is the part after \"::\";
for types in `org-link-maint-link-checkers' PATH is the whole path."
  (let ((link (org-link-unescape link)))
    (cond
     ((string-match "\\`\\([a-zA-Z][a-zA-Z0-9+_-]*\\):\\(\\(?:.\\|\n\\)*\\)\\'" link)
      (let ((type (match-string 1 link))
            (rest (match-string 2 link)))
        (cond
         ((equal type "id")
          (list type (car (split-string rest "::")) nil))
         ((assoc type org-link-maint-link-checkers)
          (list type rest nil))
         ((or (member type org-link-maint--file-types)
              (assoc type org-link-maint-command-link-types))
          (cons type (org-link-maint--split-pos rest))))))
     ((string-match-p "\\`\\(?:/\\|\\./\\|\\.\\./\\|~\\)" link)
      (cons "file" (org-link-maint--split-pos link))))))

(defun org-link-maint--command-key (type target pos source)
  "Identify one external check; equal keys give equal answers."
  (list 'command type target (and pos (not (string-empty-p pos)) pos)
        (file-name-directory source)))

(defun org-link-maint--command-result (proc)
  "Turn finished PROC into a check result: `ok' or a reason string."
  (let ((status (process-exit-status proc))
        (output (with-current-buffer (process-buffer proc)
                  (string-trim (buffer-string)))))
    (pcase status
      (0 'ok)
      (1 (if (string-empty-p output) "broken"
           (car (split-string output "\n"))))
      (_ (format "check failed (exit %d)%s" status
                 (if (string-empty-p output) ""
                     (concat ": " (car (split-string output "\n")))))))))

(defun org-link-maint--run-command-checks (keys cache)
  "Run the external checks named by KEYS, a few at a time; fill CACHE.
Each key is from `org-link-maint--command-key'.  Emacs waits here, so
this is meant for background scans and the picker."
  (let ((queue keys) (running nil))
    (while (or queue running)
      (while (and queue (< (length running) org-link-maint-command-jobs))
        (pcase-let* ((key (pop queue))
                     (`(command ,type ,target ,pos ,dir) key)
                     (program (cdr (assoc type org-link-maint-command-link-types)))
                     (default-directory dir))
          (push (cons key
                      (cons (make-process
                             :name "org-link-maint-check"
                             :buffer (generate-new-buffer " *org-link-maint check*")
                             :command (append (list program "--check" target)
                                              (and pos (list pos)))
                             :connection-type 'pipe
                             :noquery t
                             :sentinel #'ignore)
                            (float-time)))
                running)))
      (accept-process-output nil 0.05)
      (dolist (job running)
        (pcase-let ((`(,key ,proc . ,start) job))
          (cond
           ((not (process-live-p proc))
            ;; Let any last output arrive before reading it.
            (while (accept-process-output proc 0))
            (puthash key (org-link-maint--command-result proc) cache)
            (kill-buffer (process-buffer proc))
            (setq running (delq job running)))
           ((> (- (float-time) start) org-link-maint-command-timeout)
            (delete-process proc)
            (puthash key "check timed out" cache)
            (kill-buffer (process-buffer proc))
            (setq running (delq job running)))))))))

(defun org-link-maint--prefetch-commands (links cache)
  "Run the external checks needed for LINKS, each distinct one once.
LINKS are (FILE LINE COL LINK DESC) entries.  Results go to CACHE.
Types whose program isn't installed are skipped with a message."
  (unless org-link-maint--skip-command-types
    (let (keys missing)
      (pcase-dolist (`(,source ,_ ,_ ,link ,_) links)
        (pcase (org-link-maint--parse-link link)
          (`(,type ,target ,pos)
           (let ((program (cdr (assoc type org-link-maint-command-link-types))))
             (when (and program (not (string-empty-p target)))
               (if (not (executable-find program))
                   (cl-pushnew program missing :test #'equal)
                 (let ((key (org-link-maint--command-key type target pos source)))
                   (unless (or (gethash key cache) (member key keys))
                     (push key keys)))))))))
      (dolist (program missing)
        (message "org-link-maint: %s not found, its links are not checked" program))
      (org-link-maint--run-command-checks (nreverse keys) cache))))

(defun org-link-maint--check-command-type (type target pos source cache)
  "Return why command-type link TYPE:TARGET::POS in SOURCE is broken, or nil."
  (unless (or org-link-maint--skip-command-types (string-empty-p target))
    (let ((result (gethash (org-link-maint--command-key type target pos source)
                           cache)))
      (and (stringp result) result))))

(defun org-link-maint--target-buffer (file cache)
  "Return a buffer holding Org FILE, memoised in CACHE.
An already visited buffer is used as is (unsaved text included);
otherwise a hidden buffer is created, recorded in CACHE so that
`org-link-maint--kill-target-buffers' can remove it afterwards."
  (or (car (gethash file cache))
      (let* ((visiting (find-buffer-visiting file))
             (buf (or visiting
                      (let ((b (generate-new-buffer
                                (concat " *org-link-maint " file "*"))))
                        (with-current-buffer b
                          (insert-file-contents file)
                          (let ((org-mode-hook nil))
                            (delay-mode-hooks (org-mode))))
                        b))))
        (puthash file (cons buf (not visiting)) cache)
        buf)))

(defun org-link-maint--kill-target-buffers (cache)
  "Kill the hidden buffers created through CACHE."
  (maphash (lambda (_key entry)
             (when (and (consp entry) (bufferp (car entry))
                        (cdr entry) (buffer-live-p (car entry)))
               (kill-buffer (car entry))))
           cache))

(defun org-link-maint--check-search (file search cache)
  "Return why SEARCH has no match in Org FILE, or nil if it matches.
SEARCH is what follows \"::\": a line number, *Heading, #custom-id,
or text (a <<target>>, #+NAME or exact heading, as Org resolves it)."
  (with-current-buffer (org-link-maint--target-buffer file cache)
    (org-with-wide-buffer
     (if (string-match-p "\\`[0-9]+\\'" search)
         (unless (<= (string-to-number search)
                     (line-number-at-pos (point-max)))
           (format "no line %s" search))
       (condition-case nil
           (let ((org-link-search-must-match-exact-headline t))
             (goto-char (point-min))
             (org-link-search search)
             nil)
         (error
          (cond ((string-prefix-p "*" search) "missing heading")
                ((string-prefix-p "#" search) "missing custom ID")
                (t "missing target"))))))))

(defun org-link-maint--check-link (link source cache)
  "Return why LINK in file SOURCE is broken, or nil if it is fine.
Checks id:, file-like links, `org-link-maint-command-link-types' and
`org-link-maint-link-checkers'.  For file links, the part after \"::\"
is checked only when the target is an Org file; remote (TRAMP) paths
are never checked."
  (pcase (org-link-maint--parse-link link)
    (`("id" ,id ,_)
     (unless (org-link-maint--id-file id) "missing ID"))
    (`(,(and type (guard (assoc type org-link-maint-link-checkers))) ,path ,_)
     (condition-case err
         (funcall (cdr (assoc type org-link-maint-link-checkers)) path source)
       (error (format "checker failed: %s" (error-message-string err)))))
    (`(,(and type (guard (assoc type org-link-maint-command-link-types))) ,target ,pos)
     (org-link-maint--check-command-type type target pos source cache))
    (`(,(and type (guard (member type org-link-maint--file-types))) ,path ,search)
     (let ((abs (expand-file-name path (file-name-directory source))))
       (cond
        ((or (string-empty-p path) (file-remote-p abs)) nil)
        ((not (file-exists-p abs)) "missing file")
        ((and search (not (string-empty-p search))
              (not (file-directory-p abs))
              (string-match-p "\\.org\\'" abs))
         (org-link-maint--check-search abs search cache)))))))

(defun org-link-maint--collect-links (files)
  "Return all bracket links in FILES as (FILE LINE COL LINK DESC).
Open buffers are read as they are, including unsaved changes."
  (let (links)
    (dolist (file files)
      (let ((buf (find-buffer-visiting file)))
        (with-temp-buffer
          (if buf
              (insert (with-current-buffer buf
                        (save-restriction
                          (widen)
                          (buffer-substring-no-properties
                           (point-min) (point-max)))))
            (insert-file-contents file))
          (goto-char (point-min))
          (while (re-search-forward org-link-bracket-re nil t)
            (let ((beg (match-beginning 0)))
              (push (list file
                          (line-number-at-pos beg)
                          (save-excursion (goto-char beg) (current-column))
                          (match-string-no-properties 1)
                          (match-string-no-properties 2))
                    links))))))
    (nreverse links)))

(defun org-link-maint--broken-links (files)
  "Return broken links in FILES as a list of (FILE LINE COL LABEL REASON TYPE).
LABEL is the description, or for a non-id link without one the link
itself (nil for an id link without one).  REASON says what is wrong."
  (let ((cache (make-hash-table :test #'equal))
        (links (org-link-maint--collect-links files))
        hits)
    (unwind-protect
        (progn
          (org-link-maint--prefetch-commands links cache)
          (pcase-dolist (`(,file ,line ,col ,link ,desc) links)
            (let ((reason (org-link-maint--check-link link file cache)))
              (when reason
                (push (list file line col
                            (or desc
                                (and (not (string-prefix-p "id:" link))
                                     (org-link-unescape link)))
                            reason
                            (car (org-link-maint--parse-link link)))
                      hits)))))
      (org-link-maint--kill-target-buffers cache))
    (nreverse hits)))

(defun org-link-maint--collect-broken ()
  "Refresh the ID database and return (FILES . BROKEN-LINKS)."
  (let ((files (org-link-maint-files)))
    (org-link-maint-refresh files)
    (cons files (org-link-maint--broken-links files))))

;;;; Cache of scan results

(defcustom org-link-maint-cache-file
  (locate-user-emacs-file "org-link-maint-cache.eld")
  "File holding the results of the last full scan.
Written by full scans (background or \\[universal-argument] \
\\[org-link-maint-consult-broken]) and read by
`org-link-maint-consult-broken'."
  :type 'file)

(defvar org-link-maint--cache nil
  "Last scan results: plist (:time FLOAT :files LIST :hits LIST).
Each file in :files was checked no earlier than :time, unless it
was modified after that.")

(defun org-link-maint--cache-write (cache)
  "Write CACHE to `org-link-maint-cache-file' atomically."
  (let ((tmp (make-temp-file
              (expand-file-name "org-link-maint-"
                                (file-name-directory org-link-maint-cache-file)))))
    (with-temp-file tmp
      (let ((print-length nil) (print-level nil))
        (prin1 cache (current-buffer))))
    (rename-file tmp org-link-maint-cache-file t)))

(defun org-link-maint--cache-current ()
  "Return the newest cache: in memory, or on disk if that is newer."
  (let ((disk (and (file-readable-p org-link-maint-cache-file)
                   (ignore-errors
                     (with-temp-buffer
                       (insert-file-contents org-link-maint-cache-file)
                       (read (current-buffer)))))))
    (when (and (plist-get disk :time)
               (or (null org-link-maint--cache)
                   (> (plist-get disk :time)
                      (plist-get org-link-maint--cache :time))))
      (setq org-link-maint--cache disk))
    org-link-maint--cache))

(defun org-link-maint--full-scan ()
  "Scan everything, store and save the result, and return the cache."
  (let* ((start (float-time))
         (result (org-link-maint--collect-broken))
         (cache (list :time start :files (car result) :hits (cdr result))))
    (org-link-maint--cache-write cache)
    (setq org-link-maint--cache cache)))

(defun org-link-maint--file-stale-p (file time)
  "Non-nil if FILE changed after TIME (on disk, or unsaved in a buffer)."
  (let ((buf (find-buffer-visiting file)))
    (or (and buf (buffer-modified-p buf))
        (time-less-p time (file-attribute-modification-time
                           (file-attributes file))))))

(defun org-link-maint--cache-refreshed (cache)
  "Return the hits of CACHE, with changed, new and deleted files rechecked.
Only links in those files are checked again, so this is fast.  A
target removed from an unchanged file is caught by the next full scan."
  (let* ((time (plist-get cache :time))
         (old-files (plist-get cache :files))
         (files (org-link-maint-files))
         (changed (cl-remove-if-not
                   (lambda (f) (or (not (member f old-files))
                                   (org-link-maint--file-stale-p f time)))
                   files))
         (kept (cl-remove-if
                (lambda (hit) (or (member (car hit) changed)
                                  (not (member (car hit) files))))
                (plist-get cache :hits))))
    (sort (append kept (org-link-maint--broken-links changed))
          (lambda (a b) (or (string< (car a) (car b))
                            (and (equal (car a) (car b))
                                 (< (cadr a) (cadr b))))))))

;;;; Background scanning

(defcustom org-link-maint-background-idle 600
  "Seconds of idle time before a background scan may start.
nil disables background scanning."
  :type '(choice (const :tag "Off" nil) integer))

(defcustom org-link-maint-background-interval 3600
  "Minimum seconds between two background scans."
  :type 'integer)

(defconst org-link-maint--library (or load-file-name buffer-file-name)
  "This file, loaded by the background Emacs process.")

(defvar org-link-maint--process nil
  "The running background scan process, if any.")

(defvar org-link-maint--idle-timer nil
  "Idle timer started by `org-link-maint-mode'.")

;;;###autoload
(defun org-link-maint-batch-scan ()
  "Full scan for use in `emacs --batch'; writes `org-link-maint-cache-file'.
Your ID database (`org-id-locations-file') is read but never written:
the scan works on a temporary copy, so it cannot clobber the copy
your running Emacs saves.  Example (cron, systemd timer, ...):

  emacs -Q --batch \\
    --eval \\='(setq my/notes-dir \"~/notes/\")\\=' \\
    -l /path/to/org-link-maint.el -f org-link-maint-batch-scan

If you use an Org newer than the built-in one, add its directory with
another --eval \\='(add-to-list (quote load-path) \"...\")\\=' first."
  (unless noninteractive
    (user-error "Use `org-link-maint-scan-async' inside Emacs"))
  (setq org-id-track-globally t)
  (when (file-readable-p org-id-locations-file)
    (org-id-locations-load))
  ;; From here on, any save goes to a throwaway file.
  (remove-hook 'kill-emacs-hook #'org-id-locations-save)
  (setq org-id-locations-file (make-temp-file "org-link-maint-ids"))
  (unwind-protect
      (let ((cache (org-link-maint--full-scan)))
        (message "org-link-maint: %d broken link(s) in %d file(s)"
                 (length (plist-get cache :hits))
                 (length (plist-get cache :files))))
    (delete-file org-id-locations-file)))

(defun org-link-maint--batch-settings ()
  "Return a form that gives the batch Emacs the same relevant settings."
  `(progn
     (setq load-path ',load-path)
     (setq org-id-locations-file ,(expand-file-name org-id-locations-file)
           org-id-locations-file-relative ,org-id-locations-file-relative
           org-link-maint-cache-file ,(expand-file-name org-link-maint-cache-file))
     (setq org-link-maint-command-link-types ',org-link-maint-command-link-types
           org-link-maint-link-checkers ',org-link-maint-link-checkers
           org-link-maint-command-jobs ,org-link-maint-command-jobs
           org-link-maint-command-timeout ,org-link-maint-command-timeout)
     ,@(when (boundp 'my/notes-dir)
         `((setq my/notes-dir ,my/notes-dir)))
     ,@(when (or (stringp org-agenda-files)
                 (cl-every #'stringp org-agenda-files))
         `((setq org-agenda-files ',org-agenda-files)))))

;;;; Desktop notifications

(defcustom org-link-maint-notify 'new
  "When a background scan sends a desktop notification (via notify-send).
`new'    - only when it finds broken links that weren't known before
           (from the previous scan, or already reported on save);
`always' - whenever any broken link exists;
nil      - never."
  :type '(choice (const :tag "New broken links" new)
                 (const :tag "Any broken links" always)
                 (const :tag "Never" nil)))

(defcustom org-link-maint-notify-max-items 5
  "How many broken links a notification lists by name."
  :type 'integer)

(defun org-link-maint--send-notification (title body &optional urgency)
  "Show a desktop notification with TITLE and BODY using notify-send.
URGENCY is \"low\", \"normal\" (default) or \"critical\"."
  (when (executable-find "notify-send")
    (call-process "notify-send" nil 0 nil
                  "--app-name=Emacs" "--icon=emacs"
                  (concat "--urgency=" (or urgency "normal"))
                  title body)))

(defun org-link-maint--hit-key (hit)
  "Identify HIT by file, label and reason (line numbers shift with edits)."
  (list (nth 0 hit) (nth 3 hit) (nth 4 hit)))

(defun org-link-maint--notify (known hits)
  "Notify about broken HITS, per `org-link-maint-notify'.
KNOWN are the hits known before the scan."
  (let* ((known-keys (mapcar #'org-link-maint--hit-key known))
         (fresh (cl-remove-if (lambda (h) (member (org-link-maint--hit-key h)
                                                  known-keys))
                              hits))
         (shown (if (eq org-link-maint-notify 'always) hits fresh)))
    (when (and org-link-maint-notify shown)
      (org-link-maint--send-notification
       (if fresh
           (format "%d new broken Org link%s" (length fresh)
                   (if (cdr fresh) "s" ""))
         (format "%d broken Org link%s" (length hits) (if (cdr hits) "s" "")))
       (concat
        (mapconcat
         (pcase-lambda (`(,file ,_line ,_col ,label ,reason . ,_))
           (format "• %s — %s (%s)"
                   (or label "(no description)") reason
                   (file-name-nondirectory file)))
         (seq-take shown org-link-maint-notify-max-items)
         "\n")
        (if (> (length shown) org-link-maint-notify-max-items)
            (format "\n…and %d more"
                    (- (length shown) org-link-maint-notify-max-items))
          "")
        (format "\n%d broken in total: M-x org-link-maint-consult-broken"
                (length hits)))))))

;;;###autoload
(defun org-link-maint-scan-async (&optional quiet)
  "Run a full scan in a separate, low-priority Emacs process.
Emacs stays usable while it runs; results go to the cache that
`org-link-maint-consult-broken' reads.  Unless QUIET, report when done."
  (interactive)
  (if (process-live-p org-link-maint--process)
      (unless quiet (message "org-link-maint: a scan is already running"))
    (let* ((emacs (expand-file-name invocation-name invocation-directory))
           (cmd (append (and (executable-find "nice") '("nice" "-n" "10"))
                        (list emacs "-Q" "--batch"
                              "--eval" (prin1-to-string
                                        (org-link-maint--batch-settings))
                              "-l" org-link-maint--library
                              "-f" "org-link-maint-batch-scan")))
           (buf (get-buffer-create " *org-link-maint scan*"))
           ;; What was already known, so only new breakage is notified.
           (known (plist-get (org-link-maint--cache-current) :hits)))
      (with-current-buffer buf (erase-buffer))
      (setq org-link-maint--process
            (make-process
             :name "org-link-maint"
             :buffer buf
             :command cmd
             :noquery t
             :sentinel
             (lambda (proc _event)
               (unless (process-live-p proc)
                 (if (and (eq (process-status proc) 'exit)
                          (zerop (process-exit-status proc)))
                     (let ((cache (org-link-maint--cache-current)))
                       (unless quiet
                         (message "org-link-maint: %d broken link(s) found"
                                  (length (plist-get cache :hits))))
                       (org-link-maint--notify known (plist-get cache :hits)))
                   (message "org-link-maint: background scan failed, see buffer %S"
                            (buffer-name (process-buffer proc)))
                   (org-link-maint--send-notification
                    "Org link scan failed"
                    (format "See buffer %s in Emacs"
                            (buffer-name (process-buffer proc)))
                    "critical"))))))
      (unless quiet (message "org-link-maint: scanning in the background...")))))

(defun org-link-maint--maybe-scan ()
  "Start a quiet background scan if the last one is old enough."
  (let ((cache (org-link-maint--cache-current)))
    (when (or (null cache)
              (> (- (float-time) (plist-get cache :time))
                 org-link-maint-background-interval))
      (org-link-maint-scan-async t))))

;;;; Checking on save

(defcustom org-link-maint-check-on-save t
  "When non-nil, `org-link-maint-mode' checks an Org file's links on save."
  :type 'boolean)

(defvar-local org-link-maint--last-count nil
  "Broken links found in this buffer at the previous save.
nil until the first save; then the last scan's count is used.")

(defun org-link-maint--update-ids-for-buffer ()
  "Make the ID database match the IDs in the current buffer's file.
Cheaper than a full rescan; also notices IDs that were deleted."
  (let ((file (expand-file-name buffer-file-name))
        stale)
    (unless (hash-table-p org-id-locations)
      (org-id-locations-load))
    (when (hash-table-p org-id-locations)
      (maphash (lambda (id f)
                 (when (equal (expand-file-name f) file)
                   (push id stale)))
               org-id-locations)
      (dolist (id stale) (remhash id org-id-locations))
      (org-with-wide-buffer
       (goto-char (point-min))
       (let ((case-fold-search t))
         (while (re-search-forward "^[ \t]*:ID:[ \t]+\\S-" nil t)
           (when (org-at-property-p)
             (org-id-add-location (org-entry-get (point) "ID") file))))))))

(defun org-link-maint--after-save ()
  "Check links in the Org file just saved; say so if new ones broke."
  (when (and org-link-maint-check-on-save
             (derived-mode-p 'org-mode)
             buffer-file-name)
    (org-link-maint--update-ids-for-buffer)
    (let* ((skip (not org-link-maint-check-on-save-command-types))
           (file (expand-file-name buffer-file-name))
           (hits (let ((org-link-maint--skip-command-types skip))
                   (org-link-maint--broken-links (list file))))
           (count (length hits))
           (before (or org-link-maint--last-count
                       (cl-count-if
                        (lambda (h)
                          (and (equal (car h) file)
                               (not (and skip (assoc (nth 5 h)
                                                     org-link-maint-command-link-types)))))
                        (plist-get (org-link-maint--cache-current) :hits)))))
      ;; Keep the cache in step, so the picker shows this file correctly.
      (when org-link-maint--cache
        (setq org-link-maint--cache
              (plist-put (copy-sequence org-link-maint--cache) :hits
                         (append
                          (cl-remove-if
                           (lambda (h)
                             (and (equal (car h) file)
                                  ;; Search types weren't rechecked: keep them.
                                  (not (and skip (assoc (nth 5 h)
                                                        org-link-maint-command-link-types)))))
                           (plist-get org-link-maint--cache :hits))
                          hits))))
      (when (> count before)
        (message "%d broken link(s) in this file (M-x org-link-maint-consult-broken)"
                 count))
      (setq org-link-maint--last-count count))
    ;; IDs may have appeared or vanished: redraw red links everywhere.
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (derived-mode-p 'org-mode)
          (font-lock-flush))))))

;;;; Visual marking of broken links

(defun org-link-maint--link-face (path)
  "Return the face for an ID link with PATH."
  (if (org-link-maint--id-file (car (split-string path "::")))
      'org-link
    'org-link-maint-broken-link))

;;;###autoload
(define-minor-mode org-link-maint-mode
  "Keep an eye on broken links.
- Broken id: links are shown with `org-link-maint-broken-link'.
- Saving an Org file checks its links (`org-link-maint-check-on-save').
- When Emacs is idle, a full scan runs in a separate process
  (`org-link-maint-background-idle', `org-link-maint-background-interval')."
  :global t
  (org-link-set-parameters
   "id" :face (if org-link-maint-mode #'org-link-maint--link-face 'org-link))
  (when (timerp org-link-maint--idle-timer)
    (cancel-timer org-link-maint--idle-timer)
    (setq org-link-maint--idle-timer nil))
  (if org-link-maint-mode
      (progn
        (add-hook 'after-save-hook #'org-link-maint--after-save)
        (when org-link-maint-background-idle
          (setq org-link-maint--idle-timer
                (run-with-idle-timer org-link-maint-background-idle t
                                     #'org-link-maint--maybe-scan))))
    (remove-hook 'after-save-hook #'org-link-maint--after-save))
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'org-mode)
        (font-lock-flush)))))

;;;; Consult interface

(declare-function consult--read "ext:consult")
(declare-function consult--jump-state "ext:consult")
(declare-function consult--jump "ext:consult")

(defun org-link-maint--loc-marker (file line col)
  "Return a marker at LINE, COL in FILE (opening it if needed)."
  (with-current-buffer (find-file-noselect file)
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (forward-line (1- line))
        (move-to-column col)
        (point-marker)))))

(defun org-link-maint--age (time)
  "Describe how long ago TIME was, briefly."
  (let ((s (- (float-time) time)))
    (cond ((< s 90) "just now")
          ((< s 5400) (format "%dm ago" (round s 60)))
          ((< s 129600) (format "%dh ago" (round s 3600)))
          (t (format "%dd ago" (round s 86400))))))

;;;###autoload
(defun org-link-maint-consult-broken (&optional rescan)
  "Pick a broken link from all Org files with consult, with live preview.
Checks id:, file:, file+sys: and file+emacs: links, including the
\"::\" part (heading, custom ID, line, target) of links to Org files.

Uses the last full scan, rechecking only files changed since then,
so it opens fast.  With prefix argument RESCAN, or when there is no
earlier scan, does a full scan first (Emacs waits for it)."
  (interactive "P")
  (unless (require 'consult nil t)
    (user-error "This command needs the consult package"))
  (let* ((cache (if rescan
                    (org-link-maint--full-scan)
                  (or (org-link-maint--cache-current)
                      (org-link-maint--full-scan))))
         (hits (org-link-maint--cache-refreshed cache))
         (age (org-link-maint--age (plist-get cache :time))))
    (if (null hits)
        (message "No broken links (full scan %s)" age)
      (let* ((candidates
              (mapcar
               (pcase-lambda (`(,file ,line ,col ,label ,reason . ,_))
                 ;; The candidate is "file:line: label" so that it is
                 ;; unique, but only the label is displayed (see :group).
                 (let ((prefix (format "%s:%d: " (abbreviate-file-name file) line)))
                   (propertize (concat prefix
                                       (or label
                                           (propertize "(no description)"
                                                       'face 'shadow)))
                               'org-link-maint-loc (list file line col)
                               'org-link-maint-file (abbreviate-file-name file)
                               'org-link-maint-reason reason
                               'org-link-maint-prefix (length prefix))))
               hits))
             (marker
              (consult--read
               candidates
               :prompt (format "Broken links (%d, full scan %s): "
                               (length hits) age)
               :category 'org-link-maint-broken
               :sort nil
               :require-match t
               ;; Dimmed reason after each entry: "missing file", etc.
               :annotate (lambda (cand)
                           (concat "  " (propertize
                                         (get-text-property
                                          0 'org-link-maint-reason cand)
                                         'face 'shadow)))
               ;; Group by file; show only "line: link" inside each group.
               :group (lambda (cand transform)
                        (if transform
                            (substring cand (get-text-property
                                             0 'org-link-maint-prefix cand))
                          (get-text-property 0 'org-link-maint-file cand)))
               ;; Turn the selected candidate into a marker for preview/jump.
               :lookup (lambda (selected candidates &rest _)
                         (when-let* ((cand (car (member selected candidates)))
                                     (loc (get-text-property
                                           0 'org-link-maint-loc cand)))
                           (apply #'org-link-maint--loc-marker loc)))
               :state (consult--jump-state))))
        ;; The jump state normally jumps on return already; make sure.
        (when (and (markerp marker)
                   (not (and (eq (current-buffer) (marker-buffer marker))
                             (= (point) marker))))
          (consult--jump marker))))))

(provide 'org-link-maint)
;;; org-link-maint.el ends here
