;;; org-link-maint.el --- Maintenance tools for Org links  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Maintenance utilities for links in your Org notes.
;;
;; How it works: every note is read once as plain text (no Org buffers)
;; into an index of its title, headings, IDs, CUSTOM_IDs, <<targets>>,
;; #+NAMEs and links.  All checks are lookups in that index, and files
;; that haven't changed since the last scan are not read again.
;;
;;  1. Broken links.  Checks id:, file:, file+sys:, file+emacs: and bare
;;     paths ([[./x.org]]); for links into Org notes, id: or file:, also
;;     the "::" part (::*Heading, ::#custom-id, ::42, ::target), matched
;;     the way Org matches them.  Custom link types: `org-link-maint-command-link-types'
;;     asks the program that opens the link (PROGRAM --check TARGET [POS]),
;;     `org-link-maint-link-checkers' takes Lisp functions.
;;
;;  2. Outdated descriptions.  Each kind of link has one expected
;;     description (`org-link-maint-description-formats'):
;;       [[id:ID]]                  the heading        ("%h")
;;       [[id:ID::#custom-id]]      title: heading     ("%t: %h")
;;       [[id:ID::*Heading]]        title: heading     ("%t: %h")
;;       [[file:x.org]]             the file's title   ("%t")
;;     (file:x.org::#custom-id and file:x.org::*Heading count the same
;;     as their id: forms.)  A file-level ID uses the title.
;;     %h is the heading without TODO keyword, priority and tags; %t the
;;     file's #+TITLE (or its name).  Every other description is reported.  For one you want to keep,
;;     answer "i" when asked, and it is never reported again.
;;
;;     Wrong link type (`org-link-maint-check-link-types').  A link to a
;;     heading should use the best target the heading has: ID >
;;     CUSTOM_ID > *Heading.  So ::*Heading to a heading with a CUSTOM_ID
;;     should be ::#custom-id, and ::#custom-id or ::*Heading to a
;;     heading with an ID should be [[id:ID]].
;;
;;       M-x org-link-maint-fix   fix descriptions and link types (asks
;;                                y/n/!/q/i; C-u fixes all without asking)
;;
;;  3. Orphan notes (`org-link-maint-check-orphans').  A note (file)
;;     with a TYPE property is an entry point; every other note must be
;;     linked from a note that leads, link by link, to a hub or activity
;;     (`org-link-maint-root-types').  A link to a heading counts as a
;;     link to its note.  Reported: orphans (no TYPE, no backlinks) and
;;     unreachable notes (backlinks, but no chain to a hub or activity).
;;     Folders to leave out: `org-link-maint-orphan-exclude-directories'.
;;
;;  4. M-x org-link-maint-consult: every problem, with live preview.
;;     Narrow with "b" (broken), "d" (descriptions), "o" (orphans) or
;;     "u" (unreachable).  Choosing an outdated description offers to
;;     fix it.
;;
;;  5. org-link-maint-mode (global minor mode):
;;     - broken id: links are shown red with strike-through;
;;     - saving an Org file checks its links;
;;     - when Emacs is idle, a full scan runs in a separate low-priority
;;       Emacs (`org-link-maint-background-idle'), and new problems are
;;       sent as desktop notifications (`org-link-maint-notify').
;;
;; "All Org files" = every Org file under `my/notes-dir', Org's ID
;; database, `org-agenda-files' and every open Org buffer (unsaved text
;; included).
;;
;; Setup:
;;
;;   (with-eval-after-load 'org
;;     (require 'org-link-maint)
;;     (org-link-maint-mode 1))

;;; Code:

(require 'org)
(require 'org-id)
(require 'ol)
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

(defcustom org-link-maint-description-formats
  '((id . "%h") (custom-id . "%t: %h") (heading . "%t: %h") (file . "%t"))
  "The description each kind of link should have.
Kinds: `id' ([[id:ID]]), `custom-id' ([[id:ID::#id]] or
\[[file:x.org::#id]]), `heading' ([[id:ID::*Heading]] or
\[[file:x.org::*Heading]]) and `file' ([[file:x.org]]).  In the format,
%h is the heading's title (without TODO keyword, priority and tags) and
%t the title of its file (#+TITLE, or the file name).  For a file-level
ID, %h is the file's title too.  A kind set to nil is not checked."
  :type '(alist :key-type (choice (const id) (const custom-id)
                                  (const heading) (const file))
                :value-type (choice (const :tag "Don't check" nil) string)))

(defcustom org-link-maint-check-link-types t
  "When non-nil, report links to a heading that use a weaker kind of link.
The order is ID > CUSTOM_ID > *Heading: a link should use the best
one the heading has.  So [[...::*Heading]] to a heading with a
CUSTOM_ID should be [[...::#custom-id]], and a ::#custom-id or
::*Heading link to a heading with an ID should be [[id:ID]]."
  :type 'boolean)

(defcustom org-link-maint-ignore-file
  (locate-user-emacs-file "org-link-maint-ignore.eld")
  "File listing link descriptions you chose to keep as they are.
Each entry is (FILE LINK DESCRIPTION); when the link or its description
changes, it is checked again."
  :type 'file)

(defface org-link-maint-broken-link
  '((t :inherit org-link :foreground "red" :strike-through t))
  "Face for ID links whose target cannot be found.")

(defvar org-link-maint--batch nil
  "Non-nil in the separate Emacs that runs background scans.")

;;;; Files

(defun org-link-maint--dir-files (dir)
  "Return Org files under DIR, recursively, skipping hidden files and dirs."
  (when (and dir (file-directory-p dir))
    (directory-files-recursively
     (expand-file-name dir)
     "\\`[^.#].*\\.org\\'"              ; skip lock files (.#foo.org)
     nil
     (lambda (d) (not (string-prefix-p "." (file-name-nondirectory d)))))))

(defun org-link-maint--org-id-load ()
  "Make sure Org's ID database is loaded as a hash table."
  (unless (hash-table-p org-id-locations)
    (org-id-locations-load))
  (unless (hash-table-p org-id-locations)
    (setq org-id-locations (make-hash-table :test #'equal))))

(defun org-link-maint-files ()
  "Return every Org file we know about, as absolute file names.
This is the union of:
- files under `my/notes-dir' (recursively),
- every file in Org's ID database,
- `org-agenda-files',
- every file currently open in an Org buffer."
  (org-link-maint--org-id-load)
  (let ((files (append
                (and (boundp 'my/notes-dir)
                     (org-link-maint--dir-files my/notes-dir))
                (copy-sequence org-id-files)
                (let (fs)
                  (maphash (lambda (_id f) (push f fs)) org-id-locations)
                  fs)
                (ignore-errors (org-agenda-files t))
                (delq nil
                      (mapcar (lambda (b)
                                (with-current-buffer b
                                  (and (derived-mode-p 'org-mode)
                                       buffer-file-name)))
                              (buffer-list))))))
    (cl-remove-if
     (lambda (f) (or (file-remote-p f) (not (file-exists-p f))))
     (delete-dups (mapcar #'expand-file-name (delq nil files))))))

;;;; The index

;; Emacs hashes long strings by sampling a few positions, so file names
;; that differ only near the end (most paths in one notes folder) end up
;; with the same hash: 2000 notes gave 5 distinct hashes.  Tables keyed
;; by file names use this test instead, which hashes the whole string.
(define-hash-table-test 'org-link-maint-string
  #'string= (lambda (s) (sxhash-equal (secure-hash 'md5 s))))

(defun org-link-maint--string-table ()
  "A hash table for string keys such as file names."
  (make-hash-table :test 'org-link-maint-string))

(cl-defstruct (org-link-maint--fi (:constructor org-link-maint--fi-make)
                                  (:copier nil))
  "What one Org file contains, as far as links are concerned."
  file stamp title lines
  type      ; the note's TYPE property (file level), or nil
  headings  ; hash: heading words (as Org compares them) -> heading title
  cids      ; hash: downcased CUSTOM_ID -> heading title
  hprops    ; hash: heading words -> (ID . CUSTOM_ID) of that heading
  cid-ids   ; hash: downcased CUSTOM_ID -> ID of the same heading
  targets   ; hash: downcased <<target>> words -> t
  names     ; hash: #+NAME words -> t
  ids       ; list of (ID . HEADING-TITLE), HEADING-TITLE nil for the file
  links)    ; list of (LINE OFFSET LINK DESC PARSED)

(defvar org-link-maint--index (org-link-maint--string-table)
  "File name -> `org-link-maint--fi'.  Kept between scans.")

(defvar org-link-maint--ids (org-link-maint--string-table)
  "ID -> (FILE . HEADING-TITLE) for every indexed file.")

(defun org-link-maint--join (string)
  "STRING's words, separated by single spaces."
  (mapconcat #'identity (split-string string) " "))

(defun org-link-maint--heading-key (title)
  "TITLE as Org compares headings: no statistics cookies, single spaces."
  (org-link-maint--join
   (replace-regexp-in-string "\\[[0-9]*\\(?:%\\|/[0-9]*\\)\\]" " " title)))

(defun org-link-maint--clean (string)
  "Make STRING safe as a link description (no square brackets)."
  (replace-regexp-in-string
   "\\]" "}" (replace-regexp-in-string "\\[" "{" string)))

(defun org-link-maint--display (title)
  "TITLE as shown in a description: links rendered, no brackets."
  (org-link-maint--clean (org-link-display-format title)))

(defun org-link-maint--todo-keywords ()
  "TODO keywords from `org-todo-keywords', without (x) suffixes."
  (let (out)
    (dolist (spec org-todo-keywords)
      (dolist (s (if (consp spec) (cdr spec) (list spec)))
        (when (and (stringp s) (not (equal s "|")))
          (push (replace-regexp-in-string "(.*)\\'" "" s) out))))
    out))

(defun org-link-maint--heading-title (text keywords)
  "Heading TEXT (after the stars) without TODO, priority, COMMENT, tags.
That is what `org-get-heading' returns with all its arguments set.
KEYWORDS are the TODO keywords in effect."
  (let ((case-fold-search nil))
    (when (string-match "[ \t]+:\\(?:[[:alnum:]_@#%]+:\\)+[ \t]*\\'" text)
      (setq text (substring text 0 (match-beginning 0))))
    (when (and (string-match "\\`\\(\\S-+\\)\\(?:[ \t]+\\|\\'\\)" text)
               (member (match-string 1 text) keywords))
      (setq text (substring text (match-end 0))))
    (when (string-match "\\`\\[#.\\][ \t]*" text)
      (setq text (substring text (match-end 0))))
    (when (string-match "\\`COMMENT\\(?:[ \t]+\\|\\'\\)" text)
      (setq text (substring text (match-end 0))))
    (string-trim text)))

(defun org-link-maint--index-current-buffer (file)
  "Index the Org text in the current buffer, which belongs to FILE."
  (let ((case-fold-search t)
        (keywords (org-link-maint--todo-keywords))
        (headings (make-hash-table :test #'equal))
        (cids (make-hash-table :test #'equal))
        (hprops (make-hash-table :test #'equal))
        (cid-ids (make-hash-table :test #'equal))
        (targets (make-hash-table :test #'equal))
        (names (make-hash-table :test #'equal))
        titles ids links first-heading file-type)
    ;; Per-file TODO keywords, so they are not mistaken for titles.
    (goto-char (point-min))
    (while (re-search-forward "^#\\+\\(?:SEQ_\\|TYP_\\)?TODO:[ \t]*\\(.*\\)$" nil t)
      (dolist (w (split-string (match-string-no-properties 1)))
        (unless (equal w "|")
          (push (replace-regexp-in-string "(.*)\\'" "" w) keywords))))
    (goto-char (point-min))
    (while (re-search-forward "^#\\+TITLE:[ \t]*\\(.*?\\)[ \t]*$" nil t)
      (push (match-string-no-properties 1) titles))
    ;; File-level ID: before the first heading.
    (goto-char (point-min))
    (setq first-heading (if (re-search-forward "^\\*+ " nil t)
                            (match-beginning 0)
                          (point-max)))
    (goto-char (point-min))
    (when (re-search-forward "^[ \t]*:ID:[ \t]+\\(\\S-+\\)" first-heading t)
      (push (cons (match-string-no-properties 1) nil) ids))
    ;; The note's TYPE: file-level drawer, or #+PROPERTY: TYPE value.
    (goto-char (point-min))
    (let ((type (and (re-search-forward
                      "^[ \t]*\\(?::TYPE:\\|#\\+PROPERTY:[ \t]+TYPE\\)[ \t]+\\(.*?\\)[ \t]*$"
                      first-heading t)
                     (match-string-no-properties 1))))
      (setq file-type (and type (not (string-empty-p type)) type)))
    ;; Headings with their property drawers.
    (goto-char first-heading)
    (while (re-search-forward "^\\*+ \\(.*\\)$" nil t)
      (let* ((title (org-link-maint--heading-title
                     (match-string-no-properties 1) keywords))
             (key (org-link-maint--heading-key title))
             (first (not (gethash key headings))) ; Org finds the first one
             id cid)
        (when first
          (puthash key (org-link-maint--display title) headings))
        (forward-line 1)
        (when (looking-at "[ \t]*\\(?:SCHEDULED\\|DEADLINE\\|CLOSED\\):")
          (forward-line 1))
        (when (looking-at "[ \t]*:PROPERTIES:[ \t]*$")
          (forward-line 1)
          (while (not (or (eobp)
                          (looking-at "[ \t]*:END:[ \t]*$")
                          (looking-at "\\*+ ")))
            (cond ((looking-at "[ \t]*:ID:[ \t]+\\(\\S-+\\)")
                   (setq id (match-string-no-properties 1)))
                  ((looking-at "[ \t]*:CUSTOM_ID:[ \t]+\\(.*?\\)[ \t]*$")
                   (setq cid (match-string-no-properties 1))))
            (forward-line 1)))
        (when (or id cid)
          (let ((shown (org-link-maint--display title)))
            (when id (push (cons id shown) ids))
            (when cid (puthash (downcase cid) shown cids))
            (when (and id cid) (puthash (downcase cid) id cid-ids))
            (when first (puthash key (cons id cid) hprops))))))
    ;; <<targets>> (not <<<radio targets>>>) and #+NAME:s.
    (goto-char (point-min))
    (while (re-search-forward "<<\\([^<>\n]+\\)>>" nil t)
      (unless (eq (char-before (match-beginning 0)) ?<)
        (puthash (downcase (org-link-maint--join (match-string-no-properties 1)))
                 t targets)))
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*#\\+NAME:[ \t]+\\(.*?\\)[ \t]*$" nil t)
      (puthash (org-link-maint--join (match-string-no-properties 1)) t names))
    ;; Links, with line numbers counted as we go.
    (goto-char (point-min))
    (let ((line 1) (bol (point-min)))
      (while (re-search-forward org-link-bracket-re nil t)
        (let* ((beg (match-beginning 0))
               (lbeg (save-excursion (goto-char beg) (line-beginning-position))))
          (setq line (+ line (count-lines bol lbeg))
                bol lbeg)
          (let ((link (match-string-no-properties 1)))
            (push (list line (- beg lbeg) link
                        (match-string-no-properties 2)
                        (org-link-maint--parse-link link))
                  links)))))
    (org-link-maint--fi-make
     :file file
     :title (if titles
                (org-link-maint--display (string-join (nreverse titles) " "))
              (file-name-base file))
     :lines (line-number-at-pos (point-max))
     :headings headings :cids cids :targets targets :names names
     :hprops hprops :cid-ids cid-ids
     :ids ids :links (nreverse links) :type file-type)))

(defun org-link-maint--stamp (file)
  "Modification time and size of FILE, or nil."
  (let ((attr (file-attributes file)))
    (and attr (list (file-attribute-modification-time attr)
                    (file-attribute-size attr)))))

(defvar org-link-maint--scan nil
  "During a scan, (CHECKED MODIFIED MEMO): files already brought up to
date in this scan, files that have unsaved buffers, and remembered
answers (resolved paths, file existence).  So each file is looked at
once per scan, however many links point to it.")

(defmacro org-link-maint--scanning (&rest body)
  "Run BODY as one scan (see `org-link-maint--scan')."
  (declare (indent 0) (debug t))
  `(let* (;; Notes are plain local files, and file name handlers (TRAMP,
          ;; compression, ...) make every path operation much slower.
          ;; Remote paths are recognized by `org-link-maint--remote-p'.
          (file-name-handler-alist nil)
          (org-link-maint--scan
          (or org-link-maint--scan
              (let ((modified (org-link-maint--string-table)))
                (dolist (buf (buffer-list))
                  (when-let* ((name (buffer-file-name buf)))
                    (when (buffer-modified-p buf)
                      (puthash (expand-file-name name) buf modified)
                      (puthash (file-truename name) buf modified))))
                (list (org-link-maint--string-table) modified
                      (org-link-maint--string-table))))))
     ,@body))

(defun org-link-maint--memo (key fn)
  "Return FN's value, computed once per scan for KEY."
  (let ((memo (nth 2 org-link-maint--scan)))
    (if (not memo)
        (funcall fn)
      (let ((v (gethash key memo 'none)))
        (if (eq v 'none) (puthash key (funcall fn) memo) v)))))

(defun org-link-maint--remote-p (path)
  "Non-nil if PATH looks like a remote (TRAMP) file name, /method:host:..."
  (string-match-p "\\`/[^/:|]+:" path))

(defun org-link-maint--resolve (path source)
  "PATH as an absolute file name, relative to the folder of SOURCE.
Remote paths are returned as they are."
  (if (org-link-maint--remote-p path)
      path
    (expand-file-name path (file-name-directory source))))

(defun org-link-maint--file-state (abs)
  "`remote', `missing', `dir' or `file' for the absolute file name ABS."
  (org-link-maint--memo abs
                        (lambda ()
                          (cond ((org-link-maint--remote-p abs) 'remote)
                                ((file-directory-p abs) 'dir)
                                ((file-exists-p abs) 'file)
                                (t 'missing)))))

(defun org-link-maint--modified-buffer (file)
  "The buffer visiting FILE, if it has unsaved changes."
  (if org-link-maint--scan
      (gethash file (nth 1 org-link-maint--scan))
    (let ((buf (find-buffer-visiting file)))
      (and buf (buffer-modified-p buf) buf))))

(defun org-link-maint--index-file (file)
  "Read FILE (or its unsaved buffer) and return its index entry."
  (let ((buf (org-link-maint--modified-buffer file)))
    (with-current-buffer (get-buffer-create " *org-link-maint index*")
      (buffer-disable-undo)
      (let ((inhibit-modification-hooks t))
        (erase-buffer)
        (if buf
            (insert (with-current-buffer buf
                      (save-restriction
                        (widen)
                        (buffer-substring-no-properties (point-min) (point-max)))))
          ;; No file handlers: much faster, and these are plain local files.
          (let ((file-name-handler-alist nil))
            (insert-file-contents file))))
      (let ((fi (org-link-maint--index-current-buffer file)))
        ;; Text from an unsaved buffer is re-read next time.
        (setf (org-link-maint--fi-stamp fi)
              (and (not buf) (org-link-maint--stamp file)))
        fi))))

(defun org-link-maint--forget-ids (fi)
  "Remove FI's IDs from `org-link-maint--ids'."
  (let ((file (org-link-maint--fi-file fi)))
    (dolist (entry (org-link-maint--fi-ids fi))
      (when (equal (car (gethash (car entry) org-link-maint--ids)) file)
        (remhash (car entry) org-link-maint--ids)))))

(defun org-link-maint--fi-get (file)
  "Return the index entry for Org FILE, reading it only if it changed.
Return nil if FILE does not exist."
  (let ((checked (car org-link-maint--scan)))
    (if (and checked (gethash file checked))
        (gethash file org-link-maint--index)
      (when checked (puthash file t checked))
      (let ((old (gethash file org-link-maint--index)))
        (if (and old
                 (org-link-maint--fi-stamp old)
                 (not (org-link-maint--modified-buffer file))
                 (equal (org-link-maint--fi-stamp old) (org-link-maint--stamp file)))
            old
          (when old
            (org-link-maint--forget-ids old)
            (remhash file org-link-maint--index))
          (when (file-exists-p file)
            (let ((fi (org-link-maint--index-file file)))
              (dolist (entry (org-link-maint--fi-ids fi))
                (puthash (car entry) (cons file (cdr entry)) org-link-maint--ids))
              (puthash file fi org-link-maint--index))))))))

(defvar org-link-maint-command-link-types)   ; defined below
(defvar org-link-maint-link-checkers)

(defvar org-link-maint--index-settings nil
  "The settings the index was built with; it is rebuilt when they change.")

(defun org-link-maint--index-update (files)
  "Bring the index up to date for FILES and drop deleted files."
  (let ((settings (list 5               ; index format version
                        org-link-maint-command-link-types
                        (mapcar #'car org-link-maint-link-checkers)
                        org-todo-keywords)))
    (unless (equal settings org-link-maint--index-settings)
      (clrhash org-link-maint--index)
      (clrhash org-link-maint--ids)
      (setq org-link-maint--index-settings settings)))
  (mapc #'org-link-maint--fi-get files)
  (let (gone)
    (maphash (lambda (f _) (unless (file-exists-p f) (push f gone)))
             org-link-maint--index)
    (dolist (f gone)
      (org-link-maint--forget-ids (gethash f org-link-maint--index))
      (remhash f org-link-maint--index))))

(defun org-link-maint--sync-org-id (&optional file)
  "Make Org's ID database agree with the index (for FILE only, if given).
Keeps Org's own ID links and the red broken-link face accurate."
  (unless org-link-maint--batch
    (org-link-maint--org-id-load)
    (let (stale)
      (maphash (lambda (id f)
                 (let ((f (expand-file-name f)))
                   (when (if file (equal f file) (gethash f org-link-maint--index))
                     (push id stale))))
               org-id-locations)
      (dolist (id stale) (remhash id org-id-locations)))
    (maphash (lambda (id node)
               (when (or (null file) (equal (car node) file))
                 (puthash id (abbreviate-file-name (car node)) org-id-locations)))
             org-link-maint--ids)
    (setq org-id-files (delete-dups (hash-table-values org-id-locations)))))

(defun org-link-maint--id-file (id)
  "Return the file that contains ID according to Org's database, or nil.
Cheap enough for font-lock."
  (org-link-maint--org-id-load)
  (let ((file (gethash id org-id-locations)))
    (and file (file-exists-p file) file)))

(defun org-link-maint--id-node (id)
  "Return (FILE . HEADING-TITLE) for ID, or nil if it doesn't exist.
HEADING-TITLE is nil for a file-level ID.  IDs in files outside the
index are found through Org's database, and those files indexed."
  (or (gethash id org-link-maint--ids)
      (let ((file (org-link-maint--id-file id)))
        (when file
          (setq file (expand-file-name file))
          (unless (gethash file org-link-maint--index)
            (org-link-maint--fi-get file)
            (gethash id org-link-maint--ids))))))

;;;; Link types

(defconst org-link-maint--file-types '("file" "file+sys" "file+emacs")
  "Link types whose path is a file name.")

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

(defun org-link-maint--split-pos (path &optional after-url)
  "Split PATH at the first \"::\" into (TARGET POS).
With AFTER-URL, split at the first \"::\" after any \"://\" instead, as
`my/link-split' does for command link types (whose target may be a
URL).  id: and file: links are split like Org splits them, at the
first \"::\", since the part after it may itself contain a URL (a
heading such as \"*Archiving [[https://...][x]]\")."
  (let ((start (if (and after-url (string-match "://" path)) (match-end 0) 0)))
    (if (string-match "::" path start)
        (list (substring path 0 (match-beginning 0))
              (substring path (match-end 0)))
      (list path nil))))

(defun org-link-maint--parse-link (link)
  "Split LINK into (TYPE PATH SEARCH), or nil if it is not a checked type.
Bare paths such as [[./foo.org]] or [[~/x.org::*H]] count as \"file\".
For id:, file and command types SEARCH is the part after \"::\"
\(id:ID::SEARCH searches inside the note with ID, as in Org 9.7);
for types in `org-link-maint-link-checkers' PATH is the whole path."
  (let ((link (org-link-unescape link)))
    (cond
     ((string-match "\\`\\([a-zA-Z][a-zA-Z0-9+_-]*\\):\\(\\(?:.\\|\n\\)*\\)\\'" link)
      (let ((type (match-string 1 link))
            (rest (match-string 2 link)))
        (cond
         ((equal type "id")
          (cons type (org-link-maint--split-pos rest)))
         ((assoc type org-link-maint-link-checkers)
          (list type rest nil))
         ((member type org-link-maint--file-types)
          (cons type (org-link-maint--split-pos rest)))
         ((assoc type org-link-maint-command-link-types)
          (cons type (org-link-maint--split-pos rest t))))))
     ((string-match-p "\\`\\(?:/\\|\\./\\|\\.\\./\\|~\\)" link)
      (cons "file" (org-link-maint--split-pos link))))))

(defun org-link-maint--org-file (path source)
  "Index entry for PATH (relative to SOURCE) if it is an existing Org file."
  (let ((abs (org-link-maint--resolve path source)))
    (and (string-match-p "\\.org\\'" abs)
         (eq (org-link-maint--file-state abs) 'file)
         (org-link-maint--fi-get abs))))

;;;; External checks (command link types)

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
Each key is from `org-link-maint--command-key'."
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
  "Run the external checks LINKS need that CACHE doesn't already answer.
LINKS are (FILE LINE OFFSET LINK DESC PARSED).  Return all keys LINKS use.
Types whose program isn't installed are skipped with a message."
  (unless org-link-maint--skip-command-types
    (let (used todo missing)
      (pcase-dolist (`(,source ,_ ,_ ,_ ,_ ,parsed) links)
        (pcase parsed
          (`(,type ,target ,pos)
           (let ((program (cdr (assoc type org-link-maint-command-link-types))))
             (when (and program (not (string-empty-p target)))
               (if (not (executable-find program))
                   (cl-pushnew program missing :test #'equal)
                 (let ((key (org-link-maint--command-key type target pos source)))
                   (unless (member key used)
                     (push key used)
                     (unless (gethash key cache) (push key todo))))))))))
      (dolist (program missing)
        (message "org-link-maint: %s not found, its links are not checked" program))
      (org-link-maint--run-command-checks (nreverse todo) cache)
      used)))

;;;; Checking links

(defun org-link-maint--check-search (fi search)
  "Return why SEARCH (after \"::\") has no match in FI, or nil.
Follows `org-link-search': #custom-id, line number, *Heading, or
text matching a <<target>>, a #+NAME or a heading."
  (let ((s (if (string-search "\n" search)
               (replace-regexp-in-string "\n[ \t]*" " " search)
             search)))
    (cond
     ((string-match-p "\\`[0-9]+\\'" s)
      (unless (<= (string-to-number s) (org-link-maint--fi-lines fi))
        (format "no line %s" s)))
     ((string-prefix-p "#" s)
      (unless (gethash (downcase (substring s 1)) (org-link-maint--fi-cids fi))
        "missing custom ID"))
     ((string-match-p "\\`(.*)\\'" s) nil)    ; coderef: not checked
     ((string-match-p "\\`/.*/\\'" s) nil)    ; regexp: not checked
     ((string-prefix-p "*" s)
      (unless (gethash (org-link-maint--join (substring s 1))
                       (org-link-maint--fi-headings fi))
        "missing heading"))
     (t
      (let ((words (org-link-maint--join s)))
        (unless (or (gethash (downcase words) (org-link-maint--fi-targets fi))
                    (gethash words (org-link-maint--fi-names fi))
                    (gethash words (org-link-maint--fi-headings fi)))
          "missing target"))))))

(defun org-link-maint--broken-reason (parsed source commands)
  "Return why the link PARSED in file SOURCE is broken, or nil.
COMMANDS holds the results of external checks."
  (pcase parsed
    (`("id" ,id ,search)
     (let ((node (org-link-maint--id-node id)))
       (cond
        ((null node) "missing ID")
        ((and search (not (string-empty-p search)))
         (when-let* ((fi (org-link-maint--fi-get (car node))))
           (org-link-maint--check-search fi search))))))
    (`(,(and type (guard (assoc type org-link-maint-link-checkers))) ,path ,_)
     (condition-case err
         (funcall (cdr (assoc type org-link-maint-link-checkers)) path source)
       (error (format "checker failed: %s" (error-message-string err)))))
    (`(,(and type (guard (assoc type org-link-maint-command-link-types))) ,target ,pos)
     (unless (or org-link-maint--skip-command-types (string-empty-p target))
       (let ((result (gethash (org-link-maint--command-key type target pos source)
                              commands)))
         (and (stringp result) result))))
    (`(,(and type (guard (member type org-link-maint--file-types))) ,path ,search)
     (unless (string-empty-p path)
       (let* ((abs (org-link-maint--resolve path source))
              (state (org-link-maint--file-state abs)))
         (cond
          ((eq state 'remote) nil)
          ((eq state 'missing) "missing file")
          ((and search (not (string-empty-p search))
                (eq state 'file)
                (string-match-p "\\.org\\'" abs))
           (org-link-maint--check-search (org-link-maint--fi-get abs) search))))))))

;;;; Descriptions

(defun org-link-maint--target-fi (parsed source)
  "Index entry of the Org note the link PARSED in SOURCE points to, or nil."
  (pcase parsed
    (`("id" ,id ,_)
     (when-let* ((node (org-link-maint--id-node id)))
       (org-link-maint--fi-get (car node))))
    (`(,(and type (guard (member type org-link-maint--file-types))) ,path ,_)
     (unless (string-empty-p path)
       (org-link-maint--org-file path source)))))

(defun org-link-maint--desc-target (parsed fi)
  "Return (KIND TITLE HEADING) if the link PARSED has a checked description.
FI is the index entry of the note it points to.  KIND is a key of
`org-link-maint-description-formats'.  TITLE is the note's title,
HEADING the heading's title (nil for a whole file)."
  (pcase parsed
    (`("id" ,id ,search)
     (when-let* ((fi fi) (node (org-link-maint--id-node id)))
       (if (and search (not (string-empty-p search)))
           (org-link-maint--search-target fi search)
         (list 'id (org-link-maint--fi-title fi) (cdr node)))))
    (`(,(and type (guard (member type org-link-maint--file-types))) ,_ ,search)
     (when fi
       (if (or (null search) (string-empty-p search))
           (list 'file (org-link-maint--fi-title fi) nil)
         (org-link-maint--search-target fi search))))))

(defun org-link-maint--search-target (fi search)
  "Return (KIND TITLE HEADING) for a \"::#custom-id\" or \"::*Heading\"
SEARCH in the note FI, whether the link is id: or file:; else nil."
  (let ((title (org-link-maint--fi-title fi)))
    (cond
     ((string-prefix-p "#" search)
      (when-let* ((heading (gethash (downcase (substring search 1))
                                    (org-link-maint--fi-cids fi))))
        (list 'custom-id title heading)))
     ((string-prefix-p "*" search)
      (when-let* ((heading (gethash (org-link-maint--join (substring search 1))
                                    (org-link-maint--fi-headings fi))))
        (list 'heading title heading))))))

(defvar org-link-maint--format-parts (make-hash-table :test #'equal)
  "FORMAT -> its pieces: strings, and the symbols `title' and `heading'.")

(defun org-link-maint--render (format title heading)
  "Fill FORMAT with TITLE (%t) and HEADING (%h)."
  (let ((parts (or (gethash format org-link-maint--format-parts)
                   (puthash format
                            (let ((pos 0) out)
                              (while (string-match "%[th%]" format pos)
                                (push (substring format pos (match-beginning 0)) out)
                                (push (pcase (match-string 0 format)
                                        ("%t" 'title) ("%h" 'heading) (_ "%"))
                                      out)
                                (setq pos (match-end 0)))
                              (push (substring format pos) out)
                              (nreverse out))
                            org-link-maint--format-parts))))
    (mapconcat (lambda (p) (pcase p ('title title) ('heading heading) (_ p)))
               parts "")))

(defun org-link-maint--expected-desc (desc target)
  "Return the description DESC should be, or nil if DESC is right.
TARGET is from `org-link-maint--desc-target'."
  (pcase-let* ((`(,kind ,title ,heading) target)
               (format (alist-get kind org-link-maint-description-formats)))
    (when format
      (let ((want (org-link-maint--render format title (or heading title))))
        (unless (equal desc want) want)))))

(defun org-link-maint--short-id (id)
  "ID shortened for messages."
  (if (> (length id) 12) (concat (substring id 0 8) "…") id))

(defun org-link-maint--better-link (parsed fi desc)
  "If the link PARSED (into note FI) should use a stronger kind of link,
return (NEW-LINK NEW-DESC REASON); else nil.  NEW-DESC is the
description NEW-LINK should have (nil if DESC is nil)."
  (pcase-let ((`(,type ,path ,search) parsed))
    (when (and fi search (not (string-empty-p search)))
      (let* ((title (org-link-maint--fi-title fi))
             (prefix (concat type ":" path))
             (better
              (cond
               ((string-prefix-p "#" search)
                (let ((cid (downcase (substring search 1))))
                  (when-let* ((id (gethash cid (org-link-maint--fi-cid-ids fi))))
                    (list (concat "id:" id) 'id
                          (gethash cid (org-link-maint--fi-cids fi))
                          "heading has an ID: use an id: link"))))
               ((string-prefix-p "*" search)
                (let* ((key (org-link-maint--join (substring search 1)))
                       (props (gethash key (org-link-maint--fi-hprops fi)))
                       (heading (gethash key (org-link-maint--fi-headings fi))))
                  (cond
                   ((car props)
                    (list (concat "id:" (car props)) 'id heading
                          "heading has an ID: use an id: link"))
                   ((cdr props)
                    (list (concat prefix "::#" (cdr props)) 'custom-id heading
                          (format "heading has a CUSTOM_ID: use ::#%s"
                                  (cdr props))))))))))
        (pcase better
          (`(,link ,kind ,heading ,reason)
           (let ((format (alist-get kind org-link-maint-description-formats)))
             (list link
                   (and desc (if format
                                 (org-link-maint--render format title heading)
                               desc))
                   reason))))))))

;;;;; Descriptions to keep

(defun org-link-maint--ignored ()
  "Set of (FILE LINK DESCRIPTION) entries to leave alone, as a hash table."
  (let ((h (make-hash-table :test #'equal)))
    (when (file-readable-p org-link-maint-ignore-file)
      (dolist (entry (ignore-errors
                       (with-temp-buffer
                         (insert-file-contents org-link-maint-ignore-file)
                         (read (current-buffer)))))
        (puthash entry t h)))
    h))

(defun org-link-maint--ignore (hit)
  "Keep HIT's link as it is: never report it again."
  (pcase-let* ((`(,file ,_ ,_ ,label ,_ ,_ ,kind ,_ ,link . ,_) hit)
               (desc (if (eq kind ?t) (nth 10 hit) label))
               (entry (list file link desc))
               (entries (hash-table-keys (org-link-maint--ignored))))
    (unless (member entry entries)
      (with-temp-file org-link-maint-ignore-file
        (let ((print-length nil) (print-level nil))
          (pp (cons entry entries) (current-buffer)))))))

;;;; Scanning

(defvar org-link-maint--edges 'off
  "While a full scan checks links, the (FROM-FI . TO-FI) note links found.")

(defun org-link-maint--check-links (links commands)
  "Check LINKS, (FILE LINE OFFSET LINK DESC PARSED) entries.  Return hits.
A hit is (FILE LINE OFFSET LABEL REASON TYPE KIND NEW LINK . MORE):
KIND is ?b for a broken link, ?d for an outdated description (NEW is
the description it should have), ?t for a link that should use a
stronger link type (MORE is (NEW-LINK DESC): the link it should be,
and its current description; NEW is the description for NEW-LINK)."
  (let ((ignored (org-link-maint--ignored))
        (source-fi nil) (source nil)
        hits)
    (pcase-dolist (`(,file ,line ,off ,link ,desc ,parsed) links)
      (unless (eq file source)
        (setq source file
              source-fi (gethash file org-link-maint--index)))
      (let* ((reason (and parsed
                          (org-link-maint--broken-reason parsed file commands)))
             (target-fi (and parsed (org-link-maint--target-fi parsed file)))
             (label (or desc (and (not (string-prefix-p "id:" link))
                                  (org-link-unescape link)))))
        ;; Note-to-note links, for `org-link-maint--orphan-hits'.
        (when (and (listp org-link-maint--edges) source-fi target-fi
                   (not (eq source-fi target-fi)))
          (push (cons source-fi target-fi) org-link-maint--edges))
        (cond
         (reason
          (push (list file line off label reason (car parsed) ?b nil link) hits))
         ((when-let* ((better (and org-link-maint-check-link-types parsed
                                   (not (gethash (list file link desc) ignored))
                                   (org-link-maint--better-link parsed target-fi desc))))
            (pcase-let ((`(,new-link ,new-desc ,why) better))
              (push (list file line off label why (car parsed) ?t new-desc link
                          new-link desc)
                    hits))))
         (t
          (when-let* ((desc desc)
                      (target (and parsed (org-link-maint--desc-target parsed target-fi)))
                      (new (org-link-maint--expected-desc desc target))
                      ((not (gethash (list file link desc) ignored))))
            (push (list file line off desc (concat "→ " new) (car parsed) ?d new link)
                  hits))))))
    (nreverse hits)))

(defun org-link-maint--links-of (files)
  "All links in FILES as (FILE LINE OFFSET LINK DESC PARSED)."
  (cl-loop for file in files
           for fi = (org-link-maint--fi-get file)
           when fi
           append (mapcar (lambda (l) (cons file l)) (org-link-maint--fi-links fi))))

(defun org-link-maint--hash (alist)
  "Hash table from ALIST."
  (let ((h (make-hash-table :test #'equal)))
    (pcase-dolist (`(,k . ,v) alist) (puthash k v h))
    h))

(defun org-link-maint--alist (hash &optional keys)
  "ALIST from HASH, limited to KEYS when given."
  (if keys
      (cl-loop for k in keys
               for v = (gethash k hash)
               when v collect (cons k v))
    (let (out)
      (maphash (lambda (k v) (push (cons k v) out)) hash)
      out)))

;;;; Reachability: orphan notes

(defcustom org-link-maint-check-orphans t
  "When non-nil, scans also report notes nothing leads to.
A note is a file.  Notes with a TYPE property (a file-level
\":TYPE: activity\", or \"#+PROPERTY: TYPE activity\") are entry points;
every other note must be linked from somewhere:
- orphan:      no TYPE and no backlinks at all;
- unreachable: has backlinks, but following them back never reaches
               a note whose TYPE is in `org-link-maint-root-types'
               (e.g. notes that only link to each other).
A link to a heading counts as a link to its note.  Only notes under
`my/notes-dir' are reported; links from any scanned file count."
  :type 'boolean)

(defcustom org-link-maint-root-types '("hub" "activity")
  "TYPE values that chains of links may end at (case-insensitive).
t means any TYPE.  Notes with another TYPE are never reported
themselves, but a note linked only from them is unreachable."
  :type '(choice (const :tag "Any TYPE" t) (repeat string)))

(defun org-link-maint--root-p (type)
  "Non-nil if a note with TYPE ends a chain of links."
  (and type
       (or (eq org-link-maint-root-types t)
           (member (downcase type) (mapcar #'downcase org-link-maint-root-types)))))

(defcustom org-link-maint-orphan-exclude-directories nil
  "Folders whose notes are never reported as orphan or unreachable.
Relative names are taken from `my/notes-dir', e.g. (\"references\").
Subfolders are excluded too.  Links from these notes still count."
  :type '(repeat directory))

(defun org-link-maint--note-p (file)
  "Non-nil if FILE is a note that must be reachable.
That is, under `my/notes-dir' and not in
`org-link-maint-orphan-exclude-directories'."
  (let ((notes (and (boundp 'my/notes-dir) my/notes-dir)))
    (and (or (not notes) (file-in-directory-p file notes))
         (not (cl-some (lambda (dir)
                         (file-in-directory-p
                          file (expand-file-name dir (or notes default-directory))))
                       org-link-maint-orphan-exclude-directories)))))

(defun org-link-maint--orphan-hits (files edges)
  "Orphan and unreachable notes among FILES, as hits.
EDGES are (FROM-FI . TO-FI) links between notes, recorded while the
links were checked.  A hit is (FILE 1 0 TITLE REASON \"note\" KIND nil
nil), KIND ?o (orphan) or ?u (unreachable)."
  (let ((out (make-hash-table :test #'eq))     ; note -> notes it links to
        (in (make-hash-table :test #'eq))      ; note -> notes linking to it
        (queue nil)
        (reached (make-hash-table :test #'eq))
        hits)
    (pcase-dolist (`(,from . ,to) edges)
      (cl-pushnew to (gethash from out) :test #'eq)
      (cl-pushnew from (gethash to in) :test #'eq))
    ;; Walk forward from every root.
    (dolist (file files)
      (let ((fi (gethash file org-link-maint--index)))
        (when (and fi (org-link-maint--root-p (org-link-maint--fi-type fi)))
          (puthash fi t reached)
          (push fi queue))))
    (while queue
      (dolist (next (gethash (pop queue) out))
        (unless (gethash next reached)
          (puthash next t reached)
          (push next queue))))
    (dolist (file files)
      (let ((fi (gethash file org-link-maint--index)))
        (when (and fi
                   (not (org-link-maint--fi-type fi))
                   (not (gethash fi reached))
                   (org-link-maint--note-p file))
          (let ((from (gethash fi in))
                (title (org-link-maint--fi-title fi)))
            (push (if from
                      (list file 1 0 title
                            (format "unreachable: linked only from %s%s"
                                    (mapconcat
                                     #'org-link-maint--fi-title
                                     (seq-take from 3) ", ")
                                    (if (> (length from) 3)
                                        (format " and %d more" (- (length from) 3))
                                      ""))
                            "note" ?u nil nil)
                    (list file 1 0 title "orphan: no TYPE, no backlinks"
                          "note" ?o nil nil))
                  hits)))))
    (nreverse hits)))

;;;; Cache of scan results

(defcustom org-link-maint-cache-file
  (locate-user-emacs-file "org-link-maint-cache.eld")
  "File holding the results of the last full scan.
Also keeps the answers of external link checks."
  :type 'file)

(defvar org-link-maint--cache nil
  "Last scan results: plist (:time :files :hits :commands).")

(defun org-link-maint--cache-write (cache)
  "Write CACHE to `org-link-maint-cache-file' atomically."
  (let ((tmp (make-temp-file
              (expand-file-name "org-link-maint-"
                                (file-name-directory org-link-maint-cache-file)))))
    (with-temp-file tmp
      (let ((print-length nil) (print-level nil) (print-circle nil))
        (prin1 cache (current-buffer))))
    (rename-file tmp org-link-maint-cache-file t)))

(defun org-link-maint--cache-current ()
  "Return the newest cache: in memory, or on disk if that is newer."
  (let ((disk (and (file-readable-p org-link-maint-cache-file)
                   (or (null org-link-maint--cache)
                       (time-less-p (seconds-to-time
                                     (plist-get org-link-maint--cache :time))
                                    (file-attribute-modification-time
                                     (file-attributes org-link-maint-cache-file))))
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

(defun org-link-maint--full-scan (&optional fresh-commands)
  "Check every link in every Org file; store, save and return the cache.
Answers of external checks from the last scan are reused unless
FRESH-COMMANDS (background scans always ask again)."
  (org-link-maint--scanning
    (let* ((start (float-time))
           (prev (org-link-maint--cache-current))
           (commands (org-link-maint--hash
                      (and (not fresh-commands) (plist-get prev :commands))))
           (files (org-link-maint-files)))
      (org-link-maint--index-update files)
      (org-link-maint--sync-org-id)
      (let* ((links (org-link-maint--links-of files))
             (used (org-link-maint--prefetch-commands links commands))
             (org-link-maint--edges nil)
             (hits (org-link-maint--check-links links commands))
             (hits (append hits
                           (and org-link-maint-check-orphans
                                (org-link-maint--orphan-hits
                                 files org-link-maint--edges))))
             (cache (list :time start :files files :hits hits
                          :commands (org-link-maint--alist commands used))))
        (org-link-maint--cache-write cache)
        (setq org-link-maint--cache cache)))))

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
Your ID database (`org-id-locations-file') is read but never written.
Normally started by `org-link-maint-scan-async'.  From cron:

  emacs -Q --batch \\
    --eval \\='(setq my/notes-dir \"~/notes/\")\\=' \\
    -l /path/to/org-link-maint.el -f org-link-maint-batch-scan"
  (unless noninteractive
    (user-error "Use `org-link-maint-scan-async' inside Emacs"))
  (setq org-link-maint--batch t
        org-id-track-globally t)
  (when (file-readable-p org-id-locations-file)
    (org-id-locations-load))
  ;; Never write the real ID database from here.
  (remove-hook 'kill-emacs-hook #'org-id-locations-save)
  (setq org-id-locations-file (make-temp-file "org-link-maint-ids"))
  (unwind-protect
      (let ((cache (org-link-maint--full-scan t)))
        (message "org-link-maint: %d problem(s) in %d file(s)"
                 (length (plist-get cache :hits))
                 (length (plist-get cache :files))))
    (delete-file org-id-locations-file)))

(defun org-link-maint--batch-settings ()
  "Return a form that gives the batch Emacs the same relevant settings."
  `(progn
     (setq load-path ',load-path)
     (setq org-id-locations-file ,(expand-file-name org-id-locations-file)
           org-id-locations-file-relative ,org-id-locations-file-relative
           org-link-maint-cache-file ,(expand-file-name org-link-maint-cache-file)
           org-todo-keywords ',org-todo-keywords
           org-link-maint-description-formats ',org-link-maint-description-formats
           org-link-maint-check-link-types ',org-link-maint-check-link-types
           org-link-maint-ignore-file ,(expand-file-name org-link-maint-ignore-file))
     (setq org-link-maint-check-orphans ',org-link-maint-check-orphans
           org-link-maint-root-types ',org-link-maint-root-types
           org-link-maint-orphan-exclude-directories
           ',org-link-maint-orphan-exclude-directories)
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
`new'    - only for problems that weren't known before
           (from the previous scan, or already reported on save);
`always' - whenever any problem exists;
nil      - never."
  :type '(choice (const :tag "New problems" new)
                 (const :tag "Any problems" always)
                 (const :tag "Never" nil)))

(defcustom org-link-maint-notify-max-items 5
  "How many problems a notification lists by name."
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

(defun org-link-maint--count-phrase (hits)
  "\"2 broken links, 1 outdated description\" for HITS."
  (let ((b (cl-count ?b hits :key (lambda (h) (or (nth 6 h) ?b))))
        (d (cl-count ?d hits :key (lambda (h) (nth 6 h))))
        (o (cl-count ?o hits :key (lambda (h) (nth 6 h))))
        (tp (cl-count ?t hits :key (lambda (h) (nth 6 h))))
        (u (cl-count ?u hits :key (lambda (h) (nth 6 h)))))
    (let ((parts (delq nil (list (and (> b 0) (format "%d broken link%s"
                                                      b (if (= b 1) "" "s")))
                                 (and (> d 0) (format "%d outdated description%s"
                                                      d (if (= d 1) "" "s")))
                                 (and (> tp 0) (format "%d wrong link type%s"
                                                       tp (if (= tp 1) "" "s")))
                                 (and (> o 0) (format "%d orphan note%s"
                                                      o (if (= o 1) "" "s")))
                                 (and (> u 0) (format "%d unreachable note%s"
                                                      u (if (= u 1) "" "s")))))))
      (and parts (string-join parts ", ")))))

(defun org-link-maint--notify (known hits)
  "Notify about HITS, per `org-link-maint-notify'.
KNOWN are the hits known before the scan."
  (let* ((known-keys (mapcar #'org-link-maint--hit-key known))
         (fresh (cl-remove-if (lambda (h) (member (org-link-maint--hit-key h)
                                                  known-keys))
                              hits))
         (shown (if (eq org-link-maint-notify 'always) hits fresh)))
    (when (and org-link-maint-notify shown)
      (org-link-maint--send-notification
       (concat (if fresh "New in your notes: " "Your notes: ")
               (org-link-maint--count-phrase shown))
       (concat
        (mapconcat
         (pcase-lambda (`(,file ,_line ,_off ,label ,reason ,_type ,kind . ,_))
           (format (if (eq kind ?d) "• %s %s (%s)" "• %s — %s (%s)")
                   (or label "(no description)") reason
                   (file-name-nondirectory file)))
         (seq-take shown org-link-maint-notify-max-items)
         "\n")
        (if (> (length shown) org-link-maint-notify-max-items)
            (format "\n…and %d more"
                    (- (length shown) org-link-maint-notify-max-items))
          "")
        "\nM-x org-link-maint-consult")))))

;;;###autoload
(defun org-link-maint-scan-async (&optional quiet)
  "Run a full scan in a separate, low-priority Emacs process.
Emacs stays usable while it runs.  Unless QUIET, report when done."
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
           ;; What was already known, so only new problems are notified.
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
                         (message "org-link-maint: %s"
                                  (or (org-link-maint--count-phrase
                                       (plist-get cache :hits))
                                      "no problems")))
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

;;;; Fixing descriptions

(defvar org-link-maint--replace-all nil
  "Non-nil once the user answered `!' during a query.")

(defun org-link-maint--ask (question)
  "Show the link matched last and ask QUESTION about changing it.
Return ?y, ?n, ?! (all remaining), ?q (quit) or ?i (keep it for good)."
  (let* ((beg (match-beginning 0))
         (ov (make-overlay beg (match-end 0))))
    (overlay-put ov 'face 'query-replace)
    (unwind-protect
        (save-match-data
          (save-excursion (goto-char beg) (org-reveal))
          (set-window-point (display-buffer (current-buffer)) beg)
          (read-char-choice
           (concat question " (y/n/!/q, i = keep for good) ")
           '(?y ?n ?! ?q ?i)))
      (delete-overlay ov))))

(defun org-link-maint--link-question (reason new-link new)
  "How to ask about replacing a link with NEW-LINK (description NEW)."
  (format "%s%s — change to [[%s]%s]?"
          (upcase (substring reason 0 1)) (substring reason 1)
          ;; Long IDs only make the question hard to read.
          (if (string-match "\\`id:\\([^:]+\\)" new-link)
              (replace-match (org-link-maint--short-id (match-string 1 new-link))
                             t t new-link 1)
            new-link)
          (if new (format "[%s]" new) "")))

(defun org-link-maint--fix-hit (hit &optional ask)
  "Fix HIT: its description, or for ?t hits the whole link.  If ASK, ask.
Return ?y (changed), ?n (skipped), ?i (kept for good) or ?q (quit).
Only HIT's file is opened, and nothing happens if the link is no
longer there."
  (pcase-let* ((`(,file ,line ,off ,label ,reason ,_ ,kind ,new ,link . ,_) hit)
               (new-link (and (eq kind ?t) (nth 9 hit)))
               (old (if new-link (nth 10 hit) label)))
    (with-current-buffer (find-file-noselect file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (forward-line (1- line))
       (forward-char (min off (- (line-end-position) (point))))
       (let ((pos (point)))
         (cl-flet ((at-link ()
                     ;; The link is still exactly as scanned.
                     (goto-char pos)
                     (and (looking-at org-link-bracket-re)
                          (equal (match-string-no-properties 1) link)
                          (equal (match-string-no-properties 2) old))))
           (if (not (at-link))
               ?n
             (let ((answer
                    (if (and ask (not org-link-maint--replace-all))
                        (org-link-maint--ask
                         (if new-link
                             (org-link-maint--link-question reason new-link new)
                           (format "Description \"%s\" → \"%s\"?" old new)))
                      ?y)))
               (when (eq answer ?!)
                 (setq org-link-maint--replace-all t
                       answer ?y))
               (when (eq answer ?i)
                 (org-link-maint--ignore hit))
               (when (eq answer ?y)
                 ;; Match again: asking may have run other searches.
                 (cond ((not (at-link)) (setq answer ?n))
                       (new-link (replace-match (org-link-make-string new-link new)
                                                t t))
                       (t (replace-match new t t nil 2))))
               answer))))))))

(defun org-link-maint--save-and-forget (fixed &optional kept)
  "Save buffers changed for the FIXED hits; drop them and KEPT from the cache."
  (when org-link-maint-save-buffers
    (dolist (file (delete-dups (mapcar #'car fixed)))
      (let ((buf (find-buffer-visiting file)))
        (when (and buf (buffer-modified-p buf))
          (with-current-buffer buf (save-buffer))))))
  (when org-link-maint--cache
    (setq org-link-maint--cache
          (plist-put (copy-sequence org-link-maint--cache) :hits
                     (cl-set-difference (plist-get org-link-maint--cache :hits)
                                        (append fixed kept) :test #'equal)))))

;;;###autoload
(defun org-link-maint-fix (&optional no-query)
  "Fix every outdated description and wrong link type in all Org files.
Asks for each one: y = yes, n = no, ! = all remaining, q = quit,
i = keep this link as it is for good (it won't be reported again).
With prefix argument NO-QUERY, fix them all without asking.
See `org-link-maint-description-formats' and
`org-link-maint-check-link-types' for what is expected."
  (interactive "P")
  (let* ((cache (org-link-maint--full-scan))
         (hits (cl-remove-if-not (lambda (h) (memq (nth 6 h) '(?d ?t)))
                                 (plist-get cache :hits)))
         ;; Last link first, so earlier positions stay valid.
         (hits (sort hits (lambda (a b)
                            (if (equal (car a) (car b))
                                (or (> (nth 1 a) (nth 1 b))
                                    (and (= (nth 1 a) (nth 1 b))
                                         (> (nth 2 a) (nth 2 b))))
                              (string< (car a) (car b))))))
         (org-link-maint--replace-all no-query)
         fixed kept)
    (if (null hits)
        (message "No outdated descriptions or wrong link types")
      (cl-loop for hit in hits
               for answer = (org-link-maint--fix-hit hit (not no-query))
               do (pcase answer
                    (?y (push hit fixed))
                    (?i (push hit kept)))
               until (eq answer ?q))
      (org-link-maint--save-and-forget fixed kept)
      (message "Fixed %d of %d link(s)%s"
               (length fixed) (length hits)
               (if kept (format ", kept %d" (length kept)) "")))))

(defalias 'org-link-maint-fix-descriptions #'org-link-maint-fix)

;;;; Checking on save

(defcustom org-link-maint-check-on-save t
  "When non-nil, `org-link-maint-mode' checks an Org file's links on save."
  :type 'boolean)

(defvar-local org-link-maint--last-count nil
  "Broken links found in this buffer at the previous save.
nil until the first save; then the last scan's count is used.")

(defun org-link-maint--broken-count (hits file skip)
  "Broken links among HITS in FILE, ignoring command types when SKIP."
  (cl-count-if (lambda (h)
                 (and (equal (car h) file)
                      (eq (or (nth 6 h) ?b) ?b)
                      (not (and skip (assoc (nth 5 h)
                                            org-link-maint-command-link-types)))))
               hits))

(defun org-link-maint--after-save ()
  "Check links in the Org file just saved; say so if new ones broke.
Only this file is read again; link targets come from the index (or
Org's ID database, until the first full scan has built the index)."
  (when (and org-link-maint-check-on-save
             (derived-mode-p 'org-mode)
             buffer-file-name)
    (org-link-maint--scanning
    (let* ((file (expand-file-name buffer-file-name))
           (skip (not org-link-maint-check-on-save-command-types))
           (cache (org-link-maint--cache-current))
           (commands (org-link-maint--hash (plist-get cache :commands)))
           (_ (org-link-maint--fi-get file))
           (links (org-link-maint--links-of (list file)))
           (hits (let ((org-link-maint--skip-command-types skip))
                   (org-link-maint--check-links links commands)))
           (count (org-link-maint--broken-count hits file nil))
           (before (or org-link-maint--last-count
                       (org-link-maint--broken-count
                        (plist-get cache :hits) file skip))))
      (org-link-maint--sync-org-id file)
      (when cache
        (let ((new (copy-sequence cache)))
          (setq new (plist-put
                     new :hits
                     (append
                      (cl-remove-if
                       (lambda (h)
                         (and (equal (car h) file)
                              ;; Orphan results depend on other files: keep.
                              (memq (or (nth 6 h) ?b) '(?b ?d ?t))
                              ;; Command types weren't rechecked: keep them.
                              (not (and skip (assoc (nth 5 h)
                                                    org-link-maint-command-link-types)))))
                       (plist-get cache :hits))
                      hits)))
          (setq org-link-maint--cache new)))
      (when (> count before)
        (message "%d broken link(s) in this file (M-x org-link-maint-consult)"
                 count))
      (setq org-link-maint--last-count count)))
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
  "Keep an eye on link problems.
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
(defvar consult--narrow)

(defun org-link-maint--loc-marker (file line off)
  "Return a marker at LINE, character OFF in FILE (opening it if needed)."
  (with-current-buffer (find-file-noselect file)
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (forward-line (1- line))
        (forward-char (min off (- (line-end-position) (point))))
        (point-marker)))))

;;;###autoload
(defun org-link-maint-consult (&optional fresh)
  "Pick a link problem in any Org file with consult, with live preview.
Shows broken links and outdated descriptions; narrow with \"b\" or \"d\".
Choosing an outdated description or wrong link type offers to fix it.

Scans everything first (fast: only changed files are read).  Answers
of external link checks are reused from the last scan; with prefix
argument FRESH they are asked again."
  (interactive "P")
  (unless (require 'consult nil t)
    (user-error "This command needs the consult package"))
  (let* ((cache (org-link-maint--full-scan fresh))
         (hits (plist-get cache :hits)))
    (if (null hits)
        (message "No link problems in %d file(s)"
                 (length (plist-get cache :files)))
      (let* ((candidates
              (mapcar
               (lambda (hit)
                 (pcase-let ((`(,file ,line ,_ ,label ,reason . ,_) hit))
                   ;; The candidate is "file:line: label" so that it is
                   ;; unique, but only the label is displayed (see :group).
                   (let ((prefix (format "%s:%d: " (abbreviate-file-name file) line)))
                     (propertize (concat prefix
                                         (or label
                                             (propertize "(no description)"
                                                         'face 'shadow)))
                                 'org-link-maint-hit hit
                                 'org-link-maint-kind (or (nth 6 hit) ?b)
                                 'org-link-maint-file (abbreviate-file-name file)
                                 'org-link-maint-reason reason
                                 'org-link-maint-prefix (length prefix)))))
               hits))
             (selected nil)
             (marker
              (consult--read
               candidates
               :prompt (format "Link problems (%s): "
                               (org-link-maint--count-phrase hits))
               :category 'org-link-maint-problem
               :sort nil
               :require-match t
               :narrow (list :predicate
                             (lambda (cand)
                               (eq (get-text-property 0 'org-link-maint-kind cand)
                                   consult--narrow))
                             :keys '((?b . "Broken") (?d . "Description") (?t . "Link type")
                                     (?o . "Orphan") (?u . "Unreachable")))
               ;; Dimmed reason: "missing file", "→ New description", ...
               :annotate (lambda (cand)
                           (concat "  " (propertize
                                         (get-text-property
                                          0 'org-link-maint-reason cand)
                                         'face 'shadow)))
               ;; Group by file; show only the label inside each group.
               :group (lambda (cand transform)
                        (if transform
                            (substring cand (get-text-property
                                             0 'org-link-maint-prefix cand))
                          (get-text-property 0 'org-link-maint-file cand)))
               ;; Turn the selected candidate into a marker for preview/jump.
               :lookup (lambda (sel candidates &rest _)
                         (when-let* ((cand (car (member sel candidates)))
                                     (hit (get-text-property
                                           0 'org-link-maint-hit cand)))
                           (setq selected hit)
                           (pcase-let ((`(,file ,line ,off . ,_) hit))
                             (org-link-maint--loc-marker file line off))))
               :state (consult--jump-state))))
        ;; The jump state normally jumps on return already; make sure.
        (when (and (markerp marker)
                   (not (and (eq (current-buffer) (marker-buffer marker))
                             (= (point) marker))))
          (consult--jump marker))
        (when (and selected (memq (nth 6 selected) '(?d ?t)))
          (let ((org-link-maint--replace-all nil))
            (pcase (org-link-maint--fix-hit selected t)
              (?y (org-link-maint--save-and-forget (list selected)))
              (?i (org-link-maint--save-and-forget nil (list selected))
                  (message "Kept; it won't be reported again")))))))))

(provide 'org-link-maint)
;;; org-link-maint.el ends here
