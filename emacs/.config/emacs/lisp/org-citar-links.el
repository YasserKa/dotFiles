;;; org-citar-links.el --- Citation links between bibliographic notes: citar, sidecar, embark -*- lexical-binding: t; -*-

;; Requires: org-backlinks.el (same directory), citar, consult, universal-sidecar.

;;; Commentary:
;;
;; Treats org-cite citations ([cite:@key], [cite/t:@a;@b], ...) as links
;; between bibliographic notes, the way org-backlinks.el and
;; org-forwardlinks.el treat [[id:...]] and [[file:...]] links.
;;
;; A "paper node" is a node whose bibliographic key is known from:
;;   - a ROAM_REFS or CITE_KEY property on the node's heading (with an :ID:),
;;   - for the file-level node: :ROAM_REFS: / :CITE_KEY: in the file property
;;     drawer, #+ROAM_REFS:, #+CITE_KEY: or #+REFERENCE: (citar-denote),
;;   - citar's notes source (`citar-get-notes'), e.g. citar-file or org-roam,
;;   - or a file name equal to a citekey in your bibliography.
;; "Node" has the same meaning as in org-backlinks.el: the closest heading
;; (self or ancestor) with an :ID:, or the file-level node.
;;
;; Paper results are shown with `citar-select-refs', i.e. the same display
;; as `org-cite-insert' / `citar-insert-citation' (author, year, title,
;; indicators), so citar's searches such as has:notes and is:cited work.
;; The selected papers are passed to `my/org-cl-action' (citar's default
;; action, `citar-open', unless changed).
;;
;; Commands in Org buffers:
;;   C-c n c   Papers cited in the node at point            (C-u: whole file)
;;   C-c n C   Papers whose notes cite this note's paper    (C-u: all papers in file)
;;   C-c n w   Where this note's paper is cited (consult)   (C-u: all papers in file)
;;
;; Embark, on a citation at point or a citar candidate:
;;   F   Papers cited in the notes of that paper   (forward)
;;   B   Papers whose notes cite that paper        (backward)
;;   W   Where that paper is cited (consult)
;;
;; Sidecar: add `org-citar-section' to `universal-sidecar-sections'.  It
;; follows `my/org-bl-scope', so C-c b / C-c B switch it too.

;;; Code:

(require 'org)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'consult)
(require 'universal-sidecar)
(require 'citar)
(require 'org-backlinks)

;;;; Customization ----------------------------------------------------------

(defgroup my/org-cl nil
  "Citation links between bibliographic notes."
  :group 'org)

(defcustom my/org-cl-action #'citar-run-default-action
  "Function called with the list of citekeys chosen from a paper prompt."
  :type 'function
  :group 'my/org-cl)

(defcustom my/org-cl-include-notes t
  "Non-nil: \"cited by\" also lists non-paper notes citing the paper.
These are notes without a citekey of their own, e.g. synthesis notes."
  :type 'boolean
  :group 'my/org-cl)

(defcustom my/org-cl-preview-return 'action
  "What RET does in the paper prompts that preview citations.
`action': call `my/org-cl-action' on the chosen paper (default).
`location': stay at the previewed citation."
  :type '(choice (const :tag "Run the paper action" action)
                 (const :tag "Jump to the citation" location))
  :group 'my/org-cl)

(defcustom my/org-cl-paper-template "${author editor:%etal} (${year issued date})"
  "Citar template for the author/year part of a paper line in the sidecar."
  :type 'string
  :group 'my/org-cl)

(defcustom my/org-cl-title-template "${title}"
  "Citar template for the title part of a paper line in the sidecar."
  :type 'string
  :group 'my/org-cl)

(defface my/org-cl-heading
  '((t :inherit font-lock-type-face :weight bold))
  "Group headings in the citations sidecar section."
  :group 'my/org-cl)

(defface my/org-cl-title
  '((t :inherit default :slant italic))
  "Paper titles in the sidecar."
  :group 'my/org-cl)

(defface my/org-cl-has-note
  '((t :inherit success))
  "Indicator for papers that have notes."
  :group 'my/org-cl)

;;;; Parsing citations and keys ---------------------------------------------

(defconst my/org-cl--cite-re "\\[cite\\(?:/[^]:\n]*\\)?:\\([^]\n]*\\)\\]"
  "Regexp matching an org-cite citation; group 1 is its contents.")

(defun my/org-cl--keys-in-cite (inner)
  "Return the citekeys in the citation contents INNER."
  (let ((re (if (boundp 'org-element-citation-key-re)
                org-element-citation-key-re
              "@\\([^];[:space:]]+\\)"))
        (i 0) keys)
    (while (string-match re inner i)
      (push (match-string-no-properties 1 inner) keys)
      (setq i (match-end 0)))
    (nreverse keys)))

(defun my/org-cl--parse-refs (value &optional bare-ok)
  "Extract citekeys from the property VALUE.
Keys written as @key or cite:key are always found.  With BARE-OK, a value
without those markers is split into bare keys (CITE_KEY, #+REFERENCE)."
  (when (and value (not (string-empty-p (string-trim value))))
    (if (string-match-p "@\\|cite:" value)
        (let ((i 0) keys)
          (while (string-match "\\(?:@\\|cite:\\)\\([^]@ \t;,\"]+\\)" value i)
            (push (match-string 1 value) keys)
            (setq i (match-end 0)))
          (nreverse keys))
      (when bare-ok
        (split-string value "[ \t;,]+" t)))))

(defconst my/org-cl--prop-re
  "^[ \t]*\\(?:#\\+\\|:\\)\\(ROAM_REFS\\|CITE_KEY\\|REFERENCE\\):[ \t]*\\(.*?\\)[ \t]*$"
  "Regexp for key-bearing properties and keywords.")

(defun my/org-cl--refs-in-region (beg end)
  "Return the citekeys declared by properties/keywords between BEG and END."
  (save-excursion
    (goto-char beg)
    (let ((case-fold-search t) keys)
      (while (re-search-forward my/org-cl--prop-re end t)
        (setq keys (append keys
                           (my/org-cl--parse-refs
                            (match-string-no-properties 2)
                            (not (string= (upcase (match-string 1)) "ROAM_REFS"))))))
      (delete-dups keys))))

(defun my/org-cl--first-heading ()
  "Position of the first heading in the buffer, or `point-max'."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^\\*+ " nil t) (match-beginning 0) (point-max))))

(defun my/org-cl--heading-refs (pos)
  "Citekeys declared on the heading at POS (its own entry, not children)."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char pos)
      (forward-line 1)
      (my/org-cl--refs-in-region
       (point)
       (save-excursion
         (if (re-search-forward "^\\*+ " nil t) (match-beginning 0) (point-max)))))))

(defun my/org-cl--all-heading-refs ()
  "Citekeys declared on any heading of the current buffer."
  (save-excursion
    (save-restriction
      (widen)
      (my/org-cl--refs-in-region (my/org-cl--first-heading) (point-max)))))

;;;; Citation index ---------------------------------------------------------

(defvar my/org-cl--index (make-hash-table :test 'equal)
  "File -> (STAMP . DATA), the cached citation index.")

(defconst my/org-cl--index-version 1
  "Bump when the shape of the index changes.")

(defun my/org-cl--scan-buffer ()
  "Index the current buffer.
Return (:file-keys KEYS :heading-refs BOOL :cites ENTRIES).  Each entry is a
plist with :key :pos :line :raw :col :heading :owner :owner-keys."
  (save-excursion
    (save-restriction
      (widen)
      (let* ((first (my/org-cl--first-heading))
             (file-keys (my/org-cl--refs-in-region (point-min) first))
             (heading-refs (save-excursion
                             (goto-char first)
                             (let ((case-fold-search t))
                               (re-search-forward
                                "^[ \t]*:\\(ROAM_REFS\\|CITE_KEY\\):" nil t))))
             (owners (make-hash-table))
             cites)
        (goto-char (point-min))
        (while (re-search-forward my/org-cl--cite-re nil t)
          (let* ((cbeg (match-beginning 0))
                 (cend (match-end 0))
                 (keys (my/org-cl--keys-in-cite (match-string-no-properties 1)))
                 (lb (line-beginning-position))
                 (line (buffer-substring-no-properties lb (line-end-position)))
                 (lead (- (length line) (length (string-trim-left line))))
                 (owner (my/org-bl--owner cbeg))
                 (okeys (when owner
                          (or (gethash owner owners)
                              (puthash owner (or (my/org-cl--heading-refs owner) :none)
                                       owners))))
                 (info (list :pos cbeg
                             :line (line-number-at-pos cbeg)
                             :raw (string-trim line)
                             :col (- cbeg lb lead)
                             :heading (save-excursion
                                        (goto-char cbeg)
                                        (when (re-search-backward "^\\*+ +\\(.*\\)$" nil t)
                                          (match-string-no-properties 1)))
                             :owner owner
                             :owner-keys (unless (eq okeys :none) okeys))))
            (dolist (k keys)
              (push (cons :key (cons k info)) cites))
            (goto-char cend)))
        (list :file-keys file-keys
              :heading-refs (and heading-refs t)
              :cites (nreverse cites))))))

(defun my/org-cl--readable-p (file)
  (or (find-buffer-visiting file) (file-exists-p file)))

(defun my/org-cl--data (file)
  "Return the (cached) citation index of FILE."
  (let* ((buf (find-buffer-visiting file))
         (stamp (cons my/org-cl--index-version
                      (if buf
                          (with-current-buffer buf (buffer-chars-modified-tick))
                        (file-attribute-modification-time (file-attributes file)))))
         (cached (gethash file my/org-cl--index)))
    (if (and cached (equal (car cached) stamp))
        (cdr cached)
      (let ((data (if buf
                      (with-current-buffer buf (my/org-cl--scan-buffer))
                    (with-temp-buffer
                      (insert-file-contents file)
                      (let ((org-mode-hook nil))
                        (delay-mode-hooks (org-mode)))
                      (my/org-cl--scan-buffer)))))
        (puthash file (cons stamp data) my/org-cl--index)
        data))))

(defun my/org-cl--cites (file)
  "Citation entries of FILE."
  (plist-get (my/org-cl--data file) :cites))

(defun my/org-cl--files (&optional extra)
  "Org files under the backlink directories and `citar-notes-paths', plus EXTRA."
  (let (files)
    (dolist (d (append (or my/org-bl-directories (list org-directory))
                       (bound-and-true-p citar-notes-paths)))
      (when (and d (file-directory-p d))
        (dolist (f (directory-files-recursively d "\\.org\\'"))
          (unless (string-match-p "\\(?:\\`\\|/\\)\\." (file-relative-name f d))
            (push (file-truename f) files)))))
    (seq-filter #'my/org-cl--readable-p
                (delete-dups (append extra (nreverse files))))))

;;;; Notes <-> keys ---------------------------------------------------------

(defvar my/org-cl--note-map nil
  "(TIME . HASH) mapping note file truenames to citekeys, from citar.")

(defun my/org-cl--note->file (note)
  "Turn a citar NOTE (file path, or org-roam node id) into a file truename."
  (cond
   ((not (stringp note)) nil)
   ((file-exists-p note) (file-truename note))
   ((fboundp 'org-roam-node-from-id)
    (when-let* ((node (ignore-errors (org-roam-node-from-id note)))
                (f (org-roam-node-file node)))
      (file-truename f)))))

(defun my/org-cl--note-map ()
  "Return a hash table note file -> citekeys, refreshed every 30 seconds."
  (if (and my/org-cl--note-map
           (< (float-time (time-since (car my/org-cl--note-map))) 30))
      (cdr my/org-cl--note-map)
    (let ((tab (make-hash-table :test 'equal))
          (notes (ignore-errors (citar-get-notes))))
      (when (hash-table-p notes)
        (maphash (lambda (key ns)
                   (dolist (n ns)
                     (when-let* ((f (my/org-cl--note->file n)))
                       (cl-pushnew key (gethash f tab) :test #'equal))))
                 notes))
      (setq my/org-cl--note-map (cons (current-time) tab))
      tab)))

(defun my/org-cl--note-files (key)
  "Note files of citekey KEY, as truenames."
  (let ((notes (ignore-errors (citar-get-notes key))))
    (delete-dups
     (delq nil (mapcar #'my/org-cl--note->file
                       (and (hash-table-p notes) (gethash key notes)))))))

(defun my/org-cl--file-keys (file)
  "Citekeys of the file-level node of FILE."
  (let ((data (my/org-cl--data file)))
    (or (plist-get data :file-keys)
        (unless (plist-get data :heading-refs)
          (gethash file (my/org-cl--note-map)))
        (let ((base (file-name-base file)))
          (and (ignore-errors (citar-get-entry base)) (list base))))))

(defun my/org-cl--entry-keys (file e)
  "Citekeys of the paper node containing the citation entry E in FILE."
  (if (plist-get e :owner)
      (plist-get e :owner-keys)
    (my/org-cl--file-keys file)))

(defun my/org-cl--keys-at (file pos)
  "Citekeys of the paper node at POS in the current buffer visiting FILE.
POS nil means all paper keys declared in the file."
  (if pos
      (let ((owner (my/org-bl--owner pos)))
        (if owner
            (my/org-cl--heading-refs owner)
          (my/org-cl--file-keys file)))
    (delete-dups (append (my/org-cl--file-keys file)
                         (my/org-cl--all-heading-refs)))))

;;;; Collecting -------------------------------------------------------------

(defun my/org-cl--self ()
  (file-truename (buffer-file-name (buffer-base-buffer))))

(defun my/org-cl--cites-here (pin)
  "Citations in the node owning PIN in the current buffer (nil: whole file).
Return an alist (KEY . ENTRIES) in order of first appearance."
  (let* ((owner (and pin (my/org-bl--owner pin)))
         (entries (my/org-cl--cites (my/org-cl--self))))
    (when pin
      (setq entries (seq-filter (lambda (e) (eql (plist-get e :owner) owner))
                                entries)))
    (seq-group-by (lambda (e) (plist-get e :key)) entries)))

(defun my/org-cl--cites-of-papers (keys)
  "Citations made in the notes of the papers KEYS, as (FILE . ENTRY)."
  (let (res found)
    (dolist (key keys)
      (dolist (f (my/org-cl--note-files key))
        (when (my/org-cl--readable-p f)
          (setq found t)
          (dolist (e (my/org-cl--cites f))
            (when (member key (my/org-cl--entry-keys f e))
              (push (cons f e) res))))))
    (unless found
      (user-error "No notes found for %s" (string-join keys ", ")))
    (nreverse res)))

(defun my/org-cl--citations-of (keys &optional extra-files)
  "All places citing any of KEYS, as (FILE . ENTRY)."
  (let (res)
    (dolist (f (my/org-cl--files extra-files))
      (dolist (e (my/org-cl--cites f))
        (when (member (plist-get e :key) keys)
          (push (cons f e) res))))
    (nreverse res)))

(defun my/org-cl--citing (keys locs)
  "Split LOCS citing KEYS into (PAPERS . OTHERS).
PAPERS is an alist (CITING-KEY . LOCS); OTHERS are LOCS in non-paper nodes.
Citations inside the notes of KEYS themselves are dropped."
  (let (papers others)
    (dolist (l locs)
      (let* ((ek (my/org-cl--entry-keys (car l) (cdr l)))
             (ck (seq-remove (lambda (k) (member k keys)) ek)))
        (cond
         (ck (dolist (k ck)
               (let ((cell (assoc k papers)))
                 (if cell
                     (setcdr cell (append (cdr cell) (list l)))
                   (push (list k l) papers)))))
         ((null ek) (push l others)))))
    (cons (nreverse papers) (nreverse others))))

;;;; Display helpers --------------------------------------------------------

(defun my/org-cl--clean (s)
  (string-trim (replace-regexp-in-string "[{}]" "" (or s ""))))

(defun my/org-cl--short (key)
  "Short label for KEY: \"Author et al. Year\", or @key if unknown."
  (let ((entry (ignore-errors (citar-get-entry key))))
    (if (not entry)
        (concat "@" key)
      (let* ((names (my/org-cl--clean (or (citar-get-value "author" entry)
                                          (citar-get-value "editor" entry))))
             (author (if (fboundp 'citar--shorten-names)
                         (citar--shorten-names names 2 "&")
                       names))
             (date (or (citar-get-value "year" entry)
                       (citar-get-value "date" entry)
                       ""))
             (year (if (>= (length date) 4) (substring date 0 4) date)))
        (string-trim (format "%s %s" author year))))))

(defun my/org-cl--fmt (template entry fallback)
  "Format ENTRY with the citar TEMPLATE, or return FALLBACK."
  (my/org-cl--clean
   (or (and (fboundp 'citar-format--entry)
            (ignore-errors (citar-format--entry template entry)))
       fallback)))

(defun my/org-cl--render (raw &optional key)
  "RAW line with citations shown as short labels and links cleaned up.
Occurrences of KEY get `my/org-bl-reference'."
  (let ((raw (or raw "")) (out "") (i 0))
    (while (string-match my/org-cl--cite-re raw i)
      (let* ((ms (match-beginning 0))
             (me (match-end 0))
             (keys (my/org-cl--keys-in-cite (match-string 1 raw))))
        (setq out (concat out
                          (substring raw i ms)
                          (mapconcat
                           (lambda (k)
                             (propertize (my/org-cl--short k) 'face
                                         (if (equal k key)
                                             'my/org-bl-reference
                                           'my/org-bl-other-link)))
                           keys "; "))
              i me)))
    (my/org-bl--render-text (concat out (substring raw i)) nil)))

(defvar my/org-cl-paper-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") #'my/org-cl-follow-paper)
    (define-key m [mouse-1] #'my/org-cl-follow-paper)
    (define-key m [follow-link] 'mouse-face)
    m)
  "Keymap for paper lines in the sidecar.")

(defun my/org-cl-open-paper (key)
  "Open the note of KEY, or run `citar-open' when it has none."
  (when (string-match-p "\\*sidecar\\*" (buffer-name))
    (when-let* ((win (get-mru-window nil nil t)))
      (select-window win)))
  (let* ((notes (ignore-errors (citar-get-notes key)))
         (ns (and (hash-table-p notes) (gethash key notes))))
    (if ns
        (citar-open-note (car ns))
      (citar-open (list key)))))

(defun my/org-cl-follow-paper (&optional event)
  "Open the paper on the line at point or clicked via EVENT."
  (interactive (list last-nonmenu-event))
  (let* ((mouse (mouse-event-p event))
         (pos (if mouse (posn-point (event-end event)) (point)))
         (win (and mouse (posn-window (event-end event))))
         (key (get-text-property pos 'my-org-cl-key
                                 (and win (window-buffer win)))))
    (when key (my/org-cl-open-paper key))))

(defun my/org-cl--paper-string (key hasnote)
  "Clickable sidecar line for the paper KEY.  HASNOTE is a citar predicate."
  (let* ((entry (ignore-errors (citar-get-entry key)))
         (label (if entry
                    (my/org-cl--fmt my/org-cl-paper-template entry (my/org-cl--short key))
                  (concat "@" key)))
         (title (if entry
                    (my/org-cl--fmt my/org-cl-title-template entry
                                    (citar-get-value "title" entry))
                  "(not in bibliography)"))
         (s (concat "  "
                    (if (and hasnote (funcall hasnote key))
                        (propertize "N " 'face 'my/org-cl-has-note)
                      "  ")
                    (propertize label 'face 'my/org-bl-node)
                    " "
                    (propertize (truncate-string-to-width title 80 nil nil "…")
                                'face 'my/org-cl-title))))
    (add-text-properties 0 (length s)
                         (list 'keymap my/org-cl-paper-map
                               'mouse-face 'highlight
                               'follow-link t
                               'help-echo "Open note (or citar-open)"
                               'my-org-cl-key key)
                         s)
    (my/org-bl--mirror-faces s)))

(defun my/org-cl--loc-string (file e &optional show-file)
  "Clickable sidecar line for the citation entry E in FILE."
  (my/org-bl--clickable
   (concat
    (propertize (if show-file
                    (format "%s:%d" (file-name-nondirectory file) (plist-get e :line))
                  (format "L%d" (plist-get e :line)))
                'face 'my/org-bl-location)
    " "
    (if-let* ((h (plist-get e :heading)))
        (propertize (format "[%s] " h) 'face 'my/org-bl-source-heading)
      "")
    (my/org-cl--render (plist-get e :raw) (plist-get e :key)))
   file (plist-get e :pos) "Jump to citation"))

(defun my/org-cl--node-title (pos)
  (save-excursion
    (save-restriction
      (widen)
      (if-let* ((o (my/org-bl--owner pos)))
          (progn (goto-char o) (org-get-heading t t t t))
        (my/org-bl--file-title)))))

;;;; Prompts ----------------------------------------------------------------

(defun my/org-cl--select-papers (keys)
  "Let the user pick among the papers KEYS with citar, then run the action."
  (let* ((keys (delete-dups (copy-sequence keys)))
         (known (seq-filter (lambda (k) (ignore-errors (citar-get-entry k))) keys))
         (tab (make-hash-table :test 'equal)))
    (unless keys (user-error "No papers found"))
    (unless known
      (user-error "Not in your bibliography: %s" (string-join keys ", ")))
    (dolist (k known) (puthash k t tab))
    (when-let* ((chosen (citar-select-refs :filter (lambda (k) (gethash k tab)))))
      (funcall my/org-cl-action chosen))))

(defun my/org-cl--consult-locations (locs prompt)
  "Pick one of the citation LOCS, as (FILE . ENTRY), with consult preview."
  (unless locs (user-error "No citations found"))
  (let ((bufs (make-hash-table :test 'equal))
        (seen (make-hash-table :test 'equal))
        cands)
    (pcase-dolist (`(,f . ,e) locs)
      (let ((id (cons f (plist-get e :line))))
        (unless (gethash id seen)
          (puthash id t seen)
          (let* ((fbuf (or (gethash f bufs)
                           (puthash f (my/org-bl-find-file-quietly f)
                                    bufs)))
                 (marker (with-current-buffer fbuf
                           (copy-marker (plist-get e :pos)))))
            (push (my/org-bl--mirror-faces
                   (propertize
                    (concat
                     (propertize (format "%s:%d " (file-name-nondirectory f)
                                         (plist-get e :line))
                                 'face 'my/org-bl-location)
                     (if-let* ((h (plist-get e :heading)))
                         (propertize (format "[%s] " h) 'face 'my/org-bl-source-heading)
                       "")
                     (my/org-cl--render (plist-get e :raw) (plist-get e :key)))
                    'consult--candidate marker
                    'my-group (file-name-nondirectory f)))
                  cands)))))
    (consult--jump
     (consult--read
      (nreverse cands)
      :prompt prompt
      :require-match t
      :sort nil
      :category 'org-citation
      :group (lambda (cand transform)
               (if transform cand (get-text-property 0 'my-group cand)))
      :lookup #'consult--lookup-candidate
      :state (consult--jump-state)))))

;;;; Paper prompt with citation preview -----------------------------------

(defun my/org-cl--group-locs (locs)
  "Group LOCS, a list of (FILE . ENTRY), into an alist (KEY . LOCS).
The key of each location is the cited key (:key of the entry)."
  (let (alist)
    (dolist (l locs)
      (let* ((k (plist-get (cdr l) :key))
             (cell (assoc k alist)))
        (if cell
            (setcdr cell (append (cdr cell) (list l)))
          (push (list k l) alist))))
    (nreverse alist)))

(defun my/org-cl--get-buffer (file cache)
  "Buffer visiting FILE, opened in the background if needed; CACHE is a table."
  (or (gethash file cache)
      (puthash file (my/org-bl-find-file-quietly file)
               cache)))

(defun my/org-cl--group-by-node (locs)
  "Group LOCS, a list of (FILE . ENTRY), by the node containing them.
Return an alist ((FILE . OWNER) . LOCS); OWNER is nil for file-level nodes."
  (let (alist)
    (dolist (l locs)
      (let* ((id (cons (car l) (plist-get (cdr l) :owner)))
             (cell (assoc id alist)))
        (if cell
            (setcdr cell (append (cdr cell) (list l)))
          (push (list id l) alist))))
    (nreverse alist)))

(defun my/org-cl--select-papers-preview (alist prompt &optional notes)
  "Pick a paper from ALIST, or a note from NOTES, with citation preview.
ALIST maps citekeys to locations (FILE . ENTRY); papers are shown with
citar's display.  NOTES maps nodes (FILE . OWNER) to locations and is shown
as a separate \"Notes\" group (synthesis notes and other non-paper notes).
Moving over a candidate shows its first location, like `consult-line'.
RET on a paper follows `my/org-cl-preview-return'; RET on a note jumps to
the citation."
  (let* ((all (if alist
                  (or (citar--format-candidates)
                      (user-error "No bibliography set"))
                (make-hash-table :test 'equal)))
         (want (make-hash-table :test 'equal))
         (order (make-hash-table :test 'equal))
         (tab (make-hash-table :test 'equal))
         (markers (make-hash-table :test 'equal))
         (bufs (make-hash-table :test 'equal))
         (i 0)
         papers note-cands)
    (pcase-dolist (`(,k . ,locs) alist)
      (puthash k locs want)
      (puthash k (cl-incf i) order))
    (maphash
     (lambda (cand key)
       (when (gethash key want)
         (let ((disp (concat (citar--make-indicator-symbols cand) cand)))
           (puthash disp key tab)
           (push disp papers))))
     all)
    (setq papers (sort papers (lambda (a b)
                                (< (gethash (gethash a tab) order)
                                   (gethash (gethash b tab) order)))))
    (pcase-dolist (`(,id . ,locs) notes)
      (let* ((file (car id))
             (buf (my/org-cl--get-buffer file bufs))
             (title (with-current-buffer buf
                      (my/org-cl--node-title (plist-get (cdr (car locs)) :pos))))
             (disp (my/org-bl--mirror-faces
                    (concat (propertize (or title (file-name-base file))
                                        'face 'my/org-bl-node)
                            (propertize (format "  %s" (file-name-nondirectory file))
                                        'face 'my/org-bl-location))))
             (key (cons :note id)))
        (while (gethash disp tab) (setq disp (concat disp " ")))
        (puthash key locs want)
        (puthash disp key tab)
        (push disp note-cands)))
    (setq note-cands (nreverse note-cands))
    (unless (or papers note-cands)
      (user-error "Not in your bibliography: %s" (mapconcat #'car alist ", ")))
    (cl-flet ((marker (key)
                (or (gethash key markers)
                    (when-let* ((loc (car (gethash key want))))
                      (puthash key
                               (with-current-buffer
                                   (my/org-cl--get-buffer (car loc) bufs)
                                 (copy-marker (plist-get (cdr loc) :pos)))
                               markers)))))
      (let* ((jump (consult--jump-state))
             (key (consult--read
                   (append papers note-cands)
                   :prompt prompt
                   :require-match t
                   :sort nil
                   :category 'org-citar-paper
                   :group (when note-cands
                            (lambda (cand transform)
                              (cond (transform cand)
                                    ((stringp (gethash cand tab)) "Papers")
                                    (t "Notes"))))
                   :annotate
                   (lambda (cand)
                     (when-let* ((locs (gethash (gethash cand tab) want))
                                 (loc (car locs)))
                       (concat
                        "  "
                        (propertize
                         (format "%s:%d%s"
                                 (file-name-nondirectory (car loc))
                                 (plist-get (cdr loc) :line)
                                 (if (cdr locs)
                                     (format " (+%d more)" (length (cdr locs)))
                                   ""))
                         'face 'my/org-bl-location))))
                   :lookup (lambda (sel &rest _) (gethash sel tab))
                   :state (lambda (action cand)
                            (funcall jump action (and cand (marker cand)))))))
        (cond
         ((null key) nil)
         ((or (consp key) (eq my/org-cl-preview-return 'location))
          (when-let* ((m (marker key)))
            (consult--jump m)))
         (t (funcall my/org-cl-action (list key))))))))

;;;; Commands on citekeys (Embark actions) ----------------------------------

;;;###autoload
(defun my/citar-cites (citekeys)
  "Papers cited in the notes of CITEKEYS (forward)."
  (interactive (list (citar-select-refs)))
  (my/org-cl--select-papers-preview
   (my/org-cl--group-locs (my/org-cl--cites-of-papers citekeys))
   (format "Cited in %s: " (mapconcat #'my/org-cl--short citekeys ", "))))

;;;###autoload
(defun my/citar-cited-by (citekeys)
  "Papers whose notes cite CITEKEYS (backward).
With `my/org-cl-include-notes', other notes citing them (e.g. synthesis
notes) are offered too, in a separate group."
  (interactive (list (citar-select-refs)))
  (let* ((citing (my/org-cl--citing
                  citekeys
                  (my/org-cl--citations-of
                   citekeys
                   (and (buffer-file-name) (list (my/org-cl--self))))))
         (papers (car citing))
         (notes (and my/org-cl-include-notes
                     (my/org-cl--group-by-node (cdr citing)))))
    (unless (or papers notes)
      (user-error "Nothing cites %s" (string-join citekeys ", ")))
    (my/org-cl--select-papers-preview
     papers
     (format "Citing %s: " (mapconcat #'my/org-cl--short citekeys ", "))
     notes)))

;;;; Commands on the node at point ------------------------------------------

(defun my/org-cl--check ()
  (unless (derived-mode-p 'org-mode) (user-error "Not in an Org buffer"))
  (unless (buffer-file-name (buffer-base-buffer))
    (user-error "Buffer is not visiting a file")))

(defun my/org-cl--paper-keys-here (whole-file)
  (or (my/org-cl--keys-at (my/org-cl--self) (unless whole-file (point)))
      (user-error "Not a bibliographic note (no citekey found)")))

;;;###autoload
(defun my/org-citar-cites-here (&optional whole-file)
  "Papers cited in the node at point.  With WHOLE-FILE, in the whole file."
  (interactive "P")
  (my/org-cl--check)
  (let ((self (my/org-cl--self)))
    (my/org-cl--select-papers-preview
     (mapcar (lambda (g)
               (cons (car g) (mapcar (lambda (e) (cons self e)) (cdr g))))
             (or (my/org-cl--cites-here (unless whole-file (point)))
                 (user-error "No citations here")))
     "Cites: ")))

;;;###autoload
(defun my/org-citar-cited-by-here (&optional whole-file)
  "Papers whose notes cite this note's paper.
With WHOLE-FILE, use every paper declared in the file."
  (interactive "P")
  (my/org-cl--check)
  (my/citar-cited-by (my/org-cl--paper-keys-here whole-file)))

;;;; Sidecar ----------------------------------------------------------------

(defun my/org-cl--sidecar-model (pos)
  "Return (:title TITLE :lines LINES) for the current buffer, or nil.
POS nil means the whole file."
  (let* ((self (my/org-cl--self))
         (paper-keys (my/org-cl--keys-at self pos))
         (cites (my/org-cl--cites-here pos))
         (citing (when paper-keys
                   (my/org-cl--citing paper-keys
                                      (my/org-cl--citations-of paper-keys (list self)))))
         (papers (car citing))
         (others (cdr citing))
         (hasnote (ignore-errors (citar-has-notes)))
         lines)
    (when (or paper-keys cites)
      (cl-flet ((hdr (s)
                  (push (my/org-bl--mirror-faces (propertize s 'face 'my/org-cl-heading))
                        lines)))
        (when paper-keys
          (hdr "This paper")
          (dolist (k paper-keys) (push (my/org-cl--paper-string k hasnote) lines))
          (push "" lines))
        (when papers
          (hdr (format "Cited by (%d)" (length papers)))
          (pcase-dolist (`(,k . ,locs) papers)
            (push (my/org-cl--paper-string k hasnote) lines)
            (pcase-dolist (`(,f . ,e) locs)
              (push (concat "      " (my/org-cl--loc-string f e t)) lines)))
          (push "" lines))
        (when others
          (hdr (format "Cited in other notes (%d)" (length others)))
          (pcase-dolist (`(,f . ,e) others)
            (push (concat "  " (my/org-cl--loc-string f e t)) lines))
          (push "" lines))
        (when cites
          (hdr (format "Cites (%d)" (length cites)))
          (pcase-dolist (`(,k . ,es) cites)
            (push (my/org-cl--paper-string k hasnote) lines)
            (dolist (e es)
              (push (concat "      " (my/org-cl--loc-string self e)) lines)))))
      (list :title (format "Citations: %s"
                           (if pos (my/org-cl--node-title pos) "all nodes"))
            :lines (nreverse lines)))))

(universal-sidecar-define-section org-citar-section
    (title)
    (:major-modes org-mode :predicate (buffer-file-name))
  (let* ((pos (and (eq my/org-bl-scope 'node)
                   (if-let* ((w (get-buffer-window buffer)))
                       (window-point w)
                     (with-current-buffer buffer (point)))))
         (model (with-current-buffer buffer (my/org-cl--sidecar-model pos))))
    (when model
      (universal-sidecar-insert-section org-citar-section
          (or title (plist-get model :title))
        (dolist (line (plist-get model :lines))
          (insert line "\n"))))))

;;;; Keys -------------------------------------------------------------------

;; Embark calls a non-interactive function with the target itself, instead
;; of re-running the command's `interactive' form (which would open a citar
;; prompt and ask for the reference again).  So the keymaps bind these
;; wrappers, and the commands above stay usable from M-x.

(defun my/org-cl--embark-keys (target)
  "Citekeys from an Embark TARGET: a key, \"a & b\" string or list."
  (cond
   ((listp target) target)
   ((fboundp 'citar--unstringify-keys) (citar--unstringify-keys target))
   (t (split-string target " & " t))))

(defun my/org-cl-embark-cites (target)
  "Embark action: papers cited in the notes of TARGET."
  (my/citar-cites (my/org-cl--embark-keys target)))

(defun my/org-cl-embark-cited-by (target)
  "Embark action: papers and notes citing TARGET."
  (my/citar-cited-by (my/org-cl--embark-keys target)))

(defun my/org-cl--bind-embark-keys ()
  "Add the citation actions to citar's Embark keymaps."
  (dolist (sym '(citar-map citar-citation-map
                 citar-embark-map citar-embark-citation-map))
    (when (and (boundp sym) (keymapp (symbol-value sym)))
      (let ((map (symbol-value sym)))
        (define-key map (kbd "F") #'my/org-cl-embark-cites)
        (define-key map (kbd "B") #'my/org-cl-embark-cited-by)))))

(my/org-cl--bind-embark-keys)
(with-eval-after-load 'citar-embark (my/org-cl--bind-embark-keys))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c n c") #'my/org-citar-cites-here)
  (define-key org-mode-map (kbd "C-c n C") #'my/org-citar-cited-by-here)
  )

(provide 'org-citar-links)
;;; org-citar-links.el ends here
