;;; my-nodes.el --- Minimal org-roam-like node finder for vanilla Org -*- lexical-binding: t; -*-

;; Usage: put this file on your `load-path' and (require 'my-nodes),
;; or paste it into your init file.
;;
;; A note (file-level node) looks like this:
;;
;;   :PROPERTIES:
;;   :ID:       3f2c1a9e-...
;;   :ALIASES:  "Emacs Lisp" elisp
;;   :END:
;;   #+title: Elisp notes
;;
;; :ALIASES: is optional. Space-separated; wrap multi-word aliases in
;; double quotes. Headings can have :ID: and :ALIASES: in their own
;; drawer too.
;;
;; The node finder shows one candidate per node. The candidate is the
;; heading (or file title), its aliases and #+filetags:, which is what
;; you match on. For headings, the outline path (the file's #+title,
;; then the parent headings), and the :TYPE: of any node, are an
;; annotation on the right, visible but not matched:
;;
;;   Proximity measures         Machine Learning › Unsupervised learning
;;   SQL client                                Database › SQL › Tools
;;   Set up a device                                  Android  activity
;;   Artificial Intelligence                                        hub
;;
;; Set `my/node-annotate-parents' to nil to show only the file's title.
;;
;; With Consult, you can narrow to one type (see `my/node-types') by
;; typing its key and SPC at the start of the input, e.g. "a SPC" for
;; activities.
;;
;; While in `my/node-find', C-x, C-v and M-w open the highlighted node
;; in a window below, a window to the right, or a new frame. Those keys
;; come from my-open-elsewhere.el, which provides them for Consult's
;; buffer, file, grep and Org heading commands too.
;;
;; :ALIASES: is a plain property, so Org does not inherit it as long as
;; `org-use-property-inheritance' is nil (the default).

(require 'org)
(require 'org-id)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'my-open-elsewhere)
(my/open-elsewhere-global-mode 1)

;;; ------------------------------------------------------------
;;; Configuration
;;; ------------------------------------------------------------

(defvar my/notes-dir (expand-file-name "~/notes/")
  "Directory containing your notes.")

(defvar my/node-alias-property "ALIASES"
  "Name of the property that holds a node's aliases.")

(defvar my/node-type-property "TYPE"
  "Name of the property whose value is shown as the node's type.")

(defvar my/node-types
  '((?a "activity" "Activities")
    (?h "hub"      "Hubs")
    (?p "paper"    "Papers"))
  "Node types used for narrowing, as (KEY TYPE LABEL).
KEY is the narrowing key, TYPE the :TYPE: value (compared ignoring
case) and LABEL the name Consult shows for the narrowing.")

(defvar my/node-default-group '(?n . "Notes")
  "(KEY . LABEL) for nodes with no :TYPE: or a type not in `my/node-types'.")

(defvar my/node-annotate-parents t
  "Non-nil means a heading's annotation shows its full outline path.
With t (the default), heading nodes are annotated with their file's
#+title and parent headings, e.g. \"Machine Learning › Unsupervised
learning\". With nil, only the file's #+title is shown.")

(defvar my/node-title-width 50
  "Column where the aliases start in the node finder.")

(defvar my/node-excluded-dirs '("references")
  "Directory names whose notes never appear as node candidates.
A directory with one of these names is excluded at any depth under
`my/notes-dir'. The notes are still known to org-id, so existing
id: links to them keep working.")

;;; ------------------------------------------------------------
;;; File listing (skips lock files, hidden paths, broken symlinks)
;;; ------------------------------------------------------------

(defun my/node--files ()
  "Return all real .org files under `my/notes-dir'.
Skips Emacs lock files (.#foo.org), anything in a hidden directory
(.git, .stversions, ...) and dangling symlinks."
  (seq-filter
   (lambda (f)
     (and (not (string-match-p "\\(?:\\`\\|/\\)\\."
                               (file-relative-name f my/notes-dir)))
          (file-exists-p f)))
   (directory-files-recursively my/notes-dir "\\.org\\'")))

;;; ------------------------------------------------------------
;;; org-id setup (stable id: links across files)
;;; ------------------------------------------------------------

(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
      org-id-track-globally t
      org-id-locations-file (expand-file-name ".org-id-locations"
                                              user-emacs-directory)
      org-id-extra-files (my/node--files))

;;; ------------------------------------------------------------
;;; Parsing helpers
;;; ------------------------------------------------------------

(defun my/node--parse-aliases (str)
  "Turn a string like \"\\\"Emacs Lisp\\\" elisp\" into a list of aliases."
  (condition-case nil
      (split-string-and-unquote str)
    (error (split-string str))))   ; fall back on unbalanced quotes

(defun my/node--format-aliases (aliases)
  "Turn a list of ALIASES into the string stored in the property."
  (combine-and-quote-strings aliases))

(defun my/node--clean-heading (line)
  "Strip stars, TODO keyword, priority and tags from heading LINE."
  (let* ((s (replace-regexp-in-string
             "^\\*+[ \t]+\\(?:\\(?:TODO\\|NEXT\\|WAIT\\|DONE\\|CANCELLED\\)[ \t]+\\)?\\(?:\\[#.\\][ \t]+\\)?"
             "" line))
         (s (replace-regexp-in-string
             "[ \t]+:[[:alnum:]_@#%:]+:[ \t]*$" "" s)))
    (string-trim s)))

(defun my/node--heading-title (pos)
  "Return the cleaned title of the heading line at POS."
  (save-excursion
    (goto-char pos)
    (my/node--clean-heading
     (buffer-substring-no-properties (line-beginning-position)
                                     (line-end-position)))))

(defun my/node--outline-parents (pos)
  "Return the titles of the headings above the heading at POS, outermost first."
  (save-excursion
    (goto-char pos)
    (let ((level (if (looking-at "\\*+") (- (match-end 0) (match-beginning 0)) 0))
          parents)
      (while (and (> level 1)
                  (re-search-backward "^\\(\\*+\\)[ \t]" nil t))
        (let ((l (- (match-end 1) (match-beginning 1))))
          (when (< l level)
            (setq level l)
            (push (my/node--heading-title (point)) parents))))
      parents)))

(defun my/node--drawer-property (name)
  "With point on an :ID: line, return property NAME from the same drawer.
Returns the trimmed value string, or nil if the property is missing."
  (save-excursion
    (let* ((beg (progn (re-search-backward "^[ \t]*:PROPERTIES:[ \t]*$" nil t)
                       (point)))
           (end (progn (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
                       (point))))
      (goto-char beg)
      (when (re-search-forward
             (format "^[ \t]*:%s:[ \t]+\\(.+\\)$" (regexp-quote name))
             end t)
        (string-trim (match-string 1))))))

(defun my/node--drawer-aliases ()
  "With point on an :ID: line, return aliases from the same property drawer."
  (let ((value (my/node--drawer-property my/node-alias-property)))
    (when value (my/node--parse-aliases value))))

(defun my/node--drawer-type ()
  "With point on an :ID: line, return the node's type, or nil."
  (let ((value (my/node--drawer-property my/node-type-property)))
    (and value (not (string-empty-p value)) value)))

(defun my/node--file-tags ()
  "Return the tags of the `#+filetags:' keyword in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+filetags:[ \t]*\\(.+\\)$" nil t)
      ;; Accepts both \":a:b:\" and the older \"a b\" format.
      (split-string (match-string 1) "[: \t]+" t))))

(defun my/node--display (title aliases &optional tags)
  "Return the candidate text: TITLE, then ALIASES and TAGS.
ALIASES are dimmed and TAGS look like org-roam's (:a:b:). All of it is
part of the text, so you can search by any of them."
  (if (or aliases tags)
      (concat (truncate-string-to-width title my/node-title-width
                                        0 ?\s "…")
              (when aliases
                (concat "  "
                        (propertize (string-join aliases ", ")
                                    'face 'completions-annotations)))
              (when tags
                (concat "  "
                        (propertize (format ":%s:" (string-join tags ":"))
                                    'face 'org-tag))))
    title))

;;; ------------------------------------------------------------
;;; Scanning
;;; ------------------------------------------------------------
;; Scanning returns one plist per :ID:, and `my/node--build-entries'
;; turns those into node entries:
;;
;;   (CANDIDATE ID FILE POS TITLE ALIASES TYPE PATH)
;;
;; CANDIDATE carries the text properties `my-node-key' (the
;; narrowing key), `my-node-type' and `my-node-path' (for annotations).

(defun my/node--scan-file (file)
  "Return a list of raw node plists for every :ID: in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((file-title
           (or (save-excursion
                 (goto-char (point-min))
                 (when (re-search-forward "^#\\+title:[ \t]*\\(.+\\)$" nil t)
                   (string-trim (match-string 1))))
               (file-name-base file)))
          (file-tags (my/node--file-tags))
          nodes)
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*:ID:[ \t]+\\(\\S-+\\)" nil t)
        (let* ((id (match-string 1))
               (id-pos (match-beginning 0))
               (aliases (my/node--drawer-aliases))
               (type (my/node--drawer-type))
               (heading-pos
                (save-excursion
                  (goto-char id-pos)
                  (when (re-search-backward "^\\*+ " nil t) (point)))))
          (push (list :id id
                      :file file
                      :pos (or heading-pos 1)
                      :title (if heading-pos
                                 (my/node--heading-title heading-pos)
                               file-title)
                      :aliases aliases
                      :type type
                      ;; #+filetags: is shown on file-level nodes only, so
                      ;; tags don't leak onto the headings of the same file.
                      :tags (unless heading-pos file-tags)
                      ;; File title, then (optionally) the parent headings.
                      :path (when heading-pos
                              (cons file-title
                                    (when my/node-annotate-parents
                                      (my/node--outline-parents heading-pos)))))
                nodes)))
      nodes)))

;;; ------------------------------------------------------------
;;; Types and candidates
;;; ------------------------------------------------------------

(defun my/node--narrow-keys ()
  "Return the narrowing keys as (KEY . LABEL)."
  (append (mapcar (lambda (e) (cons (nth 0 e) (nth 2 e))) my/node-types)
          (list my/node-default-group)))

(defun my/node--type-key (type)
  "Return the narrowing key for the :TYPE: value TYPE."
  (or (and type
           (car (seq-find (lambda (e) (string= (downcase type)
                                               (downcase (nth 1 e))))
                          my/node-types)))
      (car my/node-default-group)))

(defun my/node--path-string (raw)
  "Return RAW's outline path as \"File › Parent › ...\", or nil."
  (let ((path (plist-get raw :path)))
    (when path (string-join path " › "))))

(defun my/node--base-text (raw)
  "Return the candidate text for RAW before disambiguation."
  (my/node--display (plist-get raw :title)
                    (plist-get raw :aliases)
                    (plist-get raw :tags)))

(defun my/node--entry-lessp (a b)
  "Sort entries A and B alphabetically by candidate text."
  (string-lessp (downcase (car a)) (downcase (car b))))

(defun my/node--build-entries (raws)
  "Turn RAWS (plists from `my/node--scan-file') into sorted node entries.
The path is in the annotation, so two headings with the same name
would give the same candidate. Those get their path appended, dimmed,
so every candidate is unique and selects the right node."
  (let ((counts (make-hash-table :test #'equal))
        (seen (make-hash-table :test #'equal))
        entries)
    (dolist (raw raws)
      (cl-incf (gethash (my/node--base-text raw) counts 0)))
    (dolist (raw raws)
      (let* ((base (my/node--base-text raw))
             (path (my/node--path-string raw))
             (type (plist-get raw :type))
             (text (if (> (gethash base counts) 1)
                       (concat base " "
                               (propertize
                                (format "(%s)"
                                        (or path (file-name-nondirectory
                                                  (plist-get raw :file))))
                                'face 'completions-annotations))
                     base))
             (n (cl-incf (gethash text seen 0))))
        (when (> n 1)
          (setq text (format "%s #%d" text n)))
        (push (list (propertize text
                                'my-node-key (my/node--type-key type)
                                'my-node-type type
                                'my-node-path path)
                    (plist-get raw :id)
                    (plist-get raw :file)
                    (plist-get raw :pos)
                    (plist-get raw :title)
                    (plist-get raw :aliases)
                    type
                    path)
              entries)))
    (sort entries #'my/node--entry-lessp)))

(defun my/node--excluded-p (file)
  "Return non-nil if FILE is inside a directory in `my/node-excluded-dirs'."
  (let ((components (split-string
                     (or (file-name-directory
                          (file-relative-name file my/notes-dir))
                         "")
                     "/" t)))
    (seq-some (lambda (dir) (member dir components))
              my/node-excluded-dirs)))

(defun my/node--collect ()
  "Return node entries for all notes except those in excluded directories."
  (let ((files (my/node--files)))
    ;; Keep org-id's file list current, so it doesn't matter whether
    ;; `my/notes-dir' was set before or after this file was loaded.
    ;; Excluded directories stay in this list so id: links into them
    ;; still resolve; they are only left out of the candidates.
    (setq org-id-extra-files files)
    (my/node--build-entries
     (apply #'append
            (mapcar #'my/node--scan-file
                    (seq-remove #'my/node--excluded-p files))))))

;;; ------------------------------------------------------------
;;; Creating
;;; ------------------------------------------------------------

(defun my/node--file-name (title)
  "Return the file name for TITLE: lowercased, spaces as underscores.
Punctuation is kept. Only characters that can't be part of a file
name (/ and NUL) are replaced, with underscores as well. The note's
#+title keeps the original case."
  (concat (downcase
           (replace-regexp-in-string "[ \t/\0]+" "_" (string-trim title)))
          ".org"))

(defun my/node--create (title)
  "Create a new file-level note called TITLE and return its node entry."
  (let* ((id (org-id-new))
         (file (expand-file-name (my/node--file-name title) my/notes-dir)))
    (when (file-exists-p file)
      (user-error "A note named %s already exists" (file-name-nondirectory file)))
    (make-directory my/notes-dir t)
    (with-temp-file file
      (insert ":PROPERTIES:\n:ID:       " id "\n:END:\n"
              "#+title: " title "\n\n"))
    (org-id-add-location id file)
    (list title id file 1 title nil nil nil)))

;;; ------------------------------------------------------------
;;; Completion
;;; ------------------------------------------------------------

(defun my/node--annotate (cand)
  "Return CAND's outline path and type, right-aligned and dimmed."
  (let* ((path (get-text-property 0 'my-node-path cand))
         (type (get-text-property 0 'my-node-type cand))
         (text (string-join
                (delq nil
                      (list (and path (propertize path 'face
                                                  'completions-annotations))
                            (and type (propertize type 'face
                                                  'font-lock-type-face))))
                "  ")))
    (unless (string-empty-p text)
      (concat " "
              (propertize " " 'display
                          `(space :align-to (- right ,(+ 2 (string-width text)))))
              text))))

(defvar consult--narrow)

(defun my/node--narrow-p (cand)
  "Return non-nil if CAND belongs to the active narrowing key."
  (or (null consult--narrow)
      (eq (get-text-property 0 'my-node-key cand) consult--narrow)))

(defun my/node--table (candidates)
  "Completion table for CANDIDATES with annotations.
Used when Consult isn't (e.g. `my/node-insert'). The `my-node' category
lets you attach Marginalia/Embark behavior later."
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata (category . my-node)
                   (annotation-function . my/node--annotate)
                   (display-sort-function . identity)
                   (cycle-sort-function . identity))
      (complete-with-action action candidates string pred))))

(declare-function consult--read "consult")

(defun my/node--preview-state ()
  "Return a Consult state function that previews nodes in the current window.
Files opened only for the preview are killed again afterwards, and the
window goes back to what it showed before."
  (let* ((win (selected-window))
         (orig-buf (window-buffer win))
         (orig-point (window-point win))
         (orig-start (window-start win))
         (temp-buffers nil))
    (cl-flet ((restore ()
                (when (window-live-p win)
                  (set-window-buffer win orig-buf)
                  (set-window-point win orig-point)
                  (set-window-start win orig-start)))
              (show (entry)
                (pcase-let ((`(,_ ,_ ,file ,pos . ,_) entry))
                  (when (and (window-live-p win) (file-readable-p file))
                    (let ((buf (or (get-file-buffer file)
                                   (let ((b (find-file-noselect file)))
                                     (push b temp-buffers)
                                     b))))
                      (with-current-buffer buf
                        (save-excursion
                          (goto-char pos)
                          (when (derived-mode-p 'org-mode)
                            (org-show-context)
                            (org-show-entry))))
                      (set-window-buffer win buf)
                      (set-window-point win pos)
                      (set-window-start
                       win (with-current-buffer buf
                             (save-excursion
                               (goto-char pos)
                               (line-beginning-position)))))))))
      (lambda (action cand)
        (pcase action
          ('preview (if (consp cand) (show cand) (restore)))
          ('exit (restore)
                 (dolist (b temp-buffers)
                   (when (and (buffer-live-p b) (not (buffer-modified-p b)))
                     (kill-buffer b)))
                 (setq temp-buffers nil)))))))

(defun my/node--visit (entry)
  "Go to the node ENTRY in the selected window.
When the prompt was left with an open-elsewhere key, the new window or
frame is already selected at this point."
  (pcase-let ((`(,_ ,_ ,file ,pos . ,_) entry))
    (find-file file)
    (goto-char pos)
    (when (derived-mode-p 'org-mode)
      (org-show-context)
      (org-show-entry))))

;;; ------------------------------------------------------------
;;; Reading a node
;;; ------------------------------------------------------------

(defun my/node--read (prompt &optional for-find)
  "Prompt for a node. Return its entry, creating a new note for unknown input.
With Consult, candidates can be narrowed with the keys in
`my/node-types'. With FOR-FIND non-nil, also show the node in the
current window while you move through the candidates."
  (let* ((nodes (my/node--collect))
         (cands (mapcar #'car nodes))
         ;; Previewing opens and fontifies files, which prints messages
         ;; ("file has auto save data...", "Indentation setup for shell
         ;; type zsh" from fontified src blocks) into the prompt area.
         ;; They are still written to *Messages*.
         (inhibit-message (or for-find inhibit-message))
         (choice
          (progn
            (if (require 'consult nil t)
                (consult--read
                 cands
                 :prompt prompt
                 :category 'my-node
                 :require-match nil
                 :sort nil
                 :preview-key 'any
                 :lookup (lambda (selected &rest _)
                           (or (assoc selected nodes) selected))
                 :annotate #'my/node--annotate
                 :narrow (list :predicate #'my/node--narrow-p
                               :keys (my/node--narrow-keys))
                 ;; Live preview only when finding, not when inserting links.
                 :state (and for-find (my/node--preview-state)))
              (completing-read prompt (my/node--table cands) nil nil)))))
    (cond ((consp choice) choice)
          ((assoc choice nodes))
          (t (my/node--create choice)))))

;;; ------------------------------------------------------------
;;; Commands
;;; ------------------------------------------------------------

(defun my/node-find ()
  "Find a node (by title or alias), or create it if it doesn't exist.
While prompting, C-x, C-v and M-w open the node in a new window or
frame instead (see `my/open-elsewhere-keys')."
  (interactive)
  (my/node--visit (my/node--read "Node: " t)))

(defun my/node-insert (&optional choose-text)
  "Insert an id: link to a node, creating the node if it doesn't exist.
The link text is the node's title. With a prefix argument CHOOSE-TEXT,
pick the text from the title and the node's aliases."
  (interactive "P")
  (pcase-let ((`(,_ ,id ,_ ,_ ,title ,aliases . ,_) (my/node--read "Link to: ")))
    (let ((text (if (and choose-text aliases)
                    (completing-read "Link text: " (cons title aliases)
                                     nil nil nil nil title)
                  title)))
      (insert (org-link-make-string (concat "id:" id) text)))))

(defun my/node--current-aliases ()
  "Return the aliases of the heading at point, or of the file before the first heading."
  (let ((current (org-entry-get nil my/node-alias-property)))
    (when current (my/node--parse-aliases current))))

(defun my/node-add-alias (alias)
  "Add ALIAS to the heading at point, or to the file if before the first heading.
The prompt shows the aliases that already exist."
  (interactive
   (let ((existing (my/node--current-aliases)))
     (list (completing-read
            "New alias: "
            (lambda (string pred action)
              (if (eq action 'metadata)
                  '(metadata (display-sort-function . identity)
                             (cycle-sort-function . identity))
                (complete-with-action action existing string pred)))
            nil nil))))
  (let ((aliases (my/node--current-aliases)))
    (cond
     ((string-empty-p (string-trim alias))
      (user-error "Empty alias"))
     ((member alias aliases)
      (message "Alias \"%s\" already exists" alias))
     (t
      (org-entry-put nil my/node-alias-property
                     (my/node--format-aliases
                      (append aliases (list (string-trim alias)))))
      (message "Aliases: %s"
               (string-join (append aliases (list (string-trim alias))) ", "))))))

(defun my/node-remove-alias ()
  "Remove one alias from the node at point."
  (interactive)
  (let* ((current (org-entry-get nil my/node-alias-property))
         (aliases (when current (my/node--parse-aliases current))))
    (unless aliases (user-error "No aliases here"))
    (let* ((alias (completing-read "Remove alias: " aliases nil t))
           (rest (remove alias aliases)))
      (if rest
          (org-entry-put nil my/node-alias-property
                         (my/node--format-aliases rest))
        (org-entry-delete nil my/node-alias-property)))))

;;; ------------------------------------------------------------
;;; Keybindings
;;; ------------------------------------------------------------

(global-set-key (kbd "C-c n f") #'my/node-find)
(global-set-key (kbd "C-c n i") #'my/node-insert)
(global-set-key (kbd "C-c n a") #'my/node-add-alias)
(global-set-key (kbd "C-c n A") #'my/node-remove-alias)

(provide 'my-nodes)
;;; my-nodes.el ends here
