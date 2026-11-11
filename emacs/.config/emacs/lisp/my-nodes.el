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
;; The node finder shows one candidate per node, with the aliases in
;; a dimmed column after the title:
;;
;;   Elisp notes                                 Emacs Lisp, elisp
;;
;; The aliases are part of the candidate text, so typing an alias
;; matches the node (Vertico/Orderless/etc. all just work).
;;
;; While in `my/node-find', these keys open the highlighted node
;; somewhere else instead of in the current window (see
;; `my/node-open-keys' to change them):
;;
;;   C-x  new window below (horizontal split)
;;   C-v  new window to the right (vertical split)
;;   M-w  new frame
;;
;; :ALIASES: is a plain property, so Org does not inherit it as long as
;; `org-use-property-inheritance' is nil (the default).

(require 'org)
(require 'org-id)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

;;; ------------------------------------------------------------
;;; Configuration
;;; ------------------------------------------------------------

(defvar my/notes-dir (expand-file-name "~/notes/")
  "Directory containing your notes.")

(defvar my/node-alias-property "ALIASES"
  "Name of the property that holds a node's aliases.")

(defvar my/node-type-property "TYPE"
  "Name of the property whose value is shown as the node's type.")

(defvar my/node-title-width 50
  "Column where the aliases start in the node finder.")

(defvar my/node-excluded-dirs '("references")
  "Directory names whose notes never appear as node candidates.
A directory with one of these names is excluded at any depth under
`my/notes-dir'. The notes are still known to org-id, so existing
id: links to them keep working.")

(defvar my/node-open-keys
  '(("C-x" . my/node-open-below)
    ("C-v" . my/node-open-right)
    ("M-w" . my/node-open-frame))
  "Keys active in the `my/node-find' minibuffer, as (KEY . COMMAND).")

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

(defun my/node--display (title aliases &optional tags type)
  "Return the candidate text: TITLE, then TYPE, ALIASES and TAGS.
TYPE is a string, ALIASES are dimmed, and TAGS look like org-roam's
(:a:b:). All of it is part of the text, so you can search by any of them."
  (if (or aliases tags type)
      (concat (truncate-string-to-width title my/node-title-width
                                        0 ?\s "…")
              (when type
                (concat "  " (propertize type 'face 'font-lock-type-face)))
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
;; A node entry is: (DISPLAY ID FILE POS TITLE ALIASES)

(defun my/node--scan-file (file)
  "Return a list of node entries for every :ID: in FILE."
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
               (aliases (my/node--drawer-aliases))
               (type (my/node--drawer-type))
               (heading-pos
                (save-excursion
                  (goto-char (match-beginning 0))
                  (when (re-search-backward "^\\*+ " nil t) (point))))
               (title (if heading-pos
                          (my/node--clean-heading
                           (save-excursion
                             (goto-char heading-pos)
                             (buffer-substring-no-properties
                              (line-beginning-position)
                              (line-end-position))))
                        file-title))
               (shown (if heading-pos
                          (format "%s (%s)" title file-title)
                        title)))
          ;; #+filetags: is shown on file-level nodes only, so tags
          ;; don't leak onto the headings of the same file.
          (push (list (my/node--display shown aliases
                                        (unless heading-pos file-tags)
                                        type)
                      id file (or heading-pos 1) title aliases)
                nodes)))
      nodes)))

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
    (apply #'append
           (mapcar #'my/node--scan-file
                   (seq-remove #'my/node--excluded-p files)))))

;;; ------------------------------------------------------------
;;; Creating
;;; ------------------------------------------------------------

(defun my/node--file-name (title)
  "Return the file name for TITLE: the title as is, spaces as underscores.
Case and punctuation are kept. Only characters that can't be part of a
file name (/ and NUL) are replaced, with underscores as well."
  (concat (replace-regexp-in-string "[ \t/\0]+" "_" (string-trim title))
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
    (list title id file 1 title nil)))

;;; ------------------------------------------------------------
;;; Completion
;;; ------------------------------------------------------------

(defun my/node--table (candidates)
  "Completion table for CANDIDATES (strings) with a `my-node' category.
The category lets you attach Marginalia/Embark behavior later."
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata (category . my-node))
      (complete-with-action action candidates string pred))))

(declare-function consult--read "consult")
(declare-function vertico-exit "vertico")
(declare-function evil-local-set-key "evil-core")

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

;;; ------------------------------------------------------------
;;; Open-elsewhere keys (active only while in `my/node-find')
;;; ------------------------------------------------------------

(defvar my/node--find-session nil
  "Non-nil while the `my/node-find' prompt is active.")

(defvar my/node--open-action nil
  "How to open the chosen node: nil, `below', `right' or `frame'.")

(defun my/node--exit-with (action)
  "Exit the minibuffer with the highlighted candidate, remembering ACTION."
  (setq my/node--open-action action)
  (if (and (bound-and-true-p vertico-mode) (fboundp 'vertico-exit))
      (vertico-exit)
    (exit-minibuffer)))

(defun my/node-open-below ()
  "Open the highlighted node in a new window below."
  (interactive)
  (my/node--exit-with 'below))

(defun my/node-open-right ()
  "Open the highlighted node in a new window to the right."
  (interactive)
  (my/node--exit-with 'right))

(defun my/node-open-frame ()
  "Open the highlighted node in a new frame."
  (interactive)
  (my/node--exit-with 'frame))

(defun my/node--setup-keys ()
  "Install `my/node-open-keys' in the minibuffer for `my/node-find'.
Added to `minibuffer-setup-hook' (appended, so Evil is already set up)."
  (when my/node--find-session
    (let ((map (make-sparse-keymap))
          (evil (and (fboundp 'evil-local-set-key)
                     (bound-and-true-p evil-local-mode))))
      (pcase-dolist (`(,key . ,cmd) my/node-open-keys)
        (define-key map (kbd key) cmd)
        ;; Evil's insert/normal maps shadow the local map (e.g. C-v is
        ;; `quoted-insert' there), so bind the keys per state as well.
        (when evil
          (evil-local-set-key 'insert (kbd key) cmd)
          (evil-local-set-key 'normal (kbd key) cmd)))
      (use-local-map (make-composed-keymap map (current-local-map))))))

(defun my/node--visit (entry &optional action)
  "Go to the node ENTRY, in a new window or frame depending on ACTION."
  (pcase action
    ('below (select-window (split-window-below)))
    ('right (select-window (split-window-right)))
    ('frame (let ((frame (clone-frame)))
              (select-frame-set-input-focus frame)
              ;; clone-frame copies the window layout; keep only one window
              (delete-other-windows (frame-selected-window frame)))))
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
With FOR-FIND non-nil, enable the open-elsewhere keys and, if Consult
is available, show the node in the current window while you move
through the candidates."
  (let* ((nodes (my/node--collect))
         (cands (mapcar #'car nodes))
         (my/node--find-session for-find)
         ;; Previewing opens and fontifies files, which prints messages
         ;; ("file has auto save data...", "Indentation setup for shell
         ;; type zsh" from fontified src blocks) into the prompt area.
         ;; They are still written to *Messages*.
         (inhibit-message (or for-find inhibit-message))
         (choice
          (minibuffer-with-setup-hook (:append #'my/node--setup-keys)
            (if (and for-find (require 'consult nil t))
                (consult--read
                 cands
                 :prompt prompt
                 :category 'my-node
                 :require-match nil
                 :sort nil
                 :preview-key 'any
                 :lookup (lambda (selected &rest _)
                           (or (assoc selected nodes) selected))
                 :state (my/node--preview-state))
              (completing-read prompt (my/node--table cands) nil nil)))))
    (cond ((consp choice) choice)
          ((assoc choice nodes))
          (t (my/node--create choice)))))

;;; ------------------------------------------------------------
;;; Commands
;;; ------------------------------------------------------------

(defun my/node-find ()
  "Find a node (by title or alias), or create it if it doesn't exist.
While prompting, `my/node-open-keys' open the node in a new window
or frame instead of the current window."
  (interactive)
  (setq my/node--open-action nil)
  (let ((entry (my/node--read "Node: " t)))
    (my/node--visit entry my/node--open-action)))

(defun my/node-insert (&optional choose-text)
  "Insert an id: link to a node, creating the node if it doesn't exist.
The link text is the node's title. With a prefix argument CHOOSE-TEXT,
pick the text from the title and the node's aliases."
  (interactive "P")
  (pcase-let ((`(,_ ,id ,_ ,_ ,title ,aliases) (my/node--read "Link to: ")))
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
