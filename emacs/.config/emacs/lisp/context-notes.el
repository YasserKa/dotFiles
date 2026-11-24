;;; context-notes.el --- Show the notes relevant to what you are doing -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; An external detector (the `context-detect' script) turns the focused
;; window into a list of context keys, ordered general -> specific, e.g.
;;
;;   ("kitty" "git" "nvim" "lang:gitcommit" "git-commit")
;;   ("zathura" "ext:pdf" "file:/home/me/papers/attention.pdf")
;;
;; Notes opt in by declaring which contexts they belong to:
;;
;;   * Commit messages
;;     :PROPERTIES:
;;     :CONTEXT:  git-commit lang:gitcommit
;;     :END:
;;
;; Headings with NOTER_DOCUMENT (org-noter) match the `file:' key of the
;; document you are reading, so paper notes come up automatically.
;;
;; `dir:' values match any file you have open, or terminal/Emacs working
;; directory, under a directory:
;;
;;   :CONTEXT: dir:~/papers/ml        absolute: that tree only
;;   :CONTEXT: dir:papers/ml          fragment: any .../papers/ml/... path
;;   :CONTEXT: dir:projects/*/docs    `*' is one path segment
;;
;; The deeper pattern wins when several match.
;; For anything you don't want to tag, use `context-notes-rules'.
;;
;; Entry points:
;;   `context-notes-request'  - called by the toggle script via emacsclient
;;   `context-notes-here'     - from inside Emacs, uses the current buffer
;;   `context-notes-tag-last' - add the last context's keys to the heading
;;                              at point (teach it when nothing matched)

;;; Code:

(require 'org)
(require 'org-id)
(require 'seq)
(require 'subr-x)

(declare-function org-transclusion-mode "org-transclusion")

(defgroup context-notes nil
  "Show notes relevant to the current context."
  :group 'org)

(defcustom context-notes-directories nil
  "Directories searched for headings carrying context properties.
nil means `org-roam-directory' when set, otherwise `org-directory'."
  :type '(repeat directory))

(defcustom context-notes-property "CONTEXT"
  "Property listing the context keys a heading belongs to (space separated)."
  :type 'string)

(defcustom context-notes-document-properties '("NOTER_DOCUMENT")
  "Properties holding a document path; they match the `file:' context key."
  :type '(repeat string))

(defcustom context-notes-rules nil
  "Static mapping from context key to note locations.
Each element is (KEY TARGET...).  KEY is matched like a CONTEXT value,
so \"dir:\" patterns work.  TARGET is one of:

  \"id:XXXX\"                     heading with that ID
  (FILE)                        the whole file
  (FILE HEADING)                a heading with that title, at any depth
  (FILE PARENT ... HEADING)     HEADING somewhere under PARENT; levels in
                                between can be left out
  (nil HEADING ...)             same, searching every notes file

FILE can be a bare file name (\"zathura.org\"), a path relative to a
notes directory, or absolute.  A HEADING is its title (case-insensitive,
ignoring TODO, priority and tags) or (:re REGEXP).

  (setq context-notes-rules
        \\='((\"git-commit\" (\"git.org\" \"Commit messages\"))
          (\"zathura\"    (\"zathura.org\" \"Bindings\"))
          (\"jupyter\"    (nil \"Jupyter\" (:re \"^key\")))))"
  :type '(alist :key-type string :value-type (repeat sexp)))

(defcustom context-notes-use-transclusion t
  "When non-nil and org-transclusion is installed, combine several matches
as live transclusions; otherwise insert read-only copies."
  :type 'boolean)

(defcustom context-notes-fallback #'context-notes-fallback-default
  "Function called with the context keys when nothing matches."
  :type 'function)

(defcustom context-notes-find-command 'my/node-find
  "Command `context-notes-fallback-default' runs to pick a note by hand.
Falls back to `find-file' in the notes directory when it isn't defined."
  :type 'symbol)

(defvar context-notes--last-keys nil
  "Context keys of the most recent request.")

(defvar context-notes--display-function #'context-notes--display-in-frame
  "How a result buffer is shown.")

;;;; Matching

(defun context-notes--directories ()
  (mapcar #'expand-file-name
          (or context-notes-directories
              (list (if (and (boundp 'org-roam-directory) org-roam-directory)
                        org-roam-directory
                      org-directory)))))

(defun context-notes--properties ()
  (cons context-notes-property context-notes-document-properties))

(defun context-notes--candidate-files ()
  "Org files that mention one of the context properties at all."
  (let ((dirs (seq-filter #'file-directory-p (context-notes--directories)))
        (props (context-notes--properties)))
    (when dirs
      (if-let ((rg (executable-find "rg")))
          (apply #'process-lines-ignore-status rg "-l" "-i" "--no-messages"
                 "-g" "*.org"
                 "-e" (format "^\\s*:(%s):" (mapconcat #'regexp-quote props "|"))
                 dirs)
        (let ((re (context-notes--property-regexp))
              (case-fold-search t))
          (seq-filter (lambda (f)
                        (with-temp-buffer
                          (insert-file-contents f)
                          (re-search-forward re nil t)))
                      (mapcan (lambda (d) (directory-files-recursively d "\\.org\\'"))
                              dirs)))))))

(defun context-notes--property-regexp ()
  (format "^[ \t]*:%s:" (regexp-opt (context-notes--properties) t)))

(defun context-notes--ieq (a b)
  (eq t (compare-strings a nil nil b nil nil t)))

(defun context-notes--same-file-p (pattern path)
  "Same path (relative to the note), or the same file name.
The name fallback keeps org-noter notes matching after PDFs move."
  (or (string= (file-name-nondirectory pattern) (file-name-nondirectory path))
      (string= (file-truename (expand-file-name pattern)) (file-truename path))))

(defun context-notes--dir-regexp (pattern)
  "Regexp for PATTERN, a directory given as an absolute path (/... or ~...)
or as path segments that may appear anywhere (\"papers/ml\").  A `*'
stands for one path segment."
  (let* ((absolute (string-match-p "\\`[/~]" pattern))
         (body (string-trim (if absolute (expand-file-name pattern) pattern)
                            "/+" "/+")))
    (concat (if absolute "\\`/" "/")
            (replace-regexp-in-string "\\\\\\*" "[^/]*" (regexp-quote body) t t)
            "/")))

(defun context-notes--under-dir-p (pattern key)
  "Non-nil if the file: or cwd: KEY lies under directory PATTERN."
  (let ((path (cond ((string-prefix-p "file:" key) (substring key 5))
                    ((string-prefix-p "cwd:" key)
                     (file-name-as-directory (substring key 4))))))
    (and path (string-match-p (context-notes--dir-regexp pattern) path))))

(defun context-notes--match-strength (value key)
  "How well heading VALUE matches context KEY: nil, or a number (higher wins)."
  (cond
   ((string-prefix-p "dir:" value)
    (and (context-notes--under-dir-p (substring value 4) key)
         (length value)))
   ((and (string-prefix-p "file:" key) (string-prefix-p "file:" value))
    (and (context-notes--same-file-p (substring value 5) (substring key 5))
         5000))
   ((context-notes--ieq value key) 5000)))

(defun context-notes--entry-values ()
  "Context values declared by the entry at point."
  (append
   (split-string (or (org-entry-get nil context-notes-property) "") "[ \t,]+" t)
   (delq nil (mapcar (lambda (p)
                       (when-let ((v (org-entry-get nil p)))
                         (concat "file:" (string-trim v))))
                     context-notes-document-properties))))

(defun context-notes--score (keys values)
  "Score of VALUES against KEYS, or nil if nothing matches.
Later (more specific) keys win; within a key, stronger matches win."
  (let ((i 0) best)
    (dolist (k keys best)
      (setq i (1+ i))
      (when-let ((s (seq-max (cons 0 (delq nil (mapcar (lambda (v)
                                                          (context-notes--match-strength v k))
                                                        values))))))
        (when (> s 0)
          (setq best (max (or best 0) (+ (* i 10000) s))))))))

(defun context-notes--collect-file (file keys)
  (let (hits seen)
    (with-current-buffer (find-file-noselect file)
      (let ((default-directory (file-name-directory file))
            (re (context-notes--property-regexp))
            (case-fold-search t))
        (org-with-wide-buffer
         (goto-char (point-min))
         (while (re-search-forward re nil t)
           (save-excursion
             (if (org-before-first-heading-p)
                 (goto-char (point-min))
               (org-back-to-heading t))
             (unless (memq (point) seen)
               (push (point) seen)
               (when-let ((score (context-notes--score
                                  keys (context-notes--entry-values))))
                 (push (cons score (point-marker)) hits))))))))
    hits))

(defun context-notes--all-org-files ()
  (let ((dirs (seq-filter #'file-directory-p (context-notes--directories))))
    (if-let ((rg (executable-find "rg")))
        (apply #'process-lines-ignore-status rg "--files" "-g" "*.org" dirs)
      (mapcan (lambda (d) (directory-files-recursively d "\\.org\\'")) dirs))))

(defun context-notes--find-file (file)
  "Path of FILE: absolute, relative to a notes directory, or else any
notes file whose path ends in FILE (so \"zathura.org\" is enough)."
  (if (string-match-p "\\`[/~]" file)
      (expand-file-name file)
    (or (seq-find #'file-exists-p
                  (mapcar (lambda (d) (expand-file-name file d))
                          (context-notes--directories)))
        (seq-find (lambda (f) (string-suffix-p (concat "/" file) f))
                  (context-notes--all-org-files))
        (error "No notes file %s" file))))

(defun context-notes--heading-matches-p (spec)
  "SPEC is a heading title (case-insensitive) or (:re REGEXP)."
  (let ((title (org-get-heading t t t t))
        (case-fold-search t))
    (pcase spec
      (`(:re ,re) (string-match-p re title))
      (_ (context-notes--ieq (string-trim spec) title)))))

(defun context-notes--find-path (specs beg end)
  "Marker of a heading matching the last of SPECS, under headings matching
the earlier SPECS in that order (levels in between may be skipped)."
  (goto-char beg)
  (let (found)
    (while (and (not found) (re-search-forward org-outline-regexp-bol end t))
      (let ((h (match-beginning 0)))
        (goto-char h)
        (when (context-notes--heading-matches-p (car specs))
          (setq found
                (if (cdr specs)
                    (let ((sub-end (min end (save-excursion (org-end-of-subtree t t)))))
                      (context-notes--find-path (cdr specs)
                                                (line-beginning-position 2) sub-end))
                  (copy-marker h))))
        (goto-char h)
        (forward-line 1)))
    found))

(defun context-notes--find-in-file (file specs)
  (with-current-buffer (find-file-noselect file)
    (org-with-wide-buffer
     (if specs
         (context-notes--find-path specs (point-min) (point-max))
       (point-min-marker)))))

(defun context-notes--resolve (target)
  "Return a marker for TARGET of `context-notes-rules', or nil."
  (condition-case err
      (pcase target
        ((pred stringp)
         (or (org-id-find (string-remove-prefix "id:" target) 'marker)
             (error "No heading with id %s" target)))
        (`(nil . ,specs)
         (let ((last (car (last specs))))
           (or (seq-some (lambda (f) (context-notes--find-in-file f specs))
                         ;; For a plain title, let rg pick the files to open.
                         (if (and (stringp last) (executable-find "rg"))
                             (apply #'process-lines-ignore-status
                                    (executable-find "rg") "-l" "-i" "-F" "-g" "*.org"
                                    "-e" (string-trim last)
                                    (seq-filter #'file-directory-p
                                                (context-notes--directories)))
                           (context-notes--all-org-files)))
               (error "No heading %S in any notes file" specs))))
        (`(,file . ,specs)
         (or (context-notes--find-in-file (context-notes--find-file file) specs)
             (error "No heading %S in %s" specs file))))
    (error (message "context-notes: %S: %s" target (error-message-string err))
           nil)))

(defun context-notes--rule-hits (keys)
  (let ((i 0) hits)
    (dolist (k keys hits)
      (setq i (1+ i))
      (dolist (rule context-notes-rules)
        (when-let ((s (context-notes--match-strength (car rule) k)))
          (dolist (target (cdr rule))
            (when-let ((m (context-notes--resolve target)))
              (push (cons (+ (* i 10000) s) m) hits))))))))

(defun context-notes-collect (keys)
  "Markers of notes matching KEYS, most specific first, without duplicates."
  (let (best)
    (dolist (hit (append (context-notes--rule-hits keys)
                         (mapcan (lambda (f) (context-notes--collect-file f keys))
                                 (context-notes--candidate-files))))
      (let ((same (seq-find (lambda (h) (and (eq (marker-buffer (cdr h))
                                                 (marker-buffer (cdr hit)))
                                             (= (cdr h) (cdr hit))))
                            best)))
        (cond ((null same) (push hit best))
              ((> (car hit) (car same)) (setcar same (car hit))))))
    ;; Stable sort keeps rules ahead of tagged notes at equal specificity.
    (mapcar #'cdr (sort (nreverse best) (lambda (a b) (> (car a) (car b)))))))

;;;; Display

(defun context-notes--title (m)
  (org-with-point-at m
    (org-with-wide-buffer
     (if (org-before-first-heading-p)
         (or (cadar (org-collect-keywords '("TITLE")))
             (file-name-base (buffer-file-name)))
       (org-get-heading t t t t)))))

(defun context-notes--link (m)
  (org-with-point-at m
    (org-with-wide-buffer
     (let ((file (buffer-file-name))
           (id (org-entry-get nil "ID")))
       (cond (id (concat "id:" id))
             ((org-before-first-heading-p) (concat "file:" file))
             (t (format "file:%s::*%s" file (org-get-heading t t t t))))))))

(defun context-notes--display-in-frame (buffer)
  (delete-other-windows)
  (switch-to-buffer buffer))

(defun context-notes--kill-old ()
  (dolist (b (buffer-list))
    (when (string-prefix-p "*ctx" (buffer-name b))
      (kill-buffer b))))

(defun context-notes--show-one (m)
  (let ((buf (make-indirect-buffer (marker-buffer m)
                                   (format "*ctx: %s*" (context-notes--title m))
                                   t)))
    (with-current-buffer buf
      (widen)
      (goto-char m)
      (if (org-before-first-heading-p)
          (org-overview)
        (org-narrow-to-subtree)
        (org-fold-show-subtree)))
    (funcall context-notes--display-function buf)
    buf))

(defun context-notes--subtree-text (m)
  (org-with-point-at m
    (org-with-wide-buffer
     (buffer-substring (point)
                       (if (org-before-first-heading-p)
                           (point-max)
                         (org-end-of-subtree t t))))))

(defun context-notes--show-many (markers keys)
  (let ((buf (get-buffer-create "*ctx*"))
        (transclude (and context-notes-use-transclusion
                         (require 'org-transclusion nil t))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert (format "#+title: Context: %s\n\n" (string-join keys ", ")))
        (dolist (m markers)
          (insert (format "- %s\n" (org-link-make-string (context-notes--link m)
                                                         (context-notes--title m)))))
        (insert "\n")
        (dolist (m markers)
          (let ((file-level (org-with-point-at m
                              (org-with-wide-buffer (org-before-first-heading-p)))))
            (when file-level
              (insert "* " (context-notes--title m) "\n"))
            (if transclude
                (insert (format "#+transclude: %s :level %d\n\n"
                                (org-link-make-string (context-notes--link m))
                                (if file-level 2 1)))
              (if file-level
                  (insert (context-notes--subtree-text m) "\n")
                (goto-char (point-max))
                (org-paste-subtree 1 (context-notes--subtree-text m))
                (goto-char (point-max))))))
        (if transclude
            (org-transclusion-mode 1)
          (read-only-mode 1)))
      (goto-char (point-min))
      (org-fold-show-all))
    (funcall context-notes--display-function buf)
    buf))

(defun context-notes-fallback-default (keys)
  (message "No notes for context: %s" (string-join keys " "))
  (if (commandp context-notes-find-command)
      (call-interactively context-notes-find-command)
    (let ((default-directory (file-name-as-directory
                              (car (context-notes--directories)))))
      (call-interactively #'find-file))))

(defcustom context-notes-pick-command 'consult-org-heading
  "Command run in the combined buffer when several headings match.
nil leaves the buffer as it is."
  :type '(choice (const :tag "None" nil) function))

(defun context-notes--pick-heading (buffer)
  (set-buffer buffer)
  (when (and context-notes-pick-command
             (or (commandp context-notes-pick-command)
                 (and (require 'consult-org nil t)
                      (commandp context-notes-pick-command))))
    (call-interactively context-notes-pick-command)
    (unless (org-before-first-heading-p)
      (org-fold-show-subtree)
      (recenter 0))))

;;;; Prompt lifetime
;;
;; The fallback and the heading picker open a minibuffer prompt in the
;; notes frame.  If that frame is hidden while the prompt is open, Emacs
;; moves the prompt to the next frame you focus.  So the prompt is closed
;; when the frame is hidden (`context-notes-cancel', called by the toggle
;; script) or when it loses focus.

(defcustom context-notes-cancel-on-focus-out t
  "Close an open context-notes prompt when its frame loses focus."
  :type 'boolean)

(defvar context-notes--prompt-depth nil
  "Minibuffer depth of the open context-notes prompt; nil when none is open.
Bound dynamically around the prompt, so it resets however the prompt ends.")

(defvar context-notes--prompt-frame nil
  "Frame the open context-notes prompt belongs to.")

(defun context-notes--prompt-open-p ()
  (and context-notes--prompt-depth
       (>= (minibuffer-depth) context-notes--prompt-depth)))

(defun context-notes--with-prompt (fn &rest args)
  "Call FN with ARGS, as a prompt `context-notes-cancel' may close."
  (let ((context-notes--prompt-depth (1+ (minibuffer-depth)))
        (context-notes--prompt-frame (selected-frame)))
    (condition-case nil
        (apply fn args)
      (quit nil))))

(defun context-notes--cancel-1 ()
  (when (context-notes--prompt-open-p)
    ;; Re-queue first: `abort-recursive-edit' does not return, and it only
    ;; leaves the innermost minibuffer.
    (run-at-time 0 nil #'context-notes--cancel-1)
    (abort-recursive-edit)))

;;;###autoload
(defun context-notes-cancel ()
  "Close the context-notes prompt (fallback or heading picker) if one is open."
  (interactive)
  ;; From a timer, so it also works when called via emacsclient.
  (run-at-time 0 nil #'context-notes--cancel-1))

(defun context-notes--check-focus ()
  (when (and context-notes-cancel-on-focus-out
             (context-notes--prompt-open-p)
             (frame-live-p context-notes--prompt-frame)
             (not (eq (frame-focus-state context-notes--prompt-frame) t)))
    ;; Wait a moment: the window manager can drop focus briefly while it
    ;; moves or fullscreens the frame.
    (run-at-time 0.3 nil
                 (lambda ()
                   (when (and (context-notes--prompt-open-p)
                              (frame-live-p context-notes--prompt-frame)
                              (eq (frame-focus-state context-notes--prompt-frame) nil))
                     (context-notes--cancel-1))))))

(add-function :after after-focus-change-function #'context-notes--check-focus)

;;;; Entry points

(defun context-notes--frame (name)
  (seq-find (lambda (f) (equal (frame-parameter f 'name) name)) (frame-list)))

(defun context-notes-show (keys &optional frame-name)
  "Show notes matching KEYS, in the frame called FRAME-NAME if given."
  (when-let ((frame (and frame-name (context-notes--frame frame-name))))
    (select-frame-set-input-focus frame))
  (setq context-notes--last-keys keys)
  (context-notes--kill-old)
  (let ((markers (context-notes-collect keys)))
    (pcase (length markers)
      (0 (context-notes--with-prompt context-notes-fallback keys))
      (1 (context-notes--show-one (car markers)))
      (_ (context-notes--with-prompt #'context-notes--pick-heading
                                     (context-notes--show-many markers keys))))))

;;;###autoload
(defun context-notes-request (keys &optional frame-name)
  "Entry point for emacsclient: leave any active minibuffer, then show KEYS."
  (run-at-time 0 nil #'context-notes--request-1 keys frame-name))

(defun context-notes--request-1 (keys frame-name)
  (if (> (minibuffer-depth) 0)
      ;; Re-queue first: `abort-recursive-edit' does not return.
      (progn (run-at-time 0 nil #'context-notes--request-1 keys frame-name)
             (abort-recursive-edit))
    (context-notes-show keys frame-name)))

(defun context-notes-emacs-keys (&optional buffer)
  "Context keys describing BUFFER (default: current buffer)."
  (with-current-buffer (or buffer (current-buffer))
    (delq nil
          (list "emacs"
                (format "mode:%s" major-mode)
                (concat "lang:" (replace-regexp-in-string
                                 "\\(-ts\\)?-mode\\'" "" (symbol-name major-mode)))
                (and (or (bound-and-true-p git-commit-mode)
                         (equal (buffer-name) "COMMIT_EDITMSG"))
                     "git-commit")
                (concat "cwd:" (expand-file-name default-directory))
                (and buffer-file-name
                     (concat "ext:" (or (file-name-extension buffer-file-name) "")))
                (and buffer-file-name (concat "file:" buffer-file-name))))))

;;;###autoload
(defun context-notes-here ()
  "Show notes for the current buffer in another window."
  (interactive)
  (let ((context-notes--display-function #'pop-to-buffer))
    (context-notes-show (context-notes-emacs-keys))))

;;;###autoload
(defun context-notes-tag-last (keys)
  "Add KEYS (chosen from the last context) to the CONTEXT of the heading at point."
  (interactive
   (list (completing-read-multiple "Add context keys: " context-notes--last-keys
                                   nil nil nil nil
                                   (car (last (seq-remove
                                               (lambda (k) (string-prefix-p "file:" k))
                                               context-notes--last-keys))))))
  (dolist (k keys)
    (org-entry-add-to-multivalued-property nil context-notes-property k))
  (message "%s: %s" context-notes-property
           (org-entry-get nil context-notes-property)))

(provide 'context-notes)
;;; context-notes.el ends here
