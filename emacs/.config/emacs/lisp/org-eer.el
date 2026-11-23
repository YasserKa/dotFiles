;;; org-eer.el --- Compose Thunderbird emails in Org via External Editor Revived -*- lexical-binding: t; -*-

;; Author: Yaser Kaddoura
;; Package-Requires: ((emacs "29.1") (org "9.6"))

;;; Commentary:

;; Thunderbird's External Editor Revived opens emails as .eml files in
;; `my/eer-dir'.  Those files open in `my/eer-org-mode': the HTML body is
;; converted to Org (quotes as ">" lines), and when emacsclient is
;; released (C-c C-c, C-x #, :wq, closing the frame, killing the buffer)
;; the body is exported back to HTML with the `html-clip' backend.
;;
;; Keys: C-c C-c send back, C-c C-k discard, C-c C-e live preview.
;; Filling (M-q, gw, gq) and joining (J) keep ">" quote prefixes.
;;
;; Requires pandoc.

;;; Code:

(require 'org)
(require 'ox)
(require 'server)
(require 'browse-url)
(require 'subr-x)
(require 'cl-lib)
(require 'org-html-clip)

(defgroup org-eer nil
  "Compose Thunderbird emails in Org via External Editor Revived."
  :group 'org)

(defcustom my/eer-dir (expand-file-name "~/.cache/eer/")
  "Temp directory set in External Editor Revived's preferences."
  :type 'directory)

(defcustom my/eer-incoming-images 'placeholder
  "How to handle inline images in incoming HTML."
  :type '(choice (const :tag "Replace with [image: name]" placeholder)
                 (const :tag "Extract to files" extract)))

(defcustom my/eer-greeting-fmt "Hi %s,"
  "Greeting inserted when replying; %s is the recipients' first names.
Nil disables the greeting."
  :type '(choice string (const :tag "No greeting" nil)))

(defcustom my/eer-preview-port 8765
  "Port for the live email preview (http://localhost:PORT/)."
  :type 'natnum)

(defvar my/eer-img-dir nil
  "Where inline images from incoming HTML are extracted.
Defaults to img/ inside `my/eer-dir'.")

(defvar-local my/eer--done nil
  "Non-nil once this email has been converted to HTML.")

(defvar-local my/eer--original nil
  "The email file's contents as Thunderbird wrote it.")

(defvar my/eer-org-mode-map (make-sparse-keymap))

;;;###autoload
(define-derived-mode my/eer-org-mode org-mode "EER-Org"
  "Org mode for composing Thunderbird emails via External Editor Revived.")

;;;; Headers and body

(defun my/eer--body-start ()
  "Move point to the start of the body, after the header/body blank line.
Ensures the blank line ends with a newline.  Signal an error if there is none."
  (goto-char (point-min))
  (unless (re-search-forward "^$" nil t)
    (user-error "No blank line between headers and body"))
  (if (eobp)
      (insert "\n")      ; blank line at end of buffer: terminate it
    (forward-char 1))    ; step past the blank line's newline
  (point))

(defun my/eer--body-html (org)
  "Export ORG (the email body) to the HTML that gets sent."
  (let ((my/clip-embed-attachments t))
    (org-export-string-as org 'html-clip t
                          '(:with-toc nil :section-numbers nil :preserve-breaks t))))

;;;; Incoming: HTML -> Org

(defun my/eer--save-image (mime data)
  "Write base64 DATA of type MIME to a file in `my/eer-img-dir'; return its path."
  (let ((dir (or my/eer-img-dir (expand-file-name "img/" my/eer-dir))))
    (make-directory dir t)
    (let* ((data (replace-regexp-in-string "[ \t\n\r]" "" data))
           (ext  (replace-regexp-in-string
                  "\\+.*" "" (or (cadr (split-string mime "/")) "bin")))
           (file (expand-file-name
                  (format "%s.%s" (secure-hash 'sha1 data) ext) dir)))
      (unless (file-exists-p file)
        (let ((coding-system-for-write 'binary))
          (with-temp-file file
            (set-buffer-multibyte nil)
            (insert (base64-decode-string data)))))
      file)))

(defun my/eer--extract-data-images (html)
  "Replace inline data: images in HTML with a placeholder, or extract them."
  (let ((re "<img\\b[^>]*?src=\"data:\\([^;,\"]+\\)[^,\"]*;base64,\\([^\"]+\\)\"[^>]*>"))
    (replace-regexp-in-string
     re
     (lambda (m)
       (save-match-data
         (string-match re m)
         (let* ((mime (match-string 1 m))
                (data (match-string 2 m))
                (alt  (and (string-match "alt=\"\\([^\"]*\\)\"" m)
                           (match-string 1 m))))
           (if (eq my/eer-incoming-images 'extract)
               (format "<img src=\"%s\">" (my/eer--save-image mime data))
             (format "<span>[image%s]</span>"
                     (if (and alt (not (string-empty-p alt))) (concat ": " alt) ""))))))
     html t t)))

(defun my/eer--quote-blocks-to-prefix ()
  "Rewrite #+begin_quote blocks in the current buffer as \">\"-prefixed lines."
  (let ((case-fold-search t)
        (depth 0))
    (goto-char (point-min))
    (while (not (eobp))
      (cond ((looking-at "[ \t]*#\\+begin_quote[ \t]*$")
             (setq depth (1+ depth))
             (delete-region (point) (min (point-max) (1+ (line-end-position)))))
            ((looking-at "[ \t]*#\\+end_quote[ \t]*$")
             (setq depth (max 0 (1- depth)))
             (delete-region (point) (min (point-max) (1+ (line-end-position)))))
            (t
             (when (> depth 0)
               (insert (make-string depth ?>) (if (eolp) "" " ")))
             (forward-line 1))))))

(defun my/eer--remove-attribution ()
  "Delete the \"On … wrote:\" line just before the first quote."
  (goto-char (point-min))
  (when (re-search-forward "^>" nil t)
    (beginning-of-line)
    (skip-chars-backward " \t\n")
    (beginning-of-line)
    (when (looking-at "[ \t]*[^>\n].*:[ \t]*$")
      (delete-region (point) (progn (forward-line 1) (point))))))

(defun my/eer-html-body-to-org ()
  "If the email body is HTML, convert it to Org with pandoc."
  (save-excursion
    (let* ((start (my/eer--body-start))
           (body  (buffer-substring-no-properties start (point-max))))
      (when (string-match-p "<\\(html\\|body\\|p\\|div\\|br\\)\\b" body)
        (let ((org (with-temp-buffer
                     (insert (my/eer--extract-data-images body))
                     (unless (zerop (call-process-region
                                     (point-min) (point-max) "pandoc" t '(t nil) nil
                                     "-f" "html-native_divs-native_spans"
                                     "-t" "org" "--wrap=none"))
                       (error "Pandoc failed to convert the email body"))
                     ;; Drop lines that are only "\\" and strip trailing "\\"
                     (goto-char (point-min))
                     (while (re-search-forward "^[ \t]*\\\\\\\\[ \t]*$" nil t)
                       (replace-match ""))
                     (goto-char (point-min))
                     (while (re-search-forward "[ \t]*\\\\\\\\[ \t]*$" nil t)
                       (replace-match ""))
                     ;; Collapse runs of blank lines
                     (goto-char (point-min))
                     (while (re-search-forward "\n\\{3,\\}" nil t)
                       (replace-match "\n\n"))
                     ;; Quote blocks -> "> " lines, minus the attribution line
                     (my/eer--quote-blocks-to-prefix)
                     (my/eer--remove-attribution)
                     (buffer-string))))
          (delete-region start (point-max))
          (goto-char start)
          (insert org)
          (set-buffer-modified-p nil))))))

(defun my/eer--remember-original ()
  "Store the file as Thunderbird wrote it, for `my/eer-abort'."
  (let ((file buffer-file-name))
    (when file
      (setq my/eer--original
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert-file-contents-literally file)
              (buffer-string))))))

;;;; Greeting and cursor

(defun my/eer--first-names ()
  "First names of all To: recipients that have a display name."
  (save-excursion
    (let ((end (my/eer--body-start))
          (case-fold-search t)
          names)
      (goto-char (point-min))
      (while (re-search-forward "^To:[ \t]*\\(.*\\)$" end t)
        (let* ((value (string-trim (match-string 1)))
               (display (cond ((string-match "\\`\"\\([^\"]*\\)\"" value)
                               (match-string 1 value))
                              ((string-match "\\`\\([^<]*?\\)[ \t]*<" value)
                               (match-string 1 value))))
               ;; "Last, First" -> "First"
               (display (and display
                             (string-trim (car (last (split-string display ","))))))
               (first (and display (car (split-string display)))))
          (when (and first (not (string-match-p "@" first)))
            (push first names))))
      (delete-dups (nreverse names)))))

(defun my/eer--join-names (names)
  "Join NAMES as \"A\", \"A and B\" or \"A, B and C\"."
  (if (cdr names)
      (concat (string-join (butlast names) ", ") " and " (car (last names)))
    (car names)))

(defun my/eer--goto-body ()
  "Put point at the start of the body, with an empty line above any quote."
  (when (derived-mode-p 'my/eer-org-mode)
    (let ((mod (buffer-modified-p)))
      (goto-char (my/eer--body-start))
      ;; Drop leading blank lines
      (delete-region (point) (progn (skip-chars-forward " \t\n") (line-beginning-position)))
      ;; Reply, signature-only or empty body: open a line to type on
      (when (or (eobp) (looking-at "[ \t]*\\(>\\|-- \\|--$\\|#\\+begin_\\(example\\|src\\)\\)"))
        (let ((name (and my/eer-greeting-fmt (looking-at "[ \t]*>")
                         (my/eer--join-names (my/eer--first-names)))))
          (when name (insert (format my/eer-greeting-fmt name) "\n\n"))
          (save-excursion (insert "\n\n"))))
      (set-buffer-modified-p mod))))

;;;; Outgoing: Org -> HTML

(defun my/eer--convert-and-save ()
  "Convert the Org body to HTML and save.  Runs only once per buffer."
  (unless my/eer--done
    (setq my/eer--done t)
    (save-excursion
      (let* ((start (my/eer--body-start))
             (html  (my/eer--body-html
                     (buffer-substring-no-properties start (point-max)))))
        (delete-region start (point-max))
        (goto-char start)
        (insert html)))
    (save-buffer)))

(defun my/eer-finish ()
  "Convert, save, and return the email to Thunderbird."
  (interactive)
  (my/eer--convert-and-save)
  (server-edit))

(defun my/eer-abort ()
  "Discard all edits and return the email to Thunderbird unchanged."
  (interactive)
  (when (or (not (buffer-modified-p))
            (y-or-n-p "Discard your changes to this email? "))
    (setq my/eer--done t)               ; skip the HTML conversion
    (when my/eer--original              ; undo any earlier saves
      (let ((coding-system-for-write 'binary))
        (write-region my/eer--original nil buffer-file-name nil 'silent)))
    (set-buffer-modified-p nil)
    (kill-buffer)))                     ; releases emacsclient, closes a -c frame

(defun my/eer--before-buffer-done (buffer &rest _)
  "Convert BUFFER to HTML just before emacsclient is released."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'my/eer-org-mode)
        (with-demoted-errors "EER conversion failed: %S"
          (my/eer--convert-and-save))))))

(defun my/eer--before-server-done (&rest _)
  "Convert before `server-done', so it doesn't ask to save the Org text."
  (my/eer--before-buffer-done (current-buffer)))

(defun my/eer--on-frame-close (frame)
  "When client FRAME closes, convert and save its EER buffers."
  (let ((proc (frame-parameter frame 'client)))
    (when (processp proc)
      (dolist (buf (process-get proc 'buffers))
        (my/eer--before-buffer-done buf)))))

;;;; Quote-aware filling and joining

(defun my/eer--quote-prefix ()
  "Return (LEVEL . PREFIX-END) for the \">\" prefix of the current line."
  (save-excursion
    (beginning-of-line)
    (if (looking-at "\\(?:[ \t]*>\\)+[ \t]?")
        (cons (cl-count ?> (match-string 0)) (match-end 0))
      (cons 0 (point)))))

(defun my/eer--quote-blank-p ()
  "Non-nil if the current line is quoted but has no text, like \">\"."
  (let ((q (my/eer--quote-prefix)))
    (and (> (car q) 0)
         (save-excursion (goto-char (cdr q)) (looking-at "[ \t]*$")))))

(defun my/eer--group-bounds ()
  "Bounds (BEG . END) of the run of lines around point that share its quote level.
Quoted runs also stop at empty quoted lines such as \">\"."
  (let ((level (car (my/eer--quote-prefix)))
        beg end)
    (cl-flet ((same () (and (= (car (my/eer--quote-prefix)) level)
                            (or (= level 0) (not (my/eer--quote-blank-p))))))
      (save-excursion
        (beginning-of-line)
        (while (and (not (bobp))
                    (save-excursion (forward-line -1) (same)))
          (forward-line -1))
        (setq beg (point)))
      (save-excursion
        (beginning-of-line)
        (while (and (not (eobp)) (same))
          (forward-line 1))
        (setq end (point))))
    (cons beg end)))

(defun my/eer--fill-quoted (beg end level)
  "Fill the lines between BEG and END, all at quote LEVEL, keeping the prefix."
  (let ((prefix (concat (make-string level ?>) " "))
        (end (copy-marker end)))
    ;; Normalize "> > x" / ">>x" to ">> x", then fill with that prefix
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (delete-region (point) (cdr (my/eer--quote-prefix)))
        (insert prefix)
        (forward-line 1)))
    (let ((fill-prefix prefix))
      (fill-region-as-paragraph beg end))
    (set-marker end nil)))

(defun my/eer--fill-plain (beg end &optional at)
  "Fill the unquoted lines between BEG and END the Org way.
With AT, fill only the paragraph at that position.  The text is filled in a
separate buffer, so Org can't merge it with neighbouring quotes."
  (let* ((text (buffer-substring-no-properties beg end))
         (col fill-column)
         (filled
          (with-temp-buffer
            (insert text)
            (delay-mode-hooks (org-mode))
            (setq fill-column col)
            (cl-flet ((fill-here ()
                        ;; Only paragraphs (also inside list items) and
                        ;; comments; code blocks, tables etc. stay as they are
                        (save-excursion
                          (back-to-indentation)
                          (when (looking-at "\\([-+*]\\|[0-9]+[.)]\\)[ \t]+")
                            (goto-char (match-end 0)))
                          (when (memq (org-element-type (org-element-at-point))
                                      '(paragraph comment))
                            (org-fill-paragraph)))))
              (if at
                  (progn (goto-char (1+ (- at beg))) (fill-here))
                (goto-char (point-min))
                (while (not (eobp))
                  (fill-here)
                  (forward-line 1))))
            (buffer-string))))
    (unless (string= text filled)
      (save-excursion
        (goto-char beg)
        (delete-region beg end)
        (insert filled)))))

(defun my/eer--fill-group-at-point (&optional at)
  "Fill the quote or plain run at point; with AT, only that plain paragraph.
Return the end of the filled run."
  (let* ((level (car (my/eer--quote-prefix)))
         (bounds (my/eer--group-bounds))
         (end (copy-marker (cdr bounds))))
    (cond ((my/eer--quote-blank-p))
          ((> level 0) (my/eer--fill-quoted (car bounds) end level))
          (t (my/eer--fill-plain (car bounds) end at)))
    (prog1 (marker-position end) (set-marker end nil))))

(defun my/eer--fill-region (orig beg end &rest args)
  "Around `fill-region' (gw, gq): keep \">\" prefixes and leave headers alone.
ORIG, BEG, END and ARGS are those of `fill-region'."
  (if (not (derived-mode-p 'my/eer-org-mode))
      (apply orig beg end args)
    (save-excursion
      (let ((start (save-excursion
                     (goto-char (max beg (save-excursion (my/eer--body-start))))
                     (line-beginning-position)))
            (end (save-excursion
                   (goto-char end)
                   (if (bolp) (point) (line-end-position)))))
        (when (< start end)
          (save-restriction
            ;; Fill only the lines in the region
            (narrow-to-region start end)
            (goto-char (point-min))
            (while (not (eobp))
              (if (my/eer--quote-blank-p)
                  (forward-line 1)
                (goto-char (my/eer--fill-group-at-point))))))))))

(defun my/eer--fill-paragraph (&rest _)
  "Fill function for `my/eer-org-mode' (M-q): quotes keep their prefix."
  (save-excursion
    (let ((pos (point)))
      (unless (< pos (save-excursion (my/eer--body-start)))
        (my/eer--fill-group-at-point pos))))
  t)

(defun my/eer--join-quotes (orig &optional arg beg end)
  "Around `delete-indentation' (J): drop the joined line's \">\" prefix.
The prefix is removed before ORIG runs, so this works with any version of
`delete-indentation'.  ARG, BEG and END are those of `delete-indentation'."
  (when (and (derived-mode-p 'my/eer-org-mode) (not beg) (not end)
             ;; The line we join onto is quoted
             (save-excursion
               (unless arg (forward-line -1))
               (> (car (my/eer--quote-prefix)) 0)))
    (save-excursion
      (when arg (forward-line 1))
      (beginning-of-line)
      (when (and (not (bobp)) (looking-at "\\(?:[ \t]*>\\)+[ \t]?"))
        (delete-region (match-beginning 0) (match-end 0)))))
  (funcall orig arg beg end))

(defun my/eer--setup-fill ()
  "Use quote-aware filling in this buffer."
  (setq-local fill-paragraph-function #'my/eer--fill-paragraph))

(defun my/eer-return ()
  "RET that continues \">\" quotes.
In quoted text, split the line and start the new one with the same prefix.
On an empty quoted line (just \">\"), remove the prefix to end the quote."
  (interactive)
  (let ((q (my/eer--quote-prefix)))
    (cond
     ((or (= (car q) 0)
          (< (point) (cdr q))
          (< (point) (save-excursion (my/eer--body-start))))
      (call-interactively #'org-return))
     ((my/eer--quote-blank-p)
      (delete-region (line-beginning-position) (line-end-position)))
     (t
      (delete-horizontal-space)
      (insert "\n" (make-string (car q) ?>) " ")))))

;;;; Live preview

(defvar my/eer--preview-server nil)
(defvar my/eer--preview-clients nil)
(defvar my/eer--preview-buffer nil)
(defvar my/eer--preview-timer nil)
(defvar my/eer--preview-tick nil)
(defvar my/eer--preview-html "")

(defconst my/eer--preview-page
  "<!doctype html><meta charset=\"utf-8\"><title>Email preview</title>
<body style=\"max-width: 50em; margin: 2em auto; font-family: sans-serif;\">
<div id=\"c\">%s</div>
<script>
new EventSource('/events').onmessage = e => {
  document.getElementById('c').innerHTML = e.data;
};
</script>")

(defun my/eer--preview-send (proc)
  "Push the current preview HTML to browser connection PROC."
  (process-send-string
   proc
   (concat (mapconcat (lambda (l) (concat "data: " l))
                      (split-string my/eer--preview-html "\n") "\n")
           "\n\n")))

(defun my/eer--preview-filter (proc request)
  "Answer browser REQUEST on PROC: the page itself, or the update stream."
  (cond
   ((string-match "\\`GET /events" request)
    (process-send-string
     proc "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\nCache-Control: no-cache\r\n\r\n")
    (push proc my/eer--preview-clients)
    (my/eer--preview-send proc))
   ((string-match "\\`GET / " request)
    (let ((body (encode-coding-string
                 (format my/eer--preview-page my/eer--preview-html) 'utf-8)))
      (process-send-string
       proc (format "HTTP/1.1 200 OK\r\nContent-Type: text/html; charset=utf-8\r\nContent-Length: %d\r\nConnection: close\r\n\r\n"
                    (length body)))
      (process-send-string proc body)))
   (t
    (process-send-string
     proc "HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"))))

(defun my/eer--preview-sentinel (proc _event)
  "Forget browser connection PROC once it closes."
  (unless (process-live-p proc)
    (setq my/eer--preview-clients (delq proc my/eer--preview-clients))))

(defun my/eer-preview-stop ()
  "Stop the live preview and its server."
  (interactive)
  (when my/eer--preview-timer (cancel-timer my/eer--preview-timer))
  (mapc #'delete-process my/eer--preview-clients)
  (when my/eer--preview-server (delete-process my/eer--preview-server))
  (setq my/eer--preview-timer nil my/eer--preview-server nil
        my/eer--preview-clients nil my/eer--preview-buffer nil))

(defun my/eer--preview-update ()
  "Re-export the previewed buffer and push it if it changed."
  (let ((buf my/eer--preview-buffer))
    (if (not (buffer-live-p buf))
        (my/eer-preview-stop)
      (with-current-buffer buf
        (unless (or my/eer--done
                    (eql my/eer--preview-tick (buffer-chars-modified-tick)))
          (with-demoted-errors "EER preview: %S"
            (setq my/eer--preview-html
                  (my/eer--body-html
                   (save-excursion
                     (buffer-substring-no-properties (my/eer--body-start) (point-max)))))
            (setq my/eer--preview-tick (buffer-chars-modified-tick))
            (mapc #'my/eer--preview-send my/eer--preview-clients)))))))

(defun my/eer-preview ()
  "Toggle a live browser preview of this email."
  (interactive)
  (if (eq my/eer--preview-buffer (current-buffer))
      (progn (my/eer-preview-stop) (message "Live preview stopped"))
    (my/eer-preview-stop)
    (setq my/eer--preview-buffer (current-buffer)
          my/eer--preview-tick nil)
    (my/eer--preview-update)
    (setq my/eer--preview-server
          (make-network-process
           :name "eer-preview" :server t :host 'local
           :service my/eer-preview-port :family 'ipv4 :noquery t
           :coding 'utf-8
           :filter #'my/eer--preview-filter
           :sentinel #'my/eer--preview-sentinel))
    (setq my/eer--preview-timer
          (run-with-idle-timer 0.3 t #'my/eer--preview-update))
    (browse-url (format "http://localhost:%d/" my/eer-preview-port))))

;;;; Setup

(define-key my/eer-org-mode-map (kbd "C-c C-c") #'my/eer-finish)
(define-key my/eer-org-mode-map (kbd "C-c C-k") #'my/eer-abort)
(define-key my/eer-org-mode-map (kbd "C-c C-e") #'my/eer-preview)
(define-key my/eer-org-mode-map (kbd "RET") #'my/eer-return)

(add-hook 'my/eer-org-mode-hook #'my/eer--remember-original -90)
(add-hook 'my/eer-org-mode-hook #'my/eer-html-body-to-org)
(add-hook 'my/eer-org-mode-hook #'my/eer--setup-fill)
(add-hook 'server-visit-hook #'my/eer--goto-body)
;; Depth -50: run before the server's own handler, which releases the client
(add-hook 'delete-frame-functions #'my/eer--on-frame-close -50)
(advice-add 'server-done :before #'my/eer--before-server-done)
(advice-add 'server-buffer-done :before #'my/eer--before-buffer-done)
(advice-add 'fill-region :around #'my/eer--fill-region)          ; gw, gq
(advice-add 'delete-indentation :around #'my/eer--join-quotes)   ; J

(make-directory my/eer-dir t)
(set-file-modes my/eer-dir #o700)
(add-to-list 'auto-mode-alist
             (cons (concat "\\`" (regexp-quote (file-name-as-directory
                                                (expand-file-name my/eer-dir)))
                           ".*\\.eml\\'")
                   'my/eer-org-mode))

(provide 'org-eer)
;;; org-eer.el ends here
