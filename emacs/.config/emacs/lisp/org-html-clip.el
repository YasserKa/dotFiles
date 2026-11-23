;;; org-html-clip.el --- Export Org to clipboard-friendly HTML -*- lexical-binding: t; -*-

;; Author: Yaser Kaddoura
;; Package-Requires: ((emacs "29.1") (org "9.6"))

;;; Commentary:

;; Defines the `html-clip' export backend: Org's HTML exporter with
;; formulas as MathML, inline styles that survive pasting, nested quotes
;; from ">" lines, #+ATTR_ORG widths for images, and local images
;; embedded as base64.  `my/org-html-to-clipboard' copies the result to
;; the clipboard as text/html (Wayland or X).
;;
;; Optional: htmlize, for syntax-coloured source blocks.

;;; Code:

(require 'org)
(require 'ox-html)
(require 'mailcap)
(require 'url-util)
(require 'subr-x)
(require 'htmlize nil t)

(defgroup org-html-clip nil
  "Export Org to clipboard-friendly HTML."
  :group 'org-export)

(defvar my/clip-embed-attachments nil
  "Non-nil: embed local file links as data: attachments (External Editor flow).")

(defvar my/clip-unwrapped-files nil
  "Files whose links were turned into plain text during the last export.")

;;;; Formulas

(defun my/latex-to-mathml (latex)
  "Convert LATEX to MathML using pandoc.  Fall back to the raw LaTeX."
  (with-temp-buffer
    (insert latex)
    (if (zerop (call-process-region (point-min) (point-max) "pandoc" t t nil
                                    "-f" "latex" "-t" "html" "--math-method=mathml"))
        (replace-regexp-in-string "\\`\\s-*<p>\\|</p>\\s-*\\'" "" (buffer-string))
      (org-html-encode-plain-text latex))))

(defun my/clip-latex-fragment (fragment _contents _info)
  "Export a LaTeX FRAGMENT as MathML."
  (my/latex-to-mathml (org-element-property :value fragment)))

(defun my/clip-latex-environment (env _contents _info)
  "Export a LaTeX environment ENV as display MathML."
  (format "<div>%s</div>"
          (my/latex-to-mathml (org-remove-indentation (org-element-property :value env)))))

;;;; Before parsing: quotes and image widths

(defun my/org-md-quotes-to-blocks (backend)
  "Turn \">\"-prefixed lines into nested HTML blockquotes (`html-clip' BACKEND only)."
  (when (org-export-derived-backend-p backend 'html-clip)
    (let ((depth 0))
      (goto-char (point-min))
      (while (not (eobp))
        (let ((level 0))
          (beginning-of-line)
          (unless (org-in-block-p '("src" "example"))
            ;; Count and strip the ">" markers; accepts both "> > x" and ">> x"
            (while (looking-at "[ \t]*> ?")
              (setq level (1+ level))
              (replace-match "")))
          ;; Insert tags before this line; point ends up after them
          (dotimes (_ (max 0 (- depth level)))
            (insert "#+html: </blockquote>\n"))
          (dotimes (_ (max 0 (- level depth)))
            (insert "#+html: <blockquote type=\"cite\">\n"))
          (setq depth level)
          (forward-line 1)))
      ;; Close anything still open at the end
      (unless (bolp) (insert "\n"))
      (dotimes (_ depth)
        (insert "#+html: </blockquote>\n")))))

(defun my/org-attr-org-width-to-html (backend)
  "Copy :width from #+ATTR_ORG to #+ATTR_HTML (`html-clip' BACKEND only)."
  (when (org-export-derived-backend-p backend 'html-clip)
    (let ((case-fold-search t))
      (goto-char (point-min))
      (while (re-search-forward
              "^\\([ \t]*\\)#\\+attr_org:[^\n]*?:width[ \t]+\\([^ \t\n]+\\)" nil t)
        (let* ((indent (match-string 1))
               (width  (match-string 2))
               ;; The group of #+ATTR_ lines this one belongs to
               (block-start (save-excursion
                              (beginning-of-line)
                              (while (and (not (bobp))
                                          (save-excursion
                                            (forward-line -1)
                                            (looking-at "[ \t]*#\\+attr_")))
                                (forward-line -1))
                              (point)))
               (block-end (save-excursion
                            (forward-line 1)
                            (while (looking-at "[ \t]*#\\+attr_")
                              (forward-line 1))
                            (point))))
          (unless (save-excursion
                    (goto-char block-start)
                    (re-search-forward "#\\+attr_html:[^\n]*:width" block-end t))
            (end-of-line)
            (insert "\n" indent "#+attr_html: :width " width)))))))

;;;; Local files: images and links

(defun my/clip--local-file (src dir)
  "Return the absolute path for SRC, relative to DIR, if it is a readable file."
  (unless (or (string-prefix-p "#" src)
              (string-match-p "\\`[a-z][a-z0-9+.-]*:" (string-remove-prefix "file:" src)))
    (let ((file (expand-file-name
                 (url-unhex-string (replace-regexp-in-string "\\`file://" "" src))
                 dir)))
      (and (file-regular-p file) (file-readable-p file) file))))

(defun my/clip--data-uri (file &optional with-filename)
  "Return FILE as a base64 data: URI, with a filename parameter if WITH-FILENAME."
  (format "data:%s%s;base64,%s"
          (or (mailcap-file-name-to-mime-type file) "application/octet-stream")
          (if with-filename (concat ";filename=" (file-name-nondirectory file)) "")
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally file)
            (base64-encode-region (point-min) (point-max) t)
            (buffer-string))))

(defun my/clip--export-dir (info)
  "Directory that relative links resolve against, from export INFO."
  (let ((in (plist-get info :input-file)))
    (if in (file-name-directory in) default-directory)))

(defun my/clip-embed-images (html info)
  "Replace local <img src> paths in HTML with base64 data: URIs."
  (let ((dir (my/clip--export-dir info))
        (re "<img\\([^>]*?\\)src=\"\\([^\"]+\\)\""))
    (replace-regexp-in-string
     re
     (lambda (m)
       (save-match-data
         (string-match re m)
         (let* ((pre (match-string 1 m))
                (file (my/clip--local-file (match-string 2 m) dir)))
           (if file
               (format "<img%ssrc=\"%s\"" pre (my/clip--data-uri file))
             m))))
     html t t)))

(defun my/clip-embed-links (html info)
  "Turn links to local files in HTML into Thunderbird-style data: attachments."
  (let ((dir (my/clip--export-dir info))
        (re "<a\\([^>]*?\\)href=\"\\([^\"]+\\)\""))
    (replace-regexp-in-string
     re
     (lambda (m)
       (save-match-data
         (string-match re m)
         (let* ((pre (match-string 1 m))
                (file (my/clip--local-file (match-string 2 m) dir)))
           (if file
               (format "<a moz-do-not-send=\"false\"%shref=\"%s\""
                       pre (my/clip--data-uri file t))
             m))))
     html t t)))

(defun my/clip-unwrap-links (html info)
  "Replace links to local files in HTML with plain text: \"Description (file.pdf)\"."
  (let ((dir (my/clip--export-dir info))
        (re "<a\\([^>]*?\\)href=\"\\([^\"]+\\)\"[^>]*>\\(\\(?:.\\|\n\\)*?\\)</a>"))
    (replace-regexp-in-string
     re
     (lambda (m)
       (save-match-data
         (string-match re m)
         (let ((file (my/clip--local-file (match-string 2 m) dir))
               (desc (match-string 3 m)))
           (if (not file)
               m
             (let ((name (file-name-nondirectory file)))
               (push name my/clip-unwrapped-files)
               (if (string= desc name)
                   desc
                 (format "%s (%s)" desc (org-html-encode-plain-text name))))))))
     html t t)))

;;;; Final output filter

(defun my/clip-inline-code-style (html _backend info)
  "Inline styles, embedded images and link handling for exported HTML."
  (setq html (replace-regexp-in-string
              "<pre\\b"
              (format "<pre style=\"font-family: monospace, monospace; font-size: 1em; background: %s; color: %s; padding: 0.5em;\""
                      (face-background 'default nil t)
                      (face-foreground 'default nil t))
              html t))
  (setq html (replace-regexp-in-string
              "<blockquote\\b"
              "<blockquote style=\"border-left: 3px solid #ccc; margin-left: 0; padding-left: 1em; color: #555;\""
              html t))
  (setq html (replace-regexp-in-string
              "<code\\b"
              "<code style=\"font-family: monospace, monospace; font-size: 1em;\""
              html t))
  (setq html (my/clip-embed-images html info))
  (setq html (if my/clip-embed-attachments
                 (my/clip-embed-links html info)
               (my/clip-unwrap-links html info)))
  html)

;;;; Backend

(org-export-define-derived-backend 'html-clip 'html
  :translate-alist '((latex-fragment    . my/clip-latex-fragment)
                     (latex-environment . my/clip-latex-environment))
  :filters-alist '((:filter-final-output . my/clip-inline-code-style)))

(add-hook 'org-export-before-parsing-functions #'my/org-attr-org-width-to-html)
(add-hook 'org-export-before-parsing-functions #'my/org-md-quotes-to-blocks)

;;;; Clipboard

;;;###autoload
(defun my/org-html-to-clipboard (&optional subtree-only)
  "Export the buffer (or active region) to HTML on the clipboard as text/html.
With a prefix argument SUBTREE-ONLY, export only the current subtree."
  (interactive "P")
  (setq my/clip-unwrapped-files nil)
  (let* ((html (org-export-as 'html-clip subtree-only nil t   ; t = body-only
                              '(:with-toc nil
                                :section-numbers nil
                                :html-postamble nil
                                :html-preamble nil)))
         (wayland (getenv "WAYLAND_DISPLAY")))
    (when (executable-find "copyq") (call-process "copyq" nil 0 nil "disable"))
    (with-temp-buffer
      (insert html)
      (if wayland
          (call-process-region (point-min) (point-max)
                               "wl-copy" nil 0 nil "--type" "text/html")
        (call-process-region (point-min) (point-max)
                             "xclip" nil 0 nil "-selection" "clipboard" "-t" "text/html")))
    (when (executable-find "copyq") (call-process "copyq" nil 0 nil "enable"))
    (message "Copied as HTML%s"
             (if my/clip-unwrapped-files
                 (format " — attach manually: %s"
                         (string-join (delete-dups (reverse my/clip-unwrapped-files)) ", "))
               ""))))

(provide 'org-html-clip)
;;; org-html-clip.el ends here
