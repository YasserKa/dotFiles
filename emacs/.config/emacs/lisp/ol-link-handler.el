;;; ol-man.el - Support for links to man pages in Org mode
(require 'ol)

; (org-link-set-parameters "link-hand"
;                          :follow #'org-man-open
;                          :export #'org-man-export
;                          :store #'org-man-store-link)
(org-link-set-parameters "link-handler"
                         :follow #'org-link-link-handler
                         :export (lambda (path desc format)
                                   (cond
                                    ((eq format 'html)
                                     (format "<a href=\"%s\">%s</a>" path (or desc path)))
                                    ((eq format 'latex)
                                     (format "\\href{%s}{%s}" path (or desc path)))
                                    (t
                                     (format "%s (%s)" desc path)))))

 (defun org-link-link-handler (path)
  "Handler function for link-handler:// links."
  (start-process-shell-command "" nil (concat "xdg-open \"link-handler:" path "\" >/dev/null" nil)))


(defcustom org-man-command 'link-handler
  "Handles link-handler"
  :group 'org-link
  :type '(choice (const man) (const woman)))

; (defun org-man-open (path _)
;   "Visit the manpage on PATH.
; PATH should be a topic that can be thrown at the man command."
;   (funcall org-man-command path))

; (defun org-man-store-link ()
;   "Store a link to a man page."
;   (when (memq major-mode '(Man-mode woman-mode))
;     ;; This is a man page, we do make this link.
;     (let* ((page (org-man-get-page-name))
;            (link (concat "man:" page))
;            (description (format "Man page for %s" page)))
;       (org-link-store-props
;        :type "man"
;        :link link
;        :description description))))

; (defun org-man-get-page-name ()
;   "Extract the page name from the buffer name."
;   ;; This works for both `Man-mode' and `woman-mode'.
;   (if (string-match " \\(\\S-+\\)\\*" (buffer-name))
;       (match-string 1 (buffer-name))
;     (error "Cannot create link to this man page")))

; (defun org-man-export (link description format _)
;   "Export a man page link from Org files."
;   (let ((path (format "http://man.he.net/?topic=%s&section=all" link))
;         (desc (or description link)))
;     (pcase format
;       (`html (format "<a target=\"_blank\" href=\"%s\">%s</a>" path desc))
;       (`latex (format "\\href{%s}{%s}" path desc))
;       (`texinfo (format "@uref{%s,%s}" path desc))
;       (`ascii (format "%s (%s)" desc path))
;       (t path))))

(provide 'ol-link-handler)

;;; ol-man.el ends here
