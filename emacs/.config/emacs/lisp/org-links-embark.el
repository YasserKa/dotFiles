;;; org-links-embark.el --- Embark actions for Org node links -*- lexical-binding: t; -*-

;; Requires: embark, org-backlinks.el, org-forwardlinks.el.

;;; Commentary:
;;
;; With point on an Org link to a node, e.g.
;;
;;     [[id:8bcd0e53-ae5d-428f-9306-d63bbaab3911][test]]
;;
;; `embark-act' (C-. in your setup) offers, in addition to Embark's general
;; actions:
;;
;;   b   Backlinks of the node the link points to         (consult)
;;   B   Backlinks of all nodes in the link target's file (consult)
;;   f   Forward links of the node the link points to     (consult)
;;   F   Forward links of all nodes in the target's file  (consult)
;;
;; These are the same searches as `my/org-backlinks' and
;; `my/org-forward-links', but run for the link target instead of for the
;; node at point.  "Node" has the usual meaning: the closest heading (self or
;; ancestor) with an :ID:, or the file-level node.
;;
;; Supported links: id:, file:x.org, file:x.org::#custom-id,
;; file:x.org::*Heading and #custom-id.
;;
;; The target finder is added in front of `embark-target-finders', so Org
;; node links win over Embark's own `org-link' target.  Other targets at
;; point remain available through `embark-cycle'.

;;; Code:

(require 'org)
(require 'org-element)
(require 'subr-x)
(require 'org-backlinks)
(require 'org-forwardlinks)

;;;; Finding the link -------------------------------------------------------

(defvar my/org-embark--last-link nil
  "Raw link string found by the last run of the Embark target finder.")

(defun my/org-embark--link-at-point ()
  "Return (RAW BEG . END) for the Org node link at point, or nil.
Only links to Org nodes (id:, .org files, #custom-id) count."
  (when (derived-mode-p 'org-mode)
    (when-let* ((link (org-element-lineage (org-element-context) '(link) t))
                (raw (org-element-property :raw-link link))
                ((my/org-bl--classify raw default-directory)))
      (cons raw
            (cons (org-element-property :begin link)
                  (- (org-element-property :end link)
                     (or (org-element-property :post-blank link) 0)))))))

(defun my/org-embark-target-link ()
  "Embark target finder for Org links to nodes."
  (when-let* ((hit (my/org-embark--link-at-point)))
    (setq my/org-embark--last-link (car hit))
    `(org-node-link ,(car hit) ,(cadr hit) . ,(cddr hit))))

;;;; Resolving the link -----------------------------------------------------

(defun my/org-embark--resolve (link)
  "Return (FILE POS LABEL) of the node the raw Org LINK points to.
LINK is interpreted relative to the current buffer."
  (let* ((self (file-truename (buffer-file-name (buffer-base-buffer))))
         (e (my/org-bl--classify link (file-name-directory self))))
    (unless e (user-error "Not an Org node link: %s" link))
    (or (my/org-fl--resolve
         e self
         (if (eq (plist-get e :kind) 'id)
             (my/org-fl--id-table (my/org-bl--files self))
           (make-hash-table :test 'equal)))
        (user-error "Cannot resolve link: %s" link))))

(defun my/org-embark--run (link fn whole-file)
  "Call FN with the buffer and position of the node LINK points to.
With WHOLE-FILE, pass nil as position (all nodes of the target's file).
LINK defaults to the link at point."
  (let* ((link (or link
                   (car (my/org-embark--link-at-point))
                   my/org-embark--last-link
                   (user-error "No Org node link at point")))
         (target (my/org-embark--resolve link))
         (file (nth 0 target))
         (buf (my/org-bl-find-file-quietly file)))
    ;; Embark injects its target into the first minibuffer opened while the
    ;; action runs and exits it at once, which silently kills a consult
    ;; prompt.  Starting the prompt from a timer takes it out of that scope.
    (let ((pin (unless whole-file (nth 1 target))))
      (run-at-time
       0 nil
       (lambda ()
         (condition-case err
             (funcall fn buf pin)
           (quit (message "Quit"))
           (error (message "%s" (error-message-string err)))))))))

;;;; Actions ----------------------------------------------------------------

(defun my/org-embark-backlinks-node (&optional link)
  "Backlinks of the node LINK points to (default: link at point)."
  (interactive)
  (my/org-embark--run link #'my/org-bl--consult nil))

(defun my/org-embark-backlinks-file (&optional link)
  "Backlinks of all nodes in the file LINK points to."
  (interactive)
  (my/org-embark--run link #'my/org-bl--consult t))

(defun my/org-embark-forward-node (&optional link)
  "Forward links of the node LINK points to (default: link at point)."
  (interactive)
  (my/org-embark--run link #'my/org-fl--consult nil))

(defun my/org-embark-forward-file (&optional link)
  "Forward links of all nodes in the file LINK points to."
  (interactive)
  (my/org-embark--run link #'my/org-fl--consult t))

;;;; Embark integration -----------------------------------------------------

(with-eval-after-load 'embark
  (defvar my/org-embark-node-link-map
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map embark-general-map)
      (define-key map (kbd "b") #'my/org-embark-backlinks-node)
      (define-key map (kbd "B") #'my/org-embark-backlinks-file)
      (define-key map (kbd "f") #'my/org-embark-forward-node)
      (define-key map (kbd "F") #'my/org-embark-forward-file)
      map)
    "Embark keymap for Org links to nodes.")
  (add-to-list 'embark-keymap-alist '(org-node-link . my/org-embark-node-link-map))
  (add-to-list 'embark-target-finders #'my/org-embark-target-link))

(provide 'org-links-embark)
;;; org-links-embark.el ends here
