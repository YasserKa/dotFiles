;;; my-open-elsewhere-citar.el --- Open a paper's notes in a split from citar-open -*- lexical-binding: t; -*-

;; Usage: put this file next to my-open-elsewhere.el and load it after
;; Citar, e.g.
;;
;;   (with-eval-after-load 'citar
;;     (require 'my-open-elsewhere-citar))
;;
;; In the `citar-open' prompt:
;;
;;   RET  as usual: choose among the paper's files, links and notes
;;   C-x  open the paper's notes in a new window below
;;   C-v  open the paper's notes in a new window to the right
;;   M-w  open the paper's notes in a new frame
;;
;; The keys take the highlighted paper plus any you already selected
;; with TAB, exactly like RET does in Citar's multiple selection.
;;
;; The keys and the window/frame handling come from my-open-elsewhere.el.
;; This file adds two things:
;; - Citar's multiple selection is a loop of prompts that ends only on
;;   its own RET (`citar--multiple-exit'), so the keys finish it the
;;   same way instead of just selecting one more paper;
;; - when the reference prompt was left with one of the keys,
;;   `citar-open-notes' is called for the chosen papers instead of
;;   `citar-open' going on to "Select resource".
;;
;; Written against Citar 1.4.

(require 'my-open-elsewhere)

(declare-function citar-open-notes "citar")
(declare-function citar--multiple-exit "citar")
(defvar citar--multiple-setup)

(defvar my/citar--in-open nil
  "Non-nil while `citar-open' is running, so other Citar prompts are left alone.")

;;; Finishing Citar's multiple selection

(defun my/citar--multiple-exit ()
  "In Citar's multiple-selection prompt, finish it like Citar's own RET.
Returns non-nil when it handled the exit (see
`my/open-elsewhere-exit-functions')."
  (when (and (boundp 'citar--multiple-setup)
             (fboundp 'citar--multiple-exit)
             (eq (lookup-key (current-local-map)
                             (kbd (cdr citar--multiple-setup)))
                 #'citar--multiple-exit))
    ;; `citar--multiple-exit' queues the key that accepts the highlighted
    ;; candidate, and Citar ends its loop when `last-command' is
    ;; `citar--multiple-exit'. Setting `this-command' makes it so, since
    ;; that is what `last-command' is set to next.
    (citar--multiple-exit)
    (setq this-command #'citar--multiple-exit)
    t))

(add-hook 'my/open-elsewhere-exit-functions #'my/citar--multiple-exit)

;;; Opening the notes instead of the usual resources

(defun my/citar--select-refs-around (orig &rest args)
  "Around advice for `citar-select-refs' (ORIG, ARGS).
Inside `citar-open', if the prompt was left with an open-elsewhere key,
throw the chosen references to `my/citar--open-around'."
  (if (not my/citar--in-open)
      (apply orig args)
    (let* ((my/open-elsewhere-prepared nil)
           (refs (apply orig args)))
      (if my/open-elsewhere-prepared
          (throw 'my/citar--notes (list (cons 'my/citar--notes refs)))
        refs))))

(defun my/citar--notes-request-p (args)
  "Non-nil if ARGS is the marker thrown by `my/citar--select-refs-around'."
  (and (consp (car args)) (eq (caar args) 'my/citar--notes)))

(defun my/citar--open-notes (refs)
  "Open the notes of REFS in the selected window."
  (unless refs
    (user-error "No reference selected"))
  ;; `citar-open-prompt' lists `citar-open', which would make Citar ask
  ;; "Select resource" even for a single note.
  (let ((this-command #'citar-open-notes))
    (citar-open-notes refs)))

(defun my/citar--open-around (orig &rest args)
  "Around advice for `citar-open' (ORIG, ARGS).
Opens the notes instead when the reference prompt was left with an
open-elsewhere key. `citar-open' prompts in its interactive form, so
that is where the request is caught."
  (interactive (lambda (spec)
                 (let ((my/citar--in-open t))
                   (catch 'my/citar--notes
                     (advice-eval-interactive-spec spec)))))
  (if (my/citar--notes-request-p args)
      ;; The new window or frame is already selected.
      (my/citar--open-notes (cdar args))
    (apply orig args)))

(advice-add 'citar-select-refs :around #'my/citar--select-refs-around)
(advice-add 'citar-open :around #'my/citar--open-around)

(add-to-list 'my/open-elsewhere-commands 'citar-open)
(when my/open-elsewhere-global-mode
  ;; Advise `citar-open' for the keys as well.
  (my/open-elsewhere-global-mode 1))

(provide 'my-open-elsewhere-citar)
;;; my-open-elsewhere-citar.el ends here
