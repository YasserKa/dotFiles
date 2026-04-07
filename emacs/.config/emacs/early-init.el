;; Disable package.el in favor of straight.el
; (setq package-enable-at-startup nil)

;; Speed up startup time {{{
;; Temporarily raise garbage collection limit for initialization
(defvar my/backup-gc-cons-threshold gc-cons-threshold)

(defun my/lower-gc-cons-threshold ()
  "Revert back to something slightly bigger than the default."
  (setq gc-cons-threshold (+ my/backup-gc-cons-threshold 200000))
  (remove-function after-focus-change-function #'my/lower-gc-cons-threshold)
  (remove-hook 'after-focus-change-function #'my/lower-gc-cons-threshold))

(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 1024 gc-cons-threshold))
            (run-with-idle-timer 3 nil #'my/lower-gc-cons-threshold)
            (add-function :after after-focus-change-function #'my/lower-gc-cons-threshold)))

(setq use-package-compute-statistics t)

;; Speed up Inhibit file handlers during startup
(defvar my/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook (lambda () (setq file-name-handler-alist my/file-name-handler-alist)))

;; Performance Tweaks from https://emacsredux.com/blog/2026/04/07/stealing-from-the-best-emacs-configs/
;; Disable Bidirectional Text Scanning
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)
;; Skip Fontification During Input
(setq redisplay-skip-fontification-on-input t)
;; Increase Process Output Buffer for LSP
(setq read-process-output-max (* 4 1024 1024)) ; 4MB
;; Don't render cursors in non-focused windows
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
;; }}}

;; Prevent package.el from loading (needed by straight)
(setq package-enable-at-startup nil)
