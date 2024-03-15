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
;; }}}
