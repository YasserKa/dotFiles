(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'load-path "~/.emacs.d/plugins/evil-org")

(setq package-enable-at-startup nil)
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(initial-buffer-choice "~/org/general.org")
 '(org-agenda-files
   (quote
    ("~/org/giveaway_bot.org" "~/org/general.org" "~/org/knowledge_base.org")))
 '(package-selected-packages
   (quote
    (magit org evil-org helm-core general evil-visual-mark-mode ##))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; remove backup files (ends with ~)
(setq make-backup-files nil)
;; avoid being prompted with symbolic link to git-controlled
(setq vc-follow-symlinks t)
(set-default-font "Inconsolata-14")
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

(electric-pair-mode 1)
;; make electric-pair-mode work on more brackets
(setq electric-pair-pairs
      '(
	(?\" . ?\")
	(?\< . ?\>)
	(?\[ . ?\])
	(?\` . ?\`)
	(?\{ . ?\})))

(require 'helm-config)
(helm-mode 1)

(define-key helm-map "\C-h" 'delete-backward-char)
(define-key helm-map "\C-w" 'backward-kill-word)
(define-key helm-map "\C-k" 'helm-previous-line)
(define-key helm-map "\C-j" 'helm-next-line)

(define-key helm-map "\C-l" 'helm-end-of-buffer)

(define-key helm-find-files-map "\C-x" 'helm-ff-run-switch-other-window)
(define-key helm-find-files-map "\C-v" (kbd "C-u C-c o"))

(define-key helm-find-files-map "\C-l" 'helm-execute-persistent-action)


(require 'evil)
(evil-mode t)

(require 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(evil-org-set-key-theme '(navigation insert textobjects additional calendar))
(require 'evil-org-agenda)
(evil-org-agenda-set-keys)
;; remove the annoying Enter (C-j)
(global-unset-key "\C-j")

(define-key global-map "\C-q" 'help)
(define-key global-map (kbd "\C-x \C-m") 'execute-extended-command)
(define-key global-map (kbd "<escape>") 'keyboard-escape-quit)

(define-key global-map "\C-h" 'delete-backward-char)
(define-key global-map "\C-w" 'backward-kill-word)

(define-key isearch-mode-map (kbd "\C-h") 'isearch-delete-char)
(define-key isearch-mode-map (kbd "\C-q") 'help)

(define-key evil-normal-state-map ",c " 'comment-line)
(define-key evil-visual-state-map ",c " 'comment-or-uncomment-region)

(require 'general)
(general-evil-setup t)
(nvmap :prefix ","
  "ss" 'source-init-file
  "es" 'edit-init-file
  "m" 'execute-extended-command
  "p" 'helm-find-files)
(general-define-key
 :states 'motion
 ";" 'evil-ex
 ":" 'evil-repeat-find-char
 :states 'normal
 "\C-u" 'evil-scroll-up
 )
;; ORG MODE
(require 'org)

(evil-define-key 'normal org-mode-map (kbd "RET") 'org-open-at-point)
(define-key org-mode-map "\C-j" nil)

;; prevent inserting a new element to split the line
(setq org-M-RET-may-split-line 'nil)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)

(define-key org-mode-map "\M-l" 'org-metaright)
(define-key org-mode-map (kbd "M-l") 'org-shiftmetaright)
(define-key org-mode-map "\M-h" 'org-metaleft)
(define-key org-mode-map (kbd "M-H") 'org-shiftmetaleft)
(define-key org-mode-map "\M-k" 'org-metaup)
(define-key org-mode-map (kbd "M-K") 'org-shiftmetaup)
(define-key org-mode-map "\M-j" 'org-metadown)
(define-key org-mode-map (kbd "M-J") 'org-shiftmetadown)

;; TODO: might not be needed
;; Navigation
(evil-define-key 'normal org-mode-map (kbd "gk") 'org-previous-visible-heading)
(evil-define-key 'normal org-mode-map (kbd "gj") 'org-next-visible-heading)
(evil-define-key 'normal org-mode-map (kbd "gh") 'org-up-element)
(evil-define-key 'normal org-mode-map (kbd "gl") 'org-down-element)

;; Narrowing buffer to subree/ widen
(define-key org-mode-map (kbd "C-c n") 'org-toggle-narrow-to-subtree)

;; change markdown to org
(define-key org-mode-map (kbd "C-c C-m") 'markdown-to-org)

;; open using external softwares
(setq org-file-apps
      '((auto-mode . emacs)
	("\\.pdf\\'" . "zathura \"%s\"")))
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "qutebrowser")

;; FUNCTIONS
(defun markdown-to-org ()
  (interactive)
  (setq l (shell-command-to-string
	   (concat "echo -n '" (thing-at-point 'line t) "' | pandoc --wrap=none -t org")))
  (kill-whole-line)
  (insert l)
  )

(defun source-init-file ()
  (interactive)
  (load-file "~/.emacs"))

(defun edit-init-file ()
  (interactive)
  (split-window-below)
  (find-file "~/.emacs"))

;; ORG NOTIFICATION

(require 'appt)
(setq appt-time-msg-list nil)    ;; clear existing appt list
(setq appt-display-interval nil)  ;; warn every 5 minutes from t - appt-message-warning-time
(setq
 appt-message-warning-time '10  ;; send first warning 15 minutes before appointment
 appt-display-mode-line nil     ;; don't show in the modeline
 appt-display-format 'window)   ;; pass warnings to the designated window function
(appt-activate 1)                ;; activate appointment notification
(display-time)                   ;; activate time display

(defun org-agenda-to-appt-clear-message ()
  (interactive)
  (org-agenda-to-appt)
  (message nil)
  )

(run-at-time 0 3600 'org-agenda-to-appt-clear-message)           ;; update appt list hourly
(org-agenda-to-appt-clear-message)             ;; generate the appt list from org agenda files on emacs launch
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt-clear-message) ;; update appt list on agenda view

(defvar toast-notifier-path
  "/usr/bin/zenity"
  )

;; set up the call to the notifier
(defun toast-appt-send-notification (title msg)
  (shell-command (concat toast-notifier-path
			 " --info --title \"" title "\""
			 " --text \"" msg "\"")))

;; designate the window function for my-appt-send-notification
(defun toast-appt-display (min-to-app new-time msg)
  (toast-appt-send-notification
   (format "Appointment in %s minutes" min-to-app)    ;; passed to -t in toast call
   (format "%s" msg)))                                ;; passed to -m in toast call

(setq appt-disp-window-function (function toast-appt-display))
