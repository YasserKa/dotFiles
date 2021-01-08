(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("2b9dc43b786e36f68a9fd4b36dd050509a0e32fe3b0a803310661edb7402b8b6" default))
 '(evil-want-Y-yank-to-eol 1)
 '(helm-completion-style 'emacs)
 '(org-agenda-files
   '("~/notes/org/university.org" "~/notes/org/knowledge_base.org" "~/notes/org/general.org"))
 '(package-selected-packages
   '(helm-core evil evil-magit magit evil-org org-plus-contrib orgalist helm evil-surround general evil-visual-mark-mode gruvbox-theme ##)))

;; Install not presented packages
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

(load-theme 'gruvbox-light-medium t)

;; Remove backup files (ends with ~)
(setq make-backup-files nil)
;; Avoid being prompted with symbolic link to git-controlled
(setq vc-follow-symlinks t)
(set-frame-font "Inconsolata-14")
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;; Remove emacs' bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

;; Remove the annoying Enter (C-j)
(global-unset-key "\C-j")

;; Setting it from <C-h>
(setq help-char (string-to-char "?"))

(define-key global-map "\C-q" 'help)
(define-key global-map "\C-V" 'yank)
(define-key global-map (kbd "\C-x \C-m") 'execute-extended-command)
(define-key global-map (kbd "<escape>") 'keyboard-escape-quit)

(define-key global-map "\C-h" 'delete-backward-char)
(define-key global-map "\C-w" 'backward-kill-word)

(define-key isearch-mode-map (kbd "\C-h") 'isearch-delete-char)
(define-key isearch-mode-map (kbd "\C-q") 'help)

;; unbind C-, in fly mode
;; HELM
(eval-after-load "flyspell"
'(define-key flyspell-mode-map (kbd "C-,") nil))


(electric-pair-mode 1)
;; Make electric-pair-mode work on more brackets
(setq electric-pair-pairs
      '(
	(?\" . ?\")
	(?\< . ?\>)
	(?\[ . ?\])
	(?\` . ?\`)
	(?\{ . ?\})))

;; HELM
(require 'helm-config)
(helm-mode 1)

(define-key helm-map "\C-h" 'delete-backward-char)
(define-key helm-map "\C-w" 'backward-kill-word)
(define-key helm-map "\C-j" 'helm-confirm-and-exit-minibuffer)
(define-key helm-map "\C-l" 'helm-end-of-buffer)

(define-key helm-find-files-map "\C-x" 'helm-ff-run-switch-other-window)
(define-key helm-find-files-map "\C-v" (kbd "C-u C-c o"))
(define-key helm-find-files-map "\C-l" 'helm-execute-persistent-action)

;; EVIL
(require 'evil)
(evil-mode 1)

;; Initial state is evil
(setq evil-normal-state-modes
      (append evil-emacs-state-modes
	      evil-insert-state-modes
	      evil-normal-state-modes
	      evil-motion-state-modes))

;; Registers
(define-key evil-normal-state-map "m"  'bookmark-set)
(define-key evil-normal-state-map "'"  'bookmark-jump-other-window)

;; Y is y$
(setq evil-want-Y-yank-to-eol '1)

;; gx opens urls
(define-key evil-normal-state-map "gx" 'org-open-at-point)

;; Commenting
(define-key evil-normal-state-map "gcc" 'comment-line)
(define-key evil-visual-state-map "gcc" 'comment-or-uncomment-region)

;; Enter in command mode
(define-key evil-ex-completion-map "\C-j" 'exit-minibuffer)
(defvar my-overriding-binding-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-h] 'delete-backward-char)
    (define-key map [?\M-h] 'backward-kill-word)
    map))

(define-minor-mode my-overriding-binding-mode
  "Personal global key-bindings."
  :global t)

(my-overriding-binding-mode 1)


(require 'evil-surround)
(global-evil-surround-mode 1)

;; Spell checking toggle with yos
(evil-define-key 'operator evil-surround-mode-map "os" 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
(define-key evil-insert-state-map "\C-l"  'flyspell-auto-correct-previous-word)

(require 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(evil-org-set-key-theme '(navigation insert textobjects additional calendar))
(require 'evil-org-agenda)
(evil-org-agenda-set-keys)

;; MAGIT
(require 'evil-magit)
(evil-define-key evil-magit-state magit-mode-map "?" 'evil-search-backward)
(setq magit-repository-directories '(("~/dotFiles" . 0) ("~/university/courses/introduction_to_datascience/lecture_notes" . 0) ("~/Projects/" . 1) ("/srv/http/cooldown" . 0)))
;; Close the popups in magit
(define-key transient-edit-map   (kbd "<escape>") 'transient-quit-one)
(define-key transient-map        (kbd "<escape>") 'transient-quit-one)
(define-key transient-sticky-map (kbd "<escape>") 'transient-quit-seq)

;; Enable spell checking while commiting
(add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)

;; Open file using nvim
(evil-define-key evil-magit-state magit-mode-map (kbd "RET") 'vil-diff-visit-file)
(defun vil-diff-visit-file (file &optional other-window)
  (interactive (list (magit-file-at-point t t) current-prefix-arg))
  (shell-command (concat "nvim-qt " file nil)))

;; Updating the original by closing the list of repos window
(defun magit-repolist-status (&optional _button)
  "Show the status for the repository at point."
  (interactive)
  (--if-let (tabulated-list-get-id)
      (let ((p (selected-window)))
	(magit-status-setup-buffer (expand-file-name it)) (delete-window p))
    (user-error "There is no repository at point")))


;; VIM LEADER
(require 'general)
(general-evil-setup t)

(defun source-init-file ()
  (interactive)
  (load-file "~/.emacs"))

(defun edit-init-file ()
  (interactive)
  (split-window-below)
  (find-file "~/.emacs"))

(nvmap :prefix ","
  "ss" 'source-init-file
  "es" 'edit-init-file
  "p" 'helm-find-files)
(general-define-key
 :states 'motion
 ";" 'evil-ex
 ":" 'evil-repeat-find-char
 :states 'normal
 "\C-u" 'evil-scroll-up
 "g:" 'goto-last-change
 )
;; ORG MODE
(require 'org)

(evil-define-key 'normal org-mode-map (kbd "RET") 'org-open-at-point)
(define-key org-mode-map "\C-j" nil)

;; Prevent inserting a new element to split the line
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
(setq org-cycle-separator-lines -1)
(evil-define-key 'normal org-mode-map (kbd "<SPC>") 'org-cycle)

;; Narrowing buffer to subree/ widen
(define-key org-mode-map (kbd "C-c n") 'org-toggle-narrow-to-subtree)

;; Open using external softwares
(setq org-file-apps
      '((auto-mode . emacs)
	("\\.pdf\\'" . "zathura \"%s\"")))
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "qutebrowser")

;; ORG NOTIFICATION

(require 'appt)

(setq
 appt-time-msg-list nil                           ;; clear existing appt list
 appt-message-warning-time '10                    ;; send first warning before appointment
 appt-display-interval '5                         ;; warn every every X minutes from t - appt-message-warning-time
 appt-display-mode-line nil                       ;; don't show in the modeline
 appt-display-format 'window)                     ;; pass warnings to the designated window function
(setq appt-disp-window-function (function toast-appt-display))

(appt-activate 1)                ;; activate appointment notification

;; Set up the call to the notifier
(defun toast-appt-send-notification (title msg)
  (shell-command (concat "/usr/bin/dunstify -a Emacs " " \"" title "\" \"" msg "\"")))

;; Designate the window function for my-appt-send-notification
(defun toast-appt-display (min-to-app new-time msg)
  (toast-appt-send-notification
   (format "%s minutes" min-to-app)    ;; passed to -t in toast call
   (format "%s" msg))                                 ;; passed to -m in toast call
  (message nil))

(defun org-agenda-to-appt-clear-message ()
  (interactive) (org-agenda-to-appt) (message nil))

(run-at-time 0 3600 'org-agenda-to-appt-clear-message)                 ;; update appt list hourly
(org-agenda-to-appt-clear-message)                                     ;; generate the appt list from org agenda files on emacs launch
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt-clear-message) ;; update appt list on agenda view
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
