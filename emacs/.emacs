(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'load-path "~/.emacs.d/private/org-roam-ui")

(setq package-enable-at-startup nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("2b9dc43b786e36f68a9fd4b36dd050509a0e32fe3b0a803310661edb7402b8b6" default))
 '(evil-want-Y-yank-to-eol 1)
 '(org-agenda-files
   '("~/notes/RoamNotes/20210908134129project.org" "/home/yasser/notes/RoamNotes/advanced_probabilisitc_machine_learning.org" "/home/yasser/notes/RoamNotes/data_mining.org" "/home/yasser/notes/RoamNotes/interested_in.org"))
 '(org-roam-ui-mode nil)
 '(package-selected-packages
   '(org-appear deft orderless marginalia vertico evil-textobj-anyblock cdlatex auctex simple-httpd websocket use-package undo-tree undo-redo evil evil-collection org-roam evil-org org-plus-contrib orgalist evil-surround general evil-visual-mark-mode gruvbox-theme ##)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit outline-1 :height 1.25))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.15))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.1))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.05)))))

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
;; (set-frame-font "Inconsolata-14" nil)
(add-to-list 'default-frame-alist
	     '(font . "DejaVu Sans Mono-13"))
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

;; line wrapping
(global-visual-line-mode t)
(add-hook 'org-mode-hook '(lambda () (setq fill-column 100)))
(add-hook 'org-mode-hook 'turn-on-auto-fill)

(define-key global-map "\C-h" 'delete-backward-char)
(define-key global-map "\C-w" 'backward-kill-word)

(define-key isearch-mode-map (kbd "\C-h") 'isearch-delete-char)
(define-key isearch-mode-map (kbd "\C-q") 'help)

;; unbind C-, in fly mode
(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-,") nil)
  )
(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-M-i") nil)
  )

(electric-pair-mode 1)
;; Make electric-pair-mode work on more brackets
(setq electric-pair-pairs
      '(
	(?\" . ?\")
	(?\< . ?\>)
	(?\$ . ?\$)
	(?\[ . ?\])
	(?\` . ?\`)
	(?\{ . ?\})))
;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )
(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))
;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; HELM
;; (require 'helm-config)
;; (helm-mode 1)

;; (define-key helm-map "\C-h" 'delete-backward-char)
;; (define-key helm-map "\C-w" 'backward-kill-word)
;; (define-key helm-map "\C-j" 'helm-confirm-and-exit-minibuffer)
;; (define-key helm-map "\C-l" 'helm-end-of-buffer)

;; (define-key helm-find-files-map "\C-x" 'helm-ff-run-switch-other-window)
;; (define-key helm-find-files-map "\C-v" (kbd "C-u C-c o"))
;; (define-key helm-comp-read-map "\C-V" 'evil-paste-after)

;; ;; ignore text under cursor when executing helm-find-files
;; (setq helm-find-files-ignore-thing-at-point t)

;; EVIL
(setq evil-want-keybinding 'nil)
(require 'evil)
(evil-mode 1)

(evil-collection-init)

;; Initial state is evil
(setq evil-normal-state-modes
      (append evil-emacs-state-modes
	      evil-insert-state-modes
	      evil-normal-state-modes
	      evil-motion-state-modes))

;; redoing-undoing in evil
(global-undo-tree-mode)
(evil-set-undo-system 'undo-tree)

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
(define-key evil-ex-completion-map "\C-V" 'evil-paste-after)
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

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/notes/RoamNotes")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("l" "programming language" plain
      "* Characteristics\n\n- Family: %?\n- Inspired by: \n\n* Reference:\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("b" "book notes" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>${slug}.org" "#+title: ${title}\n#+FILETAGS: book_notes\n")
      :unnarrowed t)
     )
   )
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)
	 :map org-mode-map
	 ("C-M-i" . completion-at-point))
  :config
  (load-library "org-roam-ui")
  (org-roam-setup))

;; MAGIT
(evil-define-key evil-collection-magit-state magit-mode-map "?" 'evil-search-backward)
(setq magit-repository-directories '(("~/dotFiles" . 0) ("~/Projects/" . 1) ("/srv/http/cooldown" . 0)))
;; Close the popups in magit
;; (define-key transient-edit-map   (kbd "<escape>") 'transient-quit-one)
;; (define-key transient-map        (kbd "<escape>") 'transient-quit-one)
;; (define-key transient-sticky-map (kbd "<escape>") 'transient-quit-seq)

;; Enable spell checking while commiting
(add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)

;; Open file using nvim
(evil-define-key evil-collection-magit-state magit-mode-map (kbd "RET") 'vil-diff-visit-file)
(defun vil-diff-visit-file (file &optional other-window)
  (interactive (list (magit-file-at-point t t) current-prefix-arg))
  (shell-command (concat "nvim-qt " file nil)))

;; Updating the original by closing the list of repos window
(defun my-magit-repolist-status (&optional _button)
  "Show the status for the repository at point."
  (interactive)
  (--if-let (tabulated-list-get-id)
      (let ((p (selected-window)))
	(magit-status-setup-buffer (expand-file-name it)) (delete-window p))
    (user-error "There is no repository at point")))


(define-key evil-motion-state-map (kbd "RET") 'my-magit-repolist-status)

;; LATEX

;; update the document header for latex preview
(setq org-format-latex-header (concat org-format-latex-header "\n\\input{$HOME/.config/latex/preamble.tex}\n"))

;; local configuration for TeX modes
(plist-put org-format-latex-options :scale 1.5)

;; auto-complete
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)

;; making dollar signs as text object
(defmacro define-and-bind-text-object (key start-regex end-regex)
  (let ((inner-name (make-symbol "inner-name"))
	(outer-name (make-symbol "outer-name")))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
	 (evil-select-paren ,start-regex ,end-regex beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
	 (evil-select-paren ,start-regex ,end-regex beg end type count t))
       (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
       (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))

; between dollar signs
(define-and-bind-text-object "$" "\\$" "\\$")


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
  "p" 'find-file)
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
(setq org-startup-folded t)
(setq org-startup-with-latex-preview t)
(setq org-startup-with-inline-images t)
(setq org-enforce-todo-dependencies t)
; show emphasis markers when hovering over text
(use-package org-appear
  :hook (org-mode . org-appear-mode))
(setq org-hide-emphasis-markers t)

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/notes/tasks.org" "Tasks")
	 "* TODO %?\n  %i\n  %a")
	("g" "Groceries" entry (file+datetree "~/notes/groceries.org")
	 "* %?\nEntered on %U\n  %i\n  %a")))

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

; fix the offset of tags for (org-tags-view)
(setq org-agenda-window-frame-fractions '(0 0))

; indent nested items
(require 'org-indent)
(setq org-startup-indented t)

; exit agenda mode when clicking on an item
(evil-define-key 'motion org-agenda-mode-map
  (kbd "<RET>") '(lambda() (interactive) (org-agenda-switch-to t)))

(setq org-cycle-separator-lines -1)
(evil-define-key 'normal org-mode-map (kbd "<SPC>") 'org-cycle)

;; Narrowing buffer to subree/ widen
;; (define-key org-mode-map (kbd "C-c n") 'org-toggle-narrow-to-subtree)

;; Open using external softwares
(setq org-file-apps
      '((auto-mode . emacs)
	("\\.pdf\\'" . "zathura \"%s\"")
	("\\.epub\\'" . "zathura \"%s\"")
	("\\.djvu\\'" . "zathura \"%s\"")))
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "qutebrowser")

;; ORG NOTIFICATION

(require 'appt)
(appt-activate 1)                ;; activate appointment notification

(setq
 appt-time-msg-list nil                           ;; clear existing appt list
 appt-message-warning-time '10                    ;; send first warning before appointment
 appt-display-interval '5                         ;; warn every every X minutes from t - appt-message-warning-time
 appt-display-mode-line nil                       ;; don't show in the modeline
 appt-display-format 'window)                     ;; pass warnings to the designated window function
(setq appt-disp-window-function (function toast-appt-display))


;; update appt after saving file
(add-hook 'after-save-hook
	  '(lambda ()
	     (if (string= (file-name-directory buffer-file-name) (concat (getenv "HOME") "/notes/org/"))
		 (org-agenda-to-appt-clear-message))))

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
  (interactive) (setq appt-time-msg-list nil) (org-agenda-to-appt) (message nil))

(org-agenda-to-appt-clear-message)                                     ;; generate the appt list from org agenda files on emacs launch
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt-clear-message) ;; update appt list on agenda view
