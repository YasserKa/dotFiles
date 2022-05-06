;; -*- lexical-binding: t; -*-
;; Initialization (speed up)
;; Initialize package sources
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("nognu" . "https://elpa.nongnu.org/nongnu/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
;; Raise gc while during startup and when minibuffer is active
(defun my/defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(setq my/gc-cons-threshold 16777216) ;; 16mb

;; Startup
(my/defer-garbage-collection-h)

(add-hook 'emacs-init-hook (lambda () (setq gc-cons-threshold my/gc-cons-threshold)))

;; Minibuffer
(defun my/restore-garbage-collection-h ()
  ;; Defer it so that commands launched immediately after will enjoy the
  ;; benefits.
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold my/gc-cons-threshold))))

(add-hook 'minibuffer-setup-hook #'my/defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'my/restore-garbage-collection-h)

;; https://github.com/hlissner/doom-emacs/blob/master/docs/faq.org#user-content-unset-file-na me-handler-alist-temporarily
;; Unset this variable during startup to make Emacs ignore it
(defvar my/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook (lambda () (setq file-name-handler-alist my/file-name-handler-alist)))
;; Load Emacs Lisp packages, and activate them
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Evaluate at compile time
(eval-when-compile (require 'use-package))

;; Check load time for packages
;; (setq use-package-verbose t)
;; Ensure that all packages are installed
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Don't make package-selected-packages to be created
(defun package--save-selected-packages (&rest opt) nil)
;; Place Emacs generated variables somewhere else, and don't load them
(setq custom-file (concat user-emacs-directory "custom.el"))

;; Use y/n for yes/no prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; Saves files such as undo tree and auto saves in .emacs.d
(use-package no-littering
  :custom
  (auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(defun my/message-off-advice (oldfun &rest args)
  "Quiet down messages in adviced OLDFUN."
  (let ((message-off (make-symbol "message-off")))
    (unwind-protect
        (progn
          (advice-add #'message :around #'ignore (list 'name message-off))
          (apply oldfun args))
      (advice-remove #'message message-off))))

;; Aesthetics
(add-to-list 'default-frame-alist '(font . "Inconsolata-14"))

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda ()
                   (display-line-numbers-mode)
                   (setq display-line-numbers 'relative))))

;; Remove startup screen and miniuffer message
(setq inhibit-startup-message t)
(defun display-startup-echo-area-message () (message nil))

;; Remove emacs' bars
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(menu-bar-mode -1)          ; Disable the menu bar
(set-fringe-mode -1)        ; Remove the extra finges at the left

;; Show column number
(column-number-mode)
;; Treat each long line as multiple screen lines
(global-visual-line-mode t)

;; Line wrapping
(setq-default fill-column 100)
;; Highlight current line
(global-hl-line-mode 1)

;; Emacs, recenter the screen after reaching the edge,
;; Disable that by making it move with the screen with n lines
(setq scroll-margin 5)
(setq scroll-conservatively 5)

;; Avoid being prompted with symbolic link to git-controlled
(setq vc-follow-symlinks t)

;; Don't save bookmarks, because it's making annoying prompts
(setq bookmark-save-flag nil)

;; Setting it from <C-h>
(setq help-char (string-to-char "?"))

;; Exit emacs without getting a prompt to kill processes
(setq confirm-kill-processes nil)

;; Trims spaces from end of line
(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

;; Unfolding an item with emojis is slow, this package fixes this problem
(use-package emojify
  :init
  :hook (after-init . global-emojify-mode)
  :config
  )

(use-package gruvbox-theme
  :config (load-theme 'gruvbox-light-medium t))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

;; Highlight matching braces
(use-package paren
  :ensure nil
  :custom
  (show-paren-delay 0)
  :config (show-paren-mode 1)
  )

;; Better help
(use-package helpful)


(use-package openwith
  :config
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("mpg" "mpeg" "mp3" "mp4"
                  "avi" "wmv" "wav" "mov" "flv"
                  "ogm" "ogg" "mkv")) "mpv" '(file))
         (list (openwith-make-extension-regexp
                '("pdf" "epub" "djvu")) "okular" '(file))
         (list (openwith-make-extension-regexp
                '("markdown")) "nvim-qt" '(file))
         ))
  (openwith-mode t))

;; Auto close pairs
(use-package elec-pair
  :ensure nil
  :custom (electric-pair-pairs
           '((?\" . ?\")
             (?\[ . ?\])
             (?\< . ?\>)
             (?\` . ?\`)
             (?\{ . ?\})))
  :config
  (defun my/ignore-elec-pairs ()
    ;; Ignore < in org mode for yassnippets
    (setq electric-pair-inhibit-predicate
          (lambda (c)
            (cond ((char-equal c ?\<) (electric-pair-default-inhibit c) t)
          ))
    ))
  (add-hook 'org-mode-hook 'my/ignore-elec-pairs)

  (add-hook 'org-mode-hook (lambda ()
                             (add-to-list 'insert-pair-alist (list ?\$ ?\$))
                             (define-key org-mode-map (kbd "$")
                               #'(lambda () (interactive)
                                   (if (org-in-src-block-p) (insert "$") (insert-pair))))
                             ))
  (electric-pair-mode 1)
  )

;; Remove backup files (ends with ~)
;; Remove auto-recover files
(setq auto-save-default nil)
(setq ad-redefinition-action 'accept)

(use-package flyspell
  :ensure nil
  :custom
  ;; Showing error messages slows down using flyspell-buffer
  (flyspell-issue-message-flag t)
  ;; Use hunspell instead of ispell
  (ispell-program-name (executable-find "hunspell"))
  (ispell-dictionary "en_US")
  :hook (;; Enable spelling mode for files
         ;; Enable flyspell for comments and strings only in programming modes
         (text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)
         (flyspell-mode . (lambda ()
                            ;; Remove annoying bindings
                            (evil-define-key nil flyspell-mode-map (kbd "C-,") nil)
                            (evil-define-key nil flyspell-mode-map (kbd "C-M-i") nil)

                            ;; Correct last misspelled word
                            (evil-define-key 'insert flyspell-mode-map "\C-l"  'flyspell-auto-correct-previous-word)
                            ;; Spell checking toggle with yos
                            (evil-collection-define-operator-key 'yank 'global-map "os" #'flyspell-mode)
                            )))
  )

(use-package yasnippet
  :custom
  (yas-snippet-dirs (list (concat user-emacs-directory "snippets")))
  :config
  (yas-global-mode t)
  (use-package yasnippet-snippets)
  )

;; Auto completion
(use-package company
  :after (evil evil-collection)
  :hook ((after-init . global-company-mode)
         (evil-collection-setup .
                                (lambda (mode-keymaps &rest _rest)
                                  (evil-collection-define-key nil 'company-active-map
                                    (kbd "C-j") 'company-complete-selection
                                    (kbd "C-h") 'evil-delete-backward-char-and-join
                                    (kbd "C-w") 'evil-delete-backward-word
                                    (kbd "<return>") 'newline))))
  :custom
  (company-minimum-prefix-length 4)
  (company-selection-wrap-around t)
  :config

  ;; Load basic company backend
  (defun my/append-company-backends ()
    (setq-local company-backends
                (append '((company-capf company-yasnippet)) company-backends)))
  (add-hook 'org-mode #'my/append-company-backends)
  )

;; Traverse file changes in git
(use-package git-timemachine)
(use-package browse-at-remote)
(use-package git-gutter
  :init
  :disabled
  (global-git-gutter-mode t)
  :config
  (custom-set-variables
   '(git-gutter:visual-line t)
   '(git-gutter:window-width 1))
  )

(use-package magit
  :after evil-collection
  :custom
  (evil-collection-magit-use-y-for-yank t)
  (evil-collection-magit-want-horizontal-movement t)
  ;; Update return in repo list, should be done after evil-collection
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-repository-directories '(("~/dotfiles" . 0) ("~/Projects/" . 1) ("/srv/http/cooldown" . 0)))
  :config

  (evil-define-key 'normal magit-status-mode-map
    (kbd "?") 'evil-search-backward
    (kbd "<return>") 'my/vil-diff-visit-file)

  ;; Open file using nvim
  (defun my/vil-diff-visit-file (file &optional other-window)
    (interactive (list (magit-file-at-point t t) current-prefix-arg))
    (shell-command (concat "nvim-qt " file nil)))
  )

(use-package magit-delta
  :disabled
  :hook (magit-mode . magit-delta-mode))

(use-package evil
  :init
  ;; Needs to be initialized before evil loads
  ;; Y is y$
  (setq evil-want-Y-yank-to-eol t)
  :demand t
  :custom
  ;; Needed by evil-collection
  (evil-want-keybinding nil)
  ;; Use evil search module
  (evil-search-module 'evil-search)
  ;; Use <C-u> to scroll up
  (evil-want-C-u-scroll t)
  (evil-want-C-u-delete t)
  ;; Move across physical lines
  (evil-respect-visual-line-mode t)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  ;; Needed for redo functionality
  (evil-undo-system 'undo-tree)
  ;; Don't show previous command at  prompt
  (evil-want-empty-ex-last-command nil)
  ;; Yank the replaced text after pasting
  (evil-kill-on-visual-paste t)

  ;; Tab width
  (tab-width 2)
  (evil-shift-width tab-width)
  (indent-tabs-mode nil)
  (tab-always-indent nil)
  :config
  (evil-mode 1)
  (evil-define-key 'insert 'global (kbd "C-h") 'evil-delete-backward-char-and-join)

  (evil-define-key '(insert normal emacs) 'global (kbd "C-q") 'help)

  (evil-define-key nil 'global
    (kbd "<escape>") 'keyboard-escape-quit
    (kbd "C-+") 'text-scale-increase
    (kbd "C--") 'text-scale-decrease
    (kbd "C-=") #'(lambda () (interactive) (let ((inhibit-message t)) (text-scale-adjust 0)))
    )
  ;; Readline keybinding
    (evil-define-key 'insert 'global
      (kbd "C-a") 'back-to-indentation ;; Start of line
      (kbd "C-e") 'end-of-line
      (kbd "C-d") 'delete-char
      ;; Kill from current position to start of next word
      (kbd "M-d") #'(lambda () (interactive) (apply 'evil-delete (list (point) (nth 1 (evil-a-word))))))

  ;; Make underscore to be identified as a part of word, so <C-w> removes it
  (modify-syntax-entry ?_ "w")

  ;; Making text object using https://stackoverflow.com/a/22418983/4921402
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

  ;; between dollar signs
  (define-and-bind-text-object "$" "\\$" "\\$")
  (define-and-bind-text-object "~" "\\~" "\\~")
  )

(use-package evil-vimish-fold
  :init (add-hook 'prog-mode-hook 'evil-vimish-fold-mode)
  :config (use-package vimish-fold)
  )


;; Expand/contract selected region
(use-package expand-region
  :after evil
  :config
  (evil-define-key '(normal visual) 'global
    (kbd "+") 'er/expand-region
    (kbd "_") 'er/contract-region)
  )

;; Align operator
(use-package evil-lion
  :config
  (setq evil-lion-left-align-key (kbd "g a"))
  (setq evil-lion-right-align-key (kbd "g A"))
  (evil-lion-mode)
  )

(use-package evil-commentary
  :config (evil-commentary-mode))

;; Better sentence navigation with ) & (
(use-package sentence-navigation)

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

;; Jumping between matched tags
(use-package evil-matchit
  :config (global-evil-matchit-mode 1))

;; Persist history over Emacs restarts.
;; Vertico sorts by history position.
;; Used for evil jumps
(use-package savehist
  :ensure nil
  :config (savehist-mode))

;; Needed for evil-undo-system
;; Used instead of undo-fu because of tree visualizer
(use-package undo-tree
  :after evil
  :init (global-undo-tree-mode)
  :hook ((evil-collection-setup . (lambda (mode-keymaps &rest _rest)
                                    (evil-collection-define-operator-key 'yank 'global-map
                                      "eu" #'undo-tree-visualize))))
  :custom (undo-tree-visualizer-diff t)
  :config
  ;; Save undo steps between sessions
  (use-package undo-fu-session)
  (global-undo-fu-session-mode)

  (evil-set-initial-state 'undo-tree-visualizer-mode 'emacs)

  (evil-define-key 'emacs undo-tree-visualizer-mode-map
    (kbd "j") 'evil-next-line
    (kbd "k") 'evil-previous-line
    (kbd "h") 'undo-tree-visualize-switch-branch-left
    (kbd "l") 'undo-tree-visualize-switch-branch-right
    ;; Revert back
    (kbd "C-[") 'undo-tree-visualizer-abort
    (kbd "q") 'undo-tree-visualizer-abort
    ;; Accept changes
    (kbd "<return>") 'undo-tree-visualizer-quit)
  )

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init)

  ;; Swap ; & :
  ;; evil-collectin-swap-key doesn't make the rebinding in minibuffer work
  (evil-collection-translate-key nil 'evil-motion-state-map
    ";" ":"
    ":" ";")

  ;; Trigger the background theme
  (defun my/trigger-theme ()
    (interactive)
    (if (eq (car custom-enabled-themes) 'gruvbox-light-medium)
        (load-theme 'gruvbox-dark-soft t)
      (load-theme 'gruvbox-light-medium t)))

  (evil-collection-define-operator-key 'yank 'global-map "ob" #'my/trigger-theme)
  (evil-collection-define-operator-key 'yank 'global-map "ow" #'visual-line-mode)

  ;; Use evil bindings for search
  (evil-select-search-module 'evil-search-module 'evil-search)

  (dolist (map '( minibuffer-local-map
                  minibuffer-local-ns-map
                  minibuffer-local-completion-map
                  minibuffer-local-must-match-map
                  minibuffer-local-isearch-map
                  evil-ex-completion-map
                  ))
    (evil-collection-define-key 'insert map (kbd "<escape>") 'abort-recursive-edit)
    (evil-collection-define-key 'insert map (kbd "C-j") 'exit-minibuffer)
    (evil-collection-define-key 'insert map (kbd "C-p") 'previous-line-or-history-element)
    (evil-collection-define-key 'insert map (kbd "C-n") 'next-line-or-history-element)
    )
  )

(defun my/org-mode-setup ()
  ;; Indentation for headings and items
  (org-indent-mode)
  ;; Automatically break lines
  (auto-fill-mode)
  ;; Remove number lines to the right
  (display-line-numbers-mode 0)
  )


(use-package org
  :defer t
  :ensure org-contrib
  :hook ((org-mode . my/org-mode-setup))
  :custom
  (org-ellipsis " ▾")
  (org-hide-emphasis-markers t "Hide symbols")
  (org-startup-folded 'content)
  (org-startup-folded t)
  (org-enforce-todo-dependencies t)
  (org-cycle-separator-lines -1  "No empty lines needed to fold subtrees")
  (org-startup-with-inline-images t)
  (org-image-actual-width nil)
  ;; Remove completed deadline, scheduled, completed from agenda
  ;; The time when it get closed will be shown
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-timestamp-if-done t)
  (org-agenda-start-day "-3d")
  (org-deadline-warning-days 7)
  (org-agenda-compact-blocks t)
  (org-agenda-block-separator nil)
  (org-agenda-start-on-weekday nil "Show today +7 days")
  ;; Padding
  (line-spacing 0.05)
  ;; Hide title in the header
  (org-hidden-keywords '(title))
  :config

  ;; Default applications in org mode
  (setq org-file-apps '(("\\.pdf\\'" . "zathura %s & disown")
                        ("\\.djvu\\'" . "zathura %s & disown")
                        ("\\.epub\\'" . "zathura %s & disown")
                        ("\\.markdown\\'" . "nvim-qt %s & disown")))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.25)
                  (org-level-2 . 1.15)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Inconsolata-14" :weight 'regular :height (cdr face)))

  ;; Doesn't work in use-package
  (custom-set-faces
   '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link ((t (:foreground "royal blue" :underline t))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

  ;; Keywords
  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                (sequence "WAITING(w)" "HOLD(h)"))))
  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("NEXT" :foreground "blue" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
                ("WAITING" :foreground "orange" :weight bold)
                ("HOLD" :foreground "magenta" :weight bold)
                )))

  (setq org-tag-alist
        '(("productivity" . ?p)
          ("important" . ?i)))

  (setq org-refile-targets '((nil :maxlevel . 9) ;; Refile to current directory at any level
                             (org-agenda-files :maxlevel . 3)
                             (org-buffer-list :maxlevel . 2)))

  ;; Insert mode after going to capture
  (add-hook 'org-capture-mode-hook 'evil-insert-state)

  ;; Save capture on :wq
  (evil-define-key nil org-capture-mode-map
    [remap evil-save-and-close] #'my-finalize-if-no-todo
    [remap evil-save-modified-and-close] #'my-finalize-if-no-todo
    [remap evil-quit] #'org-capture-kill)

  ;; Don't capture empty TODO tasks
  (defun my-finalize-if-no-todo ()
    (interactive)
    (setq matches (count-matches "* TODO \n" 0))
    (if (= matches 1)
        (setq org-note-abort t)
      nil))
  (defun my-finalize-reset-abort ()
    (interactive)
    (setq org-note-abort nil))

  (add-hook 'org-capture-prepare-finalize-hook #'my-finalize-if-no-todo)
  (add-hook 'org-capture-after-finalize-hook #'my-finalize-reset-abort)

  ;; Make the first tab behave properly, taken from doomemacs
  (add-hook 'org-tab-first-hook #'my/org-indent-maybe-h)
  (defun my/org-indent-maybe-h ()
    "Indent the current item (header or item), if possible.
Made for `org-tab-first-hook' in evil-mode."
    (interactive)
    (cond ((not (and (bound-and-true-p evil-local-mode)
                     (evil-insert-state-p)))
           nil)
          ((and (bound-and-true-p org-cdlatex-mode)
                (or (org-inside-LaTeX-fragment-p)
                    (org-inside-latex-macro-p)))
           nil)
          ((org-at-item-p)
           (if (eq this-command 'org-shifttab)
               (org-outdent-item-tree)
             (org-indent-item-tree))
           t)
          ((org-at-heading-p)
           (ignore-errors
             (if (eq this-command 'org-shifttab)
                 (org-promote)
               (org-demote)))
           t)
          ((org-in-src-block-p t)
           (save-window-excursion
             (org-babel-do-in-edit-buffer
              (call-interactively #'indent-for-tab-command)))
           t)
          ((and (save-excursion
                  (skip-chars-backward " \t")
                  (bolp))
                (org-in-subtree-not-table-p))
           (call-interactively #'tab-to-tab-stop)
           t)))

  ;; Agenda
  ;; Open agenda files to buffer at startup
  (defun my/open-all-org-agenda-files ()
    (interactive)
    (let ((files (org-agenda-files))) (mapcar (lambda (x) (find-file-noselect x)) files)))

  (add-hook 'emacs-startup-hook #'my/open-all-org-agenda-files)

  (defun my/open-super-agenda ()
    (interactive) (org-agenda nil "z") (delete-other-windows))

  (add-hook 'emacs-startup-hook 'my/open-super-agenda)

  (set-face-attribute 'org-agenda-date nil :height 1.05)
  (set-face-attribute 'org-agenda-date-today nil :height 1.05)
  (set-face-attribute 'org-agenda-date-weekend nil :height 1.05)

  ;; Get roam alias, otherwise the title of node
  (defun my/get-title-property ()
    (setq title (elt (elt (org-collect-keywords '("TITLE")) 0) 1))
    (setq roam_alias (org-entry-get-with-inheritance "ROAM_ALIASES"))
    (if roam_alias roam_alias (if title title "")))

  (setq org-agenda-current-time-string "┈┈┈┈┈┈┈┈┈┈┈ now"
        org-agenda-time-grid '((weekly today require-timed)
                               (800 1000 1200 1400 1600 1800 2000)
                               "---" "┈┈┈┈┈┈┈┈┈┈┈┈┈")
        org-agenda-prefix-format '((agenda . " %-16(my/get-title-property)%-12t%-6e% s")
                                   (todo . " %-12:(my/get-title-property) %-6e")
                                   (tags . " %-12:(my/get-title-property) %-6e")
                                   (search . " %-12:(my/get-title-property) %-6e")))

  ;; Save org buffers after quiting agenda mode
  (advice-add 'org-agenda-quit :before #'(lambda () (interactive) (let ((inhibit-message t)) (org-save-all-org-buffers))))

  ;; Log the state change
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  ;; Super agenda
  (setq org-agenda-custom-commands
        '(("z" "Super view"
           ((agenda "" ((org-agenda-span 'day)
                        (org-super-agenda-groups
                         '((:name "Today"
                                  :time-grid t
                                  :date today
                                  :scheduled today)))))
            (alltodo "" ((org-super-agenda-groups
                          '((:name "Due Today"
                                   :deadline today
                                   :scheduled today)
                            (:name "Waiting"
                                   :todo "WAITING")
                            (:name "On Hold"
                                   :todo "HOLD")
                            (:name "High priority"
                                   :priority "A")
                            (:name "Capture"
                                   :file-path ".*capture.org")
                            (:discard (:anything))
                            ))))))
          ("o" "Others"
           ((todo "DONE")
            (alltodo "test" ((org-super-agenda-groups
                              '((:priority "A")
                                (:priority "B")
                                (:tag "Productivity")
                                (:name "Short"
                                       :tag "effort< 1:01")
                                (:auto-category)
                                ))))))
          ))

  (use-package org-super-agenda
    ;; Should be loaded at the start
    :init (org-super-agenda-mode)
    :config
    (evil-define-key 'motion 'org-super-agenda-header-map (kbd "q") 'org-agenda-quit)
    (setq org-super-agenda-header-map (make-sparse-keymap))
    )

  (setq org-capture-templates
        `(("d" "default" entry (file ,(concat (getenv "NOTES_ORG_HOME") "/capture.org"))
           "* TODO %?\n")))

  ;; Update org-agenda-files after updating item states
  ;; If the state is removed, remove the file from agenda if there are no other states, otherwise, add it
  (defun my/update-agenda-files ()
    ;; Removed TODO from item
    (if (= (length org-state) 0)
        ;; No TODOs in buffer, so remove it, otherwise add it
        (if (= (length (org-map-entries nil  "+TODO={TODO\\\|NEXT\\\|DONE\\\|WAITING\\\|HOLD\\\|CANCELLED}" 'file)) 0)
            (setq curr-files (my/remove-from-agenda-files buffer-file-name))
          (setq curr-files (my/add-to-agenda-files buffer-file-name))
          )
      ;; There's a TODO in buffer
      (setq curr-files (my/add-to-agenda-files buffer-file-name))
      )
    (org-store-new-agenda-file-list curr-files)
    (let ((inhibit-message)) (org-install-agenda-files-menu))
    )

  (defun my/get-relative-path (file-path)
    ;; Transform full paths to relative paths
    (if (string= (substring file-path 1) "~")
        (file_path)
      (replace-regexp-in-string "\\(^/.*?/.*?/\\)" "~/" file-path)))

  ;; Accepts full path
  (defun my/remove-from-agenda-files (file-full-path)
    (setq relative-path (replace-regexp-in-string "\\(^/.*?/.*?/\\)" "~/" file-full-path))
    (setq curr-files (org-agenda-files))
    (setq curr-files (delete file-full-path curr-files))
    (setq curr-files (delete relative-path curr-files))
    curr-files
    )

  (defun my/add-to-agenda-files (file-full-path)
    ;; org transforms current paths to full paths then adds a relative path
    ;; Better to remove relative and full path then add the path
    (setq curr-files (my/remove-from-agenda-files file-full-path))
    (add-to-list 'curr-files (replace-regexp-in-string "\\(^/.*?/.*?/\\)" "~/" file-full-path))
    curr-files
    )

  (add-hook 'org-after-todo-state-change-hook 'my/update-agenda-files)

  (my/add-to-agenda-files (concat (getenv "NOTES_ORG_HOME") "/capture.org"))
  ;; Clocking
  ;; Loading emacs server is needed by emacsclient
  ;; emacsclient used by clocking
  (load "server")
  (unless (server-running-p) (server-start))

  (setq org-clock-persist 'history ;; Save clock history on Emacs close
        ;; Resume when clocking into task with open clock
        org-clock-in-resume t
        ;; Remove log if task was clocked for 0:00 (accidental clocking)
        org-clock-out-remove-zero-time-clocks t
        ;; The default value (5) is too conservative.
        org-clock-history-length 20)


  (defun my/org-clock-in-if-next ()
    "Clock in when the task is marked NEXT."
    (when (and (string= org-state "NEXT")
               (not (string= org-last-state org-state)))
      (org-clock-in)))
  (add-hook 'org-after-todo-state-change-hook
            'my/org-clock-in-if-next)
  (defadvice org-clock-in (after my activate)
    "Set this task's status to 'NEXT'."
    (org-todo "NEXT"))
  (defun my/org-clock-out-if-todo ()
    "Clock out when the task is marked TODO."
    (when (and (string= org-state "TODO")
               (equal (marker-buffer org-clock-marker) (current-buffer))
               (< (point) org-clock-marker)
               (> (save-excursion (outline-next-heading) (point))
                  org-clock-marker)
               (not (string= org-last-state org-state)))
      (org-clock-out)))
  (add-hook 'org-after-todo-state-change-hook
            'my/org-clock-out-if-todo)

  ;; Clock in clock out hooks with Polybar
  (defun my/add-clock-tmp-file ()
    (shell-command (concat "/bin/echo -e "
                           "\"" (org-get-heading t t t t) " \n"
                           (org-entry-get nil "Effort") " \n"
                           (substring-no-properties (org-clock-get-clock-string)) " \n"
                           (what-line) " \n"
                           (buffer-file-name) "\""
                           " > /tmp/org_current_task"))
    )

  (add-hook 'org-clock-in-hook #'my/add-clock-tmp-file)
  (dolist (hook '(org-clock-out-hook
                  org-clock-cancel-hook))
    (add-hook hook #'(lambda () (shell-command "/bin/rm /tmp/org_current_task 2> /dev/null"))))

  (defun my/org-toggle-last-clock (arg)
    "Toggles last
Clock out if an active clock is running (or cancel it if prefix ARG is non-nil).
If no clock is active, then clock into the last item. See `org-clock-in-last' to
see how ARG affects this command."
    (interactive "P")
    (require 'org-clock)
    (cond ((org-clocking-p)
           (if arg
               (org-clock-cancel)
             (org-clock-out)))
          ((and (null org-clock-history)
                (or (org-on-heading-p)
                    (org-at-item-p))
                (y-or-n-p "No active clock. Clock in on current item?"))
           (org-clock-in))
          ((org-clock-in-last arg))))

  ;; Source code indentation
  (setq org-src-preserve-indentation t
        org-fontify-quote-and-verse-blocks t
        org-src-fontify-natively t               ;; Syntax highlight in #+BEGIN_SRC blocks
        org-confirm-babel-evaluate nil
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 0)

  (custom-set-faces
   '(org-block ((t (:inherit fixed-pitch))))
   ;; Background is nil because org-block-end-line background shows in header
   ;; Check https://github.com/doomemacs/themes/issues/453
   '(org-block-begin-line ((t (:background nil :weight bold))))
   '(org-block-end-line ((t (:background nil :weight bold))))
   '(org-code ((t (:inherit (shadow fixed-pitch))))))



  (use-package plantuml-mode
    :config
    (setq org-plantuml-jar-path (concat user-emacs-directory "plantuml.jar"))
    (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
    )

  (require 'ob-makefile)
  ;; Run/highlight code using babel in org-mode
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (sql . t)
     (plantuml . t)
     (makefile . t)
     (emacs-lisp . t)
     (shell . t)))

  ;; Enables to add snippets for code blocks
  (use-package org-tempo
    :ensure nil
    :after org
    :config
    (add-to-list 'org-structure-template-alist '("shell" . "src sh"))
    (add-to-list 'org-structure-template-alist '("bash" . "src bash"))
    (add-to-list 'org-structure-template-alist '("py" . "src python"))
    (add-to-list 'org-structure-template-alist '("scala" . "src scala"))
    (add-to-list 'org-structure-template-alist '("markdown" . "src markdown"))
    (add-to-list 'org-structure-template-alist '("sql" . "src sql"))
    (add-to-list 'org-structure-template-alist '("lisp" . "src emacs-lisp")))

  ;; Go in the block with insert mode after inserting it
  (advice-add 'org-insert-structure-template :after #'(lambda (orig-fun &rest args) (newline) (evil-previous-line)))

  ;; To export to markdown
  (require 'ox-md)

  ;; Exporting settings
  (setq org-export-with-broken-links t
        org-export-preserve-breaks t
        org-export-with-todo-keywords nil)

  ;; Update the document header for latex preview
  (setq org-format-latex-header (concat org-format-latex-header "\n\\input{$HOME/.config/latex/preamble.tex}\n"))

  ;; Latex image size
  (plist-put org-format-latex-options :scale 1.5)

  (use-package org-protocol
    :ensure nil
    :config

    (add-to-list 'org-capture-templates
                 `("q" "org-capture-url"
                   entry (file ,(concat (getenv "NOTES_ORG_HOME") "/capture.org"))
                   "* TODO [[%:link][%:description]]" :immediate-finish t))

    (add-to-list 'org-capture-templates
                 `("n" "org-capture-note"
                   entry (file ,(concat (getenv "NOTES_ORG_HOME") "/capture.org"))
                   "* TODO %:description" :immediate-finish t))

    (setq org-protocol-default-template-key "q")
    )
  )

;; Previewing Latex fragments
(use-package auctex
  :after org-mode)
;; Used to insert latex environments and math templates
;; Previewing Latex fragments
(use-package cdlatex
  :hook (org-mode . org-cdlatex-mode)
  :config
  ;; Inserting latex env and templates using C-j
  (evil-define-key 'insert org-cdlatex-mode-map (kbd "C-j") 'cdlatex-tab))

;; Show emphasis markers when hovering over text
(use-package org-appear
  :after (org evil)
  :custom
  ;; Show markers in insert mode
  (org-appear-trigger 'manual)
  :hook ((org-mode . org-appear-mode)
         (evil-org-mode . (lambda ()
                            ;; Show marks in insert mode
                            (add-hook 'evil-insert-state-entry-hook #'org-appear-manual-start nil t)
                            (add-hook 'evil-insert-state-exit-hook #'org-appear-manual-stop nil t))))
  )

(use-package evil-org
  :after org
  :custom
  ;; Ignore leading stars or tags on headings for appending end of line of going to start of line
  (org-special-ctrl-a/e t)
  ;; Enabled, because evil has a bug with repeat command and shifting
  (evil-org-retain-visual-state-on-shift t)
  (evil-org-use-additional-insert t)
  :hook ((org-mode . evil-org-mode)
         (evil-org-mode . (lambda ()
                            (evil-org-set-key-theme '(textobjects insert navigation todo calendar additional))
                            ;; Insert heading bindings
                            (evil-define-key '(normal insert) 'evil-org-mode
                              (kbd "M-L") 'org-shiftmetaright
                              (kbd "M-H") 'org-shiftmetaleft
                              (kbd "M-K") 'org-shiftmetaup
                              (kbd "M-J") 'org-shiftmetadown
                              (kbd "C-S-l") 'org-shiftright
                              (kbd "C-S-h") 'org-shiftleft
                              (kbd "C-S-k") 'org-shiftup
                              (kbd "C-S-j") 'org-shiftdown
                              (kbd "<C-return>") #'(lambda () (interactive) (org-insert-heading-after-current) (evil-insert 0))
                              (kbd "<C-S-return>") #'(lambda () (interactive) (org-insert-todo-heading-respect-content) (evil-insert 0))
                              ;; Move to beginning of line before insert heading, otherwise org-insert-heading will insert below
                              (kbd "<M-return>") #'(lambda () (interactive) (beginning-of-line) (org-insert-heading) (evil-insert 0))
                              (kbd "<M-S-return>") #'(lambda () (interactive) (beginning-of-line) (org-insert-todo-heading 0) (evil-insert 0)))
                            (evil-define-key 'normal 'evil-org-mode
                              (kbd "zi")  #'org-toggle-inline-images
                              (kbd "zl")  #'org-latex-preview
                              ;; Open files at cursor
                              (kbd "<return>") #'(lambda () (interactive) (let ((inhibit-message t)) (org-open-at-point)))
                              (kbd "<S-return>") #'(lambda () (interactive)
                                                     ;; Open link without losing focus of window
                                                     (let ((inhibit-message t))
                                                       (update_i3_focus_window_config)
                                                       (org-open-at-point-global)
                                                       ))))))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)

  ;; Don't display a buffer when finishing async-shell-command
  (setq display-buffer-alist '(("\\*Async Shell Command\\*" . (display-buffer-no-window))))
  (defun update_i3_focus_window_config ()
    "Changes i3 focus_window_configuration"
    (setq path_to_script (concat (getenv "XDG_CONFIG_HOME") "/i3/set_i3_focus_on_window_activation_configuration"))
    (start-process-shell-command "Update i3 focus window config" nil (concat  path_to_script " none " " && sleep 1 && " path_to_script " smart")))

  (setq org-agenda-files (concat user-emacs-directory "agenda_files"))
  )

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-item-bullet-alist '((?- . ?•) (?* . ?•) (?+ . ?‣)))
  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-roam
  :after org
  :bind (("C-c r i" . org-roam-node-insert))
  :custom
  (org-roam-directory (getenv "NOTES_ORG_HOME"))
  (org-roam-node-display-template
   (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  ;; It's slow, so disable it
  (org-roam-db-update-on-save nil)
  ;; Node completion
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :if-new (file+head "${slug}.org"
                         "#+TITLE: ${title}\n")
      :unnarrowed t)))
  :config
  (org-roam-db-autosync-mode)

  ;; Add to jump list after visiting a node
  (advice-add 'org-roam-node-visit :before #'(lambda (&rest pos) (evil-set-jump)))

  ;; Overriding org-roam-node by staying in the current buffer after inserting a new one
  ;; This is done by removing :props '(:finalize find-file) in org-roam-capture
  (cl-defun my/org-roam-node-find (&optional other-window initial-input filter-fn &key templates)
    (interactive current-prefix-arg)
    (let ((node (org-roam-node-read initial-input filter-fn)))
      (if (org-roam-node-file node)
          (org-roam-node-visit node other-window)
        (org-roam-capture-
         :node node
         :templates templates))))

  ;; Add to jump list after visiting a node
  (advice-add 'org-roam-node-find :override #'my/org-roam-node-find)
  )

(use-package websocket
  :after org-roam)
(use-package simple-httpd
  :after org-roam)

(use-package org-roam-ui
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t)
  )

(use-package vertico
  :init (vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-resize nil)
  (vertico-count 14)
  (vertico-cycle t)
  (completion-in-region-function
   (lambda (&rest args)
     (apply (if vertico-mode
                #'consult-completion-in-region
              #'completion--in-region)
            args)))
  :config
  (when evil-collection-setup-minibuffer
    ;; Open buffer in a new window, uses embark's v and x
    (evil-collection-define-key 'insert 'vertico-map (kbd "C-v") (kbd "C-. v"))
    (evil-collection-define-key 'insert 'vertico-map (kbd "C-x") (kbd "C-. x")))
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

  )

(use-package counsel)
(use-package consult)

(use-package embark
  :bind (("C-." . embark-act))
  :config

  ;; C-v and C-x splits window for org roam node prompts only
  (embark-define-keymap embark-org-roam-nodes-actions
                        "Keymap for actions for org roam nodes"
                        ("x" my/org-roam-node-find-window-x)
                        ("v" my/org-roam-node-find-window-v))

  (add-to-list 'embark-keymap-alist '(org-roam-node . embark-org-roam-nodes-actions))

  (defun my/org-roam-node-find-window-v ()
    (interactive)
    (org-roam-node-find t))

  (defun my/org-roam-node-find-window-x ()
    (interactive)
    (evil-window-split)
    (org-roam-node-find))
  )


;; Improves Vertico's completion
(use-package orderless
  :custom
  (completion-styles '(orderless substring basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion)))))
  )

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :after vertico
  :custom (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init (marginalia-mode)
  )

;; Search text
(use-package deft
  :custom
  (deft-directory (getenv "NOTES_ORG_HOME"))
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  :config
  ;; Start with insert mode
  (evil-set-initial-state 'deft-mode 'insert)

  (evil-define-key 'insert deft-mode-map
    (kbd "C-p") 'previous-line
    (kbd "C-n") 'next-line
    (kbd "<escape>") 'quit-window
    (kbd "C-h") 'deft-filter-decrement
    (kbd "C-w") 'deft-filter-decrement-word)

  ;; Show the title
  (defun my/deft-parse-title (file contents)
    "Parse the given FILE and CONTENTS and determine the title.
  If `deft-use-filename-as-title' is nil, the title is taken to
  be the first non-empty line of the FILE.  Else the base name of the FILE is
  used as title."
    (let ((begin (string-match "^#\\+[tT][iI][tT][lL][eE]: .*$" contents)))
      (if begin
          (string-trim (substring contents begin (match-end 0)) "#\\+[tT][iI][tT][lL][eE]: *" "[\n\t ]+")
        (deft-base-filename file))))

  (advice-add 'deft-parse-title :override #'my/deft-parse-title)

  ;; Don't show start of org node files
  (setq deft-strip-summary-regexp
        (concat "\\("
                "[\n\t]" ;; blank
                "\\|^#\\+[[:alpha:]_]+:.*$" ;; org-mode metadata
                "\\|^:PROPERTIES:\n\\(.+\n\\)+:END:\n"
                "\\)"))
  )

;; ORG NOTIFICATION
(use-package appt
  :after org
  :ensure nil
  :config (appt-activate 1)

  (setq appt-time-msg-list nil                      ;; clear existing appt list
        appt-message-warning-time '10                    ;; send first warning before appointment
        appt-display-interval '5                         ;; warn every every X minutes from t - appt-message-warning-time
        appt-display-mode-line nil                       ;; don't show in the modeline
        appt-display-format 'window)                     ;; pass warnings to the designated window function
  (setq appt-disp-window-function (function toast-appt-display))

  ;; Set up the call to the notifier
  (defun toast-appt-send-notification (title msg)
    (shell-command (concat "/usr/bin/dunstify --appname emacs_org " " \"" title "\" \"" msg "\"")))

  ;; Designate the window function for my/appt-send-notification
  (defun toast-appt-display (min-to-app new-time msg)
    (toast-appt-send-notification
     (format "%s minutes" min-to-app)    ;; passed to -t in toast call
     (format "%s" msg))                                 ;; passed to -m in toast call
    (message nil))

  (defun org-agenda-to-appt-clear-message ()
    (interactive) (let ((inhibit-message t)) (setq appt-time-msg-list nil) (org-agenda-to-appt)))

  ;; generate the appt list from org agenda files on emacs launch
  (run-at-time nil 3600 'org-agenda-to-appt))

;; Misc
;; Auto update to window size
(use-package golden-ratio
  :init
  (golden-ratio-mode 1)
  :after evil
  :config
  (defun my/toggle-evil-window-keys-golden-ratio ()
    (if (bound-and-true-p golden-ratio-mode)
        ;; Enable
        (progn
          (advice-add 'evil-window-down :after 'golden-ratio)
          (advice-add 'evil-window-up :after 'golden-ratio)
          (advice-add 'evil-window-right :after 'golden-ratio)
          (advice-add 'evil-window-left :after 'golden-ratio)
          )
      ;; Disable
      (progn
        (balance-windows)
        (advice-remove 'evil-window-down  'golden-ratio)
        (advice-remove 'evil-window-up  'golden-ratio)
        (advice-remove 'evil-window-right  'golden-ratio)
        (advice-remove 'evil-window-left  'golden-ratio)
        )
      )
    )
  (my/toggle-evil-window-keys-golden-ratio)

  (add-hook 'golden-ratio-mode-hook 'my/toggle-evil-window-keys-golden-ratio)

  (evil-collection-define-operator-key 'yank 'global-map "eg" #'golden-ratio-mode)
  )

;; Vim leader / which-key {{{
(use-package which-key
  :init
  :custom
  (which-key-idle-delay 1)
  (which-key-max-display-columns 3)
  :config (which-key-mode))

(use-package general
  :after (evil evil-collection hydra)
  :config
  (general-evil-setup t)

  (general-define-key
   :states 'normal
   "\C-u" 'evil-scroll-up
   "g:" 'goto-last-change
   )

  (general-define-key
   :states '(normal visual motion)
   :keymaps 'global
   "," 'main-hydra/body)

  (general-define-key
   :states 'insert
   :keymaps 'global
   "C-," 'main-hydra/body)

  (add-hook 'org-mode-hook
            #'(lambda ()
                (general-define-key
                 :states '(normal visual motion)
                 :keymaps 'local
                 "," 'org-hydra/body)
                (general-define-key
                 :states '(insert)
                 :keymaps 'local
                 "C-," 'org-hydra/body))
            )

  (add-hook 'org-agenda-mode-hook
            #'(lambda ()
                (general-define-key
                 :states '(normal visual motion)
                 :keymaps 'local
                 "," 'agenda-hydra/body))
            )
  )

(use-package hydra)
(defun org-capture-full-screen ()
  (interactive)
  (org-capture nil "d")
  (delete-other-windows)
  )

(defhydra main-hydra (:exit t :idle 1)
  (" '" vertico-repeat "resume last search" :column " general")
  (" H" help-hydra/body "help")
  (" h" evil-ex-nohighlight "highlight")
  (" b" switch-to-buffer "Switch buffer")
  (" d" deft "deft")
  (" g" git-hydra/body "git")
  (" x" (lambda () (interactive) (org-capture nil "d")) "capture")
  ("ss" (lambda () (interactive) (load-file (concat user-emacs-directory "/init.el"))) "source rc")
  ("es" (lambda () (interactive) (split-window-below) (find-file (concat user-emacs-directory "/init.el"))) "edit rc")
  )

(defhydra git-hydra (:exit t :hint nil :idle 1)
  (" r" git-gutter:revert-hunk "revert hunk" :column " hunks")
  (" ]" git-gutter:next-hunk  "next hunk")
  (" [" git-gutter:previous-hunk "previous hunk")
  (" g" magit-status "status" :column " magit")
  (" c" magit-show-commit "show commit")
  (" L" magit-log-buffer-file "log")

  (" o" browse-at-remote "browse at remote")

  (" t" git-timemachine-toggle "toggle" :column " timemachine")
  )

(defhydra help-hydra (:exit t :idle 1)
  (" a" consult-apropos "apropos interactive" :column " help")
  (" A" apropos "apropos")
  (" k" helpful-key "key")

  (" b" embark-bindings "binding")
  (" f" helpful-callable "function")
  (" c" helpful-command "command")
  (" p" helfpul-at-point "at point")
  (" l" find-library "library file")
  (" L" apropos-library "library commands" :column "")
  (" m" describe-mode "all modes")
  (" M" my/describe-active-minor-mode "one mode")
  (" d" apropos-documentation "doc")
  (" v" helpful-variable "var")
  (" V" my/help-custom-variable "my/custom-var")
  (" o" apropos-user-option "option")
  (" e" apropos-value "value")
  )

;; Org
(defhydra org-hydra (:hint nil :exit t :idle 1 :inherit (main-hydra/heads))
  (" *" org-ctrl-c-star "make header" :column " org")
  (" -" org-ctrl-c-minus "make item")
  (" c" org-clock-hydra/body "clock")
  (" r" roam-hydra/body "roam")
  (" o" org-org-hydra/body "org")
  (" a" org-agenda "agenda")
  (" p" org-set-property "set property")
  )

(defhydra org-clock-hydra (:exit t :hint nil :idle 1)
  (" i" org-clock-in "in" :column "clock")
  (" o" org-clock-out "out")
  (" t" my/org-toggle-last-clock "toggle")
  (" c" org-clock-cancel "cancel")
  (" g" org-clock-goto "goto" :column "")
  (" e" org-set-effort "effort")
  (" r" org-clock-report "report")
  )

(defhydra roam-hydra (:exit t :idle 1)
  (" f" (lambda () (interactive) (let ((inhibit-message t)) (org-roam-node-find))) "find node" :column " node")
  (" i" org-roam-node-insert              "insert node")
  (" r" org-roam-buffer-toggle            "linked to here")
  (" R" org-roam-buffer-display-dedicated "linked to a node")
  (" g" org-roam-ui-mode                  "graph" :column " ")
  (" a" org-roam-alias-add                "add alias")
  (" s" org-roam-db-sync                 "sync")
  )

(defhydra org-org-hydra (:exit t :idle 1)
  (" r" org-refile-hydra/body "refile" :column " org")
  (" l" org-links-hydra/body "links")
  (" e" org-export-dispatch "export")
  (" a" org-archive-subtree "archive")
  (" g" org-goto-hydra/body "goto")
  )

(defhydra org-goto-hydra (:exit t :idle 1)
  (" g" consult-org-heading "file" :column " goto")
  (" r" 'org-refile-goto-last-stored "refile")
  (" G" consult-org-agenda "all")
  )

(defhydra org-refile-hydra (:exit t :idle 1)
  (" ." my/org-refile-to-current-file "current file" :column " refile")
  (" c" my/org-refile-to-running-clock "clock")
  (" a" org-refile "agenda")
  (" g" org-refile-goto-last-stored "goto refile")
  )

(defhydra org-links-hydra (:exit t :idle 1)
  (" y" org-store-link "yank" :column " links")
  (" e" org-insert-link "edit" :column " links") ; Edit link if it exists at point
  (" t" org-toggle-link-display "toggle")
  (" p" org-insert-last-stored-link "paste last")
  )

(defun my/org-refile-to-current-file (arg &optional file)
  "Refile current heading to elsewhere in the current buffer.
If prefix ARG, copy instead of move."
  (interactive "P")
  (let ((org-refile-targets `((,file :maxlevel . 10)))
        (org-refile-use-outline-path t)
        (org-refile-keep arg)
        current-prefix-arg)
    (call-interactively #'org-refile)))

(defun my/org-refile-to-running-clock (arg)
  "Refile current heading to the currently clocked in task.
If prefix ARG, copy instead of move."
  (interactive "P")
  (unless (bound-and-true-p org-clock-current-task)
    (user-error "No active clock to refile to"))
  (let ((org-refile-keep arg))
    (org-refile 2)))

;; Agenda
(defhydra agenda-hydra (:exit t :idle 1 :inherit (main-hydra/heads))
  (" c" agenda-clock-hydra/body " clock" :column " agenda")
  (" v" agenda-view-hydra/body " view")
  (" r" roam-hydra/body "roam")
  (" f"	org-agenda-follow-mode "follow"))

(defhydra agenda-clock-hydra (:exit t :idle 1)
  (" c" org-agenda-clock-cancel "cancel")
  (" g" org-agenda-clock-goto "goto")
  (" i" org-agenda-clock-in "in")
  (" o" org-agenda-clock-out "out")
  (" r" org-agenda-clockreport-mode "report"))

(defhydra agenda-view-hydra (:exit t :idle 1)
  (" d"	org-agenda-day-view "day")
  (" w"	org-agenda-week-view "week")
  (" m"	org-agenda-month-view "month")
  (" y"	org-agenda-year-view "year"))

(defun my/active-minor-modes ()
  "Return a list of active minor-mode symbols."
  (cl-loop for mode in minor-mode-list
           if (and (boundp mode) (symbol-value mode))
           collect mode))

(defun my/describe-active-minor-mode (mode)
  "Get information on an active minor mode. Use `describe-minor-mode' for a
selection of all minor-modes, active or not."
  (interactive
   (list (completing-read "Describe active mode: " (my/active-minor-modes))))
  (let ((symbol
         (cond ((stringp mode) (intern mode))
               ((symbolp mode) mode)
               ((error "Expected a symbol/string, got a %s" (type-of mode))))))
    (if (fboundp symbol)
        (helpful-function symbol)
      (helpful-variable symbol))))
;; }}}
