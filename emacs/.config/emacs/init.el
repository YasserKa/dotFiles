;; -*- lexical-binding: t; -*-
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
            (run-with-idle-timer 3 nil #'my/lower-gc-cons-threshold)
            (add-function :after after-focus-change-function #'my/lower-gc-cons-threshold)))

(setq gc-cons-threshold (* 1024 gc-cons-threshold))
(setq use-package-compute-statistics t)

;; Speed up Inhibit file handlers during startup
(defvar my/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook (lambda () (setq file-name-handler-alist my/file-name-handler-alist)))
;; }}}
;; {{{ Package/Lisp management
;; Initialize package sources
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("nognu" . "https://elpa.nongnu.org/nongnu/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Ensure that all packages are installed
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Used to update the package from upgrade_system bash function
(use-package auto-package-update
  :config
  ;; Delete the old version on updates.
  (setq auto-package-update-delete-old-versions t))

(add-to-list 'load-path (concat user-emacs-directory "/lisp"))
(require 'functions)
;; }}}
;; Better defaults {{{
;; Use y/n for yes/no prompts
(fset 'yes-or-no-p 'y-or-n-p)
(setopt use-short-answers t)
;; Exit emacs without getting a prompt to kill processes
(setq confirm-kill-processes nil)
;; Ediff mode
(setq ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)
(add-hook 'ediff-mode-hook '(lambda () (golden-ratio-mode 0)))
;; Load the newest version of a file
(setq  load-prefer-newer t)
;; Revert buffer when the file changes on disk
(auto-revert-mode 1)
;; Place point where it was lastly placed when visiting a file
;; https://www.emacswiki.org/emacs/SavePlace
(save-place-mode 1)
;; Use /path1/file /path2/file instead of file file<2> for buffer names of files for same name
(setq uniquify-buffer-name-style 'forward)
(setq inhibit-startup-message t      ;; No startup message
      server-client-instructions nil ;; Suppress "When done with this frame, type C-x 5 0" message when using emacsclient
      )
;; Better help
(use-package helpful)
;; Setting it from <C-h>
(setq help-char (string-to-char "?"))

;; Remove UI
(dolist (mode
         '(scroll-bar-mode        ; Disable visible scrollbar
           tool-bar-mode          ; Disable the toolbar
           tooltip-mode           ; Disable tooltips
           menu-bar-mode          ; Disable the menu bar
           set-fringe-mode))      ; Remove the extra finges at the left
  (funcall mode 0))

;; Persist history over Emacs restarts.
(use-package savehist
  :ensure nil
  :config (savehist-mode))

;; Backups
(setq backup-by-copying t    ; Don't delink hard links
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

(use-package no-littering
  :custom
  (auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (custom-file (no-littering-expand-etc-file-name "custom.el") "Place Emacs generated variables somewhere else, and don't load them")
  :config
  ;; Setup undo, backup, and auto-save files
  (no-littering-theme-backups))

;; Bindings
(global-set-key (kbd "C-x C-b") 'ibuffer)

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
                '("markdown")) (getenv "EDITOR_GUI") '(file))
         ))
  (openwith-mode t))
;; }}}
;; Appearance {{{
(setq my/default-font "Firacode Nerd Font-12")
(add-to-list 'default-frame-alist `(font . ,my/default-font))

;; Emacs, recenter the screen after reaching the edge,
;; disable that by making it move with the screen with n lines
(setq scroll-margin 5)
(setq scroll-conservatively 5)

;; Prioritize vertical on horizontal split
(setq split-width-threshold 80)

;; Show tooltips (e.g. org links) in modeline
(setq help-at-pt-display-when-idle t
      help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda ()
                   (display-line-numbers-mode)
                   (setq display-line-numbers 'relative))))

;; Show column number
(column-number-mode 1)
;; Treat each long line as multiple screen lines
(global-visual-line-mode 1)

;; Line wrapping
(setq-default fill-column 100)
;; Highlight current line
(global-hl-line-mode 1)

(use-package modus-themes
  :config
  ;; Better org mode code block syntax
  (setq modus-themes-bold-constructs t
        modus-themes-italic-constructs t
        modus-themes-org-blocks 'gray-background)
  (load-theme 'modus-operandi-tinted :no-confirm))
(set-face-attribute 'italic nil :family "FiraSans" :slant 'italic)

(use-package doom-modeline
  :custom
  (doom-modeline-buffer-file-name-style 'relative-from-project "Show full path")
  (doom-modeline-buffer-encoding 'nondefault "Only show file encoding if it's non-UTF-8")
  :hook (after-init . doom-modeline-mode))
;; Minor mode to show total matches during search in modeline
(use-package anzu :after isearch-mode)
(use-package evil-anzu :after evil :config (global-anzu-mode +1))

;; Balance balance window margins
(use-package olivetti
  :disabled)

;; Icons for doom-modeline
(use-package nerd-icons
  :config
  ;; Add a glyph for hypothesis links
  (add-to-list 'nerd-icons-url-alist '("^\\(https?://\\)?\\(www\\.\\)?hyp\\.is" nerd-icons-mdicon "nf-md-alpha_h_box"))
  (add-to-list 'nerd-icons-url-alist '("^link-handler://" nerd-icons-mdicon "nf-md-alpha_h_box"))
  (unless (package-installed-p 'nerd-icons)
    (nerd-icons-install-fonts))
  )

;; Unfolding an item with emojis is slow, this package fixes this problem
;; (use-package emojify
;;   :disabled
;;   :hook (after-init . global-emojify-mode)
;;   :config)

;; Highlight matching braces
(use-package paren
  :ensure nil
  :custom
  (show-paren-delay 0)
  :config (show-paren-mode 1)
  )
;; }}}
;; Editing {{{
;; Trims spaces from end of line
(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

(use-package elec-pair
  :ensure nil
  :config

  (with-eval-after-load 'org
    (modify-syntax-entry ?$ "($" org-mode-syntax-table)
    )

  (electric-pair-mode 1)
  )

(use-package flycheck)

;; Spell checking
(use-package flyspell
  :ensure nil
  :custom
  ;; Showing error messages slows down using flyspell-buffer
  (flyspell-issue-message-flag t)
  :hook (;; Enable spelling mode for files
         ;; Enable flyspell for comments and strings only in programming modes
         (text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)
         (evil-collection-setup . (lambda (mode-keymaps &rest _rest)
                            ;; Remove annoying bindings
                            (evil-define-key nil flyspell-mode-map (kbd "C-,") nil)
                            (evil-define-key nil flyspell-mode-map (kbd "C-M-i") nil)

                            ;; Correct last misspelled word
                            (evil-define-key 'insert flyspell-mode-map "\C-l"  'flyspell-auto-correct-previous-word)
                            ;; Spell checking toggle with yos
                            (evil-collection-define-operator-key 'yank 'global-map "os" #'flyspell-mode)
                            )))
  )


(with-eval-after-load "ispell"
  ;; Use hunspell instead of ispell
  (setq ispell-program-name (executable-find "hunspell"))
  (setq ispell-personal-dictionary (getenv "DICPATH"))
  )

(defun my/save-word-to-dictionary ()
  (interactive)
  (let ((current-location (point))
        (word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))
    (message "Word \"%s\" added to dictionary." (car word))
    )
  )

(defun my/remove-word-from-dictionary ()
  (interactive)
  (let ((word (thing-at-point 'word)))
    (with-temp-file (getenv "DICPATH")
      (insert-file-contents (getenv "DICPATH"))
      (goto-char (point-min))
      (while (re-search-forward (concat "^" word "$") nil t)
        (kill-whole-line)))
    (message "Word \"%s\" removed from dictionary." word)))

;; Add vim's binding
(with-eval-after-load 'evil
  (evil-define-key 'normal 'global (kbd "zg")  #'my/save-word-to-dictionary)
  (evil-define-key 'normal 'global (kbd "zw")  #'my/remove-word-from-dictionary)
  )

;; Expands on conf-mode for i3 config files
(use-package i3wm-config-mode
  :custom
  indent-line-function (lambda () "noindent") "Disable auto-indentation"
  :config
  ;; Enable mode for sway config files as well
  (add-to-list 'auto-mode-alist '("/sway/.*config.*/" . i3wm-config-mode))
  (add-to-list 'auto-mode-alist '("/sway/config\\'" . i3wm-config-mode))
  )

(use-package crontab-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.cron\\'" . crontab-mode)))

;; Snippet engine
(use-package yasnippet
  :custom
  (yas-snippet-dirs (list (concat user-emacs-directory "snippets")))
  :config
  (yas-global-mode t)
  (use-package yasnippet-snippets)
  )

(use-package abbrev
  :ensure nil
  :custom
  (save-abbrevs 'silently)
  :config
  (setq-default abbrev-mode t)
  (define-abbrev-table 'global-abbrev-table
    '(("latex" "LaTeX" nil 1)
      ))
  )

;; Auto completion
(use-package corfu
  :custom
  (corfu-cycle t)                 ; Allows cycling through candidates
  (corfu-auto t)                  ; Enable auto completion
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-preview-current 'insert) ; Do not preview current candidate
  (corfu-preselect-first nil)
  (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets

  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
              ("C-n"        . corfu-next)
              ("C-p"      . corfu-previous)
              ("C-m" . corfu-insert))

  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode) ; Popup completion info
  :config
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local corfu-quit-at-boundary t
                                   corfu-quit-no-match t
                                   corfu-auto nil)
              (corfu-mode))))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  (add-hook 'my-completion-ui-mode-hook
   	        (lambda ()
   	          (setq completion-in-region-function
   		              (kind-icon-enhance-completion
   		               completion-in-region-function))))

  )

(use-package cape
  :bind ("C-c f" . cape-file)
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (defalias 'dabbrev-after-2 (cape-capf-prefix-length #'cape-dabbrev 2))
  (add-to-list 'completion-at-point-functions 'dabbrev-after-2 t)
  (cl-pushnew #'cape-file completion-at-point-functions)
  :config
  ;; Silence then pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

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
  ;; Adjust splitting position
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

  ;; Support Ibeam cursor for insert mode in the terminal
  (use-package evil-terminal-cursor-changer
    :after evil
    :config
    (unless (display-graphic-p)
      (require 'evil-terminal-cursor-changer)
      (evil-terminal-cursor-changer-activate) ; or (etcc-on)
      )
    )

  (evil-define-key 'insert 'key-translation-map [?\C-h] [?\C-?])

  ;;   (evil-define-key 'insert 'global (kbd "C-h") 'delete-backward-char)
  ;; (global-set-key "\C-h" 'delete-backward-char)
  ;; Change search module to make it work for invisible text in org mode
  ;; This approach is used instead of using (setq org-fold-core-style 'overlays)
  ;; since the default value "text-properties" is faster than "overlays"
  ;; https://github.com/emacs-evil/evil/issues/1630
  (evil-select-search-module 'evil-search-module 'isearch)
  (defun my/remove-char-in-isearch-mode ()
    "Delete last char of the search string."
    (interactive)
    (unless (equal isearch-string "") (isearch-pop-state))
    (isearch-update))

  (define-key isearch-mode-map (kbd "C-h") 'my/remove-char-in-isearch-mode)

  (define-key key-translation-map (kbd "C-<escape>") (kbd "ESC"))

  (evil-define-key 'normal 'global
    (kbd "0") 'evil-beginning-of-line
    (kbd "j") 'evil-next-visual-line
    (kbd "k") 'evil-previous-visual-line
    (kbd "g0") 'evil-beginning-of-visual-line
    )

  (evil-define-key nil 'global
    (kbd "<escape>") 'keyboard-escape-quit
    (kbd "C-+") 'text-scale-increase
    (kbd "C--") 'text-scale-decrease
    (kbd "C-=") #'(lambda () (interactive) (let ((inhibit-message t)) (text-scale-adjust 0)))
    )

  ;; Navigate through start of line, indentation, comment
  (use-package mwim)

  ;; Emacs' keybinding
  (evil-define-key 'insert 'global
    ;; Navigation
    ;; Line
    (kbd "C-a") 'mwim-beginning      ;; Navigate through start of line, non-blank, comment
    (kbd "M-a") 'back-to-indentation
    (kbd "C-e") 'mwim-end
    ;; Word
    (kbd "C-b") 'backward-char
    (kbd "C-f") 'forward-char
    (kbd "M-b") 'backward-word
    (kbd "M-f") 'forward-word
    (kbd "M-B") 'backward-sexp
    (kbd "M-F") 'forward-sexp
    ;; Editing
    ;; Line
    (kbd "C-k") 'evil-delete-line
    ;; Word
    (kbd "C-d") 'delete-char
    ;; Kill from current position to start of next word
    (kbd "M-d") #'(lambda () (interactive) (apply 'evil-delete (list (point) (nth 1 (evil-a-word)))))
    (kbd "M-w") 'backward-kill-sexp
    (kbd "M-D") 'kill-sexp
    )

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

  (defun my/join-line-keep-point ()
    "Join line without moving point"
    (interactive)
    (point-to-register 'z)
    (let ((current-prefix-arg 4)) ;; emulate C-u
      (call-interactively 'join-line) ;; invoke align-regexp interactively
      )
    (jump-to-register 'z)
    )

  (evil-define-key 'normal 'global (kbd "J") 'my/join-line-keep-point)
  (evil-define-key 'visual 'global (kbd "J") 'join-line)
)

(use-package windresize
  :after evil
  :config
  (setq windresize-increment 5)
  (evil-define-key 'normal 'global
    (kbd "C-S-h") 'windresize-left
    (kbd "C-S-j") 'windresize-down
    (kbd "C-S-k") 'windresize-up
    (kbd "C-S-l") 'windresize-right
    )
  )

;; Auto update to window size
(use-package golden-ratio
  :init
  (golden-ratio-mode 1)
  :after (evil evil-collection)
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

  (evil-collection-define-key 'normal 'global-map (kbd "C-w m") #'golden-ratio-mode)
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
;; Enable C-a
(use-package evil-numbers
  :after evil
  :config
  (evil-define-key '(normal visual) 'global
    (kbd "C-a") 'evil-numbers/inc-at-pt
    ;; (kbd "C-x") 'evil-numbers/dec-at-pt
    )
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

;; Needed for evil-undo-system
;; Used instead of undo-fu because of tree visualizer
(use-package undo-tree
  ;; t : timestamp
  ;; d : diff
  ;; s : selection
  :after evil
  :init (global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-visualizer-diff t)
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
    (if (eq (car custom-enabled-themes) 'modus-operandi-tinted)
        (load-theme 'modus-vivendi-tinted :no-confirm)
      (load-theme 'modus-operandi-tinted :no-confirm))

    (my/update-org-level-face)
    )

  (evil-collection-define-operator-key 'yank 'global-map "ob" #'my/trigger-theme)
  (evil-collection-define-operator-key 'yank 'global-map "ow" #'visual-line-mode)

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
    (evil-collection-define-key 'insert map (kbd "C-.") 'embark-act)
    )
  )

;; }}}
;; Git {{{
;; If visited file is a symbolic link to a file under VC, visit the real file
(setq vc-follow-symlinks t)

;; Traverse file changes in git
(use-package git-timemachine
  :after (evil-collection)
  :config
  (evil-define-minor-mode-key 'normal 'git-timemachine-mode (kbd "C-n") 'git-timemachine-show-next-revision)
  (evil-define-minor-mode-key 'normal 'git-timemachine-mode (kbd "C-p") 'git-timemachine-show-previous-revision)
  )

(use-package browse-at-remote)
(use-package git-gutter
  :custom
  (git-gutter:update-interval 2)
  :config
  (dolist (mode '(prog-mode-hook
                  conf-mode-hook))
    (add-hook mode 'git-gutter-mode))

  (evil-collection-define-key 'normal 'global-map
    (kbd "[g") 'git-gutter:previous-hunk
    (kbd "]g") 'git-gutter:next-hunk)

  (custom-set-variables
   '(git-gutter:visual-line t)
   '(git-gutter:window-width 1))
  )
(use-package git-modes)

(use-package magit
  :after evil-collection
  :custom
  (evil-collection-magit-use-y-for-yank t)
  (evil-collection-magit-want-horizontal-movement t)
  ;; Update return in repo list, should be done after evil-collection
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-repository-directories '(("$DOTFILES_DIR" . 0)))
  :config

  ;; Use External GTK pinetry program to run pinentry
  (setenv "PINENTRY_USER_DATA" "gtk")

  (evil-define-key 'normal magit-status-mode-map
    (kbd "?") 'evil-search-backward
    (kbd "<return>") 'my/vil-diff-visit-file)

  ;; Open file using nvim
  (defun my/vil-diff-visit-file (file &optional other-window)
    (interactive (list (magit-file-at-point t t) current-prefix-arg))
    (start-process-shell-command "Start default application" nil (concat (getenv "EDITOR_GUI") " " file nil))
    )
  )

(use-package magit-delta
  :disabled
  :hook (magit-mode . magit-delta-mode))
;; }}}
;; LateX {{{
(use-package latex
  :ensure nil
  :init
  :after tex
  :config
  (defun my/setup-latex-mode ()
    (setq server-name "latex-mode")
    (load "server")
    (unless (server-running-p) (server-start))
    )
  ;; TODO: Server problems
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook #'my/setup-latex-mode)
  (setq TeX-PDF-mode t
        TeX-source-correlate-mode t
        ;; TeX-source-correlate-method 'synctex
        TeX-source-correlate-start-server t
        TeX-output-dir "./tex_output"
        TeX-auto-save t
        TeX-parse-self t
        TeX-electric-escape nil
        TeX-error-overview-open-after-TeX-run nil
        LaTeX-command "latex -synctex=1")

  (setq-default TeX-output-dir "./tex_output")
  (add-to-list 'TeX-expand-list
	             '("%sn" (lambda () server-name)))

  (add-to-list 'TeX-view-program-list
	             '("Zathura"
	               ("zathura %o"
		              (mode-io-correlate " --synctex-forward %n:0:\"%b\" -x \"emacsclient --socket-name=%sn +%{line} %{input}\""))
	               "zathura"))

  (add-to-list 'TeX-view-program-selection
               '(output-pdf "Zathura"))
  )
;; }}}
;; Org {{{
(defun my/org-mode-setup ()
  "Setup after org-mode is loaded"
  ;; Indent headlines
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
  (org-directory (getenv "NOTES_ORG_HOME"))
  (org-ellipsis " ▾")
  (org-hide-emphasis-markers t "Hide symbols")
  (org-startup-folded 'content)
  (org-startup-folded t)
  (org-cycle-include-plain-lists 'integrate "Keep items folded when cycling")
  (org-enforce-todo-dependencies t)
  (org-cycle-separator-lines -1  "No empty lines needed to fold subtrees")
  (org-startup-with-inline-images t)
  (org-image-actual-width nil)
  ;; Padding
  (line-spacing 0.05)
  ;; Hide title in the header
  (org-hidden-keywords '(title))
  :config

  (require 'ol-man)
  (require 'ol-link-handler)

  (defun my/update-org-level-face ()
    (dolist (face '((org-level-1 . 1.25)
                    (org-level-2 . 1.15)
                    (org-level-3 . 1.05)
                    (org-level-4 . 1.1)
                    (org-level-5 . 1.1)
                    (org-level-6 . 1.1)
                    (org-level-7 . 1.1)
                    (org-level-8 . 1.1)))
      (set-face-attribute (car face) nil :font my/default-font :weight 'bold :height (cdr face))))
  (my/update-org-level-face)

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

  ;; Disabled, because #+filetags: is used to tag files
  (setq org-use-tag-inheritance nil)

  ;; Keywords
  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                (sequence "TO-READ(r)" "WAITING(w@)" "|" "CANCELLED(c@/!)")
                )))

  (defface org-todo-default '((t :weight bold :inverse-video t :height 0.8)) "default face for todo keywords")

  (setq
   TODO_color "red4"
   TOREAD_color "dark green"
   NEXT_color "blue1"
   WAITING_color "darkorange3"
   DONE_color "forest green"
   CANCELLED_color "forest green"
   )

  (setq org-todo-keyword-faces
        (list
         `("TODO" :foreground ,TODO_color :inherit org-todo-default :box (:line-width 3 :color ,TODO_color))
         `("TO-READ" :foreground ,TOREAD_color :inherit org-todo-default :box (:line-width 3 :color ,TOREAD_color))
         `("NEXT" :foreground ,NEXT_color :inherit org-todo-default :box (:line-width 3 :color ,NEXT_color))
         `("WAITING" :foreground ,WAITING_color :inherit org-todo-default :box (:line-width 3 :color ,WAITING_color))
         `("DONE" :foreground ,DONE_color :inherit org-todo-default :box (:line-width 3 :color ,DONE_color))
         `("CANCELLED" :foreground ,CANCELLED_color :inherit org-todo-default :box (:line-width 3 :color ,CANCELLED_color))
         ))

  (defun get-org-files ()
    (interactive)
    (directory-files (getenv "NOTES_ORG_HOME") nil ".org$"))
  ;; (get-org-files :maxlevel . 3)
  (setq org-refile-targets '((nil :maxlevel . 9) ;; Refile to current directory at any level
                             (org-agenda-files :maxlevel . 3)
                             (org-buffer-list :maxlevel . 2)))

  (setq org-refile-use-outline-path 'file      ;; Allow files in refiling candidates
        org-outline-path-complete-in-steps nil ;; Don't remove headings
        org-refile-allow-creating-parent-nodes 'confirm ;; Add a new parent header to the refiled header
        )

  ;; Insert mode after going to capture
  (add-hook 'org-capture-mode-hook 'evil-insert-state)
  ;; Insert mode after going to add a note to logs
  (add-hook 'org-log-buffer-setup-hook #'(lambda () (evil-insert 0)))

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
  (setq
   ;; Remove completed deadline, scheduled, completed from agenda
   ;; The time when it get closed will be shown
   org-agenda-skip-deadline-if-done t
   org-agenda-skip-scheduled-if-done t
   ;; Hide the deadline prewarning prior to scheduled date.
   org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled
   ;; Calendar starts at Monday
   calendar-week-start-day 1
   org-deadline-warning-days 0
   org-agenda-compact-blocks t
   org-agenda-block-separator nil
   ;;Show today +7 days
   org-agenda-start-on-weekday nil)

  (defun my/open-all-org-agenda-files ()
    (interactive)
    (let ((files (org-agenda-files))) (mapcar (lambda (x) (find-file-noselect x)) files)))

  ;; Add daylight saving schedule to agenda
  (setq org-agenda-include-diary t
        calendar-holidays holiday-solar-holidays)

  (custom-set-faces
   '(org-agenda-dimmed-todo-face ((t (:inverse-video nil :box nil :weight normal))))
   )
  ;; Icons for org-link-beautify
  (use-package all-the-icons
    :if (display-graphic-p))
  ;; Add icons to links
  (use-package org-link-beautify
    :after org
    :requires all-the-icons
    :custom
    ;; Increase performance
    (org-element-use-cache t)
    ;; Disable PDF preview
    (org-link-beautify-pdf-preview nil)
    :config
    (org-link-beautify-mode 1)
    ;; Org agenda hangs when using the mode
    ;; Disable the mode in agenda mode and re-enable
    (add-hook 'org-agenda-mode-hook 'org-link-beautify-disable)
    (add-hook 'org-agenda-finalize-hook 'org-link-beautify-enable)

    (defun add-link-handler-icon (orig-fun &rest args)
      "Add link-handler icon"
      (if (string= "link-handler" orig-fun) (nerd-icons-faicon "nf-fa-link") nil)
      )

    (advice-add 'org-link-beautify--return-icon :before-until #'add-link-handler-icon)
    )

  (use-package org-download
    :after org
    :config (setq-default org-download-image-dir "~/notes/org/images/drag_drop")
    )

  (defun my/open-super-agenda ()
    (interactive) (org-agenda nil "z") (delete-other-windows))

  (add-hook 'emacs-startup-hook 'my/open-super-agenda)

  (set-face-attribute 'org-agenda-date nil :height 1.05)
  (set-face-attribute 'org-agenda-clocking nil :background "light gray" :box '(:color "light gray"))

  '(org-document-info ((t (:foreground "dark orange"))))
  (set-face-attribute 'org-agenda-date-today nil :height 1.05)
  (set-face-attribute 'org-agenda-date-weekend nil :height 1.05)

  ;; Get roam alias, otherwise the title of node
  (defun my/get-title-property ()
    (setq title (elt (elt (org-collect-keywords '("TITLE")) 0) 1))
    (setq roam_alias (org-entry-get-with-inheritance "ROAM_ALIASES"))
    (setq max_length 11)
    (if (> (length title) max_length ) (setq title (concat (substring title 0 max_length) "...")))
    (if roam_alias roam_alias (if title title "")))

  (setq org-agenda-current-time-string "┈┈┈┈┈┈┈┈┈┈┈ now"
        org-agenda-time-grid '((weekly today require-timed)
                               (800 1000 1200 1400 1600 1800 2000)
                               "---" "┈┈┈┈┈┈┈┈┈┈┈┈┈")
        org-agenda-prefix-format '((agenda . " %-16(my/get-title-property)%-12t%-6e% s")
                                   (todo . " %-14:(my/get-title-property) %-6e")
                                   (tags . " %-12:(my/get-title-property) %-6e")
                                   (search . " %-12:(my/get-title-property) %-6e")))

  ;; Save org buffers after quiting agenda mode
  (advice-add 'org-agenda-quit :before #'(lambda () (interactive) (let ((inhibit-message t)) (org-save-all-org-buffers))))

  ;; Log the state change
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  ;; Show closed items, not the clocked ones
  (setq org-agenda-log-mode-items '(closed))
  ;; Show today's clocked report
  (setq org-clock-clocktable-default-properties '(:maxlevel 2 :narrow 40! :link t :sort (5 . ?t) :fileskip0 t :stepskip0 t :scope agenda :block today :properties ("Effort")))
  ;; Persist clock history on Emacs close
  (org-clock-persistence-insinuate)
  (setq org-clock-persist 'clock)

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
                          '((:name "Next"
                                   :todo "NEXT")
                            (:name "Waiting"
                                   :todo "WAIT")
                            (:name "High priority"
                                   :priority "A")
                            (:name "Capture"
                                   :file-path ".*capture.org")
                            (:discard (:anything))
                            ))))))
          ("o" "Others"
           ((todo "DONE")
            (alltodo "" ((org-super-agenda-groups
                          '((:priority "A")
                            (:priority "B")
                            (:todo "TO-READ")
                            (:name "Short"
                                   :tag "effort< 1:01")
                            (:discard (:priority "C"))
                            ))))))
          ("A" "All"
           ((todo "DONE")
            (alltodo "" ((org-super-agenda-groups
                          '((:priority "A")
                            (:priority "B")
                            (:name "Short"
                                   :tag "effort< 1:01")
                            (:auto-category)
                            ))))))
          ))

  (use-package org-super-agenda
    ;; Should be loaded at the start
    :init (org-super-agenda-mode)
    :config

    (defun my/org-agenda-switch-to ()
      "Go to entry and expand parent element"
      (interactive)
      (org-agenda-switch-to)
      (bookmark-set "agenda")
      (org-up-element)
      (org-up-element)
      (evil-toggle-fold)
      (evil-toggle-fold)
      (bookmark-jump "agenda")
      )

    ;; Open parent element when switching from agenda
    (advice-add 'org-agenda-switch-to :after #'(lambda ()
                                                 (bookmark-set "agenda")
                                                 (org-up-element)
                                                 (org-up-element)
                                                 (evil-toggle-fold)
                                                 (evil-toggle-fold)
                                                 (bookmark-jump "agenda")
                                                 ))

    (evil-define-key 'motion 'org-super-agenda-header-map
      (kbd "q") 'org-agenda-quit
      (kbd "C-h") 'evil-window-left
      (kbd "C-j") 'org-agenda-switch-to
      (kbd "gj") 'org-agenda-next-item
      (kbd "gk") 'org-agenda-previous-item
      (kbd "gx") 'org-open-at-point-global)
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
        ;; TODO: make the string dynamic
        (if (= (length (org-map-entries nil  "+TODO={TODO\\\|NEXT\\\|DONE\\\|WAITING\\\TO-READ\\\|CANCELLED}" 'file)) 0)
            (setq curr-files (my/remove-from-agenda-files buffer-file-name))
          (setq curr-files (my/add-to-agenda-files buffer-file-name))
          )
      ;; There's a TODO in buffer
      (setq curr-files (my/add-to-agenda-files buffer-file-name))
      )
    (org-store-new-agenda-file-list curr-files)
    ;; Sort agenda files, so that there's not that often changes to be tracked by git
    (shell-command (concat "sort " (concat (getenv "NOTES_ORG_HOME") "/agenda_files") " | sponge " (concat (getenv "NOTES_ORG_HOME") "/agenda_files")))
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
  (setq ;; Resume when clocking into task with open clock
   org-clock-in-resume t
   ;; Remove log if task was clocked for 0:00 (accidental clocking)
   org-clock-out-remove-zero-time-clocks t
   ;; The default value (5) is too conservative.
   org-clock-history-length 20)


  (defun my/org-clock-in-if-next ()
    "Clock in when the task is marked NEXT"
    (when (and (string= org-state "NEXT")
               (not (string= org-last-state org-state)))
      (org-clock-in)))

  (add-hook 'org-after-todo-state-change-hook
            'my/org-clock-in-if-next)

  (setq org-clock-out-switch-to-state "TODO")
  (setq org-clock-in-switch-to-state "NEXT")

  (defun my/org-clock-out-done ()
    "Clock out and switch state of task to done"
    (interactive)
    (setq org-clock-out-switch-to-state "DONE")
    (org-clock-out)
    (setq org-clock-out-switch-to-state "TODO"))


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

  ;; Generated by chatGPT
  (defun my/org-goto-next-scheduled ()
    "Go to the next entry with the closest future scheduled timestamp (including time) from all Org files in the agenda."
    (interactive)
    (let ((closest-entry nil)
          (closest-timestamp (time-add (current-time) (days-to-time 365)))) ; Initialize with a far future timestamp

      ;; Read the list of files from the file specified by org-agenda-files
      (with-temp-buffer
        (insert-file-contents org-agenda-files)
        (setq org-agenda-file-list (split-string (buffer-string) "\n" t)))

      ;; Iterate over each file in the Org agenda file list
      (dolist (file org-agenda-file-list)
        (with-current-buffer (find-file-noselect file)
          ;; Iterate over each entry in the buffer
          (org-map-entries
           (lambda ()
             (let* ((scheduled (org-entry-get nil "SCHEDULED"))
                    (timestamp (and scheduled (org-time-string-to-time scheduled))))
               ;; Compare the timestamp (with time) with the closest-timestamp and future timestamps
               (when (and timestamp
                          (time-less-p (current-time) timestamp)
                          (time-less-p timestamp closest-timestamp)
                          (string-match-p "[0-9]+:[0-9]+" scheduled))
                 (setq closest-entry (point-marker))
                 (setq closest-timestamp timestamp)))))))

      ;; Go to the closest entry if found
      (if closest-entry
          (progn
            (switch-to-buffer (marker-buffer closest-entry))
            (goto-char (marker-position closest-entry)))
        (message "No entry found with a future scheduled timestamp including time."))))

  (add-hook 'org-clock-in-hook #'my/add-clock-tmp-file)

  (dolist (advice '(org-clock-out
                    org-clock-cancel))
    (advice-add advice :before #'(lambda (&rest pos) (shell-command "/bin/rm /tmp/org_current_task 2> /dev/null")))
    )

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
   '(org-block ((t (:inherit fixed-pitch :height 1.1))))
   ;; Background is nil because org-block-end-line background shows in header
   ;; Check https://github.com/doomemacs/themes/issues/453
   '(org-block-begin-line ((t (:background unspecified :weight bold))))
   '(org-block-end-line ((t (:background unspecified :weight bold))))
   '(org-code ((t (:inherit (shadow fixed-pitch) :height 1.1))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch) :height 1.1))))
   )


  ;; Used by babel
  (use-package plantuml-mode
    :config
    (setq org-plantuml-jar-path (concat user-emacs-directory "plantuml.jar"))
    (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
    )
  (use-package yaml-mode)
  (use-package dockerfile-mode)
  (use-package docker-compose-mode)
  (use-package lua-mode)

  (use-package ob-async)
  (require 'ob-makefile)

  ;; Run/highlight code using babel in org-mode
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (sql . t)
     (lua . t)
     (plantuml . t)
     (makefile . t)
     (emacs-lisp . t)
     (shell . t)
     ))

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
    (add-to-list 'org-structure-template-alist '("lisp" . "src emacs-lisp"))
    (add-to-list 'org-structure-template-alist '("lua" . "src lua"))
    )

  ;; Go in the block with insert mode after inserting it
  (advice-add 'org-insert-structure-template :after #'(lambda (orig-fun &rest args) (newline) (evil-previous-line)))

  ;; To export to markdown
  (require 'ox-md)

  ;; Exporting settings
  (setq org-export-with-broken-links t
        org-export-coding-system 'utf-8
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
                 `("q" "org-capture-url" entry
                   (file ,(concat (getenv "NOTES_ORG_HOME") "/capture.org"))
                   "* TODO [[%:link][%:description]]" :immediate-finish t))

    (add-to-list 'org-capture-templates
                 `("n" "org-capture-note" entry
                   (file ,(concat (getenv "NOTES_ORG_HOME") "/capture.org"))
                   "* TODO %:description" :immediate-finish t))
    (add-to-list 'org-capture-templates
                 `("w" "Web site" entry
                   (file ,(concat (getenv "NOTES_ORG_HOME") "/org_protocol_html.org")) ;
                   "* %a :website:\n\n%U %?\n\n%:initial"))

    (setq org-protocol-default-template-key "q")
    )
  (use-package org-protocol-capture-html
    :ensure nil)
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
  (evil-define-key 'insert org-cdlatex-mode-map (kbd "C-j") 'cdlatex-tab)
  ;; Disable ` functionality that prompts for symbols
  (evil-define-key 'insert org-cdlatex-mode-map (kbd "`") #'(lambda () (interactive) (insert "`")))
  )

(use-package citar
  :no-require
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  ;; Open files using external program
  (citar-file-open-functions (list (cons t 'citar-file-open-external)))
  ;; Icons
  (setq citar-symbols
        `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
          (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
          (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " ")))
  (setq citar-symbol-separator "  ")
  :bind )

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
  ;; Don't insert a new item when pressing o/O in org-mode
  (evil-org-special-o/O '(table-row))
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

                              ;; Move to beginning of line before insert heading, otherwise org-insert-heading will insert below
                              ;; Insert a bullet in case point is on a bullet point
                              (kbd "<C-return>") #'(lambda () (interactive)
                                                     (cond ((org-at-item-p)
                                                            (let ((evil-org-special-o/O '(table-row item))) (evil-org-open-above 1)) t)
                                                           ((progn (beginning-of-line) (org-insert-heading) (evil-insert 0)))))
                              (kbd "<C-S-return>") #'(lambda () (interactive) (beginning-of-line) (org-insert-todo-heading 0) (evil-insert 0))
                              (kbd "<M-return>") #'(lambda () (interactive)
                                                     (cond ((org-at-item-p)
                                                            (let ((evil-org-special-o/O '(table-row item))) (evil-org-open-below 1)) t)
                                                           ((progn (org-insert-heading-after-current) (evil-insert 0)))))
                              (kbd "<M-S-return>") #'(lambda () (interactive) (end-of-line) (let ((current-prefix-arg '(16))) (call-interactively 'org-insert-todo-heading)) (evil-insert 0))
                            )
                            ;; readline keybinding
                            (evil-define-key 'insert 'evil-org-mode
                              (kbd "C-d") 'delete-char
                              )
                            (evil-define-key 'normal 'evil-org-mode
                              (kbd "zi")  #'org-toggle-inline-images
                              (kbd "<C-SPC>") 'org-cycle
                              (kbd "zl")  #'org-latex-preview
                              ;; Open files at cursor
                              (kbd "gx") #'(lambda () (interactive) (let ((inhibit-message t)) (org-open-at-point)))
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
    (start-process-shell-command "Update i3 focus window config" nil (concat  path_to_script " none 2")))
  (setq org-agenda-files (concat (getenv "NOTES_ORG_HOME") "/agenda_files"))
  )

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-item-bullet-alist '((?- . ?•) (?* . ?•) (?+ . ?•)))
  (org-superstar-headline-bullets-list '("●" "○" "●" "○" "●" "○" "●")))

(use-package org-roam
  :after org
  :custom
  (org-roam-directory (getenv "NOTES_ORG_HOME"))
  (org-roam-node-display-template
   (concat "${title:*} " (propertize "${tags:50}" 'face 'org-tag)))
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

;; ORG NOTIFICATION
(use-package appt
  :after org
  :ensure nil
  :config
  (appt-activate 1)

  (setq appt-time-msg-list nil                           ;; clear existing appt list
        appt-message-warning-time '10                    ;; send first warning before appointment
        appt-display-interval '5                         ;; warn every every X minutes from t - appt-message-warning-time
        appt-display-mode-line nil                       ;; don't show in the modeline
        appt-display-format 'window)                     ;; pass warnings to the designated window function
  (setq appt-disp-window-function (function toast-appt-display))

  ;; Set up the call to the notifier
  (defun toast-appt-send-notification (title msg)
    (start-process-shell-command "" nil (concat "[[ $(dunstify '" title "' '" msg "' --appname emacs_org --action='action,label') == 'action' ]] && xdotool search --sync --name 'emacs_org_name' windowactivate && emacsclient -e '(my/org-goto-next-scheduled)'")))


  ;; Designate the window function for my/appt-send-notification
  (defun toast-appt-display (min-to-app new-time msg)
    (toast-appt-send-notification
     (format "%s minutes" min-to-app)    ;; passed to -t in toast call
     (format "%s" msg))                                 ;; passed to -m in toast call
    (message nil))

  (defun org-agenda-to-appt-clear-message ()
    (interactive) (let ((inhibit-message t)) (setq appt-time-msg-list nil) (org-agenda-to-appt)))

  (add-hook 'emacs-startup-hook 'org-agenda-to-appt-clear-message)

  ;; Generate the appt list from org agenda files on emacs launch and every 15 minutes
  (run-at-time nil 900 'org-agenda-to-appt-clear-message))
;; }}}
;; Navigation {{{
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
  :bind (:map vertico-map
              ("M-C-j" . vertico-quick-jump))
  :config
  (when evil-collection-setup-minibuffer
    ;; Open buffer in a new window, uses embark's v and x
    (evil-collection-define-key 'insert 'vertico-map (kbd "C-v") (kbd "C-. v"))
    (evil-collection-define-key 'insert 'vertico-map (kbd "C-x") (kbd "C-. x")))

  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

  )

(use-package counsel)
(use-package consult
  :config
  (global-set-key [remap repeat-complex-command] #'consult-complex-command)
  )
(use-package embark-consult)

(use-package embark
  :bind (("C-." . embark-act))
  :config

  (evil-define-key 'normal 'global (kbd "C-.") 'embark-act)

  ;; Use helpful package
  (defvar-keymap embark-command-actions
    :doc "Actions for helpful command"
    :parent embark-general-map
    "h" #'helpful-command)
  (add-to-list 'embark-keymap-alist '(command . embark-command-actions))

  ;; C-v and C-x window splits window
  ;; Org-roam-nodes
  (defvar-keymap embark-org-roam-nodes-actions
    :doc "Keymap for actions for org roam nodes"
    :parent embark-general-map
    "x" #'my/org-roam-node-find-window-x
    "v" #'my/org-roam-node-find-window-v)
  (add-to-list 'embark-keymap-alist '(org-roam-node . embark-org-roam-nodes-actions))

  (defun my/org-roam-node-find-window-v ()
    (interactive)
    (org-roam-node-find t))

  (defun my/org-roam-node-find-window-x ()
    (interactive)
    (evil-window-split)
    (org-roam-node-find))

  ;; Buffers
  (defvar-keymap embark-buffer-actions
    :doc "Keymap for actions for buffers"
    :parent embark-general-map
    "x" #'my/switch-to-buffer-other-window-x
    "v" #'my/switch-to-buffer-other-window-v)
  (add-to-list 'embark-keymap-alist '(buffer . embark-buffer-actions))

  (defun my/switch-to-buffer-other-window-x (buf)
    (interactive "bBuffer: ")
    (split-window-below) (windmove-down)
    (switch-to-buffer buf))

  (defun my/switch-to-buffer-other-window-v (buf)
    (interactive "bBuffer: ")
    (split-window-right) (windmove-right)
    (switch-to-buffer buf))

  ;; Files
  (defvar-keymap embark-file-actions
    :doc "Keymap for actions for files"
    :parent embark-general-map
    "x" #'my/switch-to-file-other-window-x
    "v" #'my/switch-to-file-other-window-v)
  (add-to-list 'embark-keymap-alist '(file . embark-file-actions))

  (defun my/switch-to-file-other-window-x (file)
    (interactive "bFile: ")
    (split-window-below) (windmove-down)
    (find-file file))

  (defun my/switch-to-file-other-window-v (file)
    (interactive "bFile: ")
    (split-window-right) (windmove-right)
    (find-file file))

  ;; Insert using embark on searching for org headings
  ;; https://gist.github.com/jdtsmith/8602d998116b953725218224b77b8766?permalink_comment_id=4465637
  (defun my/org-link-heading-here (cand)
    (when-let ((marker (get-text-property 0 'consult--candidate cand)))
      (save-excursion
        (with-current-buffer (marker-buffer marker)
          (goto-char marker)
          (org-store-link nil t)))
      (org-insert-all-links 1 "" " ")))

  (defvar-keymap embark-consult-org-heading-map
    :doc "Keymap for operating on org headings"
    :parent embark-general-map
    "l" 'my/org-link-heading-here)

  (add-to-list 'embark-keymap-alist '(consult-org-heading . embark-consult-org-heading-map))
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
;; }}}
;; Vim leader / which-key {{{
;; Remap universal argument
(global-set-key (kbd "M-u") 'universal-argument)
(define-key universal-argument-map (kbd "M-u") 'universal-argument-more)

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

  (general-override-mode)

  (general-def 'normal 'override
    "C-h" 'evil-window-left
    "C-j" 'evil-window-down
    "C-k" 'evil-window-up
    "C-l" 'evil-window-right
    )

  (general-define-key
   :states '(normal visual motion)
   :keymaps 'global
   "SPC" 'main-hydra/body)

  (general-define-key
   :states 'insert
   :keymaps 'global
   "M-SPC" 'main-hydra/body)

  (add-hook 'org-mode-hook
            #'(lambda ()
                (general-define-key
                 :states '(normal visual motion)
                 :keymaps 'local
                 "SPC" 'org-hydra/body)
                (general-define-key
                 :states '(insert)
                 :keymaps 'local
                 "M-SPC" 'org-hydra/body))
            )

  (add-hook 'org-agenda-mode-hook
            #'(lambda ()
                (general-define-key
                 :states '(normal visual motion)
                 :keymaps 'local
                 "SPC" 'agenda-hydra/body))
            )
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

;; Keep links stored after insertion
(setq org-link-keep-stored-after-insertion t)

(defun my/org-insert-last-stored-link-with-title (arg)
  "Insert the last stored link with the title of the file and the header without removing it from the link list."
  (interactive "P")
  (let ((org-link-keep-stored-after-insertion t)
        (links (copy-sequence org-stored-links))
        (pr "- ")
        (po "\n")
        (cnt 1) l
        file-buffer)
    (if (null org-stored-links)
        (message "No link to insert")
      (while (and (or (listp 1) (>= 1 cnt))
                  (setq l (pop links)))
        (setq cnt (1+ cnt))
        (let* ((file-path (car l))
               (header (cadr l))
               (file-buffer (find-file-noselect (nth 1 (split-string file-path ":"))))
               (file-title (org-global-prop-value "TITLE" file-buffer))
               (file-link (format "%s" (abbreviate-file-name file-path)))
               (link-text (format "%s: %s" file-title header))
               (link (concat file-link "::" header)))
          (insert pr)
          (insert (concat "[["file-link"]["link-text"]]"))
          (insert po)))
      (when file-buffer
        (kill-buffer file-buffer)))))

;; Get property value for a buffer
;; https://emacs.stackexchange.com/questions/21713/how-to-get-property-values-from-org-file-headers
(defun org-global-prop-value (key buffer)
  "Get global org property KEY of current buffer."
  (org-element-property :value (car (org-global-props key buffer))))

(defun org-global-props (&optional property buffer)
  "Get the plists of global org properties of current buffer."
  (unless property (setq property "PROPERTY"))
  (with-current-buffer (or buffer (current-buffer))
    (org-element-map (org-element-parse-buffer) 'keyword (lambda (el) (when (string-match property (org-element-property :key el)) el)))))

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
  (" f" find-hydra/body "find")
  (" d" deft "deft")
  (" e" toggle-plugins/body "toggle plugins")
  (" g" git-hydra/body "git")
  (" x" (lambda () (interactive) (org-capture nil "d")) "capture")
  ;; Exit Emacs (1 window) or window (>1 window)
  (" q" (lambda () (interactive) (let ((inhibit-message t)) (save-buffer)
                                      (if (eq (length (window-list)) 1) (save-buffers-kill-emacs) (delete-window))
                                      )) "Save and delete buffer")
  (" Q" evil-quit-all "Exit Emacs")
  (" w" (lambda () (interactive) (let ((inhibit-message t)) (save-buffer))) "Save buffer")
  ("ss" (lambda () (interactive) (load-file (concat user-emacs-directory "/init.el"))) "source rc")
  )

(defhydra toggle-plugins (:exit t :idle 1)
  (" u" undo-tree-visualize "undo tree")
  (" s" (lambda () (interactive) (split-window-below) (find-file (concat user-emacs-directory "/init.el"))) "edit rc")
  )

(defhydra find-hydra (:exit t :idle 1)
  (" b" switch-to-buffer "Buffer")
  (" B" org-cite-insert "BibTex")
  (" f" (lambda () (interactive) (let ((inhibit-message t)) (org-roam-node-find))) "find node" :column " node")
  )

;; Git diff, git blame, reset buffer can be added using magit
(defhydra git-hydra (:exit t :hint nil :idle 1)
  (" r" git-gutter:revert-hunk "revert hunk" :column " hunk")
  (" s" git-gutter:stage-hunk "stage hunk")
  (" p" git-gutter:popup-hunk "preview hunk")
  (" k" git-gutter:previous-hunk "previous hunk")
  (" j" git-gutter:next-hunk  "next hunk")
  (" S" magit-stage-buffer-file "stage buffer" :column " buffer")
  (" U" magit-unstage-buffer-file "unstage buffer")
  (" g" magit-status "status" :column " magit")
  (" c" magit-commit "commit")
  (" C" magit-show-commit "show commit")
  (" L" magit-log-buffer-file "log")

  (" o" browse-at-remote "browse at remote")

  ;; C-j C-k previous/forward historic version
  (" t" git-timemachine-toggle "toggle" :column " timemachine")
  )

(defhydra help-hydra (:exit t :idle 1)
  (" a" consult-info "info pages" :column " help")
  (" A" apropos "apropos")
  (" k" helpful-key "key")

  (" b" embark-bindings "binding")
  (" f" helpful-callable "function")
  (" c" helpful-command "command")
  (" ." helpful-at-point "at point")
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
  (" d" my/org-clock-out-done "done")
  (" t" my/org-toggle-last-clock "toggle")
  (" c" org-clock-cancel "cancel")
  (" g" org-clock-goto "goto" :column "")
  (" e" org-set-effort "effort")
  (" m" org-clock-modify-effort-estimate "modify current effort")
  (" r" org-clock-report "report")
  (" d" org-clock-display "display subtree times")
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
  (" c" (find-file (concat (getenv "NOTES_ORG_HOME") "/capture.org"))
   "goto capture")
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
  (" t" (lambda () (interactive) (call-interactively #'org-link-beautify-mode) (org-toggle-link-display)) "toggle")
  (" p" my/org-insert-last-stored-link-with-title "paste last")
  )

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
;;;; }}}
