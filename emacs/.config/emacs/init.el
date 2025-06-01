;; -*- lexical-binding: t; -*-
;; {{{ Package/Lisp management

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
 (setq straight-check-for-modifications '(find-when-checking
                                           check-on-save)
        straight-vc-git-default-clone-depth 2)
(when (daemonp) (setq straight-build-dir (concat "build" "_" (daemonp))))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)
(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

;; PACKAGE.el
;; Initialize package sources
;; (setq package-archives '(("melpa" . "https://melpa.org/packages/")
;;                          ("nognu" . "https://elpa.nongnu.org/nongnu/")
;;                          ("elpa" . "https://elpa.gnu.org/packages/")))
;; Ensure that all packages are installed
;; (require 'use-package-ensure)
;; (setq use-package-always-ensure t)

;; Used to update the package from upgrade_system bash function
(use-package auto-package-update
  :config
  ;; Delete the old version on updates.
  (setq auto-package-update-delete-old-versions t))

(add-to-list 'load-path (concat user-emacs-directory "/lisp"))
(require 'functions)
;; }}}
;; Defining variables {{{
(defconst notes-dir (concat (getenv "NOTES_ORG_HOME") "/") "Notes directory")
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
(add-hook 'ediff-mode-hook (lambda () (golden-ratio-mode 0)))
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
  :hook (after-init . savehist-mode)
  :config
  (setq history-length 1000)
  (setq history-delete-duplicates t)
  )

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
  (no-littering-theme-backups)

  ;; Create a savehistory file for each daemon, so they don't complain about stealing it from each other
  (when (daemonp) (setq savehist-file (concat user-emacs-directory "var/savehist_" (daemonp) ".el")))
  )

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
  :disabled
  :config
  ;; Add a glyph for hypothesis links
  (add-to-list 'nerd-icons-url-alist '("^\\(https?://\\)?\\(www\\.\\)?hyp\\.is" nerd-icons-mdicon "nf-md-alpha_h_box"))
  (add-to-list 'nerd-icons-url-alist '("^link-handler://" nerd-icons-mdicon "nf-md-alpha_h_box"))
  (unless (package-installed-p 'nerd-icons)
    (nerd-icons-install-fonts))
  )

;; Insert text content using links
(use-package org-transclusion
  :after org
  :hook (org-mode . org-transclusion-mode)
  :custom (org-transclusion-exclude-elements '(property-drawer planning))
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
  :hook (org-mode . (lambda ()
                      (modify-syntax-entry ?$ "\"" org-mode-syntax-table)
                      ))
(emacs-lisp-mode . (lambda ()
  (setq electric-pair-preserve-balance t)
                      ))
  :custom
  (electric-pair-preserve-balance nil)
  :config
  (electric-pair-mode t)

  (defun my/ignore-elec-pairs (c)
    (cond
     ((and (char-equal c ?$) (org-in-src-block-p)) t)
     ((when (char-equal c (preceding-char))) nil)
     ((when (eq (following-char) 0) ) nil)
     ))

     (add-function :before-until electric-pair-inhibit-predicate 'my/ignore-elec-pairs)
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
  (setq ispell-alternate-dictionary ispell-personal-dictionary)
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
  :straight nil
  :ensure nil
  :custom
  (save-abbrevs 'silently)
  :config
  (setq-default abbrev-mode t)
  (setq abbrev-list '(("latex" "LaTeX" nil 1)))

  (define-abbrev-table 'latex-mode-abbrev-table
    abbrev-list)

  (define-abbrev-table 'org-mode-abbrev-table
    abbrev-list)
  )

;; Source https://tony-zorman.com/posts/join-lines-comments.html
(defun delete-indentation (&optional arg beg end)
  "Join this line to previous and fix up whitespace at join.
If there is a fill prefix, delete it from the beginning of this
line.
With prefix ARG, join the current line to the following line.
When BEG and END are non-nil, join all lines in the region they
define.  Interactively, BEG and END are, respectively, the start
and end of the region if it is active, else nil.  (The region is
ignored if prefix ARG is given.)

When joining lines, smartly delete comment beginnings, such that one
does not have to do this by oneself."
  (interactive
   (progn (barf-if-buffer-read-only)
          (cons current-prefix-arg
                (and (use-region-p)
                     (list (region-beginning) (region-end))))))
  ;; Consistently deactivate mark even when no text is changed.
  (setq deactivate-mark t)
  (if (and beg (not arg))
      ;; Region is active.  Go to END, but only if region spans
      ;; multiple lines.
      (and (goto-char beg)
           (> end (line-end-position))
           (goto-char end))
    ;; Region is inactive.  Set a loop sentinel
    ;; (subtracting 1 in order to compare less than BOB).
    (setq beg (1- (line-beginning-position (and arg 2))))
    (when arg (forward-line)))
  (let ((prefix (and (> (length comment-start) 0)
                     (regexp-quote comment-start))))
    (while (and (> (line-beginning-position) beg)
                (forward-line 0)
                (= (preceding-char) ?\n))
      (if (save-excursion (forward-line -1) (eolp))
          (delete-char -1)
        (delete-char -1)
        ;; If the appended line started with the fill prefix, delete it.
        (let ((prev-comment?            ; Don't delete the start of a comment.
               (save-excursion
                 (back-to-indentation)
                 (looking-at prefix))))
          (delete-horizontal-space)
          (while (and prev-comment? prefix (looking-at prefix))
            (replace-match "" t t))
          (fixup-whitespace))))))

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

;; Use company backends for Corfu
(use-package company)

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
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)

  ;; Use Company backends as Capfs.
  (require 'company)
  ;; Use the company-dabbrev and company-elisp backends together.
  (add-to-list 'completion-at-point-functions
         (cape-company-to-capf
          (apply-partially #'company--multi-backend-adapter
                           '(company-yasnippet))))
  )

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

  ;; Swap C-? & C-h
  (evil-define-key '(normal insert) 'global  (kbd "C-?") 'help-command)  ;; Bind C-? to help-command

  (evil-define-key 'insert 'key-translation-map [?\C-h] [?\C-?])
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

  ;; Paste using <C-S-v>; used for insert_link
  (evil-define-key 'insert 'global  (kbd "C-S-v") 'evil-paste-after)

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

  ;; Override evil-collection by removing last empty line (if exists)
  (defun evil-collection-unimpaired-paste-above ()
    "Paste above current line with preserving indentation."
    (interactive)
    (let ((indent (current-indentation))
          (column (current-column))
          (cleaned (replace-regexp-in-string "\n\\s-*$" "" (current-kill 0 t))))
      (evil-insert-newline-above)
      (indent-to indent)
      (insert cleaned)
      (move-to-column column)))

  (defun evil-collection-unimpaired-paste-below ()
    "Paste below current line with preserving indentation."
    (interactive)
    (let ((indent (current-indentation))
          (column (current-column))
          (cleaned (replace-regexp-in-string "\n\\s-*$" "" (current-kill 0 t))))
      (evil-insert-newline-below)
      (indent-to indent)
      (insert cleaned)
      (move-to-column column)))

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

  (evil-collection-define-operator-key 'yank 'global-map
    "ob" #'my/trigger-theme
    "ow" #'visual-line-mode
    )

  (dolist (map '( minibuffer-local-map
                  minibuffer-local-ns-map
                  minibuffer-local-completion-map
                  minibuffer-local-must-match-map
                  minibuffer-local-isearch-map
                  evil-ex-completion-map
                  ))

    (evil-collection-define-key 'insert map
      (kbd "<escape>") 'abort-recursive-edit
      (kbd "C-p") 'previous-line-or-history-element
      (kbd "C-n") 'next-line-or-history-element
      (kbd "C-.") 'embark-act
      )
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
  :after (evil-collection nerd-icons)
  :custom
  (evil-collection-magit-use-y-for-yank t)
  (magit-format-file-function #'magit-format-file-nerd-icons)
  ;; Disabled because of a bug
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
(add-hook 'org-mode-hook (lambda () (interactive) (org-toggle-pretty-entities) (setq org-pretty-entities-include-sub-superscripts nil)))

(use-package laas
  :hook ((LaTeX-mode . laas-mode)
         (org-mode . laas-mode))
  :config ; do whatever here
  (aas-set-snippets 'laas-mode
                    ;; set condition!
                    :cond (lambda () string= (sexp-at-point) "dm")
                    "dm" '(yas "\\[
                                   `(save-excursion (previous-line)(make-string (current-indentation) ?\s))`$0
                                \\] ")
                    :cond #'texmathp ; expand only while in math
                    "t=" "\\triangleq"
                    "Sum" '(yas "\\sum_{$1}^{$2} $0")
                    "Prod" '(yas "\\prod_{$1}^{$2} $0")
                    ;; add accent snippets
                    :cond #'laas-object-on-left-condition
                    "qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))))

;; Used to insert latex environments and math templates
;; Previewing Latex fragments
(use-package cdlatex
  :hook (org-mode . org-cdlatex-mode)
  :config
  ;; Navigation & inserting latex env and templates using C-j
  (evil-define-key 'insert org-cdlatex-mode-map (kbd "C-j") 'cdlatex-tab)

  (setq
   cdlatex-math-symbol-alist
   '( ;; adding missing functions to 3rd level symbols
     (?2    ("^2"           "\\sqrt{?}"     ""     ))
     (?3    ("^3"           "\\sqrt[3]{?}"  ""     ))
     (?F    ("\\Phi"))
     (?.    ("\\cdot" "\\dots"))
     (?:    ("\\vdots" "\\ddots"))
     (?*    ("\\times" "\\star" "\\ast")))
   cdlatex-math-modify-alist
   '(
     (?b "\\bm" "\\textbf" t nil nil)
     (?B "\\mathbb" nil t nil)
     ))

  ;; Insert $$ when using cdlatex-math-symbol by overriding org--math-p behavior that makes it get ignored
  (defun my/update-command-name (orig-fun &rest args)
    ;; If it's before a dollar, it's in LaTeX fragment
    (if (eq (following-char) ?$) t
      (let ((this-command "random-command"))
        (apply orig-fun args)))
    )
  (advice-add 'org--math-p :around #'my/update-command-name)
  )

(use-package evil-tex
  ;; Needed otherwise package throws an error: AUCTeX-version variable is void
  :requires (auctex)
  :after (:any org latex)
  :hook ((org-mode . evil-tex-mode)
         (LaTeX-mode . evil-tex-mode)
         )
  :config
  (evil-tex-bind-to-cdlatex-accents-map '(("b" . "bm") ("t" . "text")))
  )

(use-package latex
  :straight nil
  :ensure nil
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
  (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
  (add-hook 'LaTeX-mode-hook 'prettify-symbols-mode)
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

(use-package auctex
  :straight nil
  :config
  (fmakunbound 'ConTeXt-mode)
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
  ;; :ensure org-contrib
  :hook ((org-mode . my/org-mode-setup))
  :custom
  (org-directory notes-dir)
  (org-ellipsis " ▾")
  (org-hide-emphasis-markers t "Hide symbols")
  (org-startup-folded 'content)
  (org-startup-folded t)
  (org-cycle-include-plain-lists 'integrate "Keep items folded when cycling")
  (org-enforce-todo-dependencies t)
  (org-cycle-separator-lines -1  "No empty lines needed to fold subtrees")
  (org-startup-with-inline-images t)
  (org-image-actual-width 500)
  ;; Padding
  (line-spacing 0.05)
  ;; Hide title in the header
  (org-hidden-keywords '(title))
  :config
  (use-package helm)
  (use-package helm-org)
  (use-package avy)
  (use-package org-ql) ;; Needed by org-sidebar
  (use-package org-sidebar)
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
                (sequence "BLOCKED(b@)" "|" "CANCELED(c@/!)")
                )))

  (defface org-todo-default '((t :weight bold :inverse-video t :height 0.8)) "default face for todo keywords")

  (setq
   TODO_color "red4"
   NEXT_color "DarkCyan"
   BLOCKED_color "darkorange3"
   DONE_color "forest green"
   CANCELED_color "forest green"
   )

  (setq org-todo-keyword-faces
        (list
         `("TODO" :foreground ,TODO_color :inherit org-todo-default :box (:line-width 3 :color ,TODO_color))
         `("NEXT" :foreground ,NEXT_color :inherit org-todo-default :box (:line-width 3 :color ,NEXT_color))
         `("BLOCKED" :foreground ,BLOCKED_color :inherit org-todo-default :box (:line-width 3 :color ,BLOCKED_color))
         `("DONE" :foreground ,DONE_color :inherit org-todo-default :box (:line-width 3 :color ,DONE_color))
         `("CANCELED" :foreground ,CANCELED_color :inherit org-todo-default :box (:line-width 3 :color ,CANCELED_color))
         ))

  (defun get-org-files ()
    (interactive)
    (directory-files notes-dir nil ".org$"))
  ;; (get-org-files :maxlevel . 3)
  (setq org-refile-targets '((nil :maxlevel . 9) ;; Refile to current directory at any level
                             (org-agenda-files :maxlevel . 3)
                             (org-buffer-list :maxlevel . 2)))

  (setq org-refile-use-outline-path 'file      ;; Allow files in refiling candidates
        org-outline-path-complete-in-steps nil ;; Don't remove headings
        org-refile-allow-creating-parent-nodes 'confirm ;; Add a new parent header to the refiled header
        )

  ;; Save files after 5 idle minutes
  (setq my/idle-timer-until-save-buffers (* 5 60))
  (run-with-idle-timer my/idle-timer-until-save-buffers my/idle-timer-until-save-buffers (lambda () (interactive) (save-some-buffers t)))

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

  ;; Open buffer anywhere
  (use-package yequake
    :straight (yequake :fetcher github :repo "alphapapa/yequake")
    :init
    ;; Override the original function by waiting until refiling is done
    (defun my/yequake-org-capture-refile (&optional goto keys)
      "Call `org-capture' in a Yequake frame.
Adds a function to `org-capture-after-finalize-hook' that closes
the recently toggled Yequake frame and removes itself from the
hook.

Note: if another Yequake frame is toggled before the capture is
finalized, when the capture is finalized, the wrong Yequake frame
will be toggled."
      (let* ((remove-hook-fn (lambda ()
                               (remove-hook 'org-capture-after-finalize-hook #'yequake-retoggle)
                               (advice-remove 'org-after-refile-insert-hook #'yequake-retoggle)
                               )))
        (add-hook 'org-capture-after-finalize-hook remove-hook-fn)
        (add-hook 'org-capture-after-finalize-hook #'yequake-retoggle)

        (advice-add 'org-capture-refile :after remove-hook-fn)
        (advice-add 'org-capture-refile :after #'yequake-retoggle)

        (advice-add 'yequake-retoggle :around #'toggle-if-not-refiling)
        ;; MAYBE: Propose an `org-capture-switch-buffer-fn' variable that could be rebound here.

        ;; NOTE: We override `org-switch-to-buffer-other-window' because
        ;; it always uses `switch-to-buffer-other-window', and we want to
        ;; display the template menu and capture buffer in the existing
        ;; window rather than splitting the frame.
        (cl-letf* (((symbol-function #'org-switch-to-buffer-other-window)
                    (symbol-function #'switch-to-buffer)))
          (condition-case nil
              (progn
                (org-capture goto keys)
                (delete-other-windows)
                ;; Be sure to return the "CAPTURE-" buffer, which is the current
                ;; buffer at this point.
                (current-buffer))
            ((error quit)
             ;; Capture aborted: remove the hook and hide the capture frame.
             (remove-hook 'org-capture-after-finalize-hook #'yequake-retoggle)
             (advice-remove 'org-capture-refile #'yequake-retoggle)
             (advice-remove 'yequake-retoggle #'toggle-if-not-refiling)
             (yequake-retoggle))))))

    (defun toggle-if-not-refiling (orig-fun &rest args)
      (unless org-capture-is-refiling
        (apply orig-fun args)))
    :custom
    (yequake-frames
     '(("org-capture-yeq"
        (buffer-fns . ((lambda () (my/yequake-org-capture-refile nil "d"))))
        (width . 0.75)
        (height . 0.5)
        (alpha . 1.0)
        )))
    )

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

  ;; Archive
  (defun my/update-archive-location (orig-fun &rest args)
    "Update org-archive-location to use the top- level heading"
    (let (org-archive-location)
      (save-excursion
        (let ((current-heading-title (org-get-heading t t)))
          (while (org-up-heading-safe))
          ;; If it's top-level heading, just archive it as is
          (setq org-archive-location
                (if (string= current-heading-title (org-get-heading t t)) "%s_archive::"
                  (concat "%s_archive::* " (org-get-heading t t))))))
      (apply orig-fun args)
      ))

  (advice-add 'org-archive-subtree :around #'my/update-archive-location)

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

  ;; Add daylight saving schedule to agenda
  (setq org-agenda-include-diary t)

  ;; Show only daylight saving start and end dates
  ;; Hook is needed to wait for library to load
  (add-hook 'org-agenda-finalize-hook
            (lambda ()
              (setq calendar-holidays
                    '((holiday-sexp calendar-daylight-savings-starts
                                    (format "Daylight Saving Time Begins %s"
                                            (solar-time-string
                                             (/ calendar-daylight-savings-starts-time
                                                (float 60))
                                             calendar-standard-time-zone-name)))
                      (holiday-sexp calendar-daylight-savings-ends
                                    (format "Daylight Saving Time Ends %s"
                                            (solar-time-string
                                             (/ calendar-daylight-savings-ends-time
                                                (float 60))
                                             calendar-daylight-time-zone-name))))
                    )))

  (custom-set-faces
   '(org-agenda-dimmed-todo-face ((t (:inverse-video nil :box nil :weight normal))))
   )
  ;; Icons for org-link-beautify
  (use-package all-the-icons
    :if (or (display-graphic-p) (server-mode)))

  (use-package org-link-beautify
    :after (org all-the-icons)
    :init
    (eval-after-load "org-link-beautify"
      '(defun org-link-beautify--display-icon (start end description icon)
         "Display ICON for link on START and END with DESCRIPTION."
         (put-text-property
          start end
          'display
          (propertize
           (concat
            (propertize "[" 'face 'org-link-beautify-link-decorator-face)
            (propertize description 'face 'org-link-beautify-link-description-face)
            (propertize "]" 'face 'org-link-beautify-link-decorator-face)
            (propertize "⟨" 'face 'org-link-beautify-link-decorator-face)
            (propertize icon)
            (propertize "⟩" 'face 'org-link-beautify-link-decorator-face))))))
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

    (defun add-icons (orig-fun &rest args)
      "Add icons to links"
      (cond
       ((string= "link-handler" orig-fun) (nerd-icons-faicon "nf-fa-link"))
       ((string= "roam" orig-fun) (nerd-icons-mdicon "nf-md-text_search"))
       ((string= "gls" orig-fun) (nerd-icons-mdicon "nf-md-book_search"))
       ))

    (advice-add 'org-link-beautify--return-icon :before-until #'add-icons)
    )

  (use-package org-download
    :after org
    :config (setq-default org-download-image-dir (concat notes-dir "/images/drag_drop"))
    )

  (defun my/open-super-agenda ()
    (interactive) (org-agenda nil "a") (delete-other-windows))

  (add-hook 'emacs-startup-hook 'my/open-super-agenda)

  (set-face-attribute 'org-agenda-clocking nil :background "light gray" :box '(:color "light gray"))
  '(org-document-info ((t (:foreground "dark orange"))))

  (defun truncate-string-with-ellipsis (string length)
    "Truncate STRING to a maximum of LENGTH characters, appending '…' if truncated."
    (if (> (length string) length)
        (concat (substring string 0 (max 0 (- length 3))) "…")
      string))
  (defun get-top-heading-in-block ()
    "Get the title of the top-level heading in the current block."
    (interactive)

    ;; Check if the current line is a heading (in agenda, empty lines (e.g. 8:00 ----) needs to be skipped)
    ;; org-agenda-clock has org-at-heading-p set to nil, so check if the command is used
    (if (or (org-at-heading-p) (string-match  "org-agenda-clock-.*" (format "%s" this-command)))
        (save-excursion
          ;; Get the top most heading
          (while (org-up-heading-safe))
          (let ((categroy (org-entry-get nil "CATEGORY"))
                (file-name (file-name-sans-extension (buffer-name)))
                (org-heading-title (truncate-string-with-ellipsis (org-get-heading t t) 18))
                )
            (if (and (not (member category `("" ,file-name)))) category (if org-heading-title org-heading-title "")
                ))) "")
    )
  (setq org-agenda-current-time-string "---*> now <*---"
        org-agenda-time-grid '((weekly today require-timed)
                               (800 1000 1200 1400 1600 1800 2000)
                               "…  " "---------------")
        org-agenda-prefix-format '((agenda . " %-16:(get-top-heading-in-block)%12t  %s %-4e")
                                   (todo . " %-18:(get-top-heading-in-block)  %-4e ")
                                   (tags . " %-12:(get-top-heading-in-block) %-6e")
                                   (search . " %-12:(get-top-heading-in-block) %-6e")))

  ;; Save org buffers after quiting agenda mode
  (advice-add 'org-agenda-quit :before #'(lambda () (interactive) (let ((inhibit-message t)) (org-save-all-org-buffers))))

  ;; Don't show logs at startup
  (setq org-agenda-start-with-log-mode nil)
  ;; Don't show repeated tasks in the future
  (setq org-agenda-show-future-repeats nil)
  ;; Show closed items, not the clocked ones
  (setq org-agenda-log-mode-items '(closed clock))
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  ;; Show today's clocked report
  (setq org-clock-clocktable-default-properties '(:maxlevel 2 :narrow 40! :link t :sort (5 . ?t) :fileskip0 t :stepskip0 t :scope agenda :block today :properties ("Effort")))
  ;; Persist clock history on Emacs close
  (org-clock-persistence-insinuate)
  (setq org-clock-persist 'clock)
  (setq org-effort-durations '(("m" . 1) ("h" . 60) ("d" . 1440) ("w" . 10080) ("mon" . 43200) ("y" . 525960.0)))
  (setq org-global-properties
        '(("COLUMNS". "%50ITEM(Task) %2PRIORITY %5Effort(Effort){:} %5CLOCKSUM(Spent)")
          ))

  ;; Convert effort to HH:MM format
  (defun org-normalize-effort ()
    "Convert effort to HH:MM format in Org mode without causing recursion."
    (when (org-entry-get nil "Effort")
      (let ((inhibit-org-property-changed-functions t)
            (effort (org-duration-from-minutes
                     (org-duration-to-minutes (org-entry-get nil "Effort")))))
        (org-entry-put nil "Effort" effort))))

  ;; For repeated tasks having ADJUST_TO_WEEKEND property
  ;; adjust the repeated date to a weekend
  (defun my/org-adjust-to-weekend (date)
    "Adjust DATE to the next weekend if it falls on a weekday."
    (let* ((day (calendar-day-of-week (calendar-gregorian-from-absolute (org-time-string-to-absolute date)))))
      (cond
       ;; If Monday to Friday, shift to Saturday
       ((and (>= day 1) (<= day 5))
        (format-time-string "<%Y-%m-%d %a>" (time-add (org-time-string-to-time date) (days-to-time (- 6 day)))))
       ;; If Saturday or Sunday, leave as is
       (t date))))

  (defun my/org-adjust-repeater-to-weekend ()
    "When a repeater updates, adjust it to the next weekend if it falls on a weekday."
    (when (org-entry-get nil "ADJUST_TO_WEEKEND")
      (when (and (org-entry-get nil "LAST_REPEAT"))
        (let* ((scheduled (org-entry-get nil "SCHEDULED"))
               (deadline (org-entry-get nil "DEADLINE")))
          (when scheduled
            (org-entry-put nil "SCHEDULED" (my/org-adjust-to-weekend scheduled)))
          (when deadline
            (org-entry-put nil "DEADLINE" (my/org-adjust-to-weekend deadline)))))))

  (add-hook 'org-todo-repeat-hook #'my/org-adjust-repeater-to-weekend)

  (defvar inhibit-org-property-changed-functions nil
    "Prevent org-property-changed-functions from being triggered.")

  (defun org-effort-normalization-hook (property _value)
    "Normalize effort format when the Effort property is modified."
    (when (and (string= property "Effort")
               (not inhibit-org-property-changed-functions))
      (org-normalize-effort)))

  (add-hook 'org-property-changed-functions 'org-effort-normalization-hook)

  ;; Remove space before header
  (setq org-super-agenda-header-prefix "")

  ;; Agenda setup
  ;; Date for one week from today; used for deadline
  (setq one-week-from-today (format-time-string "%Y-%m-%d" (org-read-date nil t "+1w")))
  ;; Move habit bar
  (setq org-habit-graph-column 60)
  (setq org-agenda-custom-commands
        '(("a" "Default agenda"
           (
            (agenda ""
                    ((org-agenda-span 'day)
                     (org-agenda-prefix-format '((agenda . " %?-20:(get-top-heading-in-block)%?-12t%?-s %4e ")))
                     (org-super-agenda-groups
                      '((:name none
                               :time-grid t
                               :scheduled today
                               :scheduled past)
                        (:discard (:anything))
                        ))))
            (alltodo ""
                     (
                      (org-agenda-overriding-header "")
                      (org-super-agenda-groups
                       '(
                         (:discard (:scheduled today))
                         (:name "Tasks" :todo "NEXT")
                         (:discard (:anything))
                         ))))
            (agenda nil
                    (
                     (org-agenda-span 'day)
                     (org-super-agenda-header-separator "")
                     (org-agenda-format-date "")
                     (org-deadline-warning-days 7)
                     (org-agenda-prefix-format '((agenda . " %-20(get-top-heading-in-block)%s %4e ")))
                     (org-agenda-deadline-leaders '("Deadline: " "(%1dd.)" "%2d d. ago: "))
                     (org-super-agenda-groups
                      `((:discard (:log closed))
                        (:discard (:scheduled t))
                        (:name "Deadline"
                               :deadline (before ,one-week-from-today))
                        (:discard (:anything))
                        ))))
            (alltodo ""
                     (
                      (org-agenda-overriding-header "")
                      (org-super-agenda-groups
                       '(
                         (:name "Blocked" :todo "BLOCKED")
                         (:name "High priority"
                                :priority "A")
                         (:name "Capture"
                                :file-path ".*capture.org")
                         (:discard (:anything))
                         ))))
            ))
          ("o" "Others"
           ((alltodo ""
                     (
                      (org-agenda-prefix-format '((todo . " %-5e")))
                      (org-agenda-overriding-header "")
                      (org-super-agenda-header-separator "")
                      (org-super-agenda-groups
                       '(
                         (:name "Capture"
                                :file-path ".*capture.org")
                         (:name "\nTasks"
                                :file-path ".*/tasks.org")
                         (:discard (:anything))
                         ))))
            (todo "DONE" ((org-agenda-overriding-header "\nCompleted")))
            (todo "CANCELED" ((org-agenda-overriding-header "\nCanceled")))
            ))
          ("c" "Calendar"
           (
            (agenda ""
                    ((org-agenda-prefix-format '((agenda . " %-16c%?-12t%s %4e "))))
                    )))
          ))

  (use-package org-super-agenda
    ;; Should be loaded at the start
    :init (org-super-agenda-mode)
    :config
    ;; Open parent element when switching from agenda
    (advice-add 'org-agenda-switch-to :after
                #'(lambda ()
                    (bookmark-set "agenda")
                    (org-up-element)
                    (org-up-element)
                    (evil-toggle-fold)
                    (evil-toggle-fold)
                    (bookmark-jump "agenda")
                    ))

    ;; Truncate lines in agenda buffer
    (add-hook 'org-agenda-finalize-hook (lambda () (interactive) (let ((inhibit-message t)) (setq truncate-lines t))))

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
        `(("d" "default" entry (file ,(concat notes-dir "/capture.org"))
           "* TODO %?\n")))

  ;; Clocking
  (setq ;; Resume when clocking into task with open clock
   org-clock-in-resume t
   ;; Remove log if task was clocked for 0:00 (accidental clocking)
   org-clock-out-remove-zero-time-clocks t
   ;; The default value (5) is too conservative.
   org-clock-history-length 20
   ;; Only today's clocked time is considered
   org-clock-mode-line-total 'today
   )

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

  (defun my/org-clock-out-done ()
    "Clock out and switch state of task to done"
    (interactive)
    (setq org-clock-out-switch-to-state "DONE")
    (org-clock-out)
    (setq org-clock-out-switch-to-state "TODO"))

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
   '(org-code ((t (:background "#e0e0e0" :foreground "#000000"))))
   '(org-verbatim ((t (:background "#e0e0e0" :foreground "#000000"))))
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
    :straight nil
    :ensure nil
    :after org
    :init
    ;; Hack to make sure that preambles are added once only
    ;; Init and emacs startup hooks doesn't work while using daemons
    (unless (boundp 'preambles-flag)
      (setq org-format-latex-header (concat org-format-latex-header "\n\\input{$HOME/.config/latex/preamble.tex}\n"))
      )
    (setq preambles-flag t)
    :config
    (add-to-list 'org-structure-template-alist '("shell" . "src sh"))
    (add-to-list 'org-structure-template-alist '("bash" . "src bash"))
    (add-to-list 'org-structure-template-alist '("py" . "src python"))
    (add-to-list 'org-structure-template-alist '("scala" . "src scala"))
    (add-to-list 'org-structure-template-alist '("markdown" . "src markdown"))
    (add-to-list 'org-structure-template-alist '("sql" . "src sql"))
    (add-to-list 'org-structure-template-alist '("elisp" . "src emacs-lisp"))
    (add-to-list 'org-structure-template-alist '("lua" . "src lua"))
    )

  ;; Go in the block with insert mode after inserting it
  (advice-add 'org-insert-structure-template :after #'(lambda (orig-fun &rest args) (newline) (evil-previous-line)))

  ;; To export to markdown
  (require 'ox-md)

  (defun my/org-to-clipboard-as-markdown ()
    "Export to clipboard as markdown"
    (interactive)
    (let ((org-export-with-toc nil))
      (with-current-buffer (org-md-export-as-markdown)
        (clipboard-kill-region (point-min) (point-max))
        (delete-window)
        (evil-exit-visual-state))))

  ;; Exporting settings
  (setq org-export-with-broken-links t
        org-export-coding-system 'utf-8
        org-export-preserve-breaks t
        org-export-with-todo-keywords nil)

  ;; Latex image size
  (plist-put org-format-latex-options :scale 1.5)

  (use-package org-protocol
    :straight nil
    :ensure nil
    :config
    (add-to-list 'org-capture-templates
                 `("q" "org-capture-url" entry
                   (file ,(concat notes-dir "/capture.org"))
                   "* TODO [[%:link][%:description]]" :immediate-finish t))

    (add-to-list 'org-capture-templates
                 `("p" "org-capture-note" entry
                   (file+headline ,(concat notes-dir "/papers.org")  "Inbox")
                   "* TODO %:description" :immediate-finish t))

    (add-to-list 'org-capture-templates
                 `("n" "org-capture-note" entry
                   (file ,(concat notes-dir "/capture.org"))
                   "* TODO %:description" :immediate-finish t))
    (add-to-list 'org-capture-templates
                 `("w" "Web site" entry
                   (file ,(concat notes-dir "/org_protocol_html.org")) ;
                   "* %a :website:\n\n%U %?\n\n%:initial"))

    (setq org-protocol-default-template-key "q")
    )
  (use-package org-protocol-capture-html
    :ensure nil)

  (defun my/org-open-all-links-in-subtree ()
    "Open all the links in the current subtree.
Note: this uses Org's internal variable `org-link--search-failed'."
    (interactive)
    (save-excursion
      (save-restriction
        (org-narrow-to-subtree)
        (goto-char (point-min))
        (let ((inhibit-message t)
              (message-log-max nil))
          (setq org-link--search-failed nil)
          (while (progn (org-next-link)
                        (not org-link--search-failed))
            (org-open-at-point))))))
  )

(use-package citar
  :no-require
  :after all-the-icons
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  ;; Open files using external program
  (citar-file-open-functions (list (cons t 'citar-file-open-external)))
  :hook
  ((org-mode . (lambda ()
                 (cursor-sensor-mode 1)
                 (org-cite-csl-activate-render-all)
                 (citar-capf-setup)
                 ))
   ;; Breaks org mode
   ;; (LaTeX-mode . citar-capf-setup)
   )
  :config

  ;; Activation processor for Org to show links as a CSL style
  (require 'oc-csl-activate)
  (setq org-cite-activate-processor 'csl-activate)
  (setq org-cite-csl-activate-use-citar-cache t)
  (setq org-cite-csl--fallback-style-file "~/.config/Zotero/styles/chicago-manual-of-style-17th-edition-edited-title.csl")

  ;; Citar icons
  (defvar citar-indicator-files-icons
    (citar-indicator-create
     :symbol (all-the-icons-faicon
              "file-pdf-o"
              :face 'all-the-icons-green
              :v-adjust -0.05)
     :function #'citar-has-files
     :padding " " ; need this because the default padding is too low for these icons
     :tag "has:files"))

  (defvar citar-indicator-links-icons
    (citar-indicator-create
     :symbol (all-the-icons-faicon
              "link"
              :face 'all-the-icons-orange
              :height 0.8
              :v-adjust 0)
     :function #'citar-has-links
     :padding " "
     :tag "has:links"))

  (defvar citar-indicator-notes-icons
    (citar-indicator-create
     :symbol (all-the-icons-faicon
              "sticky-note-o"
              :face 'all-the-icons-blue
              :height 0.95
              :v-adjust -0.01)
     :function #'citar-has-notes
     :padding " "
     :tag "has:notes"))

  (defvar citar-indicator-cited-icons
    (citar-indicator-create
     :symbol (all-the-icons-faicon
              "circle-o"
              :v-adjust -0.01
              :height 0.9
              :face 'all-the-icons-green)
     :function #'citar-is-cited
     :padding " "
     :tag "is:cited"))

  (setq citar-indicators
        (list citar-indicator-files-icons
              citar-indicator-links-icons
              citar-indicator-notes-icons
              citar-indicator-cited-icons))
  )

(use-package citar-embark
  :after (citar embark)
  :no-require
  :custom
  (citar-at-point-function 'embark-act)
  :config
  (citar-embark-mode)

  (defun my/focus-zotero-entry (cite-key)
    (citar-open-entry-in-zotero cite-key)
    (start-process-shell-command "" nil (concat "i3-msg '[class=\"Zotero\"] focus'"))
    )

  (defvar-keymap embark-citar-actions
    :doc "Keymap for actions for tab-bar tabs (when mentioned by name)."
    :parent citar-embark-map
    "z" #'my/focus-zotero-entry)

  (add-to-list 'embark-keymap-alist '(citar-citation . embark-citar-actions))
  )

(use-package citar-org-roam
  :after (citar org-roam)
  :custom
  (citar-org-roam-subdir "references")
  :config (citar-org-roam-mode))

;; Show emphasis markers when hovering over text
(use-package org-appear
  :after (org evil)
  :custom
  ;; toggle links
  (org-appear-autolinks t)
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
                              ;; <S-TAB>
                              (kbd "<backtab>") 'org-fold-hide-subtree
                              (kbd "<C-tab>") 'org-shifttab
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
  (setq org-agenda-files (concat notes-dir "/agenda_files"))
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
  (org-roam-directory notes-dir)
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

  (use-package consult-org-roam
    :ensure t
    :after (org-roam consult)
    :init
    (require 'consult-org-roam)
    :custom
    ;; Use `ripgrep' for searching with `consult-org-roam-search'
    (consult-org-roam-grep-func #'consult-ripgrep)
    ;; Configure a custom narrow key for `consult-buffer'
    (consult-org-roam-buffer-narrow-key ?r)
    ;; Display org-roam buffers right after non-org-roam buffers
    ;; in consult-buffer (and not down at the bottom)
    (consult-org-roam-buffer-after-buffers t)
    :config
    (consult-customize
     consult-org-roam-forward-links
     :preview-key "M-.")
    )
  )

(use-package websocket
  :after org-roam)
(use-package simple-httpd
  :after org-roam)

(use-package org-roam-ui
  :after org-roam
  ;; :straight (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t)
  )

;; ORG NOTIFICATION
(use-package appt
  :straight nil
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

  ;; Generate the appt list from org agenda files on emacs launch and every 15 minutes
  (run-at-time nil 900 'org-agenda-to-appt-clear-message))
;; }}}
;; Navigation {{{
(use-package vertico
  :init (vertico-mode)
  :custom
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
  :bind (("C-." . embark-act)
         ("C-SPC". embark-select))
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
    "v" #'my/org-roam-node-find-window-v
    "w" #'my/org-roam-node-find-other-frame
    )

  (add-to-list 'embark-keymap-alist '(org-roam-node . embark-org-roam-nodes-actions))

  (defun my/org-roam-node-find-window-v ()
    (interactive)
    (org-roam-node-find t))

  (defun my/org-roam-node-find-other-frame ()
    (interactive)
    (clone-frame)
    (org-roam-node-find))

  (defun my/org-roam-node-find-window-x ()
    (interactive)
    (evil-window-split)
    (org-roam-node-find))

  ;; Buffers
  (defvar-keymap embark-buffer-actions
    :doc "Keymap for actions for buffers"
    :parent embark-buffer-map
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
    :parent embark-file-map
    "x" #'my/switch-to-file-other-window-x
    "v" #'my/switch-to-file-other-window-v
    "w" #'my/switch-to-file-other-frame)
  (add-to-list 'embark-keymap-alist '(file . embark-file-actions))

  (defun my/switch-to-file-other-window-x (file)
    (interactive "bFile: ")
    (split-window-below) (windmove-down)
    (find-file file))

  (defun my/switch-to-file-other-window-v (file)
    (interactive "bFile: ")
    (split-window-right) (windmove-right)
    (find-file file))

  (defun my/switch-to-file-other-frame (file)
    (interactive "bFile: ")
    (clone-frame)
    (find-file file))

  ;; Override the function by inserting link with title
  (defun embark-org-insert-link-to (target)
    "Insert a link to the TARGET in the source window.
If TARGET is an agenda item and `other-window-for-scrolling' is
displaying an org mode buffer, then that is the source window.
If TARGET is a minibuffer completion candidate, then the source
window is the window selected before the command that opened the
minibuffer ran."
    (embark-org--in-source-window target
                                  (lambda (marker)
                                    (org-with-point-at marker (org-store-link nil t))
                                    (my/org-insert-last-stored-link-with-title ""))))
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

  (setq my/leader-key "SPC")
  (setq my/leader-key-insert (concat "M-" my/leader-key))
  (setq my/local-leader-key ",")
  (setq my/local-leader-key-insert (concat "M-" my/local-leader-key))

  (general-define-key
   :states '(normal visual motion)
   :keymaps 'override
   my/leader-key 'main-hydra/body)

  (general-define-key
   :states 'insert
   :keymaps 'global
   my/leader-key-insert 'main-hydra/body)

  (add-hook 'org-mode-hook
            #'(lambda ()
                (general-define-key
                 :states '(normal visual motion)
                 :keymaps 'local
                 my/local-leader-key 'org-hydra/body)
                (general-define-key
                 :states 'insert
                 :keymaps 'local
                 my/local-leader-key-insert 'org-hydra/body))
            )

  (add-hook 'org-agenda-mode-hook
            #'(lambda ()
                (general-define-key
                 :states '(normal visual motion)
                 :keymaps 'local
                 my/local-leader-key 'agenda-hydra/body))
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

(defun get-file-paths-for-open-buffers ()
  "Return a list of file paths for all open file buffers."
  (interactive)
  (let ((file-paths '()))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (buffer-file-name) (file-readable-p (buffer-file-name)))
          (push (buffer-file-name) file-paths))))
    file-paths))

(defun my/org-columns-buffer ()
  (interactive)
  (let ((current-prefix-arg 4)) (call-interactively 'org-columns))
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
  (" x" my/scratch-buffer-other "scratch")
  (" h" evil-ex-nohighlight "highlight")
  (" f" find-hydra/body "find")
  (" e" toggle-plugins/body "toggle plugins")
  (" g" git-hydra/body "git")
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
  (" b" consult-buffer "Buffer")
  (" f" consult-find "File")
  (" t" consult-line "Text in buffer")
  (" T" consult-ripgrep "Text in directory")
  )

;; Git diff, git blame, reset buffer can be added using magit
(defhydra git-hydra (:exit t :hint nil :idle 1)
  (" r" git-gutter:revert-hunk "revert hunk" :column " hunk")
  (" s" git-gutter:stage-hunk "stage hunk")
  (" p" git-gutter:popup-hunk "preview hunk")
  (" k" git-gutter:previous-hunk "previous hunk")
  (" j" git-gutter:next-hunk  "next hunk")
  (" S" magit-stage-buffer-file "stage buffer" :column " buffer")
  (" R" vc-revert "revert file" :column " hunk")
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
(defhydra org-hydra (:hint nil :exit t :idle 1 )
  (" *" org-ctrl-c-star "make header" :column " org")
  (" -" org-ctrl-c-minus "make item")
  (" c" org-clock-hydra/body "clock")
  (" x" (lambda () (interactive) (org-capture nil "d")) "capture")
  (" r" org-roam-hydra/body "org-roam")
  (" f" org-find-hydra/body "find")
  (" R" org-refile-hydra/body "refile")
  (" l" org-links-hydra/body "links")
  (" a" org-agenda "agenda")
  (" A" (lambda() (interactive) (org-agenda nil "a")) "default agenda")
  (" p" org-set-property "set property")
  (" g" org-goto-hydra/body "goto hydra")
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
  (" v" my/org-columns-buffer  "column view")
  )

(defhydra org-goto-hydra (:exit t :idle 1)
  (" g" consult-org-heading "file" :column "headers")
  (" G" (consult-org-heading t (get-file-paths-for-open-buffers)) "buffers")
  (" a" consult-org-agenda "agenda")
  (" r" org-refile-goto-last-stored "last refiled" :column "files")
  (" c" (find-file (concat notes-dir "/capture.org")) "capture.org")
  (" i" (find-file (concat notes-dir "/ideas.org")) "ideas.org")
  (" p" (find-file (concat notes-dir "/projects.org")) "projects.org")
  (" t" (find-file (concat notes-dir "/tasks.org")) "tasks.org")
  )

(defun node-not-bib-reference-note (file)
  "Return FILE if it does not exist in DIR, otherwise return nil."
  (message (file-name-nondirectory (org-roam-node-file file)))
  (let ((dir-files (directory-files "~/notes/references" nil directory-files-no-dot-files-regexp)))
    (if (member (file-name-nondirectory (org-roam-node-file file)) dir-files)
        nil
      file)))

(defhydra org-find-hydra (:exit t :idle 1)
  (" f" (lambda () (interactive) (let ((inhibit-message t)) (org-roam-node-find nil "" 'node-not-bib-reference-note))) "find node")
  (" B" org-cite-insert "BibTex")
  )

(defhydra org-roam-hydra (:exit t :idle 1)
  (" i" org-roam-node-insert                 "insert node" :column " node")
  (" r" consult-org-roam-backlinks           "links to here")
  (" R" consult-org-roam-backlinks-recursive "links to here recursive")
  (" f" consult-org-roam-forward-links       "links from this node")
  (" g" org-roam-ui-mode                     "graph" :column " ")
  (" a" org-roam-alias-add                   "add alias")
  (" s" org-roam-db-sync                     "sync")
  )

(defhydra org-refile-hydra (:exit t :idle 1)
  (" ." my/org-refile-to-current-file "to current file" :column " refile")
  (" c" my/org-refile-to-running-clock "to clocked")
  (" t" (org-refile nil nil (list nil (concat notes-dir "/tasks.org"))) "to tasks.org")
  (" i" (org-refile nil nil (list nil (concat notes-dir "/ideas.org"))) "to ideas.org")
  (" p" (org-refile nil nil (list nil (concat notes-dir "/projects.org"))) "to projects.org")
  (" a" org-refile "to agenda/buffers")
  (" g" org-refile-goto-last-stored "goto refile")
  )

(defhydra org-links-hydra (:exit t :idle 1)
  (" y" org-store-link "yank" :column " links")
  (" e" org-insert-link "edit" :column " links") ; Edit link if it exists at point
  (" t" (lambda () (interactive) (call-interactively #'org-link-beautify-mode) (org-toggle-link-display)) "toggle")
  (" p" my/org-insert-last-stored-link-with-title "paste last")
  )

;; Agenda
(defhydra agenda-hydra (:exit t :idle 1)
  (" c" agenda-clock-hydra/body "clock" :column " org")
  (" g" org-goto-hydra/body "goto hydra")
  (" r" org-roam-hydra/body "org-roam")
  (" R" org-refile-hydra/body "refile")
  (" c" agenda-clock-hydra/body "clock" :column " agenda")
  (" v" agenda-view-hydra/body "view")
  (" l" org-agenda-log-mode "log")
  (" f"	org-agenda-follow-mode "follow")
  )

(defhydra agenda-clock-hydra (:exit t :idle 1)
  (" c" org-agenda-clock-cancel "cancel")
  (" g" org-agenda-clock-goto "goto")
  (" i" org-agenda-clock-in "in")
  (" o" org-agenda-clock-out "out")
  (" v" org-agenda-columns "column view")
  (" r" org-agenda-clockreport-mode "report"))

(defhydra agenda-view-hydra (:exit t :idle 1)
  (" d"	org-agenda-day-view "day")
  (" w"	org-agenda-week-view "week")
  (" m"	org-agenda-month-view "month")
  (" y"	org-agenda-year-view "year"))
;;;; }}}
;;;; {{{ Programming
;; LSP-mode
(use-package lsp-mode
  :init
  ;; (setenv "LSP_USE_PLISTS" "true")
  (setq lsp-keymap-prefix "C-c l")
  :custom
  ;; (lsp-use-plist t)
  (read-process-output-max (* 1024 1024))
  ;; (lsp-auto-guess-root t)
  (lsp-log-io nil)
  ;; (lsp-restart 'auto-restart)
  ;; (lsp-enable-symbol-highlighting nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-signature-auto-activate nil)
  (lsp-prefer-capf t)
  ;; (lsp-signature-render-documentation nil)
  ;; (lsp-eldoc-hook nil)
  ;; (lsp-modeline-code-actions-enable nil)
  ;; (lsp-modeline-diagnostics-enable nil)
  (lsp-headerline-breadcrumb-enable nil)
  ;; (lsp-semantic-tokens-enable nil)
  (lsp-enable-folding nil)
  ;; (lsp-enable-imenu nil)
  (lsp-enable-snippet t)
  ;; (read-process-output-max (* 1024 1024)) ;; 1MB
  ;; (lsp-idle-delay 0.5)
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  :hook ((python-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  ;; (setq lsp-enable-file-watchers nil)

  (setq lsp-completion-provider :none)
  (defun corfu-lsp-setup ()
    (setq-local completion-styles '(orderless)
                completion-category-defaults nil))
  (add-hook 'lsp-mode-hook #'corfu-lsp-setup)
  )

(use-package lsp-ui :commands lsp-ui-mode)
;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol)

;; Python
(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)))
(use-package lsp-pyright
  :ensure t
  :hook (python-ts-mode . (lambda ()
                            (require 'lsp-pyright)
                            (lsp)))
  :config
  ;; (setq lsp-pyright-diagnostic-mode "workspace")
  )

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
;;;; }}}
(use-package elfeed
  :custom
  (elfeed-search-title-max-width 80)
  (elfeed-search-remain-on-entry t)
  ;; :hook
  :config

  (defun my/elfeed-entry-capture-show ()
    (interactive)
    (let ((entry (elfeed-search-selected :single)))
      (start-process-shell-command "" nil (concat "org_capture '" (elfeed-entry-link entry) "' '" (elfeed-entry-title entry)"'")) (elfeed-search-show-entry entry)))

  (evil-collection-define-key 'normal 'elfeed-search-mode-map (kbd "r") 'elfeed-search-clear-filter)
  (evil-collection-define-key 'normal 'elfeed-search-mode-map (kbd "C")  'my/elfeed-entry-capture-show)
  (evil-collection-define-key 'normal 'elfeed-search-mode-map (kbd "M-p")  'scroll-other-window-down)
  (evil-collection-define-key 'normal 'elfeed-search-mode-map (kbd "M-n")  'scroll-other-window)

  (evil-collection-define-key 'normal 'elfeed-search-mode-map (kbd "gj")  'elfeed-goodies/split-show-next)
  (evil-collection-define-key 'normal 'elfeed-search-mode-map (kbd "gk")  'elfeed-goodies/split-show-prev)

  (evil-collection-define-key 'normal 'elfeed-search-mode-map (kbd "o")  'elfeed-search-browse-url)
  (evil-collection-define-key 'normal 'elfeed-search-mode-map (kbd "A") '(lambda () (interactive) (mark-whole-buffer) (elfeed-search-untag-all-unread)))
  (evil-collection-define-key 'normal 'elfeed-search-mode-map (kbd "q")  'save-buffers-kill-terminal)

  (defun elfeed-format-relative-time (timestamp)
    "Format TIMESTAMP as a relative time string."
    (let* ((now (float-time (current-time)))       ;; Current time in seconds
           (elapsed-seconds (- now timestamp))     ;; Elapsed time in seconds
           (days (/ elapsed-seconds 86400))       ;; Days in elapsed time
           (hours (/ elapsed-seconds 3600))       ;; Hours in elapsed time
           (minutes (/ elapsed-seconds 60)))      ;; Minutes in elapsed time
      (cond ((>= days 1) (format "%dd" days (if (= days 1) "")))
            ((>= hours 1) (format "%dh" hours (if (= hours 1) "")))
            ((>= minutes 1) (format "%dm" minutes (if (= minutes 1) "")))
            (t "<1m"))))

  (set-face-attribute 'elfeed-search-date-face nil :foreground "blue1")

  (defface elfeed-search-authors-face
    '((((class color) (background light)) (:foreground "gold4"))
      (((class color) (background dark))  (:foreground "gold4")))
    "Face used in search mode for authors entry titles."
    :group 'elfeed)
  (setq elfeed-search-face-alist '((unread elfeed-search-unread-title-face) (self_host elfeed-test)))
  (defun concatenate-authors (authors-list)
    "Given AUTHORS-LIST, list of plists; return string of all authors
concatenated."
    (mapconcat
     (lambda (author) (plist-get author :name))
     authors-list ", "))
  (defun my/elfeed-search-print (entry)
    "Print ENTRY to the buffer."
    (let* ((relative-time (elfeed-format-relative-time (elfeed-entry-date entry)))
           (entry-title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
           (feed (elfeed-entry-feed entry))
           (feed-title
            (when feed
              (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
           (points (elfeed-meta entry :points))
           (comments (elfeed-meta entry :comments))
           (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
           (authors (concatenate-authors
                     (elfeed-meta entry :authors)))
           (metadata (cond
                      ((and points comments) (format "%4d  %4d 󰻞  " points comments))
                      (points (format "%4d   " points))
                      (comments (format "%4d 󰻞  " comments))
                      (t "")))
           (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
           (tags-str (concat "("(mapconcat
                                 (lambda (s) (propertize s 'face 'elfeed-search-tag-face))
                                 tags ",") ")"))
           (date-column (elfeed-format-column
                         relative-time (elfeed-clamp 2 4 4) :left))
           (title-width (- (window-width) 10 elfeed-search-trailing-width))
           (entry-title-column (elfeed-format-column
                                entry-title (elfeed-clamp elfeed-search-title-min-width title-width 90) :left))
           (tag-column (elfeed-format-column
                        tags-str (elfeed-clamp 18 (length tags-str) 30) :left))
           (metadata-column (elfeed-format-column
                             metadata (elfeed-clamp 15 15 15) :left))
           (authors-column (elfeed-format-column
                            authors (elfeed-clamp 16 (length authors) 40) :left))
           (feed-title-column (elfeed-format-column
                               feed-title (elfeed-clamp 16 (length feed-title) 20) :left))
           )
      (insert (propertize date-column 'face 'elfeed-search-date-face) " ")
      (insert metadata-column)
      (insert (propertize feed-title-column 'face 'elfeed-search-feed-face) " ")
      (insert (propertize tag-column 'face 'elfeed-search-tag-face) " ")
      (insert (propertize entry-title-column 'face title-faces 'kbd-help entry-title) "  ")
      (insert (propertize authors-column 'face 'elfeed-search-authors-face 'kbd-help authors))
      ))

  (setq elfeed-search-print-entry-function #'my/elfeed-search-print)

  (defun my-elfeed-tag-sort (a b)
    "Sort on feed title then date"
    (let* ((a-title (elfeed-feed-title (elfeed-entry-feed a)))
           (b-title (elfeed-feed-title (elfeed-entry-feed b))))
      (if (string= a-title b-title)
          (< (elfeed-entry-date b) (elfeed-entry-date a)))
      (string< a-title b-title)))

  (setf elfeed-search-sort-function #'my-elfeed-tag-sort)
  )

;; NOTE: Doesn't work inside use-package
(defun my/elfeed-parse-metadata (_type entry db-entry)
  "Extract Points and Comments from Hacker News entry description."
  (let* ((feed-url (elfeed-feed-url (elfeed-entry-feed db-entry)))
         (content-ref (elfeed-entry-content db-entry))
         (content (elfeed-deref content-ref))
         (points nil)
         (comments nil))
    (cond ((string= feed-url  "https://hnrss.org/frontpage")
           (setq points (when (string-match "Points: \\([0-9]+\\)" content)
                          (string-to-number (match-string 1 content)))
                 comments (when (string-match "# Comments: \\([0-9]+\\)" content)
                            (string-to-number (match-string 1 content))))
           (setf (elfeed-meta (elfeed-entry-feed db-entry) :title) "Hacker News"))
          ((string-match-p "lemmy" feed-url)
           (setq points (when (string-match "\\([0-9]+\\) points" content)
                          (string-to-number (match-string 1 content)))
                 comments (when (string-match "\\([0-9]+\\) comments" content)
                            (string-to-number (match-string 1 content))))))
    (when points
      (setf (elfeed-meta db-entry :points) points))
    (when comments
      (setf (elfeed-meta db-entry :comments) comments))
    ))
(add-hook 'elfeed-new-entry-parse-hook #'my/elfeed-parse-metadata)

(use-package elfeed-org
  :after elfeed
  :custom (rmh-elfeed-org-files (list (concat notes-dir "feeds.org")))
  :config (elfeed-org)
  )

(use-package elfeed-goodies
  :after elfeed
  :custom
  (elfeed-goodies/entry-pane-position 'bottom)
  (elfeed-goodies/switch-to-entry nil)
  (elfeed-goodies/feed-source-column-width 50)
  (elfeed-goodies/tag-column-width 50)
  (elfeed-show-entry-switch #'elfeed-goodies/switch-pane)
  (elfeed-show-entry-delete #'elfeed-goodies/delete-pane)
  :config
  (require 'elfeed-goodies)

  (setq elfeed-search-print-entry-function #'my/elfeed-search-print)
  )

(use-package link-hint
  :ensure t
  :bind
  ("C-c l o" . link-hint-open-link)
  ("C-c l c" . link-hint-copy-link))
