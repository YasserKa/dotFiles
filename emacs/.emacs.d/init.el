;; Initialize package sources
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("nognu" . "https://elpa.nongnu.org/nongnu/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Speed up Emacs startup by increasing garbage collection threshold during it
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

;; Load Emacs Lisp packages, and activate them (Needed if emacs needs to be used in the command line)
(package-initialize)

;; Evaluate at compile time
(eval-when-compile (require 'use-package))

;; Ensure that all packages are installed
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Don't make package-selected-packages to be created
(defun package--save-selected-packages (&rest opt) nil)
;; Place Emacs generated variables somewhere else, and don't load them
(setq custom-file (concat user-emacs-directory "/custom.el"))

;; Use y/n for yes/no prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; Saves files such as undo tree and auto saves in .emacs.d
(use-package no-littering
  :custom
  (auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; Unfolding an item with emojis is slow, this package fixes this problem
(use-package emojify
  :hook (after-init . global-emojify-mode))

(use-package gruvbox-theme
  :config (load-theme 'gruvbox-light-medium t))

(add-to-list 'default-frame-alist
             '(font . "DejaVu Sans Mono-13"))

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda ()
                   (display-line-numbers-mode)
                   (setq display-line-numbers 'relative))))

;; Remove startup screen and minibuffer message
(setq inhibit-startup-message t)
(defun display-startup-echo-area-message ()
  (message nil))

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

;; Loading emacs server is needed by emacsclient
;; emacsclient used by clocking
(load "server")
(unless (server-running-p) (server-start))

;; Exit emacs without getting a prompt to kill processes
(setq confirm-kill-processes nil)

;; Trims spaces from end of line
(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

(use-package openwith
  :config
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("mpg" "mpeg" "mp3" "mp4"
                  "avi" "wmv" "wav" "mov" "flv"
                  "ogm" "ogg" "mkv")) "mpv" '(file))
         (list (openwith-make-extension-regexp
                '("pdf" "epub" "djvu")) "zathura" '(file))))
  (openwith-mode t))

(use-package which-key
  :init
  :custom (which-key-idle-delay 1)
  :config (which-key-mode))

;; Auto close pairs
(use-package elec-pair
  :ensure nil
  :hook ((org-mode .  (lambda ()
                        ;; Ignore < in org mode for yassnippets
                        (setq electric-pair-inhibit-predicate
                              (lambda (c)
                                (if (char-equal c ?\<) t (electric-pair-default-inhibit c)))))))
  :custom (electric-pair-pairs
           '((?\" . ?\")
             (?\$ . ?\$)
             (?\[ . ?\])
             (?\< . ?\>)
             (?\` . ?\`)
             (?\{ . ?\})))
  :config
  (defun electric-pair ()
    "If at end of line, insert character pair without surrounding spaces.
    Otherwise, just insert the typed character."
    (interactive)
    (if (eolp) (let (parens-require-spaces) (insert-pair)) (self-insert-command 1)))
  (electric-pair-mode 1)
  )

;; Highlight matching braces
(use-package paren
  :ensure nil
  :custom
  (show-paren-delay 0)
  :config (show-paren-mode 1)
  )

;; Remove backup files (ends with ~)
;; (setq make-backup-files nil)
;; Avoid being prompted with symbolic link to git-controlled
(setq vc-follow-symlinks t)

;; Setting it from <C-h>
(setq help-char (string-to-char "?"))

;; Adjusting text scale

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

;; Expand/contract selected region
(use-package expand-region
  :after evil
  :config
  (evil-define-key '(normal visual) 'global
    (kbd "+") 'er/expand-region
    (kbd "_") 'er/contract-region)
  )

(use-package yasnippet
  :custom
  (yas-snippet-dirs '("~/.emacs.d/snippets"))
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
  (defun my-append-company-backends ()
    (setq-local company-backends
                (append '((company-capf)) company-backends)))
  (add-hook 'org-mode #'my-append-company-backends)
  )

;; Vim leader
(use-package general
  :after evil
  :config
  (general-evil-setup t)
  ;; Create leader
  (general-create-definer my-leader-key-def
    :keymaps '(normal visual emacs)
    :prefix ",")
  (my-leader-key-def
    "ss" '(lambda () (interactive) (load-file (concat user-emacs-directory "/init.el")))
    "es" '(lambda () (interactive) (split-window-below) (find-file (concat user-emacs-directory "/init.el")))
    "h" 'evil-ex-nohighlight
    "b" 'switch-to-buffer
    "p" 'find-file)

  (general-define-key
   :states 'normal
   "\C-u" 'evil-scroll-up
   "g:" 'goto-last-change
   ))

;; Traverse file changes in git
(use-package git-timemachine)

(use-package magit
  :after (evil)
  :custom
  (evil-collection-magit-use-y-for-yank t)
  (evil-collection-magit-want-horizontal-movement t)
  ;; Update return in repo list, should be done after evil-collection
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-repository-directories '(("~/dotfiles" . 0) ("~/Projects/" . 1) ("/srv/http/cooldown" . 0)))
  :config

  (evil-define-key 'normal magit-status-mode-map
    (kbd "?") 'evil-search-backward
    (kbd "<return>") 'my-vil-diff-visit-file)

  ;; Open file using nvim
  (defun my-vil-diff-visit-file (file &optional other-window)
    (interactive (list (magit-file-at-point t t) current-prefix-arg))
    (shell-command (concat "nvim-qt " file nil)))
  )



(use-package evil
  :custom
  ;; Needed by evil-collection
  (evil-want-keybinding nil)
  ;; Use evil search module
  (evil-search-module 'evil-search)
  ;; Use <C-u> to scroll up
  (evil-want-C-u-scroll t)
  (evil-want-C-u-delete t)
  ;; Y is y$
  (evil-want-Y-yank-to-eol t)
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
  :config
  (evil-mode 1)
  (evil-define-key 'insert 'global (kbd "C-h") 'evil-delete-backward-char-and-join)

  (evil-define-key '(insert normal emacs) 'global (kbd "C-q") 'help)


  (evil-define-key nil 'global
    (kbd "<escape>") 'keyboard-escape-quit
    (kbd "C-+") 'text-scale-increase
    (kbd "C--") 'text-scale-decrease
    (kbd "C-=") '(lambda () (interactive) (let ((inhibit-message t)) (text-scale-adjust 0)))
    )


  ;; gx opens urls
  ;; (evil-define-key 'normal org-mode (kbd "gx") 'org-open-at-point)

  (require 'evil-exchange)
  ;; change default key bindings (if you want) HERE
  ;; (setq evil-exchange-key (kbd "zx"))
  (evil-exchange-install)

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

(use-package evil-commentary
  :config (evil-commentary-mode))

;; Better sentence navigation with ) & (
(use-package sentence-navigation
  :ensure t
  :defer t)

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
                                      "eu" #'undo-tree-visualize)))
         )
  :custom
  (undo-tree-visualizer-diff t)
  :config
  ;; Save undo steps between sessions
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
  :ensure evil
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
  (defun my-trigger-theme ()
    (interactive)
    (if (eq (car custom-enabled-themes) 'gruvbox-light-medium)
        (load-theme 'gruvbox-dark-soft t)
      (load-theme 'gruvbox-light-medium t)))

  (evil-collection-define-operator-key 'yank 'global-map "ob" #'my-trigger-theme)

  (dolist (map '(minibuffer-local-map
                 minibuffer-local-ns-map
                 minibuffer-local-completion-map
                 minibuffer-local-must-match-map
                 minibuffer-local-isearch-map
                 evil-ex-completion-map))
    ;; Exit minibuffer instead of going to normal mode
    (evil-collection-define-key 'insert map (kbd "<escape>") 'abort-recursive-edit)
    (evil-collection-define-key 'insert map (kbd "C-h") 'delete-backward-char)
    ;; (define-key map [?\C-h] 'delete-backward-char)
    (evil-collection-define-key 'insert map (kbd "C-q") 'help))
  )

(defun my-org-mode-setup ()
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
  :hook ((org-mode . my-org-mode-setup))
  :custom
  (org-ellipsis " ▾")
  (org-hide-emphasis-markers t)              ;; Hide symbols
  (org-startup-folded 'content)
  (org-startup-folded t)
  (org-enforce-todo-dependencies t)
  (org-cycle-separator-lines -1)             ;; No empty lines needed to fold subtrees
  (org-startup-with-inline-images t)
  (org-image-actual-width nil)
  ;; Source code indentation
  (org-src-preserve-indentation nil)
  (org-fontify-quote-and-verse-blocks t)
  (org-src-fontify-natively t)               ;; Syntax highlight in #+BEGIN_SRC blocks
  (org-src-tab-acts-natively t)
  (org-edit-src-content-indentation 0)
  ;; Exporting settings
  (org-export-with-broken-links t)
  (org-export-preserve-breaks t)
  (org-export-with-todo-keywords nil)
  (org-confirm-babel-evaluate nil)
  ;; Remove completed deadline, scheduled, completed from agenda
  ;; The time when it get closed will be shown
  (org-agenda-skip-deadline-if-done t)

  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-timestamp-if-done t)
  (org-deadline-warning-days 7)
  (org-agenda-compact-blocks t)
  (org-agenda-block-separator nil)
  (org-agenda-start-on-weekday nil)           ;; Show today +7 days
  ;; Padding
  (line-spacing 0.05)
  ;; Hide title in the header
  (org-hidden-keywords '(title))
  :config

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.25)
                  (org-level-2 . 1.15)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "DejaVu Sans Mono-13" :weight 'regular :height (cdr face)))

  (custom-set-faces
   '(org-block ((t (:inherit fixed-pitch))))
   ;; Background is nil because org-block-end-line background shows in header
   ;; Check https://github.com/doomemacs/themes/issues/453
   '(org-block-begin-line ((t (:background nil :weight bold))))
   '(org-block-end-line ((t (:background nil :weight bold))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
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
                (sequence "WAITING(w)" "HOLD(h)" "|" "CANCELLED(c@/!)"))))
  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("NEXT" :foreground "blue" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
                ("WAITING" :foreground "orange" :weight bold)
                ("HOLD" :foreground "magenta" :weight bold)
                ("CANCELLED" :foreground "forest green" :weight bold))))

  (setq org-tag-alist
        '(("productivity" . ?p)
          ("important" . ?i)))

  ;; Save Org buffers after refiling!
  (add-hook 'org-refile :after '(lambda () (interactive) (let ((inhibit-message t))
                                                             (org-save-all-org-buffers))))

  (setq org-refile-targets '((nil :maxlevel . 9) ;; Refile to current directory at any level
                             (org-agenda-files :maxlevel . 3)
                             (org-buffer-list :maxlevel . 2)))

  ;; Insert mode after going to capture
  (add-hook 'org-capture-mode-hook 'evil-insert-state)
  ;; Insert mode after adding note (note to change of state)
  (advice-add 'org-add-log-note :after 'evil-insert-state)
  (advice-add 'org-add-note :after 'evil-insert-state)

  ;; Save capture on :wq
  (evil-define-key nil org-capture-mode-map
    [remap evil-save-and-close] #'org-capture-finalize
    [remap evil-save-modified-and-close] #'org-capture-finalize
    [remap evil-quit] #'org-capture-kill)

  ;; Agenda styling
  (set-face-attribute 'org-agenda-date nil :height 1.05)
  (set-face-attribute 'org-agenda-date-today nil :height 1.05)
  (set-face-attribute 'org-agenda-date-weekend nil :height 1.05)

  ;; Get roam alias, otherwise the title of node
  (defun my-get-title-property ()
    (setq title (elt (elt (org-collect-keywords '("TITLE")) 0) 1))
    (setq roam_alias (org-entry-get-with-inheritance "ROAM_ALIASES"))
    (if roam_alias roam_alias (if title title "")))

  (setq org-agenda-current-time-string "┈┈┈┈┈┈┈┈┈┈┈ now"
        org-agenda-time-grid '((weekly today require-timed)
                               (800 1000 1200 1400 1600 1800 2000)
                               "---" "┈┈┈┈┈┈┈┈┈┈┈┈┈")
        org-agenda-prefix-format '((agenda . " %-16(my-get-title-property)%-12t%-6e% s")
                                   (todo . " %-12:(my-get-title-property) %-6e")
                                   (tags . " %-12:(my-get-title-property) %-6e")
                                   (search . " %-12:(my-get-title-property) %-6e")))

  ;; Save org buffers after quiting agenda mode
  (advice-add 'org-agenda-quit :before '(lambda () (interactive) (let ((inhibit-message t)) (org-save-all-org-buffers))))

  ;; Don't open a new window after clicking in agenda
  (evil-define-key 'motion org-agenda-mode-map (kbd "<return>") '(lambda() (interactive) (org-agenda-switch-to t)))

  ;; Log the state change
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  ;; Super agenda
  (setq org-agenda-custom-commands
        '(("z" "Super view"
           ((agenda "" ((org-agenda-span 'day) ;; Show one day
                        (org-super-agenda-groups
                         '((:name "Today"
                                  :time-grid t
                                  :date today
                                  :scheduled today)))))
            (alltodo "" ((org-super-agenda-groups
                          '((:name "Due Today"
                                   :deadline today
                                   :scheduled today)
                            (:name "Important"
                                   :tag "important")
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
           (
            (todo "DONE")
            (alltodo "test" ((org-super-agenda-groups
                          '((:priority "A")
                            (:priority "B")
                            (:tag "Productivity")
                            (:name "Short"
                                   :tag "effort< 1:01")
                            (:auto-category)
                            ))))
            )
           ))
        )


  (use-package org-super-agenda
    :init (org-super-agenda-mode)
    :config
    (evil-define-key 'motion 'org-super-agenda-header-map (kbd "q") 'org-agenda-quit)
    (setq org-super-agenda-header-map (make-sparse-keymap))
    )

  (setq org-capture-templates
        '(("d" "default" entry (file "~/notes/org/capture.org")
           "* TODO %?\n")))

  ;; Update org-agenda-files after updating item states
  ;; If the state is removed, remove the file from agenda if there are no other states, otherwise, add it
  (defun my-update-agenda-files ()
    ;; Removed TODO from item
    (if (= (length org-state) 0)
        ;; No TODOs in buffer, so remove it, otherwise add it
        (if (= (length (org-map-entries nil  "+TODO={TODO\\\|NEXT\\\|DONE\\\|WAITING\\\|HOLD\\\|CANCELLED}" 'file)) 0)
            (setq curr-files (my-remove-from-agenda-files buffer-file-name))
          (setq curr-files (my-add-to-agenda-files buffer-file-name))
          )
      ;; There's a TODO in buffer
      (setq curr-files (my-add-to-agenda-files buffer-file-name))
      )
    (org-store-new-agenda-file-list curr-files)
    (let ((inhibit-message)) (org-install-agenda-files-menu))
    )

  (defun my-get-relative-path (file-path)
    ;; Transform full paths to relative paths
    (if (string= (substring file-path 1) "~")
        (file_path)
      (replace-regexp-in-string "\\(^/.*?/.*?/\\)" "~/" file-path)))

  ;; Accepts full path
  (defun my-remove-from-agenda-files (file-full-path)
    (setq relative-path (replace-regexp-in-string "\\(^/.*?/.*?/\\)" "~/" file-full-path))
    (setq curr-files (org-agenda-files))
    (setq curr-files (delete file-full-path curr-files))
    (setq curr-files (delete relative-path curr-files))
    curr-files
    )

  (defun my-add-to-agenda-files (file-full-path)
    ;; org transforms current paths to full paths then adds a relative path
    ;; Better to remove relative and full path then add the path
    (setq curr-files (my-remove-from-agenda-files file-full-path))
    (add-to-list 'curr-files (replace-regexp-in-string "\\(^/.*?/.*?/\\)" "~/" file-full-path))
    curr-files
    )

  (add-hook 'org-after-todo-state-change-hook 'my-update-agenda-files)

  (my-add-to-agenda-files (concat (getenv "HOME") "/notes/org/capture.org"))

  ;; Clocking
  ;; Clock in clock out hooks with Polybar
  (add-hook 'org-clock-in-hook
            '(lambda () (shell-command (concat "/bin/echo -e "
                                               "\"" (org-get-heading t t t t) " \n"
                                               (org-entry-get nil "Effort") " \n"
                                               (what-line) " \n"
                                               (buffer-file-name) "\""
                                               " > /tmp/org_current_task"))
               (shell-command "xdotool set_window --classname emacs-org-mode $(xdotool getactivewindow)")))

  (dolist (hook '(org-clock-out-hook
                  org-clock-cancel-hook))
    (add-hook hook (lambda () (shell-command "/bin/rm /tmp/org_current_task"))))

  ;; Run/highlight code using babel in org-mode
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (sql . t)
     (emacs-lisp . t)
     (shell . t)))

  ;; Enables to add snippets for code blocks
  (use-package org-tempo
    :ensure nil
    :after org
    :config
    (add-to-list 'org-structure-template-alist '("shell" . "src sh"))
    (add-to-list 'org-structure-template-alist '("py" . "src python :results output"))
    (add-to-list 'org-structure-template-alist '("scala" . "src scala"))
    (add-to-list 'org-structure-template-alist '("markdown" . "src markdown"))
    (add-to-list 'org-structure-template-alist '("sql" . "src sql"))
    (add-to-list 'org-structure-template-alist '("lisp" . "src emacs-lisp")))

  ;; Go in the block with insert mode after inserting it
  (advice-add 'org-insert-structure-template :after '(lambda (orig-fun &rest args) (newline) (evil-previous-line)))

  (my-leader-key-def
    "o"   '(:ignore t :which-key "org mode")

    "oi"  '(:ignore t :which-key "insert")
    "oil" '(org-insert-link :which-key "insert link")

    "on"  '(org-toggle-narrow-to-subtree :which-key "toggle narrow")
    "ol"  '(org-latex-preview :which-key "latex preview")
    "ob"  '(org-switchb :which-key "switch buffer")
    "od"  '(deft :which-key "search in org files")

    "oa"  '(org-agenda :which-key "status")
    "ot"  '(org-todo-list :which-key "todos")
    "oc"  '((lambda () (interactive) (org-capture nil "d")) :which-key "capture"))

  ;; To export to markdown
  (require 'ox-md)

  ;; Update the document header for latex preview
  (setq org-format-latex-header (concat org-format-latex-header "\n\\input{$HOME/.config/latex/preamble.tex}\n"))
  ;; Latex image size
  (plist-put org-format-latex-options :scale 1.5)
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
                            (add-hook 'evil-insert-state-entry-hook #'org-appear-manual-start nil t)
                            (add-hook 'evil-insert-state-exit-hook #'org-appear-manual-stop nil t))))
  )


(use-package evil-org
  :after org
  ;; Ignore leading stars or tags on headings for appending end of line of going to start of line
  :custom
  (org-special-ctrl-a/e t)
  ;; Enabled, because evil has a bug with repeat command and shifting
  (evil-org-retain-visual-state-on-shift t)
  (evil-org-use-additional-insert t)
  :hook ((org-mode . evil-org-mode)
         (evil-org-mode . (lambda ()
                            (evil-org-set-key-theme '(textobjects insert navigation todo calendar additional))
                            ;; Insert heading bindings
                            (evil-define-key '(normal insert) 'evil-org-mode
                              (kbd "M-L") 'org-shiftright
                              (kbd "M-H") 'org-shiftleft
                              (kbd "M-K") 'org-shiftup
                              (kbd "M-J") 'org-shiftdown
                              (kbd "<C-return>") '(lambda () (interactive) (org-insert-heading-after-current) (evil-insert 0))
                              (kbd "<C-S-return>") '(lambda () (interactive) (org-insert-todo-heading-respect-content) (evil-insert 0))
                              ;; Move to beginning of line before insert heading, otherwise org-insert-heading will insert below
                              (kbd "<M-return>") '(lambda () (interactive) (beginning-of-line) (org-insert-heading) (evil-insert 0))
                              (kbd "<M-S-return>") '(lambda () (interactive) (beginning-of-line) (org-insert-todo-heading 0) (evil-insert 0)))
                            (evil-define-key 'normal 'evil-org-mode
                              ;; Open files at cursor
                              (kbd "<return>") '(lambda () (interactive) (let ((inhibit-message t)) (org-open-at-point)))
                              (kbd "<S-return>") '(lambda () (interactive)
                                                    ;; Open link without losing focus of window
                                                    (let ((inhibit-message t))
                                                      (update_i3_focus_window_config)
                                                      (org-open-at-point-global)
                                                      ))))))
  :config
  (require 'evil-org-agenda)

  ;; Don't display a buffer when finishing async-shell-command
  (setq display-buffer-alist '(("\\*Async Shell Command\\*" . (display-buffer-no-window))))
  (defun update_i3_focus_window_config ()
      "Changes i3 focus_window_configuration"
      (setq path_to_script (concat (getenv "XDG_CONFIG_HOME") "/i3/set_i3_focus_on_window_activation_configuration"))
      (start-process-shell-command "Update i3 focus window config" nil (concat  path_to_script " none " " && sleep 2 && " path_to_script " smart")))

  (evil-org-agenda-set-keys)
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
  :demand t
  :bind (("C-c r i" . org-roam-node-insert))
  :custom
  (org-roam-directory "~/notes/org")
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
  (advice-add 'org-roam-node-visit :before '(lambda (&rest pos) (evil-set-jump)))

  ;; Overriding org-roam-node by staying in the current buffer after inserting a new one
  ;; This is done by removing :props '(:finalize find-file) in org-roam-capture
  (cl-defun my-org-roam-node-find (&optional other-window initial-input filter-fn &key templates)
    (interactive current-prefix-arg)
    (let ((node (org-roam-node-read initial-input filter-fn)))
      (if (org-roam-node-file node)
          (org-roam-node-visit node other-window)
        (org-roam-capture-
         :node node
         :templates templates))))

  ;; Add to jump list after visiting a node
  (advice-add 'org-roam-node-find :override #'my-org-roam-node-find)

  (my-leader-key-def
    "r"   '(:ignore t :which-key "org-roam mode")

    "rl" '(org-roam-buffer-toggle :which-key "links to this node")
    "rf" '((lambda () (interactive) (let ((inhibit-message t)) (org-roam-node-find))) :which-key "find node")

    "ri"  '(org-roam-node-insert :which-key "insert node")
    "rg"  '(org-roam-ui-mode :which-key "graph of nodes"))
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
  :custom (vertico-cycle t)
  :config
  (when evil-collection-setup-minibuffer
    ;; Open buffer in a new window, uses embark's v and x
    (evil-collection-define-key 'insert 'vertico-map (kbd "C-v") (kbd "C-. v"))
    (evil-collection-define-key 'insert 'vertico-map (kbd "C-x") (kbd "C-. x")))
  )

(use-package embark
  :bind (("C-." . embark-act))
  :config

  ;; C-v and C-x splits window for org roam node prompts only
  (embark-define-keymap embark-org-roam-nodes-actions
                        "Keymap for actions for org roam nodes"
                        ("x" my-org-roam-node-find-window-x)
                        ("v" my-org-roam-node-find-window-v))

  (add-to-list 'embark-keymap-alist '(org-roam-node . embark-org-roam-nodes-actions))

  (defun my-org-roam-node-find-window-v ()
    (interactive)
    (let ((inhibit-message t))
      (advice-add 'org-roam-node-visit :before 'evil-window-vnew)
      (org-roam-node-find)
      (advice-remove 'org-roam-node-visit 'evil-window-vnew)))

  (defun my-org-roam-node-find-window-x ()
    (interactive)
    (let ((inhibit-message t))
      (advice-add 'org-roam-node-visit :before  'evil-window-new)
      (org-roam-node-find)
      (advice-remove 'org-roam-node-visit 'evil-window-new)))
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
  (deft-directory "~/notes/org")
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
  (defun my-deft-parse-title (file contents)
    "Parse the given FILE and CONTENTS and determine the title.
  If `deft-use-filename-as-title' is nil, the title is taken to
  be the first non-empty line of the FILE.  Else the base name of the FILE is
  used as title."
    (let ((begin (string-match "^#\\+[tT][iI][tT][lL][eE]: .*$" contents)))
      (if begin
          (string-trim (substring contents begin (match-end 0)) "#\\+[tT][iI][tT][lL][eE]: *" "[\n\t ]+")
        (deft-base-filename file))))

  (advice-add 'deft-parse-title :override #'my-deft-parse-title)

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
  :ensure nil
  :after org
  :config
  (appt-activate 1)

  ;; Update appt after saving file
  (add-hook 'after-save-hook
            '(lambda ()
               (if (string= (file-name-directory buffer-file-name) (concat (getenv "HOME") "/notes/org/"))
                   (org-agenda-to-appt-clear-message))))

  (setq appt-time-msg-list nil                      ;; clear existing appt list
        appt-message-warning-time '10                    ;; send first warning before appointment
        appt-display-interval '5                         ;; warn every every X minutes from t - appt-message-warning-time
        appt-display-mode-line nil                       ;; don't show in the modeline
        appt-display-format 'window)                     ;; pass warnings to the designated window function
  (setq appt-disp-window-function (function toast-appt-display))

  ;; Set up the call to the notifier
  (defun toast-appt-send-notification (title msg)
    (shell-command (concat "/usr/bin/dunstify --appname emacs_org " " \"" title "\" \"" msg "\"")))

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
  )
