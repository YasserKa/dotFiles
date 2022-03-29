(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("nognu" . "https://elpa.nongnu.org/nongnu/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(require 'use-package)

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

;; Load the helper package for commands like `straight-x-clean-unused-repos'
(require 'straight-x)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4eb6fa2ee436e943b168a0cd8eab11afc0752aebb5d974bba2b2ddc8910fca8f" "6b5c518d1c250a8ce17463b7e435e9e20faa84f3f7defba8b579d4f5925f60c1" "83e0376b5df8d6a3fbdfffb9fb0e8cf41a11799d9471293a810deb7586c131e6" "2b9dc43b786e36f68a9fd4b36dd050509a0e32fe3b0a803310661edb7402b8b6" default))
 '(evil-digit-bound-motions '(evil-beginning-of-visual-line))
 '(evil-want-Y-yank-to-eol 1)
 '(org-agenda-files
   '("/home/yasser/notes/org/20211116083953-thesis.org" "/home/yasser/notes/org/20210911093036-general.org"))
 '(package-selected-packages
   '(org-gcal org-appear deft company orderless marginalia vertico evil-textobj-anyblock cdlatex auctex simple-httpd websocket use-package undo-tree  evil evil-collection org-roam evil-org orgalist evil-surround general evil-visual-mark-mode gruvbox-theme)))
;; Set the variable pitch face

(use-package undo-tree
  :init
  (global-undo-tree-mode 1)
  )

;; EVIL
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-tree)
  ;; Y is y$
  (setq evil-want-Y-yank-to-eol '1)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-select-search-module 'evil-search-module 'evil-search)

  (evil-define-key 'operator evil-surround-mode-map "eu" 'undo-tree-visualize)
  )

(use-package evil-collection
  :after evil
  :init
  (setq evil-collection-company-use-tng nil)  ;; Is this a bug in evil-collection?
  :custom
  (evil-collection-outline-bind-tab-p nil)
  (evil-collection-setup-minibuffer t)
  :config

  (evil-collection-init)
  ;; needed for company package
  (evil-collection-define-key nil 'company-active-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (evil-collection-define-key nil 'company-active-map (kbd "C-w") 'evil-delete-backward-word)

  (evil-collection-define-key '(insert normal) 'evil-ex-completion-map (kbd "C-q") 'help)
  (evil-collection-define-key '(insert normal) 'evil-ex-completion-map (kbd "<escape>") 'abort-recursive-edit)

  (evil-define-key 'insert evil-ex-search-keymap (kbd "C-q") 'help)
  (evil-define-key 'insert evil-ex-search-keymap (kbd "<escape>") 'abort-recursive-edit)

  (evil-collection-define-key '(insert normal) 'vertico-map (kbd "<escape>") 'abort-recursive-edit)

  (setq evil-collection-magit-state 'normal)

  (define-key global-map "\C-q" 'help)
  ;; Updating the original by closing the list of repos window
  (defun my-magit-repolist-status (&optional _button)
    "Show the status for the repository at point."
    (interactive)
    (--if-let (tabulated-list-get-id)
        (let ((p (selected-window)))
          (magit-status-setup-buffer (expand-file-name it)) (delete-window p))
      (user-error "There is no repository at point")))

  (define-key evil-motion-state-map (kbd "RET") 'my-magit-repolist-status)

  (setq magit-repository-directories '(("~/dotfiles" . 0) ("~/Projects/" . 1) ("/srv/http/cooldown" . 0)))
  (evil-define-key evil-collection-magit-state magit-mode-map "?" 'evil-search-backward)
                                        ; Enable spell checking while commiting
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)

  ;; Open file using nvim
  (defun vil-diff-visit-file (file &optional other-window)
    (interactive (list (magit-file-at-point t t) current-prefix-arg))
    (shell-command (concat "nvim-qt " file nil)))

  (evil-define-key evil-collection-magit-state magit-mode-map (kbd "RET") 'vil-diff-visit-file)

  )

;; VIM LEADER
(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer my-leader-key-def
    :keymaps '(normal visual emacs)
    :prefix ","
    )
  (my-leader-key-def
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
  )


(defun source-init-file ()
  (interactive)
  (load-file "~/.emacs")
  )

(defun edit-init-file ()
  (interactive)
  (split-window-below)
  (find-file "~/.emacs"))


(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))


;; Remove emacs' bars
(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)       ; Give some breathing room
(menu-bar-mode -1)            ; Disable the menu bar

(column-number-mode)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package diminish)

;; highlight matching braces
(use-package paren
  :config
  ;; (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (show-paren-mode 1))

(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)
(setq-default indent-tabs-mode nil)

;; Commenting
(use-package evil-commentary
  :config
  (evil-mode 1)
  (evil-commentary-mode)
  )
(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

;; Enable vertico
(use-package vertico
  :straight '(vertico :host github
                      :repo "minad/vertico"
                      :branch "main")
  :custom
  (vertico-cycle t)
  :custom-face
  :init
  (vertico-mode)
  (defun my/macro-to-open-buffer-in-another-window ()
    (interactive)
    (execute-kbd-macro (kbd "C-. C-v")))
  :bind (:map vertico-map ("C-v" . my/function))
  :config
  (when evil-collection-setup-minibuffer
    (evil-collection-define-key 'insert 'vertico-map
      (kbd "C-v") (kbd "C-. C-v")))
  )

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  (define-key embark-general-map (kbd "C-x") 'my/org-roam-node-find-window)

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))


(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))


(defun dw/get-project-root ()
  (when (fboundp 'projectile-project-root)
    (projectile-project-root)))

(use-package consult
  :straight t
  :demand t
  :bind (("C-s" . consult-line)
         ("C-M-l" . consult-imenu)
         ("C-M-j" . persp-switch-to-buffer*)
         :map minibuffer-local-map
         ("C-r" . consult-history))
  :custom
  (consult-project-root-function #'dw/get-project-root)
  (completion-in-region-function #'consult-completion-in-region)
  :config
  (consult-preview-at-point-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :config
  (setq history-length 25)
  (savehist-mode 1))

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))


(use-package openwith
  :config
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("mpg" "mpeg" "mp3" "mp4"
                  "avi" "wmv" "wav" "mov" "flv"
                  "ogm" "ogg" "mkv"))
               "mpv"
               '(file))
         ;; (list (openwith-make-extension-regexp
         ;;        '("xbm" "pbm" "pgm" "ppm" "pnm"
         ;;          "png" "gif" "bmp" "tif" "jpeg")) ;; Removed jpg because Telega was
         ;;       ;; causing feh to be opened...
         ;;       "feh"
         ;;       '(file))
         (list (openwith-make-extension-regexp
                '("pdf" "epub" "djvu"))
               "zathura"
               '(file))))
  (openwith-mode t)
  )

;; line wrapping
(setq-default fill-column 100)

;; org mode
;; Turn on indentation and auto-fill mode for Org files
(defun my-org-mode-setup ()
  (org-indent-mode)
  (global-visual-line-mode t)
  (auto-fill-mode 1)
  (visual-line-mode 1)
  (setq evil-auto-indent nil)
  (diminish org-indent-mode))

;; auto-complete
(use-package org
  :defer t
  :ensure org-contrib
  :hook (org-mode . my-org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t ;; hide symbols
        org-src-fontify-natively t
        org-fontify-quote-and-verse-blocks t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 2
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-startup-folded 'content
        org-startup-with-inline-images t
        org-image-actual-width nil
        org-enforce-todo-dependencies t
        org-startup-folded t
        ;; Remove completed deadline, scheduled, completed from agenda
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-timestamp-if-done t
        ;; source code indentation
        org-src-preserve-indentation nil
        org-edit-src-content-indentation 0
        ;; Exporting settings
        org-export-with-broken-links t
        org-export-preserve-breaks t
        )

  ;; Keywords
  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
                ("WAITING" :foreground "orange" :weight bold)
                )))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "DONE(d)")
          (sequence "WAITING(w)")))
  ;; Don't export with keywords
  (setq-default org-export-with-todo-keywords nil)
  ;; To export to markdown
  (require 'ox-md)

  ;; Padding
  (lambda () (progn
               (setq left-margin-width 2)
               (setq right-margin-width 2)
               (set-window-buffer nil (current-buffer))))

  (setq line-spacing 0.1)

  ;; Agenda styling
  (defun my/style-org-agenda()
    (set-face-attribute 'org-agenda-date nil :height 1.1)
    (set-face-attribute 'org-agenda-date-today nil :height 1.1)
    (set-face-attribute 'org-agenda-date-weekend nil :height 1.1))

  ;; Get the title property in the file
  ;; Usage: displayed in agenda mode
  ;; (defun my/get-title-property ()
  ;;   (setq title (elt (elt (org-collect-keywords '("TITLE")) 0) 1))
  ;;   (if title title ""))

  (defun my/get-title-property ()
    (setq title (elt (elt (org-collect-keywords '("TITLE")) 0) 1))
    (setq roam_alias (org-entry-get-with-inheritance "ROAM_ALIASES"))
    (if roam_alias roam_alias (if title title "")))

  (add-hook 'org-agenda-mode-hook 'my/style-org-agenda)

  (setq org-agenda-breadcrumbs-separator " ❱ "
        org-agenda-current-time-string "⏰ ┈┈┈┈┈┈┈┈┈┈┈ now"
        org-agenda-time-grid '((weekly today require-timed)
                               (800 1000 1200 1400 1600 1800 2000)
                               "---" "┈┈┈┈┈┈┈┈┈┈┈┈┈")

        org-agenda-prefix-format '((agenda . "%i %-16:(my/get-title-property)%?-12t%b%-4e% s")
                                   (todo . "%i %-12:(my/get-title-property) %-4e")
                                   (tags . "%i %-12:(my/get-title-property) %-4e")
                                   (search . "%i %-12:(my/get-title-property) %-4e")))

  (evil-define-key '(normal insert visual) org-mode-map (kbd "M-j") 'org-metadown)
  (evil-define-key '(normal insert visual) org-mode-map (kbd "M-k") 'org-metaup)
  ;; (setq org-insert-heading-respect-content t)

  ;; Save org buffers after quiting agenda mode
  (advice-add 'org-agenda-quit :before '(lambda () (org-save-all-org-buffers) (message nil)))

  (add-hook 'org-clock-out-hook
            '(lambda ()
               (shell-command (concat "/bin/rm /tmp/org_current_task"))))

  (add-hook 'org-clock-cancel-hook
            '(lambda ()
               (shell-command (concat "/bin/rm /tmp/org_current_task"))))

  ;; Super agenda
  (setq org-agenda-custom-commands

        '(("z" "Super view"
           ((agenda "" ((org-agenda-span 'day)
                        (org-super-agenda-groups
                         '((:name "Today"
                                  :time-grid t
                                  :date today
                                  :todo "TODAY"
                                  :scheduled today
                                  :order 1)))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '(;; Each group has an implicit boolean OR operator between its selectors.
                            (:name "Today"
                                   :deadline today
                                   :face (:background "black"))
                            (:name "Passed deadline"
                                   :and (:deadline past :todo ("TODO" "WAITING" "HOLD" "NEXT"))
                                   :face (:background "#7f1b19"))
                            (:name "Work important"
                                   :and (:priority>= "B" :category "Work" :todo ("TODO" "NEXT")))
                            (:name "Work other"
                                   :and (:category "Work" :todo ("TODO" "NEXT")))
                            (:name "Important"
                                   :priority "A")
                            (:priority<= "B"
                                         ;; Show this section after "Today" and "Important", because
                                         ;; their order is unspecified, defaulting to 0. Sections
                                         ;; are displayed lowest-number-first.
                                         :order 1)
                            (:name "Waiting"
                                   :todo "WAITING"
                                   :order 9)
                            (:name "On hold"
                                   :todo "HOLD"
                                   :order 10)))))))))
  (add-hook 'org-agenda-mode-hook 'org-super-agenda-mode)
  (setq org-super-agenda-header-map (make-sparse-keymap))

  (setq org-capture-templates
        '(("d" "default" entry (file "~/notes/org/20210909221237-capture.org")
           "* TODO %?\n")))

  ;; Insert mode after going to capture
  (add-hook 'org-capture-mode-hook 'evil-insert-state)

  ;; save capture on :wq
  (evil-define-key nil org-capture-mode-map
    [remap evil-save-and-close] #'org-capture-finalize
    [remap evil-save-modified-and-close] #'org-capture-finalize
    [remap evil-quit] #'org-capture-kill)

  ;; running scala code in babel
  ;; (load-file (concat user-emacs-directory "ob-scala.el"))
  ;; (setq org-babel-scala-command "amm")
  ;; (setq org-babel-scala-wrapper-method "%s")

  ;; Run/highlight code using babel in org-mode
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (python . t)
     ;; (ipython . t) ; could be enabled by it requires jupyter-notebook (either use )
     ;; (scala . t)
     (sql . t)
     (shell . t)
     ))

  ;; Syntax highlight in #+BEGIN_SRC blocks
  (setq org-src-fontify-natively t)
  ;; Don't prompt before running code in org
  (setq org-confirm-babel-evaluate nil)
  ;; Fix an incompatibility between the ob-async and ob-ipython packages
  (setq ob-async-no-async-languages-alist '("ipython"))

  ;; show emphasis markers when hovering over text
  (use-package org-appear
    :hook (org-mode . org-appear-mode))

  (use-package org-superstar
    :after org
    :hook (org-mode . org-superstar-mode)
    :custom
    (org-superstar-remove-leading-stars t)
    (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

  (set-face-attribute 'org-document-title nil :font "DejaVu Sans Mono" :weight 'bold :height 1.3)
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(org-level-1 ((t (:inherit outline-1 :height 1.25))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.15))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.1))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.05)))))
  ;; indent nested items
  (require 'org-indent)

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

  ;; Get rid of the background on column views
  (set-face-attribute 'org-column nil :background nil)
  (set-face-attribute 'org-column-title nil :background nil)

  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("shell" . "src sh"))
  (add-to-list 'org-structure-template-alist '("py" . "src python :results output"))
  (add-to-list 'org-structure-template-alist '("scala" . "src scala"))
  (add-to-list 'org-structure-template-alist '("markdown" . "src markdown"))
  (add-to-list 'org-structure-template-alist '("sql" . "src sql"))

  (use-package evil-org
    :after org
    :hook (
           (org-mode . evil-org-mode)
           (org-agenda-mode . evil-org-mode)
           (evil-org-mode . (lambda ()
                              (evil-org-set-key-theme '(navigation todo insert textobjects additional calendar))
                              ;; Insert heading bindings
                              (evil-define-key 'normal 'evil-org-mode
                                (kbd "<C-return>") '(lambda () (interactive) (org-insert-heading-after-current) (evil-insert 0))
                                (kbd "<C-S-return>") '(lambda () (interactive) (org-insert-todo-heading-respect-content) (evil-insert 0))
                                (kbd "<M-return>") '(lambda () (interactive) (org-ctrl-c-ret) (evil-insert 0))
                                (kbd "<M-S-return>") '(lambda () (interactive) (org-insert-todo-heading 0) (evil-insert 0))))))
    :config
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys)
    )


  (my-leader-key-def
    "o"   '(:ignore t :which-key "org mode")

    "oi"  '(:ignore t :which-key "insert")
    "oil" '(org-insert-link :which-key "insert link")

    "on"  '(org-toggle-narrow-to-subtree :which-key "toggle narrow")
    "ol"  '(org-latex-preview :which-key "latex preview")
    "ob"  '(org-switchb :which-key "switch buffer")
    "od" '(deft :which-key "search in org files")

    "oa"  '(org-agenda :which-key "status")
    "ot"  '(org-todo-list :which-key "todos")
    "oc"  '((lambda () (interactive) (org-capture nil "d")) :which-key "capture")
    )
  )

(use-package org-roam
  :ensure t
  :demand t
  :bind (("C-c r i" . org-roam-node-insert))
  :custom
  (org-roam-directory "~/notes/org")
  (org-roam-completion-everywhere t)
  (org-roam-completion-system 'default)
  (org-roam-capture-templates
   '(
     ("d" "default" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)
     ("b" "book notes" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+filetags: book_notes\n")
      :unnarrowed t)
     ))
  :config
  (org-roam-db-autosync-mode)
  (setq org-roam-db-update-on-save nil)

  (defun my/org-roam-node-find-window ()
    (interactive)
    (org-roam-node-find t))
  (my-leader-key-def
    "r"   '(:ignore t :which-key "org-roam mode")

    "rl" '(org-roam-buffer-toggle :which-key "links to this node")
    "rf" '(org-roam-node-find :which-key "find node")
    "rF" '(my/org-roam-node-find-window :which-key "find node - new window")

    "ri"  '(org-roam-node-insert :which-key "insert node")
    "rg"  '(org-roam-ui-mode :which-key "graph of nodes")

    )
  )

(use-package org-roam-ui
  :straight
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package deft
  :custom
  (deft-directory "~/notes/org")
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  )

;; Clock in clock out hooks with polybar

(add-hook 'org-clock-in-hook
          '(lambda ()
             (shell-command (concat "/bin/echo -e "
                                    "\"" (org-get-heading t t t t) " \n"
                                    (org-entry-get nil "Effort") " \n"
                                    (what-line) " \n"
                                    (buffer-file-name) "\""
                                    " > /tmp/org_current_task")
                            )
             (shell-command "xdotool set_window --classname emacs-org-mode $(xdotool getactivewindow)")
             ))
;; Loading emacs server is needed by emacsclient to focus on file that's used for clock
(load "server")
(unless (server-running-p) (server-start))

;; ORG NOTIFICATION
(require 'appt)
(appt-activate 1)

;; update appt after saving file
(add-hook 'after-save-hook
          '(lambda ()
             (if (string= (file-name-directory buffer-file-name) (concat (getenv "HOME") "/notes/org/"))
                 (org-agenda-to-appt-clear-message))))


(setq
 appt-time-msg-list nil                           ;; clear existing appt list
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

;; Spell checking toggle with yos
(evil-define-key 'operator evil-surround-mode-map "os" 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
(define-key evil-insert-state-map "\C-l"  'flyspell-auto-correct-previous-word)


;; update the document header for latex preview
(setq org-format-latex-header (concat org-format-latex-header "\n\\input{$HOME/.config/latex/preamble.tex}\n"))

(add-hook 'org-mode-hook 'turn-on-org-cdlatex)
(define-key org-cdlatex-mode-map (kbd "C-j") 'cdlatex-tab)


;; local configuration for TeX modes
(plist-put org-format-latex-options :scale 1.5)


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

;; between dollar signs
(define-and-bind-text-object "$" "\\$" "\\$")
(define-and-bind-text-object "~" "\\~" "\\~")

;; Open files at cursor
(evil-define-key 'normal org-mode-map (kbd "RET") 'org-open-at-point)
(evil-define-key 'normal org-mode-map (kbd "<S-return>") 'org-open-at-point-global)

;; Adjusting text scale
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)
(define-key global-map (kbd "C-=") '(lambda () (interactive) (text-scale-adjust 0) (message nil)))

(define-key org-mode-map "\C-j" nil)

;; Prevent inserting a new element to split the line
(setq org-M-RET-may-split-line 'nil)

(define-key org-mode-map "\M-l" 'org-metaright)
(define-key org-mode-map (kbd "M-l") 'org-shiftmetaright)
(define-key org-mode-map "\M-h" 'org-metaleft)
(define-key org-mode-map (kbd "M-H") 'org-shiftmetaleft)
(define-key org-mode-map "\M-k" 'org-metaup)
(define-key org-mode-map (kbd "M-K") 'org-shiftmetaup)
(define-key org-mode-map "\M-j" 'org-metadown)
(define-key org-mode-map (kbd "M-J") 'org-shiftmetadown)

;; have one buffer when clicking on org-agenda
(evil-define-key 'motion org-agenda-mode-map
  (kbd "<RET>") '(lambda() (interactive) (org-agenda-switch-to t)))

(setq org-cycle-separator-lines -1)

;; gx opens urls
(define-key evil-normal-state-map "gx" 'org-open-at-point)

;; Remove the annoying Enter (C-j)
(global-unset-key "\C-j")

;; Install not presented packages
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

(load-theme 'gruvbox-light-medium t)

;; trigger the background theme
(defun my-trigger-theme ()
  (interactive)
  (if (eq (car custom-enabled-themes) 'gruvbox-light-medium)
      (load-theme 'gruvbox-dark-soft t)
    (load-theme 'gruvbox-light-medium t)
    ))

(evil-define-key 'operator evil-surround-mode-map "ob" 'my-trigger-theme)

;; Remove backup files (ends with ~)
(setq make-backup-files nil)
;; Avoid being prompted with symbolic link to git-controlled
(setq vc-follow-symlinks t)
;; (set-frame-font "Inconsolata-14" nil)
(add-to-list 'default-frame-alist
             '(font . "DejaVu Sans Mono-13"))

;; Setting it from <C-h>
(setq help-char (string-to-char "?"))

(define-key global-map "\C-q" 'help)
(define-key global-map "\C-V" 'yank)
(define-key global-map (kbd "\C-x \C-m") 'execute-extended-command)
(define-key global-map (kbd "<escape>") 'keyboard-escape-quit)

(define-key global-map "\C-w" 'backward-kill-word)

;; unbind C-, in fly mode
(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-,") nil)
  )

(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-M-i") nil)
  )

;; Enter in command mode
(define-key evil-ex-completion-map "\C-j" 'exit-minibuffer)
(define-key evil-ex-completion-map "\C-V" 'evil-paste-after)

(require 'evil-surround)
(global-evil-surround-mode 1)


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

;; Keybindings for command mode
(defvar my-overriding-binding-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-h] 'delete-backward-char)
    (define-key map [?\M-h] 'backward-kill-word)
    map))

(define-minor-mode my-overriding-binding-mode
  "Personal global key-bindings."
  :global t)

(define-key isearch-mode-map (kbd "\C-h") 'isearch-delete-char)
(define-key isearch-mode-map (kbd "\C-q") 'help)

(my-overriding-binding-mode 1)

;; (use-package yasnippet
;;   :hook (prog-mode . yas-minor-mode)
;;   :config
;;   (yas-reload-all))

(add-hook 'after-init-hook 'global-company-mode)
;; local configuration for TeX modes
(defun my-latex-mode-setup ()
  (setq-local company-backends
              (append '((company-capf))
                      company-backends)))

(add-hook 'org-mode 'my-latex-mode-setup)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit outline-1 :height 1.25))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.15))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.1))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.05)))))

;; Go to https://console.cloud.google.com/apis/credentials to get the credentials
;; (require 'org-gcal)
;; (setq org-gcal-client-id ""
;;       org-gcal-client-secret ""
;;       org-gcal-fetch-file-alist '(("yasser.kaddoura19@gmail.com" .  "~/notes/org/20210911093036-general.org")
;;                                   ))
