;;; my-open-elsewhere.el --- Open the selected candidate in a split or new frame -*- lexical-binding: t; -*-

;; Usage: put this file on your `load-path' next to my-nodes.el and
;; (require 'my-open-elsewhere), then turn on the mode:
;;
;;   (my/open-elsewhere-global-mode 1)
;;
;; In the minibuffer of the commands listed in
;; `my/open-elsewhere-commands' (Consult buffers, files, grep, Org
;; headings, `find-file', `my/node-find', ...), these keys accept the
;; highlighted candidate and open it somewhere else:
;;
;;   C-x  new window below (horizontal split)
;;   C-v  new window to the right (vertical split)
;;   M-w  new frame (one window)
;;
;; How it works: the key remembers where to open and exits the
;; minibuffer like RET. As soon as the prompt returns, the window is
;; split (or a frame made) and selected, so the command's own action
;; (switching buffer, visiting the file, jumping to the grep match or
;; heading) happens there. That is why one mechanism works for every
;; command, including ones that don't use Consult.

(require 'cl-lib)

;;; ------------------------------------------------------------
;;; Configuration
;;; ------------------------------------------------------------

(defvar my/open-elsewhere-commands
  '(;; Buffers
    consult-buffer consult-project-buffer switch-to-buffer
    ;; Files
    find-file consult-find consult-fd consult-locate
    consult-recent-file recentf-open project-find-file
    ;; Grep
    consult-grep consult-ripgrep consult-git-grep
    ;; Org headings
    consult-org-heading consult-org-agenda
    ;; Notes (my-nodes.el)
    my/node-find)
  "Commands whose minibuffer gets the open-elsewhere keys.
The keys are active whenever one of these runs, whether you call it
directly or from a lambda or wrapper command bound to a key. Add your
own commands with e.g.
\(add-to-list \\='my/open-elsewhere-commands \\='consult-line)
and then run (my/open-elsewhere-global-mode 1) again.
Use t to enable the keys in every minibuffer.")

(defvar my/open-elsewhere-keys
  '(("C-x" . my/open-elsewhere-below)
    ("C-v" . my/open-elsewhere-right)
    ("M-w" . my/open-elsewhere-frame))
  "Keys bound while `my/open-elsewhere-mode' is on, as (KEY . COMMAND).
Change this before loading the file, or call
`my/open-elsewhere--bind-keys' again after changing it.")

;;; ------------------------------------------------------------
;;; Keymap and minor mode (buffer-local, in the minibuffer only)
;;; ------------------------------------------------------------

(defvar my/open-elsewhere-mode-map (make-sparse-keymap)
  "Keymap for `my/open-elsewhere-mode'.")

(declare-function evil-define-minor-mode-key "evil-core")
(declare-function evil-normalize-keymaps "evil-core")
(declare-function vertico-exit "vertico")

(define-minor-mode my/open-elsewhere-mode
  "Minibuffer keys that open the selected candidate in a split or frame.
Turned on automatically by `my/open-elsewhere-global-mode' in the
minibuffers of `my/open-elsewhere-commands'."
  :keymap my/open-elsewhere-mode-map
  :group 'convenience)

(defun my/open-elsewhere--bind-keys ()
  "Bind `my/open-elsewhere-keys' in the mode map, and for Evil if loaded.
Evil's insert and normal state maps shadow ordinary minor-mode maps
\(C-v is `quoted-insert' there), so the keys are also bound with
`evil-define-minor-mode-key', which takes precedence over them."
  (setcdr my/open-elsewhere-mode-map nil)
  (pcase-dolist (`(,key . ,cmd) my/open-elsewhere-keys)
    (define-key my/open-elsewhere-mode-map (kbd key) cmd))
  (when (fboundp 'evil-define-minor-mode-key)
    (pcase-dolist (`(,key . ,cmd) my/open-elsewhere-keys)
      (evil-define-minor-mode-key '(insert normal) 'my/open-elsewhere-mode
        (kbd key) cmd))))

(my/open-elsewhere--bind-keys)
(with-eval-after-load 'evil (my/open-elsewhere--bind-keys))

(defvar my/open-elsewhere--inside nil
  "Non-nil while one of `my/open-elsewhere-commands' is running.
Set by advice on those commands, so the keys also work when a command
is called from a lambda, a wrapper command or another package.")

(defun my/open-elsewhere--mark-inside (orig &rest args)
  "Around advice: run ORIG with ARGS with `my/open-elsewhere--inside' set.
The interactive form covers commands like `find-file' and
`switch-to-buffer', which prompt in their interactive spec: that is
evaluated before the function body, so outside the `let' below."
  (interactive (lambda (spec)
                 (let ((my/open-elsewhere--inside t))
                   (advice-eval-interactive-spec spec))))
  (let ((my/open-elsewhere--inside t))
    (apply orig args)))

(defun my/open-elsewhere--setup ()
  "Turn `my/open-elsewhere-mode' on or off for the new minibuffer."
  (my/open-elsewhere-mode
   (if (or (eq my/open-elsewhere-commands t)
           my/open-elsewhere--inside
           (memq this-command my/open-elsewhere-commands))
       1 -1))
  (when (and my/open-elsewhere-mode (fboundp 'evil-normalize-keymaps))
    (evil-normalize-keymaps)))

;;; ------------------------------------------------------------
;;; The keys: remember where, then exit like RET
;;; ------------------------------------------------------------

(defvar my/open-elsewhere--pending nil
  "(WHERE . DEPTH) set by an open-elsewhere key, or nil.
WHERE is `below', `right' or `frame'; DEPTH is the minibuffer depth
the key was pressed in, so only that prompt acts on it.")

(defvar my/open-elsewhere-exit-functions nil
  "Functions that may leave the minibuffer in a special way.
Each is called with no arguments when an open-elsewhere key is pressed.
The first one that returns non-nil has taken care of exiting (now or
later); if none does, the minibuffer is exited like RET. For prompts
that need something other than RET to finish, e.g. Citar's
multiple selection.")

(defun my/open-elsewhere--exit (where)
  "Accept the current candidate and open it according to WHERE."
  (setq my/open-elsewhere--pending (cons where (minibuffer-depth)))
  (unless (run-hook-with-args-until-success 'my/open-elsewhere-exit-functions)
    (if (and (bound-and-true-p vertico-mode) (fboundp 'vertico-exit))
        (vertico-exit)
      (exit-minibuffer))
    ;; Exiting throws out of this function. Reaching this line means the
    ;; minibuffer refused to exit (e.g. "Match required"), so forget it.
    (setq my/open-elsewhere--pending nil)))

(defun my/open-elsewhere-below ()
  "Open the selected candidate in a new window below."
  (interactive)
  (my/open-elsewhere--exit 'below))

(defun my/open-elsewhere-right ()
  "Open the selected candidate in a new window to the right."
  (interactive)
  (my/open-elsewhere--exit 'right))

(defun my/open-elsewhere-frame ()
  "Open the selected candidate in a new frame."
  (interactive)
  (my/open-elsewhere--exit 'frame))

;;; ------------------------------------------------------------
;;; After the prompt returns: make and select the new window/frame
;;; ------------------------------------------------------------

(defvar my/open-elsewhere-prepared nil
  "The last WHERE that `my/open-elsewhere--prepare' acted on, or nil.
Other code can let-bind this to nil around a prompt and check it
afterwards to see whether the prompt was left with one of the keys,
e.g. to open something other than the command's default result.")

(defun my/open-elsewhere--prepare (where)
  "Split the selected window or clone the frame according to WHERE."
  (setq my/open-elsewhere-prepared where)
  (pcase where
    ('below (select-window (split-window-below)))
    ('right (select-window (split-window-right)))
    ('frame (let ((frame (clone-frame)))
              (select-frame-set-input-focus frame)
              ;; clone-frame copies the window layout; keep one window.
              (delete-other-windows (frame-selected-window frame))))))

(defun my/open-elsewhere--around-read (orig &rest args)
  "Around advice for prompt functions: act on a pending open-elsewhere key.
Runs ORIG with ARGS. If the prompt it opened was exited with one of the
open-elsewhere keys, prepare the new window or frame before returning,
so the caller's action happens there."
  (let ((depth (1+ (minibuffer-depth))))
    ;; A key is only pressed to leave a prompt, so anything pending now
    ;; is left over from a prompt that wasn't advised. Drop it.
    (setq my/open-elsewhere--pending nil)
    (prog1 (apply orig args)
      (when (and my/open-elsewhere--pending
                 (= (cdr my/open-elsewhere--pending) depth))
        (let ((where (car my/open-elsewhere--pending)))
          (setq my/open-elsewhere--pending nil)
          (my/open-elsewhere--prepare where))))))

;; `completing-read' covers Consult, `read-file-name' (find-file),
;; `project-find-file' and most other prompts. `read-buffer' is needed
;; because it calls the completion code from C, which bypasses advice.
;; `completing-read-multiple' (used by e.g. Citar) reads the minibuffer
;; directly, not through `completing-read'.
(defconst my/open-elsewhere--advised
  '(completing-read completing-read-multiple read-buffer))

(defvar my/open-elsewhere--marked nil
  "Commands currently advised with `my/open-elsewhere--mark-inside'.")

;;;###autoload
(define-minor-mode my/open-elsewhere-global-mode
  "Enable the open-elsewhere keys in the minibuffers of chosen commands.
See `my/open-elsewhere-commands' and `my/open-elsewhere-keys'.
After changing `my/open-elsewhere-commands', turn the mode on again
so the new commands are picked up."
  :global t
  :group 'convenience
  (dolist (fn my/open-elsewhere--advised)
    (advice-remove fn #'my/open-elsewhere--around-read))
  (dolist (fn my/open-elsewhere--marked)
    (advice-remove fn #'my/open-elsewhere--mark-inside))
  (setq my/open-elsewhere--marked nil)
  (remove-hook 'minibuffer-setup-hook #'my/open-elsewhere--setup)
  (when my/open-elsewhere-global-mode
    (dolist (fn my/open-elsewhere--advised)
      (advice-add fn :around #'my/open-elsewhere--around-read))
    ;; Mark the listed commands, so a call from a lambda or wrapper
    ;; (where `this-command' is the lambda) still gets the keys. Works
    ;; for commands that aren't loaded yet, too.
    (when (listp my/open-elsewhere-commands)
      (dolist (fn my/open-elsewhere-commands)
        (advice-add fn :around #'my/open-elsewhere--mark-inside)
        (push fn my/open-elsewhere--marked)))
    ;; Late depth, so it runs after Vertico and Consult set up the prompt.
    (add-hook 'minibuffer-setup-hook #'my/open-elsewhere--setup 90)))

(provide 'my-open-elsewhere)
;;; my-open-elsewhere.el ends here
