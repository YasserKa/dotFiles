;;; my-open-elsewhere-embark.el --- Embark actions to open Org links and citations elsewhere -*- lexical-binding: t; -*-

;; Usage: put this file next to my-open-elsewhere.el and load it after
;; Embark, e.g.
;;
;;   (with-eval-after-load 'embark
;;     (require 'my-open-elsewhere-embark))
;;
;; After `embark-act' on an Org link, e.g. [[id:...][Tasks]] or a
;; [[file:...]] link:
;;
;;   x  open the link in a new window below
;;   v  open the link in a new window to the right
;;   w  open the link in a new frame
;;
;; After `embark-act' on a citation, e.g. [cite:@vellemanHowProveIt2019],
;; the same keys open the paper's notes (needs Citar and
;; my-open-elsewhere-citar.el).
;;
;; For Org links, w replaces Embark's copy submenu, which moves to W.
;; The keys are only added for links that open in Emacs (id:, file:,
;; internal links, org-node's id: links, ...), not for web, mail or
;; elisp: links. See `my/embark-elsewhere-types' for the target types.

(require 'my-open-elsewhere)
(require 'embark)
(require 'embark-org)

(defvar org-link-frame-setup)

;;; Org links

(defun my/open-elsewhere--org-link ()
  "Open the Org link at point in the selected window.
Org opens file and id: links in another window by default
\(`org-link-frame-setup'); here they open in the window just made."
  (let ((org-link-frame-setup
         (cons '(file . find-file) org-link-frame-setup)))
    (org-open-at-point-global)))

(defun my/embark-org-link-below ()
  "Open the Org link at point in a new window below."
  (interactive)
  (my/open-elsewhere-call 'below #'my/open-elsewhere--org-link))

(defun my/embark-org-link-right ()
  "Open the Org link at point in a new window to the right."
  (interactive)
  (my/open-elsewhere-call 'right #'my/open-elsewhere--org-link))

(defun my/embark-org-link-frame ()
  "Open the Org link at point in a new frame."
  (interactive)
  (my/open-elsewhere-call 'frame #'my/open-elsewhere--org-link))

(defvar-keymap my/embark-org-link-elsewhere-map
  :doc "Open-elsewhere actions for Org links, on top of `embark-org-link-map'."
  "x" #'my/embark-org-link-below
  "v" #'my/embark-org-link-right
  "w" #'my/embark-org-link-frame
  "W" #'embark-org-link-copy-map)

;;; Citations (Citar)

(declare-function citar-key-at-point "citar")
(declare-function citar-citation-at-point "citar")
(declare-function my/citar--open-notes "my-open-elsewhere-citar")

(defun my/embark--citation-keys ()
  "Return the citation keys at point as a list."
  (let ((keys (or (citar-key-at-point) (citar-citation-at-point))))
    (cond ((null keys) (user-error "No citation at point"))
          ((listp keys) keys)
          (t (list keys)))))

(defun my/embark--citation-notes (where)
  "Open the notes of the citation at point according to WHERE."
  (let ((keys (my/embark--citation-keys)))
    (my/open-elsewhere-call where (lambda () (my/citar--open-notes keys)))))

(defun my/embark-citation-notes-below ()
  "Open the notes of the citation at point in a new window below."
  (interactive)
  (my/embark--citation-notes 'below))

(defun my/embark-citation-notes-right ()
  "Open the notes of the citation at point in a new window to the right."
  (interactive)
  (my/embark--citation-notes 'right))

(defun my/embark-citation-notes-frame ()
  "Open the notes of the citation at point in a new frame."
  (interactive)
  (my/embark--citation-notes 'frame))

(defvar-keymap my/embark-citation-elsewhere-map
  :doc "Open-elsewhere actions for citations, on top of Citar's Embark map."
  "x" #'my/embark-citation-notes-below
  "v" #'my/embark-citation-notes-right
  "w" #'my/embark-citation-notes-frame)

(with-eval-after-load 'citar
  (require 'my-open-elsewhere-citar))

;;; Adding the maps to Embark

(defvar my/embark-elsewhere-types
  '((org-link       my/embark-org-link-elsewhere-map embark-org-link-map)
    (org-file-link  my/embark-org-link-elsewhere-map embark-org-link-map)
    ;; id: links as targeted by org-node
    (org-node-link  my/embark-org-link-elsewhere-map embark-org-link-map)
    (citar-citation my/embark-citation-elsewhere-map citar-embark-citation-map)
    (citar-key      my/embark-citation-elsewhere-map citar-embark-citation-map))
  "Embark target types that get the open-elsewhere keys, as (TYPE MAP FALLBACK).
MAP is put in front of the type's keymaps in `embark-keymap-alist', so
its keys win. FALLBACK is used under it when the type has no keymaps
of its own. Add a type here if `embark-act' shows another type for
the links you use (it is the first word after \"Act on\").")

(defun my/embark-elsewhere--install (&rest _)
  "Put the maps of `my/embark-elsewhere-types' in `embark-keymap-alist'.
Run before every `embark-act', so it still works when another package
or your config sets a type's keymaps after this file is loaded."
  (pcase-dolist (`(,type ,map ,fallback) my/embark-elsewhere-types)
    (let ((maps (remq map (ensure-list (alist-get type embark-keymap-alist)))))
      (when (and (null maps) (boundp fallback))
        (setq maps (list fallback)))
      (setf (alist-get type embark-keymap-alist) (cons map maps)))))

(advice-add 'embark-act :before #'my/embark-elsewhere--install)

(provide 'my-open-elsewhere-embark)
;;; my-open-elsewhere-embark.el ends here
