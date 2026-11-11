;;; org-table-widget-link-beautify.el --- Show org-link-beautify links inside org-table-widget tables -*- lexical-binding: t; -*-

;;; Commentary:

;; org-table-widget builds each cell from `buffer-substring', which copies
;; text properties (so Org's invisible link brackets and hidden emphasis
;; markers survive) but NOT overlays.  org-link-beautify draws everything
;; through the overlays that Org's `org-link-preview' creates
;; (`before-string' with the icon, `display' with ｢description｣), so the
;; widget never sees the beautified form and measures/draws the raw link.
;;
;; This file:
;;  1. Replaces `org-table-widget--line-cells' with a version that folds
;;     each link-preview overlay into the cell text (before-string +
;;     display string + after-string), so the widget both draws and
;;     measures the link exactly as it looks outside the table.
;;  2. Re-renders tables whenever link previews are created or cleared,
;;     because previews arrive asynchronously (idle-timer batches) and
;;     overlay changes don't alter the widget's cache key.
;;
;; Usage: load after org-table-widget, e.g.
;;   (with-eval-after-load 'org-table-widget
;;     (require 'org-table-widget-link-beautify))

;;; Code:

(require 'cl-lib)
(require 'org-table-widget)

(defvar org-table-widget-link-beautify-images nil
  "Non-nil means show image previews (thumbnails) inside table widgets.
When nil, links whose preview is an image are shown as plain Org links,
which keeps rows one line tall.")

(defconst org-table-widget-link-beautify--link-props
  '(mouse-face help-echo keymap follow-link htmlize-link)
  "Link text properties copied onto the folded overlay text.")

(defun org-table-widget-link-beautify--preview-overlays (start end)
  "Return finished link-preview overlays wholly inside START..END, in order."
  (sort (cl-remove-if-not
         (lambda (ov)
           (and (overlay-get ov 'org-image-overlay)
                (<= start (overlay-start ov))
                (<= (overlay-end ov) end)
                (< (overlay-start ov) (overlay-end ov))))
         (overlays-in start end))
        (lambda (a b) (< (overlay-start a) (overlay-start b)))))

(defun org-table-widget-link-beautify--overlay-text (ov)
  "Return the text OV displays, as a propertized string."
  (let* ((start (overlay-start ov))
         (end (overlay-end ov))
         (source (buffer-substring start end))
         (display (overlay-get ov 'display))
         (body (cond
                ((stringp display) (copy-sequence display))
                ((null display) source)
                ;; Image (or other non-string) display spec.
                (org-table-widget-link-beautify-images
                 ;; One character, so wrapping can never split it.
                 (propertize " " 'display display))
                (t source)))
         (text (concat (or (overlay-get ov 'before-string) "")
                       body
                       (or (overlay-get ov 'after-string) "")))
         (link-props (cl-loop for prop in org-table-widget-link-beautify--link-props
                              for value = (get-text-property start prop)
                              when value append (list prop value))))
    ;; Keep the link clickable / hoverable inside the widget.
    (when link-props
      (add-text-properties 0 (length text) link-props text))
    text))

(defun org-table-widget-link-beautify--substring (start end)
  "Like `buffer-substring' from START to END, with link previews folded in."
  (let ((pos start)
        parts)
    (dolist (ov (org-table-widget-link-beautify--preview-overlays start end))
      (when (>= (overlay-start ov) pos)
        (push (buffer-substring pos (overlay-start ov)) parts)
        (push (org-table-widget-link-beautify--overlay-text ov) parts)
        (setq pos (overlay-end ov))))
    (push (buffer-substring pos end) parts)
    (apply #'concat (nreverse parts))))

(defun org-table-widget-link-beautify--line-cells ()
  "Replacement for `org-table-widget--line-cells' that sees link previews."
  (let ((end (line-end-position))
        cells)
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward " \t")
      (when (eq (char-after) ?|)
        (forward-char)
        (let ((start (point)))
          (while (re-search-forward "|" end 'move)
            (push (org-table-widget--clean-cell
                   (org-table-widget-link-beautify--substring start (1- (point))))
                  cells)
            (setq start (point)))
          (when (< start end)
            (let ((tail (org-table-widget-link-beautify--substring start end)))
              (unless (string-blank-p tail)
                (push (org-table-widget--clean-cell tail) cells)))))))
    (nreverse cells)))

(advice-add 'org-table-widget--line-cells :override
            #'org-table-widget-link-beautify--line-cells)

;;;; Re-render when previews change

(defun org-table-widget-link-beautify--rerender (&optional buffer)
  "Drop cached table renderings in BUFFER and lay its tables out again."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (bound-and-true-p org-table-widget-mode)
          (org-table-widget--invalidate-render-cache)
          ;; Debounced, so a burst of preview batches renders once.
          (org-table-widget--schedule-relayout))))))

(defun org-table-widget-link-beautify--after-queue (buffer &rest _)
  "Re-render BUFFER's tables after a batch of link previews is drawn."
  (org-table-widget-link-beautify--rerender buffer))

(defun org-table-widget-link-beautify--after-clear (&rest _)
  "Re-render tables after link previews are removed."
  (org-table-widget-link-beautify--rerender))

(with-eval-after-load 'ol
  (when (fboundp 'org-link-preview--process-queue) ; Org 9.8+
    (advice-add 'org-link-preview--process-queue :after
                #'org-table-widget-link-beautify--after-queue)
    (advice-add 'org-link-preview-clear :after
                #'org-table-widget-link-beautify--after-clear)))

(defun org-table-widget-link-beautify-unload-function ()
  "Remove the advice installed by this file."
  (advice-remove 'org-table-widget--line-cells
                 #'org-table-widget-link-beautify--line-cells)
  (advice-remove 'org-link-preview--process-queue
                 #'org-table-widget-link-beautify--after-queue)
  (advice-remove 'org-link-preview-clear
                 #'org-table-widget-link-beautify--after-clear)
  nil)

(provide 'org-table-widget-link-beautify)
;;; org-table-widget-link-beautify.el ends here
