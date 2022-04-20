(defun org-capture-refile-override ()
  "Finalize the current capture and then refile the entry.
Refiling is done from the base buffer, because the indirect buffer is then
already gone.  Any prefix argument will be passed to the refile command."
  (interactive)
  (unless (eq (org-capture-get :type 'local) 'entry)
    (user-error "Refiling from a capture buffer makes only sense \
for `entry'-type templates"))
  (let* ((base (or (buffer-base-buffer) (current-buffer)))
	 (pos (make-marker))
	 (org-capture-is-refiling t)
	 (kill-buffer (org-capture-get :kill-buffer 'local))
	 (jump-to-captured (org-capture-get :jump-to-captured 'local))
	 (refile-targets (org-capture-get :refile-targets 'local)))
    ;; Since `org-capture-finalize' may alter buffer contents (e.g.,
    ;; empty lines) around entry, use a marker to refer to the
    ;; headline to be refiled.  Place the marker in the base buffer,
    ;; as the current indirect one is going to be killed.
    (set-marker pos (save-excursion (org-back-to-heading t) (point)) base)
    ;; `org-capture-finalize' calls `org-capture-goto-last-stored' too
    ;; early.  We want to wait for the refiling to be over, so we
    ;; control when the latter function is called.
    (org-capture-put :kill-buffer nil :jump-to-captured nil)
    (let ((org-refile-targets (or refile-targets org-refile-targets)))
      (save-window-excursion
        (with-current-buffer base
	  (org-with-point-at pos
	    (call-interactively 'org-refile)))))
    (org-capture-finalize)
    (when kill-buffer
      (with-current-buffer base (save-buffer))
      (kill-buffer base))
    (when jump-to-captured (org-capture-goto-last-stored))))
