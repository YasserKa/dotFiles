;; https://zck.org/deleting-files-in-emacs
(defun my/delete-visited-file (buffer-name)
  "Delete the file visited by the buffer named BUFFER-NAME."
  (interactive "bDelete file visited by buffer ")
  (let* ((buffer (get-buffer buffer-name))
         (filename (buffer-file-name buffer)))
    (when buffer
      (when (and filename
                 (file-exists-p filename))
        (delete-file filename))
      (kill-buffer buffer))))

(defun my/scratch-buffer-other ()
  "Open scratch buffer in another window"
  (interactive)
  (split-window-vertically)
  (other-window 1)
  (scratch-buffer))

(provide 'functions)
