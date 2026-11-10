;;; org-forwardlinks.el --- Forward links of an Org node: sidecar + consult -*- lexical-binding: t; -*-

;; Requires: org-backlinks.el (same directory), consult, universal-sidecar.

;;; Commentary:
;;
;; Shows the links that go OUT of the current node, grouped by the node
;; they point to (the mirror image of org-backlinks.el).
;;
;; Resolved link forms:
;;   [[id:UUID]]                    -> heading/file with that :ID:
;;   [[file:x.org]] / [[./x.org]]   -> file-level node of x.org
;;   [[file:x.org::#custom-id]]     -> heading with that CUSTOM_ID
;;   [[file:x.org::*Heading]]       -> heading with that text
;;   [[#custom-id]]                 -> heading in the same file
;; Links that cannot be resolved are listed under "Unresolved".
;; Non-Org links (URLs, other file types) are ignored.
;;
;; "Node" has the same meaning as in org-backlinks.el: the closest heading
;; (self or ancestor) with an :ID:, or the file-level node.  Links inside
;; nested nodes with their own :ID: do not belong to the outer node.
;;
;; The sidecar section follows `my/org-bl-scope', so C-c b / C-c B switch
;; both the backlinks and the forward-links sections.
;;
;; Setup:
;;   (add-to-list 'universal-sidecar-sections 'org-forward-links-section t)
;;
;; Key (Org buffers):
;;   C-c n f    Consult: forward links of the node at point;
;;              with C-u, of all nodes in the file.

;;; Code:

(require 'org)
(require 'seq)
(require 'subr-x)
(require 'consult)
(require 'universal-sidecar)
(require 'org-backlinks)

;;;; Customization ----------------------------------------------------------

(defgroup my/org-fl nil
  "Org forward links."
  :group 'org)

(defcustom my/org-fl-consult-jump 'target
  "Where selecting a candidate in `my/org-forward-links' jumps to.
`target': the node the link points to.  `source': the link itself.
Unresolved links always jump to the link."
  :type '(choice (const :tag "Linked node" target)
                 (const :tag "Link location" source))
  :group 'my/org-fl)

(defface my/org-fl-unresolved
  '((t :inherit warning))
  "Heading of the group of unresolved links."
  :group 'my/org-fl)

;;;; Target index -----------------------------------------------------------
;; Per file: title, IDs, CUSTOM_IDs and headings, each with the position and
;; label of the heading they belong to.  Cached by mtime / buffer tick.

(defvar my/org-fl--target-cache (make-hash-table :test 'equal)
  "File -> (STAMP . INDEX).")

(defconst my/org-fl--target-version 1
  "Bump when the shape of the target index changes.")

(defun my/org-fl--todo-regexp ()
  "Regexp matching the TODO keywords in `org-todo-keywords', or nil."
  (let (kws)
    (dolist (seq org-todo-keywords)
      (when (consp seq)
        (dolist (k (if (memq (car seq) '(sequence type)) (cdr seq) seq))
          (when (and (stringp k) (not (string= k "|")))
            (push (replace-regexp-in-string "(.*)\\'" "" k) kws)))))
    (and kws (regexp-opt kws))))

(defun my/org-fl--clean-heading (text kwre)
  "Strip tags, TODO keyword (matched by KWRE), priority and COMMENT from TEXT."
  (let ((s (replace-regexp-in-string
            "[ \t]+:[[:alnum:]_@#%:]+:[ \t]*\\'" "" text)))
    (when kwre
      (setq s (replace-regexp-in-string
               (concat "\\`\\(?:" kwre "\\)[ \t]+") "" s)))
    (setq s (replace-regexp-in-string "\\`\\[#[A-Z0-9]+\\][ \t]*" "" s))
    (setq s (replace-regexp-in-string "\\`COMMENT[ \t]+" "" s))
    (string-trim (if (fboundp 'org-link-display-format)
                     (org-link-display-format s)
                   s))))

(defun my/org-fl--scan-targets (file)
  "Index the targets in the current buffer, which holds FILE.
Return (:title T :ids L :cids L :heads L); list items are (KEY POS LABEL)."
  (let ((case-fold-search t)
        ids cids heads title cur-pos cur-label kwre)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (setq kwre (my/org-fl--todo-regexp)
              title (or (and (re-search-forward
                              "^[ \t]*#\\+title:[ \t]*\\(.*?\\)[ \t]*$" nil t)
                             (let ((s (match-string-no-properties 1)))
                               (and (not (string-empty-p s)) s)))
                        (file-name-base file))
              cur-pos 1
              cur-label title)
        (goto-char (point-min))
        (while (re-search-forward
                (concat "^\\(\\*+\\)[ \t]+\\(.*\\)$"
                        "\\|^[ \t]*:\\(ID\\|CUSTOM_ID\\):[ \t]+\\(.+?\\)[ \t]*$")
                nil t)
          (if (match-beginning 1)
              (let ((label (my/org-fl--clean-heading
                            (match-string-no-properties 2) kwre)))
                (setq cur-pos (match-beginning 0)
                      cur-label label)
                (push (list (downcase label) cur-pos label) heads))
            (let ((entry (list (match-string-no-properties 4) cur-pos cur-label)))
              (if (string= (upcase (match-string-no-properties 3)) "ID")
                  (push entry ids)
                (push entry cids)))))))
    (list :title title
          :ids (nreverse ids)
          :cids (nreverse cids)
          :heads (nreverse heads))))

(defun my/org-fl--targets-of (file)
  "Return the (cached) target index of FILE."
  (let* ((buf (find-buffer-visiting file))
         (stamp (cons my/org-fl--target-version
                      (if buf
                          (with-current-buffer buf (buffer-chars-modified-tick))
                        (file-attribute-modification-time (file-attributes file)))))
         (cached (gethash file my/org-fl--target-cache)))
    (if (and cached (equal (car cached) stamp))
        (cdr cached)
      (let ((idx (if buf
                     (with-current-buffer buf (my/org-fl--scan-targets file))
                   (with-temp-buffer
                     (insert-file-contents file)
                     (my/org-fl--scan-targets file)))))
        (puthash file (cons stamp idx) my/org-fl--target-cache)
        idx))))

(defun my/org-fl--readable-p (file)
  (or (file-exists-p file) (find-buffer-visiting file)))

(defun my/org-fl--id-table (files)
  "Return a hash table ID -> (FILE POS LABEL) over FILES."
  (let ((tab (make-hash-table :test 'equal)))
    (dolist (f files)
      (when (my/org-fl--readable-p f)
        (dolist (e (plist-get (my/org-fl--targets-of f) :ids))
          (unless (gethash (car e) tab)
            (puthash (car e) (list f (nth 1 e) (nth 2 e)) tab)))))
    tab))

;;;; Resolving and collecting -----------------------------------------------

(defun my/org-fl--resolve (e self idtab)
  "Return (FILE POS LABEL) for the link entry E, or nil if unresolved.
SELF is the truename of the file containing E; IDTAB maps IDs to targets."
  (pcase (plist-get e :kind)
    ('id (gethash (plist-get e :key) idtab))
    ('internal
     (when-let* ((hit (assoc (plist-get e :cid)
                             (plist-get (my/org-fl--targets-of self) :cids))))
       (list self (nth 1 hit) (nth 2 hit))))
    ('file
     (let ((f (plist-get e :file)))
       (when (my/org-fl--readable-p f)
         (let ((idx (my/org-fl--targets-of f))
               (cid (plist-get e :cid))
               (search (plist-get e :search)))
           (cond
            (cid
             (when-let* ((hit (assoc cid (plist-get idx :cids))))
               (list f (nth 1 hit) (nth 2 hit))))
            ((and search (string-prefix-p "*" search))
             (when-let* ((hit (assoc (downcase (string-trim (substring search 1)))
                                     (plist-get idx :heads))))
               (list f (nth 1 hit) (nth 2 hit))))
            (t (list f 1 (plist-get idx :title))))))))))

(defun my/org-fl--collect (pin)
  "Return (:groups GROUPS :title TITLE :count N) for the current buffer.
PIN nil means all links in the file; otherwise PIN is a position and only the
node owning it counts.  GROUPS is a list of (KEY (TARGET . ENTRY)...), in
order of first appearance, with unresolved links last (KEY nil).  TARGET is
(FILE POS LABEL) or nil."
  (let* ((self (file-truename (buffer-file-name (buffer-base-buffer))))
         (owner (and pin (my/org-bl--owner pin)))
         (node (and pin owner))
         (beg 1)
         (end (save-excursion
                (save-restriction
                  (widen)
                  (if node
                      (progn (goto-char owner) (org-end-of-subtree t) (point))
                    (point-max)))))
         (title (save-excursion
                  (save-restriction
                    (widen)
                    (if node
                        (progn (goto-char owner) (org-get-heading t t t t))
                      (my/org-bl--file-title)))))
         (entries
          (seq-filter
           (lambda (e)
             (let ((p (plist-get e :pos)))
               (and (>= p (if node owner beg))
                    (<= p end)
                    (or (null pin) (eql (my/org-bl--owner p) owner)))))
           (my/org-bl--entries self)))
         (idtab (if (seq-some (lambda (e) (eq (plist-get e :kind) 'id)) entries)
                    (my/org-fl--id-table (my/org-bl--files self))
                  (make-hash-table :test 'equal)))
         (pairs (mapcar (lambda (e) (cons (my/org-fl--resolve e self idtab) e))
                        entries))
         (groups (seq-group-by
                  (lambda (p)
                    (let ((tg (car p)))
                      (and tg (cons (nth 0 tg) (nth 1 tg)))))
                  pairs)))
    (setq groups
          (sort groups
                (lambda (a b)
                  (cond ((null (car a)) nil)
                        ((null (car b)) t)
                        (t (< (plist-get (cdr (cadr a)) :pos)
                              (plist-get (cdr (cadr b)) :pos)))))))
    (list :groups groups :title title :count (length pairs))))

;;;; Sidecar ----------------------------------------------------------------

(defun my/org-fl--row (e src)
  "Return a clickable sidecar row for link entry E located in file SRC."
  (my/org-bl--clickable
   (concat
    (propertize (format "L%d" (plist-get e :line)) 'face 'my/org-bl-location)
    " "
    (if-let* ((h (plist-get e :heading)))
        (propertize (format "[%s] " h) 'face 'my/org-bl-source-heading)
      "")
    (my/org-bl--render-text (plist-get e :raw) (plist-get e :col)))
   src (plist-get e :pos) "Jump to link"))

(universal-sidecar-define-section org-forward-links-section
    (title)
    (:major-modes org-mode :predicate (buffer-file-name))
  (let* ((pos (and (eq my/org-bl-scope 'node)
                   (if-let* ((w (get-buffer-window buffer)))
                       (window-point w)
                     (with-current-buffer buffer (point)))))
         (res (with-current-buffer buffer (my/org-fl--collect pos)))
         (groups (plist-get res :groups))
         (src (with-current-buffer buffer
                (file-truename (buffer-file-name (buffer-base-buffer))))))
    (when groups
      (universal-sidecar-insert-section org-forward-links-section
          (format "%s (%d)"
                  (or title
                      (if pos
                          (format "Forward links: %s" (plist-get res :title))
                        "Forward links: all nodes"))
                  (plist-get res :count))
        (dolist (group groups)
          (let ((target (car (cadr group)))
                (n (length (cdr group))))
            (if target
                (pcase-let ((`(,tfile ,tpos ,tlabel) target))
                  (insert (my/org-bl--clickable
                           (propertize tlabel 'face 'my/org-bl-node)
                           tfile tpos "Visit this node")
                          (my/org-bl--mirror-faces
                           (propertize (format " %s" (file-name-nondirectory tfile))
                                       'face 'my/org-bl-location))
                          (my/org-bl--mirror-faces
                           (propertize (format " (%d)" n) 'face 'my/org-bl-count))
                          "\n"))
              (insert (my/org-bl--mirror-faces
                       (propertize (format "Unresolved (%d)" n)
                                   'face 'my/org-fl-unresolved))
                      "\n"))
            (dolist (pair (cdr group))
              (insert "  " (my/org-fl--row (cdr pair) src) "\n"))
            (insert "\n")))))))

;;;; Consult ----------------------------------------------------------------

(defun my/org-fl--consult (buf pin)
  "Search forward links with `consult' for the node owning PIN in BUF.
PIN is a position in BUF; nil means all nodes of the file visited by BUF."
  (let* ((res (with-current-buffer buf (my/org-fl--collect pin)))
         (groups (plist-get res :groups))
         (title (plist-get res :title))
         (self (with-current-buffer buf
                 (file-truename (buffer-file-name (buffer-base-buffer)))))
         (bufs (make-hash-table :test 'equal))
         cands)
    (unless groups (user-error "No forward links found"))
    (dolist (group groups)
      (dolist (pair (cdr group))
        (let* ((target (car pair))
               (e (cdr pair))
               (to-target (and target (eq my/org-fl-consult-jump 'target)))
               (jfile (if to-target (nth 0 target) self))
               (jpos (if to-target (nth 1 target) (plist-get e :pos)))
               (fbuf (or (gethash jfile bufs)
                         (puthash jfile (my/org-bl-find-file-quietly jfile)
                                  bufs)))
               (marker (with-current-buffer fbuf (copy-marker jpos))))
          (push (my/org-bl--mirror-faces
                 (propertize
                  (concat
                   (if target
                       (propertize (nth 2 target) 'face 'my/org-bl-node)
                     (propertize "unresolved" 'face 'my/org-fl-unresolved))
                   (if target
                       (propertize (format " %s" (file-name-nondirectory (nth 0 target)))
                                   'face 'my/org-bl-location)
                     "")
                   (propertize "  ← " 'face 'my/org-bl-count)
                   (propertize (format "L%d " (plist-get e :line))
                               'face 'my/org-bl-location)
                   (if-let* ((h (plist-get e :heading)))
                       (propertize (format "[%s] " h) 'face 'my/org-bl-source-heading)
                     "")
                   (my/org-bl--render-text (plist-get e :raw) (plist-get e :col)))
                  'consult--candidate marker
                  'my-group (if target
                                (file-name-nondirectory (nth 0 target))
                              "Unresolved")))
                cands))))
    (consult--jump
     (consult--read
      (nreverse cands)
      :prompt (if pin
                  (format "Forward links (%s): " title)
                "Forward links (all nodes): ")
      :require-match t
      :sort nil
      :category 'org-forward-link
      :group (lambda (cand transform)
               (if transform
                   cand
                 (get-text-property 0 'my-group cand)))
      :lookup #'consult--lookup-candidate
      :state (consult--jump-state)))))

;;;###autoload
(defun my/org-forward-links (&optional whole-file)
  "Search the links going out of the node at point with `consult'.
Candidates are the nodes the links point to.  With prefix arg WHOLE-FILE,
use all links in the file.  See `my/org-fl-consult-jump'."
  (interactive "P")
  (unless (derived-mode-p 'org-mode) (user-error "Not in an Org buffer"))
  (unless (buffer-file-name (buffer-base-buffer))
    (user-error "Buffer is not visiting a file"))
  (my/org-fl--consult (current-buffer) (unless whole-file (point))))

;;;; Keys -------------------------------------------------------------------

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c n f") #'my/org-forward-links))

(provide 'org-forwardlinks)
;;; org-forwardlinks.el ends here
