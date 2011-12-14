;; Conveniently rename both buffer and file. Not sure why emacs doesn't have this.
(defun rename-file-and-buffer (new-name)
  "Rename both the current buffer and the file it's visiting to NEW_NAME."
  (interactive "sNew name:")
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
	(message "A buffer named '%s' already exists!" new-name)
      (progn (rename-file name new-name 1)
	     (rename-buffer new-name)
	     (set-visited-file-name new-name)
	     (set-buffer-modified-p nil)))))

;; move buffer and file to another dir
(defun move-buffer-file (dir)
  "Move both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
	 (filename (buffer-file-name))
	 (dir
	  (if (string-match dir "\\(?:/\\|\\\\)$")
	      (substring dir 0 -1) dir))
	 (new-name (concat dir "/" name)))
    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
      (progn
	(copy-file filename newname 1)
	(delete-file filename)
	(set-visited-file-name newname)
	(set-buffer-modified-p nil) t))))

;; if visiting a file, save the buffer's content to that file (used for auto-save)
(defun save-if-visiting-file (&optional args)
  "Save the current buffer only if it's visiting a file"
  (interactive)
  (if (and (buffer-file-name) (buffer-modified-p))
      (save-buffer args)))

;; Function to open a daily journal entry in org-mode
;; Uses org-journal-file and org-journal-date-format variables
;; (see http://metajack.im/2009/01/01/journaling-with-emacs-orgmode/)
(eval-after-load "org-install"
  (defun org-journal-entry ()
    "Create a new diary entry for today or append to an existing one"
    (interactive)
    (switch-to-buffer (find-file org-journal-file))
    (widen)
    (let ((today (format-time-string org-journal-date-format))
	  (isearch-forward nil))
      (end-of-buffer)
      (unless (org-goto-local-search-headings today nil t)
	((lambda ()
	   (org-insert-heading-respect-content)
	   (insert today)
	   (insert "\n\n  "))))
      (org-show-entry)
      (org-narrow-to-subtree)
      (end-of-buffer)
      (backward-char 1)
      (unless (= (current-column) 2)
	(insert "\n  --\n\n  ")))))
