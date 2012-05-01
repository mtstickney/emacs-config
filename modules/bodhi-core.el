(require 'cl)
(require 'thingatpt)

(defun bodhi-open-with ()
  "Open the underlying file of a buffer in an external program."
  (interactive)
  (when buffer-file-name
    (shell-command (concat
                    (if (eq system-type 'darwin)
                        "open"
                      (read-shell-command "Open-current-file-with: "))
                    " "
                    buffer-file-name))))

(defun bodhi-buffer-mode (buffer-or-name)
  (with-current-buffer buffer-or-name major-mode))

(defun bodhi-visit-term-buffer ()
  (interactive)
  (if (not (get-buffer "*ansi-term*"))
      (ansi-term "/bin/bash")
    (switch-buffer "*ansi-term*")))

;; Some functions for copying blocks of code to the clipboard
;; formatted for pasting as snippets etc.
(defun bodhi-indent-rigidly-and-copy-to-clipboard (begin end indent)
  "Copy the selected code region to the clipboard, indented
  according to Markdown blockquote rules."
  (let ((buffer (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring-no-properties buffer begin end)
      (indent-rigidly (point-min) (point-max) indent)
      (clipboard-kill-ring-save (point-min) (point-max)))))

(defun bodhi-indent-and-copy-to-clipboard (begin end)
  "Copy the selected code region to the clipboard, indented
  according to Markdown blockquote rules."
  (interactive "r")
  (bodhi-indent-rigidly-and-copy-to-clipboard begin end 4))

(defun bodhi-indent-nested-and-copy-to-clipboard (begin end)
  "Copy the selected code region to the clipboard indented
  according a little extra according to Markdown blockquote
  rules (useful for pasting under a bullet point)."
  (interactive "r")
  (bodhi-indent-rigidly-and-copy-to-clipboard begin end 6))

(defun bodhi-insert-empty-line ()
  "Insert an empty line and indent according to mode."
  (interactive)
  (move-end-of-line nil)
  (open-line 1)
  (next-line 1)
  (indent-according-to-mode))

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slick-copy activate compile)
  "When called interactively with no region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defun bodhi-annotate-todo ()
  "Put a fringe marker on 'TODO:' lines in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "TODO:" nil t)
      (let ((overlay (make-overlay (- (point) 5) (point))))
        (overlay-put overlay 'before-string
                     (propertize (format "A")
                                 'display '(left-fringe right-trianle)))))))

(defun copy-file-name-to-clipboard ()
  "Copy the current file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to clipboard." filename))))

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
(defun bodhi-delete-file-and-buffer ()
  "Kills the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (delete-file filename)
      (message "Deleted file '%s'" filename)))
  (kill-buffer))

(defun bodhi-view-url ()
  "Open a buffer containing the contents of a url."
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
         (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (rename-buffer url t)
    (cond ((search-forward "<?xml" nil t) (nxml-mode))
          ;; TODO: switch to nxhtml-mode
          ((search-forward "<html" nil t) (html-mode)))))

;; if visiting a file, save the buffer's content to that file (used for auto-save)
(defun save-if-visiting-file (&optional args)
  "Save the current buffer only if it's visiting a file"
  (interactive)
  (if (and (buffer-file-name) (buffer-modified-p))
      (save-buffer args)))

(defun bodhi-indent-buffer ()
  "Indents the entire buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun bodhi-indent-region-or-buffer ()
  "Indents the selected region, or the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (bodhi-indent-buffer)
        (message "Indented buffer.")))))

(defun bodhi-enable-whitespace ()
  (whitespace-mode +1))

(defun bodhi-disable-whitespace ()
  (whitespace-mode -1))

(defun bodhi-enable-abbrev ()
  (abbrev-mode +1))

(defun bodhi-disable-abbrev ()
  (abbrev-mode -1))

(defun bodhi-untabify-buffer ()
  (untabify (point-min) (point-max)))

(defun bodhi-cleanup-buffer ()
  "Perform some cleanup ops on the current buffer's whitespace."
  (interactive)
  (bodhi-indent-buffer)
  (bodhi-untabify-buffer)
  (whitespace-cleanup))

(defun bodhi-sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not (buffer-file-name)))
      (find-file (concat "/sudo:root@localhost" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost" buffer-file-name))))

(defun bodhi-insert-date ()
  "Insert timestamp formatted according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

(defun bodhi-recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(defun bodhi-swap-windows ()
  "Swaps windows if you have exactly 2 open."
  (interactive)
  (if (/= (window-count) 2)
      (message "You need exactly 2 windows to do this.")
    (let* ((w1 (first window-list))
            (w2 (second window-list))
            (b1 (window-buffer w1))
            (b2 (window-buffer w2))
            (s1 (window-start w1))
            (s2 (window-start w1)))
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)
      (set-window-start w1 s2)
      (set-window-start w2 s1)))
  (other-window 1))

(defun bodhi-kill-other-buffers ()
  "Kills all (non-special) buffers except the current one."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))

(defun bodhi-conditionally-enable-paredit-mode ()
  "Enable paredit mode in the minibuffer, during sexpr evaluation."
  (if (eq this-command 'eval-expression)
      (paredit-mode 1)))

(add-hook 'minibuffer-setup-hook 'bodhi-conditionally-enable-paredit-mode)

(defun bodhi-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun bodhi-recompile-init ()
  "Byte compile all your dotfiles again."
  (interactive)
  (byte-recompile-directory bodhi-dir 0))

;; Function to open a daily journal entry in org-mode
;; Uses org-journal-file and org-journal-date-format variables
;; (see http://metajack.im/2009/01/01/journaling-with-emacs-orgmode/)
(eval-after-load "org-install"
)

(provide 'bodhi-core)
