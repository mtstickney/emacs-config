;; Org-mode customizations file
(require 'org)

;; Use the mode for .org files
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; global keybinds
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cj" 'bodhi-org-journal-entry)
(global-set-key "\C-cm" 'bodhi-org-project-journal-entry)

;; Default locations
(setq org-directory "~/Sync/core/")
(setq org-default-notes-file (concat org-directory "notes.org"))

;; Include the gtd file in agendas.
(add-to-list 'org-agenda-files (concat org-directory "gtd_playground.org"))

;;; Basic editing settings.
;; include completion timestamps for TODOs
(setq org-log-done t)
;; don't insert headings in the middle of a subtree (add them afterwards).
(setq org-insert-heading-respect-content t)
;; adjust paragraph filling/indent/etc. to match headline depth
(setq org-adapt-indentation t)

;; Use latexmk to export latex to pdf so e.g. bibtex gets run
(require 'ox-latex)
(setq org-latex-to-pdf-process '("latexmk -pdf -cd %f"))

;; Use the listings package to list code in latex
(setq org-latex-export-listings t)
(add-to-list 'org-latex-packages-alist '("" "listings"))
;; Now with color!
(add-to-list 'org-latex-packages-alist '("" "color"))

;; Journalling setup
(defvar bodhi-org-journal-file (concat org-directory "journal.org")
  "Path to org-mode journal file")
(defvar bodhi-org-journal-date-format "%Y-%m-%d"
  "Date format for journal headings")
(defvar bodhi-org-journal-time-format "%H:%M:%S"
  "Timestamp format for individual journal entries")

(defun bodhi-org-journal-entry ()
  "Create a new diary entry for today or append to an existing one"
  (interactive)
  (switch-to-buffer (find-file bodhi-org-journal-file))
  (widen)
  (let ((today (format-time-string bodhi-org-journal-date-format))
        (now (format-time-string bodhi-org-journal-time-format))
        (isearch-forward nil))
    (end-of-buffer)
    (unless (re-search-backward (concat "^\\* " today) nil t)
      (org-insert-heading-respect-content t)
      (insert today))
    (org-show-entry)
    (org-narrow-to-subtree)
    (end-of-buffer)
    (insert "\n")
    (org-indent-line)
    (insert now "\n")
    (org-indent-line)
    (insert "--\n")
    (org-indent-line)))

(defun bodhi-org-project-journal-entry ()
  "Create a new diary entry for today or append to an existing one in the current file."
  (interactive)
  (widen)
  (let ((today (format-time-string bodhi-org-journal-date-format))
        (now (format-time-string bodhi-org-journal-time-format))
        (isearch-forward nil))
    (goto-char (point-max))
    (unless (re-search-backward "^\\* Journal" nil t)
      (org-insert-heading-respect-content t)
      (insert "Journal")
      (let ((lvl (org-current-level)))
        (dotimes (i (- lvl 1))
          (org-promote)))
      (insert "\n"))
    (goto-char (point-max))
    (let ((result (re-search-backward (concat "^\\*\\* " today) nil t)))
      (unless result
        (org-insert-subheading 0)
        (insert today)))
    (org-show-entry)
    (end-of-buffer)
    (insert "\n")
    (org-indent-line)
    (insert now "\n")
    (org-indent-line)
    (insert "--\n")
    (org-indent-line)))

;; Add "resume" class to LaTeX documents (requires 'res.sty' in current dir)
(defun bodhi-new-org-latex-class (name doc-class &optional rest)
  "Return a new Org-mode LaTeX class, based on 'article' by default."
  (let* ((article-body (cddr (assoc "article" org-latex-classes))))
    (cons name (cons doc-class
                     (if rest rest article-body)))))

;; Formatter to write a default clocktable with units in hours.
(defun bodhi-write-hour-clocktable (&rest args)
  (let ((org-duration-format 'h:mm))
    (apply 'org-clocktable-write-default args)))

(add-to-list 'org-latex-classes
             (bodhi-new-org-latex-class "resume" "\\documentclass{res}"))

(defun bodhi-org-mode-defaults ()
  (electric-indent-mode -1))

(setq bodhi-org-mode-hook 'bodhi-org-mode-defaults)

(add-hook 'org-mode-hook (lambda() (run-hooks 'bodhi-org-mode-hook)))

(provide 'bodhi-org)
