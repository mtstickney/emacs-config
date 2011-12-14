;; Org-Mode junk (doesn't get loaded until after org-mode)
(eval-after-load "org-install"
  (progn
    ;; use org-mode if no other mode matches (instead of fundamental)
    (setq-default major-mode 'org-mode)
    ;; Add a timestamp when TODO items are finished in org-mode
    (setq org-log-done t)

    ;; Use Dropbox for default org directory
    (setq org-directory "~/sync")
    (setq org-default-notes-file (concat org-directory "/notes.org"))

    ;; Insert new org-mode heading after current content body
    (setq org-insert-heading-respect-content t)

    ;; don't put blank lines in front of headings and lists
    (setq org-blank-before-new-entry '((heading) (plain-list-item)))

    ;; Add "resume" document class to LaTeX list (needs res.sty in the local dir)
    ;; Note that new-org-latex-class is our func, see efuncs.el
    (add-to-list 'org-export-latex-classes
		 (new-org-latex-class "resume" "\\documentclass{res}"))

    ;; Variables for org-mode journaling (see org-journal-entry in efuncs.el)
    (defvar org-journal-file "~/sync/journal.org"
      "Path to org-mode journal file.")
    (defvar org-journal-date-format "%d-%m-%Y"
      "Date format for journal headings")
    ;; Use latexmk to export latex files, so bibtex etc. gets run
    (setq org-latex-to-pdf-process '("latexmk -pdf -cd %f"))

    ;; Use the listings package to export code listings
    (require 'org-latex)
    (setq org-export-latex-listings t)
    (add-to-list 'org-export-latex-packages-alist '("" "listings"))))
