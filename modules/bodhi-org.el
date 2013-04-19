;; Org-mode customizations file

;; Use the mode for .org files
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; global keybinds
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cj" 'bodhi-org-journal-entry)

;; Default locations
(setq org-directory
      (if *on-windows*
          "C:/Users/mts/Dropbox/"
        "~/sync/"))
(setq org-default-notes-file (concat org-directory "notes.org"))

(setq org-log-done t)
(setq org-insert-heading-respect-content t)
;; Use latexmk to export latex to pdf so e.g. bibtex gets run
(require 'org-latex)
(setq org-latex-to-pdf-process '("latexmk -pdf -cd %f"))

;; Use the listings package to list code in latex
(setq org-latex-export-listings t)
(add-to-list 'org-export-latex-packages-alist '("" "listings"))
;; Now with color!
(add-to-list 'org-export-latex-packages-alist '("" "color"))

;; Journalling setup
(defvar bodhi-org-journal-file (concat org-directory "journal.org")
  "Path to org-mode journal file")
(defvar bodhi-org-journal-date-format "%d-%m-%Y"
  "Date format for journal headings")

(defun bodhi-org-journal-entry ()
  "Create a new diary entry for today or append to an existing one"
  (interactive)
  (switch-to-buffer (find-file bodhi-org-journal-file))
  (widen)
  (let ((today (format-time-string bodhi-org-journal-date-format))
        (isearch-forward nil))
    (end-of-buffer)
    (unless (org-goto-local-search-headings today nil t)
      (org-insert-heading-respect-content t)
      (insert today)
      (insert "\n\n  "))
    (org-show-entry)
    (org-narrow-to-subtree)
    (end-of-buffer)
    (backward-char 1)
    (unless (= (current-column) 2)
      (insert "\n  --\n\n  "))))

;; Add "resume" class to LaTeX documents (requires 'res.sty' in current dir)
(defun bodhi-new-org-latex-class (name doc-class &optional rest)
  "Return a new Org-mode LaTeX class, based on 'article' by default."
  (let* ((article-body (cddr (assoc "article" org-export-latex-classes))))
    (cons name (cons doc-class
                     (if rest rest article-body)))))

(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))
(add-to-list 'org-export-latex-classes
             (bodhi-new-org-latex-class "resume" "\\documentclass{res}"))

(defun bodhi-org-mode-defaults ()
  (electric-indent-mode -1))

(setq bodhi-org-mode-hook 'bodhi-org-mode-defaults)

(add-hook 'org-mode-hook (lambda() (run-hooks 'bodhi-org-mode-hook)))

(provide 'bodhi-org)
