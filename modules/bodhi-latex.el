;; LaTeX configuration file

;; AUCTeX configuration
(setq TeX-auto-save t)
(setq TeX-parse-self t)

(setq-default TeX-master nil)

;; use pdflatex
(setq TeX-PDF-mode t)

(setq TeX-view-program-selection
      '((output-dvi . "DVI Viewer")
        (output-pdf . "PDF Viewer")
        (output-html . "HTML Viewer")))

;; TODO: get this set up to work on windows
(setq TeX-view-program-list
      '(("DVI Viewer" . "xdvi %o")
        ("PDF Viewer" . "mupdf %o")
        ("HTML Viewer" . "chromium --incognito %o")))

(defun bodhi-latex-mode-defaults ()
  (turn-on-auto-fill)
  (abbrev-mode +1))

(setq bodhi-latex-mode-hook 'bodhi-latex-mode-defaults)

(add-hook 'LaTeX-mode-hook (lambda () (run-hooks 'bodhi-latex-mode-hook)))

(provide 'bodhi-latex)
