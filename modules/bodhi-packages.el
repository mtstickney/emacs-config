;; Packages setup and customization
(require 'package)
(dolist (repo '(("marmalade" . "https://marmalade-repo.org/packages/")
                ("melpa" . "http://melpa.org/packages/")))
  (add-to-list 'package-archives repo t))
(package-initialize)

(defvar bodhi-packages
  ;; Note that slime used to be installed by quicklisp, since the
  ;; versions in the package repo were old, but packages like
  ;; slime-company depend on the slime package, so there's really no
  ;; getting away from it.
  '(solarized-theme zenburn-theme paredit org auctex python go-mode php-mode
                    fill-column-indicator rainbow-mode volatile-highlights
                    expand-region gist helm helm-projectile magit
                    yasnippet deft fill-column-indicator
                    haskell-mode web-mode
                    web-beautify js2-mode js2-refactor xref-js2
                    slime
                    company slime-company
                    writeroom-mode
                    markdown-mode edit-indirect
                    elm-mode)
  "A list of packages to ensure are installed at launch.")

(defun bodhi-packages-installed-p ()
  (cl-reduce (lambda (a b) (and a b))
   (cl-map 'list #'package-installed-p bodhi-packages)))

(unless (bodhi-packages-installed-p)
  ;; Check for new packages (new versions)
  (message "%s" "Config is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; Install missing packages
  (dolist (p bodhi-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(provide 'bodhi-packages)
