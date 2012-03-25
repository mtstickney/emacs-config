;; Packages setup and customization
(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(defvar bodhi-packages
  '(solarized-theme zenburn-theme paredit org auctex python go-mode slime
		    fill-column-indicator)
  "A list of packages to ensure are installed at launch.")

(defun bodhi-packages-installed-p ()
  (reduce (lambda (a b) (and a b))
   (map 'list #'package-installed-p bodhi-packages)))

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
