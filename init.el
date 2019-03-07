;; Emacs24 Initialization file
;; The File Formerly Known As .emacs

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'cl)

(message "Bodhi approaches... Patience, %s."
         (getenv "USER"))

(defconst *on-windows*
  (or (eq system-type 'ms-dos)
      (eq system-type 'windows-nt)))

;; Keep everything under ~/emacs instead of .emacs.d
;; (defvar bodhi-dir
;;   (concat (if *on-windows*
;;               "C:/Users/mts/"
;;             "/home/mts/")
;;           "emacs/")
;;   "My home directory - root of all emacs configuration files")
(defvar bodhi-dir (if *on-windows*
                      "~/.emacs.d/"
                    "~/emacs/"))

(cl-labels ((add-path (p)
                      (add-to-list 'load-path
                                   (concat bodhi-dir p))))
  (add-path "modules")
  (add-path "thirdparty"))

(defvar bodhi-snippets-dir (concat bodhi-dir "snippets/"))

;; This must be loaded before the deft package is loaded in order for
;; variables to be set correctly.
(require 'bodhi-deft)

(require 'bodhi-packages)
(require 'bodhi-ui) ;; UI customizations
(require 'bodhi-core) ;; utility functions
(require 'bodhi-mode) ;; Mode customizations
(require 'bodhi-editor)
(require 'bodhi-keybinds) ;; key customizations

;; Programming & markup setup
(require 'bodhi-programming)
(require 'bodhi-org)
(require 'bodhi-latex)
(require 'bodhi-common-lisp)
(require 'bodhi-emacs-lisp)
(require 'bodhi-haskell)
(require 'bodhi-web)
(require 'bodhi-javascript)

;; ANSI terminal colors in slime.
(require 'slime-repl-ansi-color)

;; change customizations file location
(setq custom-file (concat bodhi-dir "custom.el"))

(message "The flower of awakening unfolds. Proceed with Right Understanding, %s."
         (getenv "USER"))
