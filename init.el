;; Emacs24 Initialization file
;; The File Formerly Known As .emacs

(defun vercmp (a b)
  "Compare two lists of version components. Returns 0 if they are
they same version, 1 if A is a newer version than B, and -1 if B
is a newer version than A. Version components not included in the
list are assumed to be 0."
  (let ((cmp1 (or (cl-first a) 0))
        (cmp2 (or (cl-first b) 0)))
    (cond
     ;; same length, all equal elements (or we'd have returned earlier).
     ((and (null a) (null b)) 0)
     ((< cmp1 cmp2) -1)
     ((> cmp1 cmp2) 1)
     (t (vercmp (cl-rest a) (cl-rest b))))))

(defun emacs-ver< (target)
  (< (vercmp (list emacs-major-version emacs-minor-version) target)
     0))

(defun emacs-ver> (target)
  (> (vercmp (list emacs-major-version emacs-minor-version) target)
     0))

(defun emacs-ver= (target)
  (= (vercmp (list emacs-major-version emacs-minor-version) target)
     0))

(require 'cl-lib)

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

;; change customizations file location
(setq custom-file (concat bodhi-dir "custom.el"))

(message "The flower of awakening unfolds. Proceed with Right Understanding, %s."
         (getenv "USER"))
