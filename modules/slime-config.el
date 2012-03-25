;; NOTE: the slime-helper.el does autoloads on its own
(if *on-windows*
    (progn
      (setq inferior-lisp-program "clisp.exe")
      (load "C:\\quicklisp\\slime-helper.el"))
  (progn
    (setq inferior-lisp-program "/usr/bin/sbcl")
    (load (expand-file-name "~/quicklisp/slime-helper.el"))))
