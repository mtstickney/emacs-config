(if *on-windows*
    (progn
      (setq inferior-lisp-program "clisp.exe")
      (load "C:\\quicklisp\\slime-helper.el"))
  (progn
    (require 'slime)
    (setq inferior-lisp-program "/usr/bin/sbcl")
    (slime-setup '(slime-repl))))
