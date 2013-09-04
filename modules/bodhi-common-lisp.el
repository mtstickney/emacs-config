(require 'bodhi-lisp)

;; SBCL config file and .stumpwmrc are lisp files
(loop for m in (list
                '("\\.sbclrc$" . lisp-mode)
                '("\\.stumpwmrc$" . lisp-mode))
      do (add-to-list 'auto-mode-alist m))

;; Use SLIME from quicklisp
(defun bodhi-load-common-lisp-slime ()
  (interactive)
  (let ((slime-helper-file
         (expand-file-name "~/quicklisp/slime-helper.el")))
    (if (file-exists-p slime-helper-file)
        (load-file slime-helper-file)
      (message "%s" "SLIME is not installed. Use Quicklisp to install it."))))

;; A list of possible Lisp implementations for SLIME. If SLIME is
;; invoked with a negative prefix (M-- M-x slime) the program can be
;; selected from a list. Note that inferior-lisp-program is now
;; useless.
(setq slime-lisp-implementations
      (cons (if *on-windows*
                '(ccl ("wx86cl"))
                '(ccl ("ccl")))
            '((clisp ("clisp" "-q"))
              (cmucl ("cmucl" "-quiet"))
              (sbcl ("sbcl" "--noinform") :coding-system utf-8-unix))))

(if (eq system-type 'darwin)
    (setq slime-default-lisp 'ccl)
  (setq slime-default-lisp 'sbcl))

(add-hook 'lisp-mode-hook (lambda ()
                            (bodhi-load-common-lisp-slime)
                            (run-hooks 'bodhi-lisp-coding-hook)))
(add-hook 'slime-repl-mode-hook (lambda ()
                                  (run-hooks 'bodhi-interactive-lisp-coding-hook)))

;; start slime automatically when a lisp file is opened
(defun bodhi-start-slime ()
  (unless (slime-connected-p)
    (save-excursion (slime))))

(add-hook 'slime-mode-hook 'bodhi-start-slime)

;; Keep SLIME's repl from grabbing DEL (interferes with paredit)
(defun bodhi-override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))

(eval-after-load "slime"
  '(progn
     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol
           slime-fuzzy-completion-in-place t
           slime-enable-evaluate-in-emacs t
           slime-autodoc-use-multiline-p t)
     (define-key slime-mode-map (kbd "TAB") 'slime-indent-and-complete-symbol)
     (define-key slime-mode-map (kbd "C-c i") 'slime-inspect)
     (define-key slime-mode-map (kbd "C-c C-s") 'slime-selector)))

(provide 'bodhi-common-lisp)
