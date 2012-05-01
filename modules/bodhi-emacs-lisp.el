(progn
  (require 'bodhi-lisp)

  ;; .elc probably isn't valid after save
  (defun bodhi-remove-elc-on-save ()
    "If you're saving an elisp file, the .elc probably isn't still valid."
    (make-local-variable 'after-save-hook)
    (add-hook 'after-save-hook
              (lambda () (if (file-exists-p (concat buffer-file-name "c"))
                             (delete-file (concat buffer-file-name "c"))))))

  (defun bodhi-emacs-lisp-mode-defaults ()
    (run-hooks 'bodhi-lisp-programming-hook)
    (turn-on-eldoc-mode)
    (eldoc-add-command
     'paredit-backward-delete
     'paredit-close-round
     'electrify-return-if-match)
    (bodhi-remove-elc-on-save)
    (rainbow-mode +1))

  (setq bodhi-emacs-lisp-mode-hook 'bodhi-emacs-lisp-mode-defaults)

  (add-hook 'emacs-lisp-mode-hook (lambda () (run-hooks 'bodhi-emacs-lisp-mode-hook)))

  (define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point))

(provide 'bodhi-emacs-lisp)
