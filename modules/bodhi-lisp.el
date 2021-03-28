;; Common code for lisp modes (see bodhi-common-lisp and bodhi-emacs-lisp)
(progn
  (define-key read-expression-map (kbd "TAB") 'completion-at-point)

  ;; electric return for ParEdit
  (defvar electrify-return-match
    "[\]}\)\"]"
    "If this regexp matches the text after the cursor, do an \"electric\" return.")

  (defun electrify-return-if-match (arg)
    "If the text after the cursor matches
    `electrify-return-match' then open and indent and insert and
    empty line between the cursor and the text. Move the cursor
    to a new line."
    (interactive "P")
    (let ((case-fold-search nil))
      (if (looking-at electrify-return-match)
          (save-excursion (newline-and-indent)))
      (newline arg)
      (indent-according-to-mode)))

  (defun bodhi-lisp-coding-defaults ()
    (paredit-mode +1)
    (local-set-key (kbd "RET") 'electrify-return-if-match))

  (setq bodhi-lisp-coding-hook 'bodhi-lisp-coding-defaults)

  ;; Interactive mode doesn't need a whitespace check
  (defun bodhi-interactive-lisp-coding-defaults ()
    (paredit-mode +1)
    (bodhi-disable-whitespace)
    (local-set-key (kbd "RET") 'electrify-return-if-match))

  (setq bodhi-interactive-lisp-coding-hook 'bodhi-interactive-lisp-coding-defaults))

(provide 'bodhi-lisp)
