(defun bodhi-haskell-mode-hook ()
  (run-hooks 'bodhi-prog-mode-hook)
  (subword-mode +1)
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indentation))

(add-hook 'haskell-mode-hook 'bodhi-haskell-mode-hook)

(provide 'bodhi-haskell)
