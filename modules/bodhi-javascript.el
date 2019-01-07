(require 'js2-mode)
(require 'web-beautify)
(require 'js2-refactor)
(require 'xref-js2)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(eval-after-load 'js2-mode
  '(progn
     (defun bodhi-js-mode-defaults ()
       (js2-imenu-extras-mode))
     (setq bodhi-js-mode-hook 'bodhi-js-mode-defaults)
     (add-hook 'js2-mode (lambda () (run-hooks 'bodhi-js-mode-hook)))))

(defun setup-js2-refactoring ()
  (js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-r")
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill))

(add-hook 'bodhi-js-mode-hook #'setup-js2-refactoring)

(defun setup-js2-xref ()
  ;; js2 is based on js-mode, which binds "M-."; this conflicts with
  ;; xref, so unbind it.
  (define-key js-mode-map (kbd "M-.") nil)
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))

(add-hook 'bodhi-js-mode-hook #'setup-js2-xref)

(defun setup-web-beautify ()
  (define-key js2-mode-map (kbd "C-c p") 'web-beautify-js)
  ;; This is appended because the prog-mode hook is going to be doing
  ;; whitespace cleanup, which might conflict with the beautifier's
  ;; settings.
  (add-hook 'before-save-hook 'web-beautify-js-buffer t t))

(add-hook 'bodhi-js-mode-hook #'setup-web-beautify)

(provide 'bodhi-javascript)
