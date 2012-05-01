(require 'progress-mode)

(add-to-list 'auto-mode-alist '("\\.p\\'" . progress-mode))
(add-to-list 'auto-mode-alist '("\\.i\\'" . progress-mode))

(provide 'bodhi-progress)
