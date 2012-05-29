(require 'vbnet-mode)

;; .vbs -- VBScript
;; .vb  -- Visual Basic .NET source
;; .bas -- Basic language source
;; .frm -- Visual Basic form
;; .cls -- C++ class definition

(add-to-list 'auto-mode-alist '("\\.\\(vb\\|bas\\|frm\\|cls\\)$" . vbnet-mode))

(provide 'bodhi-vbasic)
