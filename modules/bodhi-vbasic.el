(require 'visual-basic-mode)

;; .vbs -- VBScript
;; .vb  -- Visual Basic .NET source
;; .bas -- Basic language source
;; .frm -- Visual Basic form
;; .cls -- C++ class definition

(loop for e in (list "\\.vbs\\'" "\\.vb\\'" "\\.bas\\'" "\\.frm\\'" "\\.cls\\'")
      do (add-to-list 'auto-mode-alist (cons e 'visual-basic-mode)))

(provide 'bodhi-vbasic)
