;; Bodhi minor-mode

(defvar bodhi-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c o") 'bodhi-open-with)
    ;; (define-key map (kbd "C-c g") 'bodhi-google)
    (define-key map (kbd "C-c n") 'bodhi-cleanup-buffer)
    ;; (define-key map (kbd "C-c f") 'bodhi-recentf-ido-find-file)
    (define-key map (kbd "C-M-\\") 'bodhi-indent-region-or-buffer)
    (define-key map (kbd "C-c u") 'bodhi-view-url)
    (define-key map (kbd "C-c e") 'bodhi-eval-and-replace)
    (define-key map (kbd "C-c s") 'bodhi-swap-windows)
    (define-key map (kbd "C-c r") 'bodhi-rename-file-and-buffer)
    (define-key map (kbd "C-c t") 'bodhi-visit-term-buffer)
    (define-key map (kbd "C-c k") 'bodhi-kill-other-buffers)
    (define-key map (kbd "C-c h") 'helm-prelude)
    map)
  "Keymap for Bodhi mode")

(easy-menu-define bodhi-mode-menu bodhi-mode-map
  "Menu for Bodhi mode"
  '("Bodhi"
    ("Files"
     ["Open with..." bodhi-open-with]
     ["Delete file and buffer" bodhi-delete-file-and-buffer]
     ["Rename file and buffer" bodhi-rename-file-and-buffer])

    ("Buffers"
     ["Clean up buffer" bodhi-clean-up-buffer]
     ["Kill other buffers" bodhi-kill-other-buffers])

    ("Editing"
     ["Insert empty line" bodhi-insert-empty-line]
     ["Indent buffer" bodhi-indent-buffer]
     ["Indent region or buffer" bodhi-indent-region-or-buffer]
     ["Copy to clipboard as blockquote" bodhi-indent-and-copy-to-clipboard]
     ["Copy to clipboard as nested blockquote" bodhi-indent-nested-and-copy-to-clipboard]
     ["Insert timestamp" bodhi-insert-date]
     ["Eval and replace" bodhi-eval-and-replace])

    ("Navigation"
     ["Helm" helm-prelude])

    ("Windows"
     ["Swap windows" bodhi-swap-windows])

    ("General"
     ["Visit term buffer" bodhi-visit-term-buffer]
     ["View url" bodhi-view-url])))

;; define the minor-mode
(define-globalized-minor-mode bodhi-global-mode bodhi-mode bodhi-on)

(defun bodhi-on ()
  (bodhi-mode t))

(defun bodhi-off ()
  (bodhi-mode nil))

(define-minor-mode bodhi-mode
  "Minor mode to consolidate Bodhi extensions."
  :lighter " Bodhi"
  :keymap bodhi-mode-map
  (if bodhi-mode
      ;; on start
      (easy-menu-add bodhi-mode-menu bodhi-mode-map)
    ;; on stop
    (bodhi-off)))

(provide 'bodhi-mode)
