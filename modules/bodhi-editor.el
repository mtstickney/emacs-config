;; Editor behavior customizations file
;; customization group
(defgroup editor nil
  "Emacs Bodhi Editor enhancements"
  :group 'bodhi)

;; Tabs and spaces
(setq-default indent-tabs-mode nil) ;; indent with spaces instead of tabs
(setq-default tab-width 8)          ;; but make it look ok when tabs are used

;; Delete selection by typing
(delete-selection-mode t)

;; Store all backup files in the temporary directory
;; Note: this defaults to /tmp/, which is a tmpfs mount
;; any stuff that gets stored there will be blown away on restart.
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

;; Auto-save directly to the actual file, instead of separately
(setq auto-save-file-name-transforms nil)
(setq auto-save-visited-file-name t)
(setq auto-save-timeout 3) ;; every 3 seconds
(setq auto-save-interval 100) ;; ...or 100 chars, whichever comes first

;; Smart indenting and pairing
(electric-pair-mode t)
(electric-indent-mode t)
(electric-layout-mode t)

;; Auto-revert buffers when the underlying file changes
(global-auto-revert-mode t)

;; hippie expand >> dabbrev expand
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; Meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ;; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ;; don't change special buffers

(provide 'bodhi-editor)
