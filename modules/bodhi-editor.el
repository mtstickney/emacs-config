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

;; save some history to restore when files are opened
(setq savehist-additional-variables
      ;; save search history
      '(search ring regexp-search-ring)
      ;; save every minute
      savehist-auto-save-interval 60
      ;; keep $HOME clean
      savehist-file (concat user-emacs-directory "savehist"))
(savehist-mode t)


;; save (file) buffers on window/buffer switch
(defadvice switch-to-buffer (before save-buffer-now activate)
  (when (buffer-file-name) (save-buffer)))
(defadvice other-window (before other-window-now activate)
  (when (buffer-file-name (save-buffer))))
(defadvice windmove-up (before other-window-now activate)
  (when (buffer-file-name (save-buffer))))
(defadvice windmove-down (before other-window-now activate)
  (when (buffer-file-name) (save-buffer)))
(defadvice windmove-left (before other-window-now activate)
  (when (buffer-file-name) (save-buffer)))
(defadvice windmove-right (before other-window-now activate)
  (when (buffer-file-name) (save-buffer)))

;; Subtly hilight matching parens (global)
(show-paren-mode +1)
(setq show-paren-style 'parenthesis)

;; Hilight the current line
(global-hl-line-mode +1)

(require 'volatile-highlights)
(volatile-highlights-mode t)

;; tramp for sudo access (keep in mind zsh issues, see emacs wiki)
(require 'tramp)
(setq tramp-default-method "ssh")

;; ido-mode
(ido-mode t)
(setq ido-enable-prefix t
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ido-default-file-method 'selected-window)

;; auto-completion in the minibuffer
(icomplete-mode +1)

(set-default 'imenu-auto-rescan t)

;; Use flyspell for spellchecking
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)

;; force flyspell on (e.g. to use in hooks)
(defun bodhi-turn-on-flyspell ()
  (interactive)
  (flyspell-mode +1))

(add-hook 'message-mode-hook 'bodhi-turn-on-flyspell)
(add-hook 'text-mode-hook 'bodhi-turn-on-flyspell)

;; enable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; enable change-region-case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


(provide 'bodhi-editor)
