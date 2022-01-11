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
(auto-save-visited-mode)
;; This used to be 3 seconds, but that produces magic behavior on Windows
(setq auto-save-timeout 4) ;; every 4 seconds
(setq auto-save-interval 100) ;; ...or 100 chars, whichever comes first

;; No bings on failed searches, dammit.
;; (setq ring-bell-function 'ignore)
(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "#F2804F")
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))

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

;; Use company-mode for completions.
(global-company-mode)
;; Use the usual navigation keys in the completions window.
(define-key company-active-map (kbd "\C-n") 'company-select-next)
(define-key company-active-map (kbd "\C-p") 'company-select-previous)
(define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
(define-key company-active-map (kbd "M-.") 'company-show-location)

;; Meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ;; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ;; don't change special buffers

;; save some history to restore when files are opened
(setq savehist-additional-variables
      ;; save search history
      '(search-ring regexp-search-ring)
      ;; save every minute
      savehist-auto-save-interval 60
      ;; keep $HOME clean
      savehist-file (concat user-emacs-directory "savehist"))
(savehist-mode t)


;; save (file) buffers on window/buffer switch
(defadvice switch-to-buffer (before save-buffer-now activate)
  (when (buffer-file-name) (save-buffer)))
(defadvice other-window (before other-window-now activate)
  (when (buffer-file-name) (save-buffer)))
(defadvice windmove-up (before other-window-now activate)
  (when (buffer-file-name) (save-buffer)))
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

(require 'expand-region)

;; Use autofill for all text and text-like modes
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; yasnippet for templates
(require 'yasnippet)
(add-to-list 'yas/snippet-dirs bodhi-snippets-dir)
(yas/global-mode 1)

;; projectile is a project management system
(require 'projectile)
(projectile-global-mode t)

(require 'helm-misc)
(require 'helm-projectile)

(defun helm-prelude ()
  "Preconfigured `helm'."
  (interactive)
  (if (projectile-get-project-root)
      ;; add project files and buffer when in a project
      (helm-other-buffer '(helm-c-source-projectile-files-list
                           helm-c-source-projectile-buffers-list
                           helm-c-source-buffers-list
                           helm-c-source-recentf
                           helm-c-source-buffer-not-found)
                         "*helm-prelude*")
    ;; otherwise fallback to helm-mini
    (helm-mini)))

;; shorter ack-and-a-half aliases
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

;; reuse the same buffer in dired by pressing 'a'
(put 'dired-find-alternate-file 'disabled nil)

;; don't start a new frame for ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; auto-cleanup obsolete buffers
(require 'midnight)

;; abbrev setup
(add-hook 'text-mode-hook 'bodhi-enable-abbrev)

;; make shell scripts executable on save
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; better regex syntax
(require 're-builder)
(setq reb-re-syntax 'string)

;; enable Bodhi keybindings
(bodhi-global-mode t)

;; Globally swap () and [].
(defun swap-keys (key1 key2)
  (define-key key-translation-map (kbd key1) (kbd key2))
  (define-key key-translation-map (kbd key2) (kbd key1)))

(swap-keys "(" "[")
(swap-keys ")" "]")

(provide 'bodhi-editor)
