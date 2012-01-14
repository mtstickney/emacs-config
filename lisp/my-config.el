(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(set-default-font "Liberation Mono 11")
(setq inhibit-startup-screen t)
(setq initial-frame-alist nil)
;; (setq (menu-bar-mode nil))
;; (setq (scroll-bar-mode nil))
;; (setq (tool-bar-mode nil))
(setq c-default-style "linux")

;; stop making those annoying file~ files)
(setq make-backup-files nil)

;; hilight the current line if in graphical display (can't see the cursor with terminal colors)
(if (display-graphic-p)
    (progn
      (global-hl-line-mode t)
      (set-face-background 'hl-line "light grey")))

;; display line and col numbers
(setq global-linum-mode t)
(setq column-number-mode t)

;; Auto save more often so we don't have to
(setq auto-save-timeout 3)
(setq auto-save-interval 100)

;; Autopairs handled by ParEdit now
;; ;; enable autopairs
;; (require 'autopair)
;; (autopair-global-mode)
;; ;; Workaround for autopair+slime debug issue
;; (add-hook 'sldb-mode-hook #'(lambda () (setq autopair-dont-activate t)))

;; electric return for ParEdit
(defvar electrify-return-match
  "[\]}\)\"]"
  "If this regexp matches the text after the cursor, do an \"electric\" return.")

(defun electrify-return-if-match (arg)
  "If the text after the cursor matches `electrify-return-match' then open
and indent and empty line between the cursor and the text. Move the cursor
to a new line."
  (interactive "P")
  (let ((case-fold-search nil))
    (if (looking-at electrify-return-match)
	(save-excursion (newline-and-indent)))
    (newline arg)
    (indent-according-to-mode)))

;; Modifications to make in SEXPR modes
;; - Turns on paredit mode, eldoc mode, and show-paren mode
;; - Tells eldoc to refresh on paredit commands
;; - Enables electric return
(defun sexpr-mode-mods ()
  (paredit-mode t)
  (turn-on-eldoc-mode)
  (eldoc-add-command
   'paredit-backward-delete
   'paredit-close-round)

  (local-set-key (kbd "RET") 'electrify-return-if-match)
  (eldoc-add-command 'electrify-return-if-match)

  (show-paren-mode t))

;; Enable paredit for SEXPR editing
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)
(loop for hook in
      '(emacs-lisp-mode-hook lisp-mode-hook lisp-interaction-mode-hook scheme-mode-hook slime-repl-mode-hook clojure-mode-hook)
      do (add-hook hook #'sexpr-mode-mods))

;; Stop SLIME from grabbing DEL, which interferes with paredit
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook #'override-slime-repl-bindings-with-paredit)

;; better buffer switcher
(iswitchb-mode t)

;; always use a trailing newline
(setq require-final-newline t)

;; Solarized colors
(if window-system
    (progn
      (require 'color-theme)
      (require 'color-theme-solarized)
      (eval-after-load "color-theme"
	'(progn
	   (color-theme-initialize)
(color-theme-solarized-light)))))

;; Use tabs instead of spaces. Messes with styles, but that's what our other editors are doing.
;; CURRENTLY BREAKS TAB COMPLETION
;; (setq indent-tabs-mode t)
;; (setq-default indent-tabs-mode t)
;; (global-set-key (kbd "TAB") 'self-insert-command)
;; (setq default-tab-width 8)
;; (setq tab-width 8)
;; (setq c-basic-indent 8)
