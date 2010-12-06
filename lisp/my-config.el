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

;; hilight the current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "light grey")

;; display line numbers
(global-linum-mode 1)

;; Auto save more often so we don't have to
(setq auto-save-timeout 3)
(setq auto-save-interval 100)

;; use org-mode if no other mode matches (instead of fundamental)
(setq-default major-mode 'org-mode)

;; Add a timestamp when TODO items are finished in org-mode
(setq org-log-done t)
