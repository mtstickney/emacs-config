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

;; use org-mode if no other mode matches (instead of fundamental)
(setq-default major-mode 'org-mode)
;; Add a timestamp when TODO items are finished in org-mode
(setq org-log-done t)

;; Use Dropbox for default org directory
(setq org-directory "~/Dropbox")
(setq org-default-notes-file (concat org-directory "/notes.org"))

;; Insert new org-mode heading after current content body
(setq org-insert-heading-respect-content t)

;; enable autopairs
(require 'autopair)
(autopair-global-mode)

;; better buffer switcher
(iswitchb-mode t)

;; always use a trailing newline
(setq require-final-newline t)

;; don't put blank lines in front of headings and lists
(setq org-blank-before-new-entry '((heading) (plain-list-item)))

;; Use tabs instead of spaces. Messes with styles, but that's what our other editors are doing.
;; CURRENTLY BREAKS TAB COMPLETION
;; (setq indent-tabs-mode t)
;; (setq-default indent-tabs-mode t)
;; (global-set-key (kbd "TAB") 'self-insert-command)
;; (setq default-tab-width 8)
;; (setq tab-width 8)
;; (setq c-basic-indent 8)

;; Variables for org-mode jounraling (see org-journal-entry in efuncs.el)
(defvar org-journal-file "~/Dropbox/journal.org"
  "Path to org-mode journal file.")
(defvar org-journal-date-format "%d-%m-%Y"
  "Date format for journal headings")
