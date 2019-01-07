;; Use org-mode as the text format
(setq deft-extensions '("org" "txt" "text" "md" "markdown"))
(setq deft-text-mode 'org-mode)
(setq deft-directory "~/Sync/notes")
;; Don't use the timestamp-based default format. This is a) different
;; than all the other notes files we have, and b) broken on windows
;; because a colon in a filename denotes an alternate NTFS stream.
(setq deft-new-file-format "deft")

;; global notes key
(global-set-key [f8] 'deft)

(provide 'bodhi-deft)
