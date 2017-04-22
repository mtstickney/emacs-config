(require 'deft)

;; Use org-mode as the text format
(setq deft-extensions '("org" "txt" "text" "md" "markdown"))
(setq deft-text-mode 'org-mode)
(setq deft-directory (if (file-exists-p "~/Dropbox")
                        "~/Dropbox/notes"
                      "~/sync/notes"))

;; global notes key
(global-set-key [f8] 'deft)

(provide 'bodhi-deft)
