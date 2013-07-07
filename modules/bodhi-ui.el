;; UI enhancements file
;; No toolbar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; font selection
(if *on-windows*
    (set-default-font "Consolas 10")
  (set-default-font "Monaco 10")) ;; might use "Liberation Mono 10"

;; Setup for new frames
;; Remove the menubar, except on OSX (doesn't make sense)
(defun bodhi-frame-config (frame)
  (if (eq system-type 'darwin)
      (with-selected-frame frame
        (if (display-graphic-p)
            (modify-frame-parameters frame '((menu-bar-lines . 1)))
          (modify-frame-parameters frame '((menu-bar-lines . 0)))))
    (menu-bar-mode -1)))

;; configure the current frame...
(bodhi-frame-config (selected-frame))
;; ...and on any future frames
(add-hook 'after-make-frame-functions 'bodhi-frame-config)

;; Don't blink the cursor
(blink-cursor-mode -1)

;; No startup screen
(setq inhibit-startup-screen t)

;; Scrolling improvements
(setq scroll-margin 0
      scroll-conservatively 101 ;; Never recenter point after scrolling offscreen
      scroll-preserve-screen-position 1) ;; always preserve point's screen position

;; Mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Reduce the size of the gutter
(if (fboundp 'fringe-mode)
    (fringe-mode 4))

;; Enable y/n responses for yes/no questions
(fset 'yes-or-no-p 'y-or-n-p)

;; Show the file or buffer name in the frame title
(setq frame-title-format
      '("" invocation-name " Bodhi - " (:eval (if (buffer-file-name)
                                                  (abbreviate-file-name (buffer-file-name))
                                                "%b"))))

;; Load a theme (should be installed by package)
;; Use dark variant on terminals
;; (load-theme (if (display-graphic-p)
;;              'solarized-light
;;            'solarized-dark)
;;          t)
(if (display-graphic-p)
    (load-theme 'solarized-light t))

(provide 'bodhi-ui)
