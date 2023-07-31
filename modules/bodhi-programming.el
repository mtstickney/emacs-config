;; Generic programming enhancements file

;; Use ido menu (used for buffer switching) and imenu to navigate to
;; functions/vars/symbols
(require 'imenu)

(defun bodhi-ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place with Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          (ido-enable-flex-matching
           (if (boundp 'ido-enable-flex-matching)
               ido-enable-flex-matching
             t))
          name-and-pos symbol-names position)
      (unless ido-mode
        (ido-mode 1)
        (setq ido-enable-flex-matching t))
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (bodhi-ido-goto-symbol (imenu--make-index-alist))
               (setq selected-symbol (ido-completing-read "Symbol? " symbol-names))
               (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
        (push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position)))))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (bodhi-ido-goto-symbol symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null name) (null position))
          (add-to-list 'symbol-names name)
          (add-to-list 'name-and-pos (cons name position))))))))

;; Autofill comments
(defun bodhi-local-comment-autofill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

;; Hilight import words in comments
(defun bodhi-add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|\\FIXME\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))

;; Show the name of the current function in the modeline
(require 'which-func)
(which-function-mode 1)

;; Most programming modes derive from prog-mode in Emacs 24;
;; for modes that don't, bodhi-prog-mode-hook will be run directly.
;; To modify these defaults, add you own prog-mode hook like so:
;;       (add-hook 'bodhi-prog-mode-hook 'my-prog-mode-mods t)
;; (the final 't' sets the *append* argument)

(defun bodhi-whitespace-cleanup ()
  "Call whitespace-cleanup unless this is a Makefile."
  (interactive)
  (unless (or (eq major-mode 'makefile-mode)
              (eq major-mode 'makefile-gmake-mode)
              (eq major-mode 'makefile-automake-mode)
              (eq major-mode 'makefile-bsdmake-mode))
    (whitespace-cleanup)))

(defun bodhi-prog-mode-defaults ()
  "Default coding hook, useful with (almost) any programming language"
  (flyspell-prog-mode)
  (bodhi-local-comment-autofill)
  (bodhi-add-watchwords)
  ;; Maintain whitespace cleanliness
  (add-hook 'before-save-hook 'bodhi-whitespace-cleanup nil t))

(setq bodhi-prog-mode-hook 'bodhi-prog-mode-defaults)

(add-hook 'prog-mode-hook (lambda () (run-hooks 'bodhi-prog-mode-hook)))

(provide 'bodhi-programming)
