(autoload 'nxml-mode "rng-auto")
;; (load-library "rng-auto")

(add-to-list 'auto-mode-alist
	     (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss" "gsmarkup") t) "\\'") 
	     	   'nxml-mode))

(setq magic-mode-alist
      (cons '("<?xml " . nxml-mode) magic-mode-alist))

(fset 'xml-mode 'nxml-mode)
(fset 'html-mode 'nxml-mode)
