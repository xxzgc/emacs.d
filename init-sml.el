;; {{
;; lua mode
(setq load-path (cons "~/.emacs.d/packages/sml-mode" load-path))
;; (setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'sml-mode "sml-mode" "SML editing mode." t)

(setq sml-program-name "/opt/sml/bin/sml")
;; }}

(provide 'init-sml)
