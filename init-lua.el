;; {{
;; lua mode
(setq load-path (cons "~/.emacs.d/packages/lua-mode" load-path))
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
;; }}

(provide 'init-lua)
