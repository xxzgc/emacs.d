;; (add-to-list 'load-path "~/.emacs.d/packages/highlight-symbol/")

(require 'highlight)
(require 'highlight-symbol)

(setq hlt-auto-faces-flag 1)
(setq highlight-symbol-idle-delay 1)
(set-face-background 'highlight-symbol-face "gray36")

(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)

(provide 'init-highlight)
