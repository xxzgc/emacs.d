;; {{
;; auto completion mode
;; (add-to-list 'load-path "~/.emacs.d/packages/popup/")
(require 'popup)

;; (add-to-list 'load-path "~/.emacs.d/packages/auto-complete/")

(require 'auto-complete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict/")
(ac-config-default)

(auto-complete-mode t)

;; (add-hook 'after-init-hook 'global-company-mode)

;; }}

(provide 'init-autocomplete)
