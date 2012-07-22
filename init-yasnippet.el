;; {{
;; yasnippet
(add-to-list 'load-path
              "~/.emacs.d/packages/yasnippet")
(require 'yasnippet) ;; not yasnippet-bundle
;; (yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")
(yas/global-mode 1)

(require 'dropdown-list)
(setq yas/prompt-functions '(yas/dropdown-prompt
                             yas/ido-prompt
                             yas/completing-prompt))

;; }}

(provide 'init-yasnippet)
