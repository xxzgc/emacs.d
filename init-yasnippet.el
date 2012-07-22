;; {{
;; yasnippet
(add-to-list 'load-path
              "~/.emacs.d/packages/yasnippet")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")
;; }}

(provide 'init-yasnippet)
