;; {{
;; yasnippet
(add-to-list 'load-path
              "~/.emacs.d/packages/yasnippet-0.6.1c")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")
;; }}

(provide 'init-yasnippet)
