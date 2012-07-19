;; {{
;; w3m
;; apt-get install flim
(add-to-list 'load-path "~/.emacs.d/packages/emacs-w3m")
(require 'w3m-load)
(require 'mime-w3m)
(setq w3m-use-cookies t)
;; }}

(provide 'init-w3m)
