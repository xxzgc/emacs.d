(add-to-list 'load-path "~/.emacs.d/packages/clojure-mode")

(defun turn-on-paredit ()
  (paredit-mode 1))

(add-hook 'clojure-mode-hook 'turn-on-paredit)

(require 'clojure-mode)

(provide 'init-clojure)
