;; {{
;; init-paredit.el

(add-to-list 'load-path "~/.emacs.d/packages/paredit")

;; (autoload 'enable-paredit-mode "paredit"
;;   "Turn on pseudo-structural editing of Lisp code." t)

(require 'paredit)

(add-hook 'eval-expression-minibuffer-setup-hook 'paredit-mode)
(add-hook 'ielm-mode-hook             'paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'paredit-mode)

(provide 'init-paredit)

;; }}
