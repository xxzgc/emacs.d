;;; init-rainbow-delimiters.el

(add-to-list 'load-path "~/.emacs.d/packages/rainbow-delimiters")
(require 'rainbow-delimiters)

(add-hook 'eval-expression-minibuffer-setup-hook 'rainbow-delimiters-mode)
(add-hook 'ielm-mode-hook             'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)

(provide 'init-rainbow-delimiters)
;;; init-rainbow-delimiters.el ends here
