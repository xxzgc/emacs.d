;;; init-rainbow-delimiters.el

(add-to-list 'load-path "~/.emacs.d/packages/rainbow-delimiters")
(require 'rainbow-delimiters)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(provide 'init-rainbow-delimiters)
;;; init-rainbow-delimiters.el ends here
