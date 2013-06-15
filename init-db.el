(add-to-list 'load-path "~/.emacs.d/packages/emacs-edbi")

(require 'edbi)

(autoload 'e2wm:dp-edbi "e2wm-edbi" nil t)
(global-set-key (kbd "s-d") 'e2wm:dp-edbi)

(provide 'init-db)
