;;; {{

;; (add-to-list 'load-path "~/.emacs.d/packages/emacs-window-layout")
;; (add-to-list 'load-path "~/.emacs.d/packages/emacs-window-manager")

(require 'e2wm)
(global-set-key (kbd "M-+") 'e2wm:start-management)

(provide 'init-wm)

;;; }}
