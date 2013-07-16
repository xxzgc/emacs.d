;; (add-to-list 'load-path "~/.emacs.d/packages/find-file-in-project")

;; (require 'find-file-in-project)

(require 'projectile)

(setq projectile-enable-caching t)
(setq projectile-completion-system 'ido) ;; default or grizzl

(provide 'init-project)
