;; (add-to-list 'load-path "~/.emacs.d/packages/find-file-in-project")

;; (require 'find-file-in-project)

(require 'projectile)
(require 'grizzl)
;; (require 'persp-projectile)

(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'grizzl) ;; default or grizzl

;; (define-key projectile-mode-map (kbd "C-x s s") 'projectile-persp-switch-project)

(provide 'init-project)
