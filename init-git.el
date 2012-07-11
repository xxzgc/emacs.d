;; {{
;; Egg (Emacs got Git) is a git interface having many advanced features.
(add-to-list 'load-path "~/.emacs.d/packages/egg")
(require 'egg)

;; egit is an Emacs Git history interface intended to be similar to qgit or gitk. Requires git.el.
(add-to-list 'load-path "~/.emacs.d/packages/egit")
(require 'egit)
;; }}

(provide 'init-git)
