;; {{
;; Egg (Emacs got Git) is a git interface having many advanced features.
;; (add-to-list 'load-path "~/.emacs.d/packages/egg")
(require 'egg)

;; egit is an Emacs Git history interface intended to be similar to qgit or gitk. Requires git.el.
;; (add-to-list 'load-path "~/.emacs.d/packages/egit")
(require 'egit)

;; ;; It's Magit! An Emacs mode for Git
;; (require 'magit)
;; }}

(require 'git-gutter+)
(require 'git-gutter-fringe+)
(global-git-gutter+-mode t)

;; Git Modes
(require 'git-commit-mode)
(require 'git-rebase-mode)
(require 'gitconfig-mode)
(require 'gitignore-mode)
(require 'gitattributes-mode)

;;; Jump between hunks
(global-set-key (kbd "C-c n") 'git-gutter+-next-hunk)
(global-set-key (kbd "C-c p") 'git-gutter+-previous-hunk)

;;; Act on hunks
;; (global-set-key (kbd "C-c v =") 'git-gutter+-popup-hunk) ; Show detailed diff
(global-set-key (kbd "C-c r") 'git-gutter+-revert-hunk)

;; Stage hunk at point.
;; If region is active, stage all hunk lines within the region.
;; (global-set-key (kbd "C-c t") 'git-gutter+-stage-hunks)
;; (global-set-key (kbd "C-c c") 'git-gutter+-commit) ; Commit with Magit
;; (global-set-key (kbd "C-c C") 'git-gutter+-stage-and-commit)

;; (global-set-key (kbd "C-c g") 'git-gutter+-mode) ; Turn on/off in the current buffer
;; (global-set-key (kbd "C-c G") 'global-git-gutter+-mode) ; Turn on/off globally

(provide 'init-git)
