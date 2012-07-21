(defconst emacs-start-time (current-time))

;; exec path
(setenv "PATH" (concat (getenv "PATH") ":/sw/bin"))
(setq exec-path (append exec-path '("~/.emacs.d/packages/w3m/bin")))

;; load
(add-to-list 'load-path (expand-file-name "~/.emacs.d/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/packages/"))

;; (mapc (lambda (mode-hook) (add-hook mode-hook 'turn-on-watchwords))
;;         '(c-mode-common-hook tcl-mode-hook emacs-lisp-mode-hook
;;           ruby-mode-hook java-mode-hook haskell-mode-hook
;;           ess-mode-hook python-mode-hook sh-mode-hook))

(load-file "~/.emacs.d/custom.el")

(require 'init-cc)

(require 'init-general-defuns)

(require 'init-elpa)

(require 'init-session)

(require 'init-vline)

(require 'init-window-number)

(require 'init-nyan)

(require 'init-workspaces)

(require 'init-linum)

(require 'init-cua)

(require 'init-minimap)

(require 'init-w3m)

(require 'init-multiterm)

(require 'init-twitter)

(require 'init-jira)

(require 'init-irc)

(require 'init-ediff)

(require 'init-git)

(require 'init-mercurial)

(require 'init-autocomplete)

(require 'init-cmake)

(require 'init-common-lisp)

(require 'init-perl)

(require 'init-ruby)

(require 'init-php)

(require 'init-lua)

(require 'init-org)

(require 'init-yasnippet)

(require 'init-sqlite)

(require 'init-js)

(require 'init-yaml)

(require 'init-python)

(require 'init-elisp)

(require 'init-ecb)

;; (require 'init-cedet)

(require 'init-modeline)

(require 'init-keybinds)

(require 'init-heroku)

(require 'init-clojure)

(require 'init-shell-script)

(require 'init-profile)

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))


;; # Local Variables:
;; # tab-width: 2
;; # cperl-indent-level: 2
;; # End:

;; -*- coding: utf-8 -*-

;;; ~/.emacs.d/init.el ends here
