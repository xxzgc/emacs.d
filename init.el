(require 'cl)

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

(require 'init-package)

(require 'init-cc)

(require 'init-cmake)

;; (require 'init-common-lisp)
(def-custom-package
  :package-name 'common-lisp
  :mode '((common-lisp-mode "\\.clisp$"))
  :line-numbers t
  :init-package t)

;; (require 'init-perl)
(def-custom-package
  :package-name 'perl
  :mode '((cperl-mode "\\.pl$" "\\.pm$" "\\.PL$" "\\.t$")
          (xs-mode "\\.xs")
          (tt-mode "\\.tt"))
  :line-numbers t
  :init-package t)

;; (require 'init-ruby)
(def-custom-package
  :package-name 'ruby
  :mode '((ruby-mode "\\.rb$" "\\.rsel$" "\\.rhtml$" "\\.erb$" "\\.prawn$" "Rakefile$" "Gemfile$"))
  :line-numbers t
  :init-package t)

;; (require 'init-php)
(def-custom-package
  :package-name 'php
  :mode '((ruby-mode "\\.php$"))
  :line-numbers t
  :init-package t)

;; (require 'init-lua)
(def-custom-package
  :package-name 'lua
  :mode '((lua-mode "\\.lua$"))
  :line-numbers t
  :init-package t)

(require 'init-org)

(require 'init-yasnippet)

(require 'init-sqlite)

;; (require 'init-js)
(def-custom-package
  :package-name 'js
  :mode '((js2-mode "\\.js$" "\\.json$"))
  :line-numbers t
  :init-package t)

;; (require 'init-yaml)
(def-custom-package
  :package-name 'yaml
  :mode '((yaml-mode "\\.yaml$"))
  :line-numbers t
  :init-package t)

;; (require 'init-python)

;; (require 'init-elisp)
(def-custom-package
  :package-name 'elisp
  :mode '((emacs-lisp-mode "\\.el$"))
  :line-numbers t
  :init-package t)

(require 'init-ecb)

;; (require 'init-cedet)

(require 'init-modeline)

(require 'init-heroku)

;; (require 'init-clojure)
(def-custom-package
  :package-name 'clojure
  :mode '((clojure-mode "\\.clj$"))
  :line-numbers t
  :init-package t)

;; (require 'init-haskell)
(def-custom-package
  :package-name 'haskell
  :mode '((haskell-mode "\\.hs$"))
  :line-numbers t
  :init-package t)

;; (require 'init-shell-script)
(def-custom-package
  :package-name 'shell-script
  :mode '((shell-script-mode "\\.sh$" "\\.bash$" "\\.zsh$" "^.zshrc$" "^.bashrc$"))
  :line-numbers t
  :init-package t)

(require 'init-mmm)

;; (require 'init-nxhtml)

(require 'init-profile)

(require 'init-keybinds)

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
