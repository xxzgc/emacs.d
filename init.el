(require 'cl)

(defconst emacs-start-time (current-time))

;; exec path
(setenv "PATH" (concat (getenv "PATH") ":/sw/bin"))
(setq exec-path (append exec-path '("~/.emacs.d/packages/w3m/bin")))

;; load
(add-to-list 'load-path (expand-file-name "~/.emacs.d/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/packages/"))

(add-to-list 'load-suffixes ".el.gpg")

;; (mapc (lambda (mode-hook) (add-hook mode-hook 'turn-on-watchwords))
;;         '(c-mode-common-hook tcl-mode-hook emacs-lisp-mode-hook
;;           ruby-mode-hook java-mode-hook haskell-mode-hook
;;           ess-mode-hook python-mode-hook sh-mode-hook))

(load-file "~/.emacs.d/custom.el")

(require 'init-package)

(use-custom-package
  :package-name 'libraries
  :init-package t)

(use-custom-package
  :package-name 'general-defuns
  :init-package t)

(use-custom-package
  :package-name 'elpa
  :init-package t)

(use-custom-package
  :package-name 'session
  :init-package t)

(use-custom-package
  :package-name 'vline
  :init-package t)

(use-custom-package
  :package-name 'window-number
  :init-package t)

(use-custom-package
  :package-name 'nyan
  :init-package t)

;; (use-custom-package
;;  :package-name 'workspaces
;;  :init-package t)

(use-custom-package
  :package-name 'perspective
  :init-package t)

;; (use-custom-package
;;  :package-name 'wm
;;  :init-package t)

(use-custom-package
  :package-name 'linum
  :init-package t)

(use-custom-package
  :package-name 'cua
  :init-package t)

(use-custom-package
  :package-name 'minimap
  :init-package t)

(use-custom-package
  :package-name 'iedit
  :init-package t)

(use-custom-package
  :package-name 'multiplecursors
  :init-package t)

(use-custom-package
  :package-name 'w3m
  :init-package t)

(use-custom-package
  :package-name 'multiterm
  :init-package t)

(use-custom-package
  :package-name 'twitter
  :init-package t)

(use-custom-package
  :package-name 'jira
  :init-package t)

(use-custom-package
  :package-name 'irc
  :init-package t)

(use-custom-package
  :package-name 'autocomplete
  :init-package t)

(use-custom-package
  :package-name 'yasnippet
  :init-package t)

(use-custom-package
  :package-name 'flymake
  :init-package t)

(use-custom-package
  :package-name 'ediff
  :init-package t)

(use-custom-package
  :package-name 'git
  :init-package t)

(use-custom-package
  :package-name 'mercurial
  :init-package t)

(use-custom-package
  :package-name 'cc
  :init-package t)

(use-custom-package
  :package-name 'cmake
  :init-package t)

(use-custom-package
  :package-name 'paredit
  :init-package t)

(use-custom-package
  :package-name 'project
  :init-package t)

(use-custom-package
  :package-name 'db
  :init-package t)

(use-custom-package
  :package-name 'highlight
  :init-package t)

(use-custom-package
  :package-name 'rainbow-delimiters
  :init-package t)

(use-custom-package
  :package-name 'mmm
  :init-package t)

;; Geiser package together with scheme-mode
(use-custom-package
  :package-name 'geiser
  :mode '((scheme-mode "\\.scm$" "\\.ss$" "\\.rkt$"))
  :minor-modes '(rainbow-delimiters-mode paredit-mode)
  :line-numbers t
  :init-package t)

(use-custom-package
  :package-name 'mmm
  :mode '((nxml-web-mode "\\.tmpl$"))
  :line-numbers t
  :init-package t)

(use-custom-package
  :package-name 'common-lisp
  :mode '((common-lisp-mode "\\.clisp$" "\\.lisp$"))
  :minor-modes '(rainbow-delimiters-mode paredit-mode)
  :line-numbers t
  :init-package t)

(use-custom-package
  :package-name 'perl
  :mode '((cperl-mode "\\.pl$" "\\.pm$" "\\.PL$" "\\.t$" "\\.cgi$")
          (xs-mode "\\.xs$")
          (tt-mode "\\.tt$"))
  :line-numbers t
  :init-package t)

(use-custom-package
  :package-name 'ruby
  :mode '((ruby-mode "\\.rb$" "\\.rsel$" "\\.rhtml$" "\\.erb$" "\\.prawn$" "Rakefile$" "Gemfile$"))
  :line-numbers t
  :init-package t)

(use-custom-package
  :package-name 'php
  :mode '((php-mode "\\.php$"))
  :line-numbers t
  :init-package t)

(use-custom-package
  :package-name 'lua
  :mode '((lua-mode "\\.lua$"))
  :line-numbers t
  :init-package t)

(use-custom-package
  :package-name 'org
  :init-package t)

(use-custom-package
  :package-name 'sqlite
  :init-package t)

(use-custom-package
  :package-name 'js
  :mode '((js2-mode "\\.js$" "\\.json$"))
  :line-numbers t
  :init-package t)

(use-custom-package
  :package-name 'yaml
  :mode '((yaml-mode "\\.yaml$"))
  :line-numbers t
  :init-package t)

;; (use-custom-package
;;   :package-name 'python
;;   :mode '((python-mode "\\.py$"))
;;   :line-numbers t
;;   :init-package t)

(use-custom-package
  :package-name 'elisp
  :mode '((emacs-lisp-mode "\\.el$"))
  :minor-modes '(rainbow-delimiters-mode paredit-mode)
  :line-numbers t
  :init-package t)

(use-custom-package
  :package-name 'ecb
  :init-package t)

;; (use-custom-package
;;  :package-name 'cedet
;;  :init-package t)

;; (use-custom-package
;;   :package-name 'heroku
;;   :init-package t)

(use-custom-package
  :package-name 'clojure
  :mode '((clojure-mode "\\.clj$"))
  :minor-modes '(rainbow-delimiters-mode paredit-mode)
  :line-numbers t
  :init-package t)

;; (use-custom-package
;;   :package-name 'haskell
;;   :mode '((haskell-mode "\\.hs$"))
;;   :line-numbers t
;;   :init-package t)

(use-custom-package
  :package-name 'shell-script
  :mode '((shell-script-mode "\\.sh$" "\\.bash$" "\\.zsh$" "^.zshrc$" "^.bashrc$"))
  :line-numbers t
  :init-package t)

(use-custom-package
  :package-name 'markdown
  :mode '((markdown-mode "\\.md$" "\\.mdwn$" "\\.mdt$"))
  :line-numbers t
  :init-package t)

(use-custom-package
  :package-name 'sml
  :mode '((sml-mode "\\.sml$"))
  :line-numbers t
  :init-package t)

(use-custom-package
  :package-name 'scala
  :mode '((scala-mode2 "\\.scala$" "\\.sc$"))
  :line-numbers t
  :init-package t)

;; (use-custom-package
;;  :package-name 'nxhtml
;;  :init-package t)

(use-custom-package
  :package-name 'profile
  :init-package t)

(use-custom-package
  :package-name 'modeline
  :init-package t)

(use-custom-package
  :package-name 'keybinds
  :init-package t)

;;
;; Work stuff
;;
(let ((work-stuff-el "~/.emacs.d/work-stuff.el"))
  (when (file-exists-p work-stuff-el)
    (load-file work-stuff-el)
    (require 'work-stuff)))

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
