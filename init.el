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
  :add-path '("~/.emacs.d/packages/emacs-deferred"
              "~/.emacs.d/packages/emacs-epc"
              "~/.emacs.d/packages/emacs-ctable")
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
  :add-path "~/.emacs.d/packages/nyan-mode"
  :init-package t)

;; (use-custom-package
;;  :package-name 'workspaces
;;  :init-package t)

(use-custom-package
  :add-path "~/.emacs.d/packages/perspective"
  :package-name 'perspective
  :init-package t)

;; (use-custom-package
;;  :package-name 'wm
;;  :add-path '("~/.emacs.d/packages/emacs-window-layout"
;;              "~/.emacs.d/packages/emacs-window-manager")
;;  :defer t
;;  :init-package t)

(use-custom-package
  :package-name 'linum
  :init-package t)

(use-custom-package
  :package-name 'cua
  :init-package t)

(use-custom-package
  :package-name 'minimap
  :defer t
  :autoload '(minimap-create minimap-toggle)
  :init-package t)

(use-custom-package
  :package-name 'iedit
  :add-path "~/.emacs.d/packages/emacs-iedit"
  :init-package t)

(use-custom-package
  :package-name 'multiplecursors
  :add-path '("~/.emacs.d/packages/multiple-cursors.el/"
              "~/.emacs.d/packages/mark-multiple/")
  :defer t
  :init-package t)

(use-custom-package
  :package-name 'w3m
  :add-path "~/.emacs.d/packages/emacs-w3m"
  :defer t
  :autoload '(w3m)
  :init-package t)

(use-custom-package
  :package-name 'multiterm
  :defer t
  :autoload '(multi-term)
  :init-package t)

(use-custom-package
  :package-name 'twitter
  :add-path "~/.emacs.d/packages/twittering-mode"
  :defer t
  :autoload '(twit)
  :init-package t)

(use-custom-package
  :package-name 'jira
  :add-path "~/.emacs.d/packages/emacs-soap-client"
  :defer t
  :init-package t)

(use-custom-package
  :package-name 'irc
  :defer t
  :autoload '(init-irc)
  :init-package t)

(use-custom-package
  :package-name 'autocomplete
  :add-path '("~/.emacs.d/packages/popup/"
              "~/.emacs.d/packages/auto-complete/")
  :init-package t)

(use-custom-package
  :package-name 'yasnippet
  :add-path "~/.emacs.d/packages/yasnippet"
  :init-package t)

(use-custom-package
  :package-name 'flymake
  :add-path "~/.emacs.d/packages/emacs-flymake"
  :defer t
  :init-package t)

(use-custom-package
  :package-name 'ediff
  :defer t
  :autoload '(ediff ediff3)
  :init-package t)

(use-custom-package
  :package-name 'git
  :add-path '("~/.emacs.d/packages/egg"
              "~/.emacs.d/packages/egit")
  :init-package t)

(use-custom-package
  :package-name 'mercurial
  :add-path "~/.emacs.d/packages/ahg"
  :defer t
  :init-package t)

(use-custom-package
  :package-name 'cc
  :add-path "~/.emacs.d/packages/cc-mode"
  :defer t
  :init-package t)

(use-custom-package
  :package-name 'cmake
  :mode '((cmake-mode "^CMakeLists\\.txt$" "^CMakeLists$" "\\.cmake$"))
  :defer t
  :init-package t)

(use-custom-package
  :package-name 'paredit
  :add-path "~/.emacs.d/packages/paredit"
  :init-package t)

(use-custom-package
  :package-name 'project
  :add-path '("~/.emacs.d/packages/find-file-in-project"
              "~/.emacs.d/packages/projectile")
  :init-package t)

(use-custom-package
  :package-name 'db
  :add-path "~/.emacs.d/packages/emacs-edbi"
  :defer t
  :init-package t)

(use-custom-package
  :package-name 'highlight
  :add-path "~/.emacs.d/packages/highlight-symbol"
  :defer t
  :init-package t)

(use-custom-package
  :package-name 'rainbow-delimiters
  :add-path "~/.emacs.d/packages/rainbow-delimiters"
  :init-package t)

;; Geiser package together with scheme-mode
(use-custom-package
  :package-name 'geiser
  :mode '((scheme-mode "\\.scm$" "\\.ss$" "\\.rkt$"))
  :minor-modes '(rainbow-delimiters-mode paredit-mode)
  :defer t
  :line-numbers t
  :init-package t)

(use-custom-package
  :package-name 'mmm
  :add-path "~/.emacs.d/packages/mmm-mode"
  :mode '((nxml-web-mode "\\.tmpl$"))
  :defer t
  :line-numbers t
  :init-package t)

(use-custom-package
  :package-name 'common-lisp
  :mode '((common-lisp-mode "\\.clisp$" "\\.lisp$"))
  :add-path "~/.emacs.d/packages/slime"
  :minor-modes '(rainbow-delimiters-mode paredit-mode)
  :defer t
  :line-numbers t
  :init-package t)

(use-custom-package
  :package-name 'perl
  :add-path '("~/.emacs.d/packages/cperl-mode"
              "~/.emacs.d/packages/tt-mode"
              "~/.emacs.d/packages/sepia"
              "~/.emacs.d/packages/emacs-flymake-perlcritic"
              "~/.emacs.d/packages/emacs-pde/lisp")
  :mode '((cperl-mode "\\.pl$" "\\.pm$" "\\.PL$" "\\.t$" "\\.cgi$")
          (xs-mode "\\.xs$")
          (tt-mode "\\.tt$"))
  :minor-modes '(projectile-on)
  :defer t
  :line-numbers t
  :init-package t
  :init '(defalias 'perl-mode 'cperl-mode))

(use-custom-package
  :package-name 'ruby
  :mode '((ruby-mode "\\.rb$" "\\.rsel$" "\\.rhtml$" "\\.erb$" "\\.prawn$"
                     "Rakefile$" "Gemfile$"))
  :defer t
  :line-numbers t
  :init-package t)

(use-custom-package
  :package-name 'php
  :mode '((php-mode "\\.php$"))
  :defer t
  :line-numbers t
  :init-package t)

(use-custom-package
  :package-name 'lua
  :add-path "~/.emacs.d/packages/lua-mode"
  :mode '((lua-mode "\\.lua$"))
  :defer t
  :line-numbers t
  :init-package t)

(use-custom-package
  :package-name 'org
  :add-path '("~/.emacs.d/packages/org-mode/lisp"
              "~/.emacs.d/packages/org-mode/contrib/lisp")
  :defer t
  :init-package t)

(use-custom-package
  :package-name 'sqlite
  :defer t
  :init-package t)

(use-custom-package
  :package-name 'js
  :mode '((js2-mode "\\.js$" "\\.json$"))
  :defer t
  :line-numbers t
  :init-package t)

(use-custom-package
  :package-name 'yaml
  :mode '((yaml-mode "\\.yaml$" "\\.yml$"))
  :defer t
  :line-numbers t
  :init-package t)

;; (use-custom-package
;;   :package-name 'python
;;   :add-path "~/.emacs.d/packages/python-mode/"
;;   :mode '((python-mode "\\.py$"))
;;   :defer t
;;   :line-numbers t
;;   :init-package t)

(use-custom-package
  :package-name 'elisp
  :mode '((emacs-lisp-mode "\\.el$"))
  :minor-modes '(rainbow-delimiters-mode paredit-mode)
  :defer t
  :line-numbers t
  :init-package t)

(use-custom-package
  :package-name 'ecb
  :add-path "~/.emacs.d/packages/ecb/"
  :defer t
  :init-package t)

;; (use-custom-package
;;  :package-name 'cedet
;;  :defer t
;;  :init-package t)

;; (use-custom-package
;;   :package-name 'heroku
;;   :add-path "~/.emacs.d/packages/heroku"
;;   :defer t
;;   :init-package t)

(use-custom-package
  :package-name 'clojure
  :add-path "~/.emacs.d/packages/clojure-mode"
  :mode '((clojure-mode "\\.clj$"))
  :minor-modes '(rainbow-delimiters-mode paredit-mode)
  :defer t
  :line-numbers t
  :init-package t)

;; (use-custom-package
;;   :package-name 'haskell
;;   :add-path "~/.emacs.d/packages/haskell-mode"
;;   :mode '((haskell-mode "\\.hs$"))
;;   :defer t
;;   :line-numbers t
;;   :init-package t)

(use-custom-package
  :package-name 'shell-script
  :mode '((shell-script-mode "\\.sh$" "\\.bash$" "\\.zsh$" "^.zshrc$"
                             "^.bashrc$"))
  :defer t
  :line-numbers t
  :init-package t)

(use-custom-package
  :package-name 'markdown
  :add-path "~/.emacs.d/packages/markdown-mode"
  :mode '((markdown-mode "\\.md$" "\\.mdwn$" "\\.mdt$"))
  :defer t
  :line-numbers t
  :init-package t)

(use-custom-package
  :package-name 'sml
  :add-path "~/.emacs.d/packages/sml-mode"
  :mode '((sml-mode "\\.sml$"))
  :defer t
  :line-numbers t
  :init-package t)

(use-custom-package
  :package-name 'scala
  :add-path "~/.emacs.d/packages/scala-mode2"
  :mode '((scala-mode2 "\\.scala$" "\\.sc$"))
  :defer t
  :line-numbers t
  :init-package t)

;; (use-custom-package
;;  :package-name 'nxhtml
;;  :add-path "~/.emacs.d/packages/nxhtml"
;;  :defer t
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

(message "Emacs loaded in %ds"
         (destructuring-bind
             (hi lo ms unknown) (current-time)
           (- (+ hi lo) (+ (first emacs-start-time) (second emacs-start-time)))))

;; # Local Variables:
;; # tab-width: 2
;; # cperl-indent-level: 2
;; # End:

;; -*- coding: utf-8 -*-

;;; ~/.emacs.d/init.el ends here
