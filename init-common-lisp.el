(add-to-list 'load-path "~/.emacs.d/packages/slime/") ; your SLIME directory
(setq inferior-lisp-program "/usr/bin/sbcl") ; your Lisp system
(require 'slime)
(slime-setup)

(load (expand-file-name "~/quicklisp/slime-helper.el"))

(provide 'init-common-lisp)
