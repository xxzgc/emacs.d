(load-file "~/.emacs.d/packages/geiser/build/elisp/geiser-load.el")

(setq geiser-impl-installed-implementations '(racket guile))

;; (eval-after-load "geiser-impl"
;;   '(add-to-list 'geiser-implementations-alist
;;                 '((dir "/home/jao/prj/frob") guile)))


(add-to-list 'geiser-implementations-alist '(("\\.scm$" guile)
                                             ("\\.ss$" racket)
                                             ("\\.rkt$" racket)))

(provide 'init-geiser)
