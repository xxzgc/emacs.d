(add-to-list 'load-path "~/.emacs.d/packages/mmm-mode")

(require 'mmm-mode)
(require 'mmm-auto)
(require 'mmm-erb)

(setq mmm-global-mode 'auto)
(setq mmm-submode-decoration-level 0)

(mmm-add-group
 'html-tmpl
 '((embtt
    :submode tt-mode
    :face mmm-code-submode-face
    :front "\\[%"
    :back "%\\]"
    :include-front t
    :include-back t)))
;;  :insert ((?% embtt-tag nil @ "[%" @ " " _ " " @ "%]" @)))))

;; add-to-list 'mmm-mode-ext-classes-alist '(html-mode nil html-js2))

;; nXML as primary mode (supports only JS and CSS subregions):

(mmm-add-mode-ext-class 'nxml-web-mode nil 'html-tmpl)
(mmm-add-mode-ext-class 'nxml-web-mode nil 'html-js)
(mmm-add-mode-ext-class 'nxml-web-mode nil 'html-css)

(provide 'init-mmm)
