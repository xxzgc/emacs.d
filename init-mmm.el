(add-to-list 'load-path "~/.emacs.d/packages/mmm-mode")

(require 'mmm-mode)
(require 'mmm-auto)

(setq mmm-global-mode 'maybe)
(setq mmm-submode-decoration-level 0)

(mmm-add-group
 'html-tmpl
 '((embtt
    :submode tt-mode
    :face mmm-code-submode-face
    :front "\\[%"
    :back "%\\]"
    :include-front t
    :include-back t)
;;    :insert ((?= embtt nil @ "[%" @ " " _ " " @ "%]" @)))
;;   (js-script-cdata
;;    :submode js2-mode
;;    :face mmm-code-submode-face
;;    :front "<script[^>]*>[ \t\n]*\\(//\\)?<!\\[CDATA\\[[ \t]*\n?"
;;    :back "[ \t]*\\(//\\)?]]>[ \t\n]*</script>")
   (js-script
    :submode js-mode
    :face mmm-code-submode-face
    :front "<script[^>]*>[ \t]*\n?"
    :back "[ \t]*</script>")
   (css-style
    :submode css-mode
    :face mmm-code-submode-face
    :front "<style[^>]*>[ \t]*\n?"
    :back "[ \t]*</style>")))
;;    :insert ((?j js-tag nil @ "<script type=\"text/javascript\">\n"
;;                 @ "" _ "" @ "\n</script>" @)))))

(add-to-list 'mmm-mode-ext-classes-alist '(html-mode nil html-js2))

;; nXML as primary mode (supports only JS and CSS subregions):
(mmm-add-mode-ext-class 'nxml-web-mode nil 'html-tmpl)
;;(mmm-add-mode-ext-class 'nxml-web-mode nil 'html-css)

(provide 'init-mmm)
