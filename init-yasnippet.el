;; {{
;; yasnippet
(add-to-list 'load-path "~/.emacs.d/packages/yasnippet")
(require 'yasnippet) ;; not yasnippet-bundle

(setq yas/snippet-dirs
      '("~/.emacs.d/snippets"                     ;; personal snippets
        "~/.emacs.d/packages/yasnippet/snippets"  ;; the default collection
       ))
;; (yas/initialize)
;; (yas/load-directory "~/.emacs.d/snippets")

(require 'dropdown-list)
(setq yas/prompt-functions '(yas/x-prompt
                             yas/dropdown-prompt
                             yas/ido-prompt
                             yas/completing-prompt))

;; Skip the minor mode in buffers where it is not applicable
;; (no snippet tables)
(set-default 'yas/dont-activate
           #'(lambda ()
               (and yas/snippet-dirs
                    (null (yas/get-snippet-tables)))))

(yas/global-mode 1)

(provide 'init-yasnippet)
;; }}
