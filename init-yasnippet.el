;; {{
;; yasnippet
(add-to-list 'load-path "~/.emacs.d/packages/yasnippet")
(require 'yasnippet) ;; not yasnippet-bundle

;; (yas/initialize)

;; (setq yas/snippet-dirs "~/.emacs.d/snippets") 
(yas/load-directory "~/.emacs.d/snippets")

(yas/global-mode 1)

(require 'dropdown-list)
(setq yas/prompt-functions '(yas/dropdown-prompt
                             yas/ido-prompt
                             yas/completing-prompt))

;; Skip the minor mode in buffers where it is not applicable
;; (no snippet tables)
(set-default 'yas/dont-activate
           #'(lambda ()
               (and yas/snippet-dirs
                    (null (yas/get-snippet-tables)))))

;; }}

(provide 'init-yasnippet)
