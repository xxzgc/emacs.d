;; {{
;;; JIRA mode
;;; jira via xml-rpc
;; (require 'xml-rpc)
;; (require 'jira)
;;; jira via soap
(setq load-path (cons "~/.emacs.d/packages/emacs-soap-client" load-path))
(require 'soap-client)
(require 'jira2)

;; jira params
(load-file "~/.emacs-private/auth-jira.el")
;; }}

(provide 'init-jira)
