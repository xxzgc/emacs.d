;; {{
;; line numbers
(require 'linum+)
;; (require 'line-num)

;;(setq linum-mode 1)

;; Show line-number in the mode line
(line-number-mode 1)

;; Show column-number in the mode line
(column-number-mode 1)
;; }}

;; linum mode on prog-mode
(add-hook 'prog-mode-hook (lambda() (linum-mode)))

;;;; linum relative
;; (add-to-list 'load-path "~/.emacs.d/packages/linum-relative")
;; (setq linum-relative-current-symbol "->")
;; (require 'linum-relative)

(provide 'init-linum)
