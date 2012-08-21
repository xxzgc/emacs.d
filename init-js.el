(autoload #'espresso-mode "espresso" "Start espresso-mode" t)

;; (set-mode-for-filename-patterns 
;;   'espresso-mode
;;   '("\\.js$" "\\.json$"))

(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

(add-hook 'javascript-mode-hook 'javascript-custom-setup)
(add-hook 'espresso-mode-hook 'javascript-custom-setup)
(defun javascript-custom-setup ()
  (moz-minor-mode 1))

;; (add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
;; (add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))

(provide 'init-js)
