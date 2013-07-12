;;; init-rainbow-delimiters.el

(add-to-list 'load-path "~/.emacs.d/packages/rainbow-delimiters")
(require 'rainbow-delimiters)

(add-hook 'eval-expression-minibuffer-setup-hook 'rainbow-delimiters-mode)
(add-hook 'ielm-mode-hook             'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)

(custom-set-faces
 '(rainbow-delimiters-depth-1-face ((t (:foreground "green4"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "yellow4"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "cyan4"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "orange"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "MediumOrchid4"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "yellow4"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "cyan4"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "orange"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "MediumOrchid4"))))
 '(rainbow-delimiters-depth-10-face ((t (:foreground "yellow4"))))
 '(rainbow-delimiters-depth-11-face ((t (:foreground "cyan4"))))
 '(rainbow-delimiters-depth-12-face ((t (:foreground "orange"))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "#333333" :background "red" :inverse-video t)))))

(provide 'init-rainbow-delimiters)
;;; init-rainbow-delimiters.el ends here
