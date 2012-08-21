(defun* def-custom-package (&key package-name mode line-numbers init-package)
  (dolist (item mode)
    (mapcar
      (lambda (filename-pattern)
        (setq 
          auto-mode-alist 
          (cons (cons filename-pattern (car item)) auto-mode-alist)))
      (cdr item))
    (when line-numbers
      (add-hook (intern (concat (symbol-name (car item)) "-hook"))
        (lambda() (linum-mode)))))
  (if init-package
      (let ((package-s (intern (concat "init-" (symbol-name package-name)))))
        (unless (featurep package-s)
          (require package-s)))))

(provide 'init-package)
