(defun* use-custom-package (&key add-path package-name mode minor-modes defer line-numbers init-package)
  (if add-path
      (if (listp add-path)
          (dolist (path-item add-path)
            (add-to-list 'load-path path-item))
        (add-to-list 'load-path add-path)))
  (dolist (item mode)
    (mapcar
      (lambda (filename-pattern)
        (setq 
          auto-mode-alist 
          (cons (cons filename-pattern (car item)) auto-mode-alist)))
      (cdr item))
    (if (listp minor-modes)
        (dolist (mmode minor-modes)
          (lexical-let ((lmmode mmode))
              (add-hook (intern (concat (symbol-name (car item)) "-hook"))
                        (lambda ()
                          (if (boundp lmmode)
                              (funcall lmmode)
                            (warn "%s isn't defined" lmmode))))))
      (warn "minor-modes should be list"))
    (when line-numbers
      (add-hook (intern (concat (symbol-name (car item)) "-hook"))
                (lambda() (linum-mode)))))
  (if init-package
      (let ((package-s (intern (concat "init-" (symbol-name package-name)))))
        (unless (featurep package-s)
          (require package-s)))))

(provide 'init-package)
