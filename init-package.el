(defun* use-custom-package (&key add-path package-name mode minor-modes defer line-numbers init-package init autoload)
  (if add-path
      (if (listp add-path)
          (dolist (path-item add-path)
            (add-to-list 'load-path path-item))
        (add-to-list 'load-path add-path)))
  (when init
    (eval init))
  (when init-package
    (let ((package-s (intern (concat "init-" (symbol-name package-name)))))
      (if (and (not defer)
               (not (featurep package-s)))
          (require package-s))))
  (dolist (autoload-item autoload)
    (autoload
      autoload-item
      (concat "init-" (symbol-name package-name))
      (format "Lazy initialization of %s" (concat "init-" (symbol-name package-name)))
      t))
  (dolist (item mode)
    (mapcar
      (lambda (filename-pattern)
        (add-to-list 'auto-mode-alist `(,filename-pattern . ,(car item))))
      (cdr item))
    (when defer
      (eval-after-load (symbol-name (car item))
         `(let ((package-s (intern ,(concat "init-" (symbol-name package-name)))))
            (unless (featurep package-s)
              (require package-s)))))
    (if (listp minor-modes)
        (dolist (mmode minor-modes)
          (add-hook (intern (concat (symbol-name (car item)) "-hook")) mmode))
      (warn "minor-modes should be list"))
    (when line-numbers
      (add-hook (intern (concat (symbol-name (car item)) "-hook"))
                (lambda() (linum-mode))))))

(provide 'init-package)
