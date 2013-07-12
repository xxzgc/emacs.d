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

(eval-after-load 'linum
  '(progn
     (defface linum-leading-zero
       `((t :inherit 'linum
            :foreground ,(face-attribute 'linum :background nil t)))
       "Face for displaying leading zeroes for line numbers in display margin."
       :group 'linum)

     (defun linum-format-func (line)
       (let ((w (length
                 (number-to-string (count-lines (point-min) (point-max))))))
         (concat
          (propertize (make-string (- w (length (number-to-string line))) ?0)
                      'face 'linum-leading-zero)
          (propertize (number-to-string line) 'face 'linum))))

     (setq linum-format 'linum-format-func)))

(provide 'init-linum)
