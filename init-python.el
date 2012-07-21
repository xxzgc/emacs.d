;;; init-python.el --- init python

;; Copyright (C) 2012 Free Software Foundation, Inc.
;;
;; Author: taryk <mrtaryk@gmail.com>
;; Maintainer: taryk <mrtaryk@gmail.com>
;; Created: 09 Jan 2012
;; Version: 0.01
;; Keywords

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'init-python)

;;; Code:

(eval-when-compile
  (require 'cl))

(add-to-list 'load-path "~/.emacs.d/packages/python-mode.el-6.0.4/") 
(setq py-install-directory "~/.emacs.d/packages/python-mode.el-6.0.4/")
(require 'python)
(require 'python-mode)

(autoload 'python-mode "python-mode" "Python Mode." t)

(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(add-hook 'python-mode-hook (lambda ()
  (linum-mode)
  (set-variable 'python-indent-offset 4)
 ;(set-variable 'python-smart-indentation nil)



  (set-variable 'indent-tabs-mode nil)
  (define-key python-mode-map (kbd "RET") 'newline-and-indent)
  ;; (define-key python-mode-map [tab] 'yas/expand)
  ;; (setq yas/after-exit-snippet-hook 'indent-according-to-mode)
  ;; (smart-operator-mode-on)
))

;; Anything IPython

(require 'anything-ipython)
(add-hook 'python-mode-hook #'(lambda ()
                                (define-key python-mode-map (kbd "M-<tab>") 'anything-ipython-complete)))
(add-hook 'ipython-shell-hook #'(lambda ()
                                  (define-key python-mode-map (kbd "M-<tab>") 'anything-ipython-complete)))

;; pymacs

(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
;;(eval-after-load "pymacs"
;; '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auto-completion
;;; Integrates:
;;; 1) Rope
;;; 2) Yasnippet
;;; all with AutoComplete.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prefix-list-elements (list prefix)
  (let (value)
    (nreverse (dolist (element list value)
                (setq value (cons (format "%s%s" prefix element) value))))))

(defvar ac-source-rope '((candidates . (lambda ()
  (prefix-list-elements (rope-completions) ac-target))))
  "Source for Rope")

(defun ac-python-find ()
  "Python `ac-find-function'."
  (require 'thingatpt)
  (let ((symbol (car-safe (bounds-of-thing-at-point 'symbol))))
    (if (null symbol)
        (if (string= "." (buffer-substring (- (point) 1) (point)))
            (point)
          nil)
      symbol)))

(defun ac-python-candidate ()
  "Python `ac-candidates-function'"
  (let (candidates)
    (dolist (source ac-sources)
      (if (symbolp source)
          (setq source (symbol-value source)))
      (let* ((ac-limit (or (cdr-safe (assq 'limit source)) ac-limit))
             (requires (cdr-safe (assq 'requires source)))
             cand)

  (if (or (null requires)
          (>= (length ac-target) requires))
      (setq cand
            (delq nil
                  (mapcar (lambda (candidate)
                            (propertize candidate 'source source))
                          (funcall (cdr (assq 'candidates source)))))))
  (if (and (> ac-limit 1)
           (> (length cand) ac-limit))
      (setcdr (nthcdr (1- ac-limit) cand) nil))
  (setq candidates (append candidates cand))))
    (delete-dups candidates)))

(add-hook 'python-mode-hook
  (lambda ()
    (auto-complete-mode 1)
    (set (make-local-variable 'ac-sources)
         (append ac-sources '(ac-source-rope)));;(append ac-sources '(ac-source-rope) '(ac-source-yasnippet)))
    (set (make-local-variable 'ac-find-function) 'ac-python-find)
    (set (make-local-variable 'ac-candidate-function) 'ac-python-candidate)
    (set (make-local-variable 'ac-auto-start) nil)))

;; Ryan's python specific tab completion
(defun ryan-python-tab ()
  ; Try the following:
  ; 1) Do a yasnippet expansion
  ; 2) Do a Rope code completion
  ; 3) Do an indent
  (interactive)
  (if (eql (ac-start) 0)
      (indent-for-tab-command)))

(defadvice ac-start (before advice-turn-on-auto-start activate)
  (set (make-local-variable 'ac-auto-start) t))

(defadvice ac-cleanup (after advice-turn-off-auto-start activate)
  (set (make-local-variable 'ac-auto-start) nil))

(define-key python-mode-map "\t" 'ryan-python-tab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End Auto Completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Auto Syntax Error Hightlight
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))

(add-hook 'find-file-hook 'flymake-find-file-hook)

(set-mode-for-filename-patterns 
  'python-mode
  '("\\.py$"))

(provide 'init-python)

;;; init_python.el ends here
