;;; {{
;;; FlyMake

;; Let's run 8 checks at once instead.
;; (add-to-list 'load-path "~/.emacs.d/packages/emacs-flymake")

(require 'flymake)

(setq flymake-max-parallel-syntax-checks 1)

(setq flymake-log-level 3)

;; I want to see at most the first 4 errors for a line.
;; (setq flymake-number-of-errors-to-display 4)

;; I want to see all errors for the line.
(setq flymake-number-of-errors-to-display nil)

;; Nope, I want my copies in the system temp dir.
(setq flymake-run-in-place nil)

;; This lets me say where my temp dir is.
(setq temporary-file-directory "~/.emacs.d/flymake-tmp/")

;; Don't start syntax check on find file
(setq flymake-start-syntax-check-on-find-file nil)

;; Don't start syntax check if newline char was added/removed from the buffer
(setq flymake-start-syntax-check-on-newline nil)

(setq flymake-no-changes-timeout 9999)

;; faces

(set-face-attribute 'flymake-errline nil 
                    :underline "red" 
                    :background nil)
(set-face-attribute 'flymake-warnline nil
                    :underline "yellow"
                    :background nil)

(provide 'init-flymake)
;; }}
