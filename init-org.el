(setq load-path (cons "~/.emacs.d/packages/org-mode/lisp" load-path))
(setq load-path (cons "~/.emacs.d/packages/org-mode/contrib/lisp" load-path))

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook 'turn-on-font-lock) ; not needed when global-font-lock-mode is on
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; log into :LOGBOOK: drawer
(setq org-clock-into-drawer t)

;; idle time
(setq org-clock-idle-time 10)

(setq org-todo-keywords
'((sequence "TODO" "PROGRESS" "WAIT" "TESTING" "REPORT" "|" "DONE")
  (sequence "BUG" "FEATURE" "|" "FIXED")
  (sequence "|" "CENCELLED")
))

(setq org-todo-keyword-faces
'(("TODO"      . (:foreground "violet"  :weight bold))
  ("FEATURE"   . (:foreground "magenta" :weight bold))
  ("PROGRESS"  . (:foreground "cyan"    :weight bold))
  ("WAIT"      . (:foreground "orange"  :weight bold)) ;;  #ffd700
  ("TESTING"   . (:foreground "gray"    :weight bold))  
  ("REPORT"    . (:foreground "gray"    :weight bold))
  ("FIXED"     . (:foreground "green"   :weight bold))
  ("DONE"      . (:foreground "green"   :weight bold))
  ("CENCELLED" . (:foreground "red"     :weight bold))
))

;; agenda-files
(if (string= (system-name) "darkspace")
    (setq org-agenda-files (list "~/Documents/org/2012.org"
                                 "~/Documents/org/toread.org"
                                 "~/Documents/org/home.org"
                                 "~/Documents/org/development.org")))

(provide 'init-org)
