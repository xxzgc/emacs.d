(add-to-list 'load-path "~/.emacs.d/packages/perspective")
;; Load Perspective
(require 'perspective)
;; Toggle the perspective mode
(persp-mode)

(defmacro custom-persp (name &rest body)
  `(let ((initialize (not (gethash ,name perspectives-hash)))
         (current-perspective persp-curr))
     (persp-switch ,name)
     (when initialize ,@body)
     (setq persp-last current-perspective)))

(defun custom-persp-last ()
  (interactive)
  (persp-switch (persp-name persp-last)))

(defun custom-persp/main ()
  (interactive)
  (custom-persp "@main"))

(defun custom-persp/perl ()
  (interactive)
  (custom-persp "@perl"
                (sepia-repl)))

(defun custom-persp/term ()
  (interactive)
  (custom-persp "@term"
                (multi-term)))

(defun custom-persp/emacs ()
  (interactive)
  (custom-persp "@emacs"
                (dolist (elfile '("~/.emacs.d/init.el"
                                  "~/.emacs.d/custom.el"))
                  (find-file elfile))))

(defun custom-persp/org ()
  (interactive)
  (custom-persp "@org"
                (dolist (orgfile org-agenda-files)
                  (find-file orgfile))))

(defun custom-persp/irc ()
  (interactive)
  (custom-persp "@irc"
                (erc-perlorg)
                (erc-freenode)))
                
(global-set-key (kbd "M-p o") 'custom-persp/org)
(global-set-key (kbd "M-p i") 'custom-persp/irc)
(global-set-key (kbd "M-p t") 'custom-persp/term)
(global-set-key (kbd "M-p p") 'custom-persp/perl)
(global-set-key (kbd "M-p e") 'custom-persp/emacs)
(global-set-key (kbd "M-p m") 'custom-persp/main)

(provide 'init-perspective)
