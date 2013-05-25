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

(defun open-persp (name buffer1 buffer2 &rest buffers)
  (persp-switch name)
  (delete-other-windows)
  (split-window-right)
  (let ((p (if (boundp 'prefix) prefix "")))
    (let ((f1 (concat p buffer1))
          (f2 (concat p buffer2)))
      (when (file-exists-p f1)
        (find-file f1))
      (select-window (window-next-sibling))
      (when (file-exists-p f2)
        (find-file f2)))
    (dolist (b buffers)
      (let ((pb (concat p b)))
        (when (file-exists-p pb)
          (find-file pb))))))

(provide 'init-perspective)
