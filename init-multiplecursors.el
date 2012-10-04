(add-to-list 'load-path "~/.emacs.d/packages/multiple-cursors.el/")

(require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-C C-C") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(global-set-key (kbd "C-S-c C-e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-C C-e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-S-c C-a") 'mc/edit-beginnings-of-lines)
(global-set-key (kbd "C-C C-a") 'mc/edit-beginnings-of-lines)

;; Rectangular region mode
(global-set-key (kbd "H-SPC") 'set-rectangular-region-anchor)

;; Mark more like This
(global-set-key (kbd "M-æ") 'mc/mark-all-like-this)
(global-set-key (kbd "C-å") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-æ") 'mc/mark-next-like-this)
(global-set-key (kbd "C-Æ") 'mc/mark-more-like-this-extended)
(global-set-key (kbd "M-å") 'mc/mark-all-in-region)

(provide 'init-multiplecursors)
