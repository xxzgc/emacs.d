;; (global-set-key (read-kbd-macro "C-TAB") 'my-func)
;; (global-set-key [C-tab] 'my-func)
;; (global-set-key [(control tab)] 'my-func)
;; (global-set-key (kbd "<C-tab>") 'my-func)
;; (global-set-key (quote [C-tab]) (quote my-func))

;;
;; resizers
;;
(global-set-key [C-M-down] 'win-resize-minimize-vert)
(global-set-key [C-M-up] 'win-resize-enlarge-vert)
(global-set-key [C-M-left] 'win-resize-minimize-horiz)
(global-set-key [C-M-right] 'win-resize-enlarge-horiz)
(global-set-key [C-M-up] 'win-resize-enlarge-horiz)
(global-set-key [C-M-down] 'win-resize-minimize-horiz)
(global-set-key [C-M-left] 'win-resize-enlarge-vert)
(global-set-key [C-M-right] 'win-resize-minimize-vert)

;;
;; goto last change
;;
(global-set-key (kbd "C-x C-n") 'goto-last-change)

;;
;; swap buffers in windows
;;
(global-set-key "\C-c\C-u" 'swap-buffers-in-windows)

;;
;; rename current file or buffer
;;
(global-set-key [C-c C-r] 'rename-current-file-or-buffer)

;;
;; Smart beginning of line
;;
(global-set-key [home] 'smart-beginning-of-line)

;;
;; tabbar
;;
;; C-S-<tab> ;; C-S-<win>-<tab>
;; (global-set-key (kbd "<C-S-iso-lefttab>") 'tabbar-forward-tab)
;; (global-set-key (kbd "<C-S-s-iso-lefttab>") 'tabbar-backward-tab)
(global-set-key (kbd "<C-S>") 'tabbar-forward-tab)
(global-set-key (kbd "<C-S-s-iso-lefttab>") 'tabbar-backward-tab)
;; C-x C-<left> ;; C-x C-<right>
(global-set-key (kbd "C-x C-<right>") 'tabbar-forward-group)
(global-set-key (kbd "C-x C-<left>") 'tabbar-backward-group)

;;
;; Bind (shift-right) and (shift-left) function to your favorite keys. I use
;; the following so that Ctrl-Shift-Right Arrow moves selected text one 
;; column to the right, Ctrl-Shift-Left Arrow moves selected text one
;; column to the left:
;;
(global-set-key [C-S-right] 'shift-right)
(global-set-key [C-S-left] 'shift-left)

;; (global-set-key [?\C-,] 'previous-buffer)
;; (global-set-key [?\C-.] 'next-buffer)

;; (global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; M - <arrows>
(windmove-default-keybindings 'meta)

(global-set-key [f2] 'undo)
(global-set-key [f3] 'repeat-complex-command)
(global-set-key [f4] 'replace-string)
(global-set-key [f5] 'query-replace)

(global-set-key [f7] 'flymake-display-err-menu-for-current-line)
(global-set-key [f8] 'flymake-goto-next-error)

(global-set-key [f9] 'switch-to-buffer)
(global-set-key [f12] 'nuke-line) ;; Now bind the delete line function to the F12 key
;;(global-set-key [f6] 'anything)

;; macros
(global-set-key [C-f5] 'kmacro-call-macro) 
(global-set-key [C-f6] 'kmacro-start-macro-or-insert-counter)
(global-set-key [C-f7] 'kmacro-end-or-call-macro)

;; bookmarks
(global-set-key [C-f11] 'bookmark-set)
(global-set-key [C-f12] 'bookmark-jump)


(global-set-key [delete] 'delete-char)

(global-set-key "\C-l" 'goto-line)

;; (global-set-key [M-home] 'beginning-of-buffer)
;; (global-set-key [C-home] 'beginning-of-buffer)
;; (global-set-key [home]   'beginning-of-line)
;; (global-set-key [end] 'end-of-line)
;; (global-set-key [kp-home]   'beginning-of-line)
;; (global-set-key [kp-end] 'end-of-line)
;; (global-set-key [M-end]  'end-of-buffer)
;; (global-set-key [C-end]  'end-of-buffer)

;; (define-key function-key-map "\033[1~" [home])
;; (define-key function-key-map "\033[4~" [end])
;; (global-set-key [end] 'end-of-line) 

(define-key global-map [home] 'beginning-of-line)
(define-key global-map [end]  'end-of-line)

(define-key global-map [kp-home] 'beginning-of-line)
(define-key global-map [kp-end]  'end-of-line)

;; (define-key global-map [C-down]  'scroll-up)
;; (define-key global-map [C-up]    'scroll-down)
;; (define-key global-map [C-left]  'beginning-of-line)
;; (define-key global-map [C-right] 'end-of-line)

;;
;; yasnippet
;;
(global-set-key (kbd "<backtab>") 'yas/expand)

;;
;; workspaces
;;
(define-key global-map (kbd "C-<tab>") 'workspace-controller)
(define-key global-map [C-S-iso-lefttab] 'workspace-controller) ;; c-<tab> not working in org-mode
;; (global-set-key [?\C-q] 'workspace-goto)

;;
;; autocompletion mode
;;
(define-key ac-mode-map (kbd "M-RET") 'auto-complete)

;;
;; kill line or region
;;
;; (global-set-key (kbd "C-k") 'kill-line-or-region)

(provide 'init-keybinds)
