;; utf-8
(setq file-name-coding-system 'utf-8)

;; notify-send
(defvar libnotify-program "/usr/bin/notify-send")

;; Default browser
;; used by `browser-url` function
(setq browse-url-generic-program "/usr/bin/chromium-browser")

;; Line by line scrolling
;; This makes the buffer scroll by only a single line when the up or
;; down cursor keys push the cursor (tool-bar-mode) outside the
;; buffer. The standard emacs behaviour is to reposition the cursor in
;; the center of the screen, but this can make the scrolling confusing
(setq scroll-step 1)

;; Set standard indent to 2 rather that 4
(setq standard-indent 2)

;; always end a file with a newline
(setq require-final-newline t)

;; stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)

;; search highlight
(setq search-highlight t)

;; Highlight query object 
(setq query-replace-highlight t)

;; 'y or n' replaces from 'yes or no'
(fset 'yes-or-no-p 'y-or-n-p)

;; buffer switch
(iswitchb-mode t)
(ido-mode t)

(setq tramp-default-method "ssh")

(transient-mark-mode 1)

;; Startup message
(setq inhibit-startup-message t)

;; global font lock mode
(if (fboundp 'global-font-lock-mode)
    (global-font-lock-mode 1)        ; GNU Emacs
  (setq font-lock-auto-fontify t))   ; XEmacs


;; toolbar/menubar/scrollbar mode disable
(if (fboundp 'menu-bar-mode) (menu-bar-mode nil))
(if (fboundp 'tool-bar-mode) (tool-bar-mode nil))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode nil))

;; In every buffer, the line which contains the cliursor will be fully
;; highlighted
(global-hl-line-mode 1)

;; (setq set-face-background 'hl-line "#004")
;; (setq set-face-foreground 'hl-line "black")

;; Emacs normally uses both tabs and spaces to indent lines. If you
;; prefer, all indentation can be made from spaces only. To request this,
;; set `indent-tabs-mode' to `nil'. This is a per-buffer variable;
;; altering the variable affects only the current buffer, but it can be
;; disabled for all buffers.
;; (setq ...) to set value locally to a buffer
;; (setq-default ...) to set value globally
(setq-default indent-tabs-mode nil) 

;; Wheel Mouse Scrolling
(mouse-wheel-mode t) 

;; Enable backup files.
(setq make-backup-files t)

;; Enable versioning with default values (keep five last versions, I think!)
(setq version-control t)

;; Save all backup file in this directory.
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))

;; Set cursor and mouse-pointer colours
(set-cursor-color "red")
(set-mouse-color "goldenrod")

;; M-x list-color-display
(set-background-color "#333333")
(set-foreground-color "#FFFFFF")

;; Set region background colour
(set-face-background 'region "blue")

;; Fonts
;; (set-face-attribute 'default nil :height 100 :font "DejaVu Sans Mono")
;; (set-face-attribute 'default nil :height 100 :font "Droid Sans Mono")
;; (set-face-attribute 'default nil :height 100 :font "Envy Code R" :embolden t)
;; (set-face-attribute 'default nil :font "Envy Code R VS-10")
;; (if (string= (system-name) "darkspace")
(set-face-attribute 'default nil :height 120 :font "Ubuntu Mono")
;; (set-face-attribute 'default nil :height 100 :font "Droid Sans Mono"))

;; ;; show time
;; (setq display-time-interval 1)
;; (setq display-time-format "%Y.%m.%d %H:%M:%S")
;; (display-time-mode)
