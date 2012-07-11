;; 
;;   %b -- print buffer name.      
;;   %f -- print visited file name.
;;   %F -- print frame name.
;;   %* -- print %, * or hyphen.   
;;   %+ -- print *, % or hyphen.
;;         %& is like %*, but ignore read-only-ness.
;;         % means buffer is read-only and * means it is modified.
;;         For a modified read-only buffer, 
;;   %* gives % and %+ gives *.
;;   %s -- print process status.   
;;   %l -- print the current line number.
;;   %c -- print the current column number (this makes editing slower).
;;         To make the column number update correctly in all cases,
;;         `column-number-mode' must be non-nil.
;;   %i -- print the size of the buffer.
;;   %I -- like %i, but use k, M, G, etc., to abbreviate.
;;   %p -- print percent of buffer above top of window, or Top, Bot or All.
;;   %P -- print percent of buffer above bottom of window, perhaps plus Top,
;;         or print Bottom or All.
;;   %n -- print Narrow if appropriate.
;;   %t -- visited file is text or binary (if OS supports this distinction).
;;   %z -- print mnemonics of keyboard, terminal, and buffer coding systems.
;;   %Z -- like %z, but including the end-of-line format.
;;   %e -- print error message about full memory.
;;   %@ -- print @ or hyphen.  @ means that default-directory is on a
;;         remote machine.
;;   %[ -- print one [ for each recursive editing level.  %] similar.
;;   %% -- print %.   %- -- print infinitely many dashes.
;; Decimal digits after the % specify field width to which to pad.

;; Mode line setup
(setq-default  mode-line-format  '(
   ;; relative position
   (:propertize " %p" face mode-line-rel-position-face)
   " ["
   ;; Position, including warning for 80 columns
   (:propertize "%03l:" face mode-line-position-face)
   (:eval (propertize "%02c" 'face
                      (if (>= (current-column) 80)
                          'mode-line-80col-face
                        'mode-line-position-face)))
   "] "
   ;; emacsclient [default -- keep?]
   mode-line-client
   " %Z    "
   ;; buffer path
   (:propertize (:eval (shorten-directory default-directory 30))
                face mode-line-folder-face)
   ;; buffer name with read-only or modified status
   (:eval (cond (buffer-read-only 
                  (propertize "%b !" 'face 'mode-line-buffer-readonly))
                ((buffer-modified-p)
                  (propertize "%b *" 'face 'mode-line-buffer-modified))
                (t (propertize "%b" 'face 'mode-line-filename-face))))
   ;; filesize
   " (%I)"
   ;; mode indicators: vc, recursive edit, major mode, minor modes, process, global
   (vc-mode vc-mode)
   " [ %["
   (:propertize mode-name
                face mode-line-mode-face)
   "%] "
   (:eval (propertize (format-mode-line minor-mode-alist)
                      'face 'mode-line-minor-mode-face))
   (:propertize mode-line-process
                face mode-line-process-face)
   (global-mode-string global-mode-string)
   " ] "
   
   ; nyan-mode uses nyan cat as an alternative to %p
   (:eval (when nyan-mode (list (nyan-create))))
   ))

;; Helper function
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-buffer-readonly)
(make-face 'mode-line-buffer-modified)
(make-face 'mode-line-position-face)
(make-face 'mode-line-rel-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)

(set-face-attribute 'mode-line nil
    :foreground "gray80"
    :background "gray40"
    :inverse-video nil
    :box '(:line-width 2 
           :color "gray40"
           :style nil))

(set-face-attribute 'mode-line-inactive nil
    :foreground "gray60"
    :background "gray30"
    :inverse-video nil
    :box '(:line-width 2 
           :color "gray30"
           :style nil))

(set-face-attribute 'mode-line-read-only-face nil
    :inherit 'mode-line-face
    :foreground "#4271ae"
    :box '(:line-width 2 
           :color "#4271ae"))

(set-face-attribute 'mode-line-modified-face nil
    :inherit 'mode-line-face
    :foreground "#c82829"
    :background "#ffffff"
    :box '(:line-width 2 
           :color "#c82829"))

(set-face-attribute 'mode-line-folder-face nil
    :inherit 'mode-line-face
    :foreground "gray60")

(set-face-attribute 'mode-line-filename-face nil
    :inherit 'mode-line-face
    :foreground "#b3ee3a"
    :weight 'bold)

(set-face-attribute 'mode-line-buffer-readonly nil
    :inherit 'mode-line-filename-face
    :foreground "#f08080")

(set-face-attribute 'mode-line-buffer-modified nil
    :inherit 'mode-line-filename-face
    :foreground "#eab700")

(set-face-attribute 'mode-line-position-face nil
    :inherit 'mode-line-face
    :family "Ubuntu Mono"
    :height 110)

(set-face-attribute 'mode-line-rel-position-face nil
    :inherit 'mode-line-face
    :family "Ubuntu Mono"
    :weight 'bold
    :foreground "#eead0e"
    :height 120)

(set-face-attribute 'mode-line-mode-face nil
    :inherit 'mode-line-face
    :weight 'bold
    :foreground "gray80")

(set-face-attribute 'mode-line-minor-mode-face nil
    :inherit 'mode-line-mode-face
    :foreground "gray10"
    :weight 'normal
    :height 100)

(set-face-attribute 'mode-line-process-face nil
    :inherit 'mode-line-face
    :foreground "#718c00")

(set-face-attribute 'mode-line-80col-face nil
    :inherit 'mode-line-position-face
    :foreground "black"
    :background "#eab700")

(provide 'init-modeline)
