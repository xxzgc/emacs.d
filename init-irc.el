;; {{
;; ERC ( is a powerful, modular, and extensible Emacs InternetRelayChat client )
;; docs: http://mwolson.org/static/doc/erc.html )

(require 'erc-button)

(custom-set-variables
 '(erc-log-channels-directory "~/.emacs-irc-log")
 '(erc-save-buffer-on-part t)
 '(erc-log-insert-log-on-open nil)
 '(erc-enable-logging (quote erc-log-all-but-server-buffers))
;; autojoin button completion dcc fill irccontrols list match menu
;; move-to-prompt netsplit networks noncommands readonly ring stamp track
;; truncate services images highlight-nicknames hl-nicks
 '(erc-modules (quote (stamp log irccontrols completion list match button))) 
 '(erc-log-mode t)
 '(erc-timestamp-mode 1) ;; timestamp the conversations
 '(erc-log-write-after-send t)
 '(erc-log-write-after-insert t)
 '(erc-timestamp-only-if-changed-flag nil)
 '(erc-timestamp-format "[%H:%M:%S] ")
 '(erc-fill-prefix      "           ")
 '(erc-insert-timestamp-function 'erc-insert-timestamp-left)
 '(erc-hide-timestamps nil))

(require 'erc-match)
(erc-match-mode t)
(setq erc-keywords '("taras" "iagniuk" "comcure" "lviv" "ukraine"
                     "emacs")) ;; yeah, I want to know who mention emacs

(defmacro asf-erc-bouncer-connect (command server port nick ssl pass)
   "Create interactive command `command', for connecting to an IRC server. The command uses interactive mode if passed an argument."
   (fset command
         `(lambda (arg)
           (interactive "p")
           (if (not (= 1 arg))
               (call-interactively 'erc)
             (let ((erc-connect-function ',(if ssl
                                               'erc-open-ssl-stream
                                             'open-network-stream)))
               (erc :server ,server :port ,port :nick ,nick :password ,pass))))))

(require 'notifications)

(defun erc-global-notify (match-type nick message)
  "Notify when a message is recieved."
  (when (or (eq match-type 'current-nick)
            (and (eq match-type 'keyword)
                 (null (string-match "^\\(bodger\\|CHANSERV\\|server\\|*buffextras\\)" nick))))
      (notifications-notify
       :title nick
       :body message
       :app-icon "/usr/share/notify-osd/icons/gnome/scalable/status/notification-message-im.svg"
       :urgency (if (and (eq match-type 'current-nick)
                         (null (string-match "^\\(bodger\\|CHANSERV\\|server\\|*buffextras\\)" nick)))
                    'critical
                  'normal))))

(add-hook 'erc-text-matched-hook 'erc-global-notify)

;; (asf-erc-bouncer-connect erc-freenode "irc.freenode.net" 6667 "newbie" nil "pass1")
;; ssl connecion:
;; (asf-erc-bouncer-connect erc-freenodessl "irc.freenode.net" 6697 "newbie" t nil)


;; Erc Auto Join
(defmacro erc-autojoin (&rest args)
    `(add-hook 'erc-after-connect
       '(lambda (server nick)
          (cond
           ,@(mapcar (lambda (servers+channels)
                       (let ((servers (car servers+channels))
                             (channels (cdr servers+channels)))
                         `((member erc-session-server ',servers)
                           (mapc 'erc-join-channel ',channels))))
                     args)))))

;; (erc-autojoin
;;    (("irc.freenode.net" "irc.perl.org") "#perl"))

;; GPG encrypted auth info
;; to encrypt your `auth-irc.el` with a password just run:
;; `gpg -c auth-irc.el`

;;;###autoload
(defun init-irc ()
  (interactive)
  (load-file "~/.emacs-private/auth-irc.el.gpg"))

;; }}
(provide 'init-irc)
