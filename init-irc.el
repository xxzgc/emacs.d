;; {{
;; ERC ( is a powerful, modular, and extensible Emacs InternetRelayChat client )
;; docs: http://mwolson.org/static/doc/erc.html )

(custom-set-variables
 '(erc-log-channels-directory "~/.emacs-irc-log")
 '(erc-log-insert-log-on-open t)
 '(erc-log-mode t)
 '(erc-log-write-after-send t))

(defmacro asf-erc-bouncer-connect (command server port nick ssl pass)
   "Create interactive command `command', for connecting to an IRC server. The
   command uses interactive mode if passed an argument."
   (fset command
         `(lambda (arg)
           (interactive "p")
	   (if (not (= 1 arg))
	       (call-interactively 'erc)
	     (let ((erc-connect-function ',(if ssl
			       'erc-open-ssl-stream
			 		     'open-network-stream)))
 	       (erc :server ,server :port ,port :nick ,nick :password ,pass))))))

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

;; auth info
(load-file "~/.emacs-private/auth-irc.el")

;; }}
(provide 'init-irc)
