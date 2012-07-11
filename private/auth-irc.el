;; (setq rcirc-authinfo '(("irc.perl.org" nicserv "test" "")))
;; (setq rcirc-server-alist '(("irc.perl.org" :channels ("#ru.pm"))))

(asf-erc-bouncer-connect erc-perlorg "irc.perl.org" 6667 "test" nil "*******")
(asf-erc-bouncer-connect erc-freenode "irc.freenode.net" 6667 "test" nil "*******")
(asf-erc-bouncer-connect erc-freenodessl "irc.freenode.net" 6697 "test" t "*******")

(erc-autojoin
    ;; (("irc.perl.org") "#ru.pm" "#mojo" "#plack" "#perl" "#parrot")
    (("irc.perl.org") "#ru.pm" "#mojo")
    ;; (("irc.freenode.net") "#perl" "#perl6" "#mongodb" "#node.js" "#emacs"))
    (("irc.freenode.net") "#perl"))

(defun start-irc ()
   "Connect to IRC."
   (interactive)
   (erc-tls :server "irc.freenode.net" :port 6697  :nick "test")
   (setq erc-autojoin-channels-alist '(("freenode.net" "#perl"))))
