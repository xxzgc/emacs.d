(add-to-list 'load-path "~/.emacs.d/packages/emacs-jabber-0.8.91/")
(require 'jabber)

;; jabber params
(load-file "~/.emacs-private/auth-jabber.el")

(custom-set-variables
 '(jabber-alert-presence-hooks nil)
 '(jabber-auto-reconnect t)
 '(jabber-chat-buffer-show-avatar nil)
 '(jabber-chat-delayed-time-format "%Y-%m-%d %H:%M:%S")
 '(jabber-chat-time-format "%H:%M:%S")
 '(jabber-events-confirm-composing nil)
 '(jabber-history-enabled t)
 '(jabber-history-muc-enabled t)
 '(jabber-history-size-limit 1073741824)
 '(jabber-libnotify-method (quote shell))
 '(jabber-libnotify-urgency "normal")
 '(jabber-roster-show-title t)
 '(jabber-use-global-history nil)
 '(jabber-vcard-avatars-retrieve nil))

(custom-set-faces
 '(jabber-chat-prompt-local ((t (:foreground "forest green" :weight bold))))
 '(jabber-roster-user-online ((t (:foreground "royal blue" :slant normal :weight bold))))
 '(jabber-title-large ((t (:inherit variable-pitch :weight bold :height 1.0 :width ultra-expanded))))
 '(jabber-title-medium ((t (:inherit variable-pitch :weight bold :height 1.0 :width expanded)))))

(add-hook 'jabber-alert-message-hooks 'libnotify-jabber-notify)

(defun libnotify-jabber-notify (from buf text proposed-alert)
  "(jabber.el hook) Notify of new Jabber chat messages via libnotify"
  (when (or jabber-message-alert-same-buffer
            (not (memq (selected-window) (get-buffer-window-list buf))))
    (if (jabber-muc-sender-p from)
        (notify-send (format "(PM) %s"
                       (jabber-jid-displayname (jabber-jid-user from)))
               (format "%s: %s" (jabber-jid-resource from) text)))
      (notify-send (format "%s" (jabber-jid-displayname from))
             text)))

(provide 'init-jabber)
