;; {{
;; twittering-mode ( https://github.com/hayamiz/twittering-mode )
;; (add-to-list 'load-path "~/.emacs.d/packages/twittering-mode")
(require 'twittering-mode)

;; (Optional) To avoid having to authorize twittering-mode everytime you run it
(setq twittering-use-master-password t)

(setq twittering-icon-mode nil)
;; (setq twittering-convert-fix-size 24) ;; default: 48
;; (setq twittering-use-icon-storage t)
;; (setq twittering-icon-storage-file "~/.twittering-mode-icons.gz")
(setq twittering-reverse-mode nil) ;; default: t
(setq twittering-number-of-tweets-on-retrieval 30)
(setq twittering-timer-interval 3600)         ;; Update your timeline each 3600 seconds (1 hour)
;; (twittering-enable-unread-status-notifier) ;; no, I don't need that
(setq twittering-display-remaining t) ;; To display the remaining number of API calls
;; (setq twittering-retweet-format '(nil _ " %u RT @%s: %t"))

(setq twittering-url-show-status nil) ; Keeps the echo area from showing all the http processes

;; (setq twittering-initial-timeline-spec-string
;;       '("(:home+@)"
;;         "(:search/twittering mode/+:search/twmode/)"))

;; (setq twittering-status-format
;;       "%FOLD{%RT{%FACE[bold]{RT}}%i%s>>%r @%C{%Y-%m-%d %H:%M:%S} %@{}\n%FOLD[ ]{%T%RT{\nretweeted by %s @%C{%Y-%m-%d %H:%M:%S}}}}")

;; to specify timelines that should be opened initially
;; (setq twittering-initial-timeline-spec-string
;;       '(":home"
;;         ":replies"
;;         ":favorites"
;;         ":direct_messages"
;;         ":search/emacs/"
;;         "user_name/list_name"))

;; }}

(provide 'init-twitter)
