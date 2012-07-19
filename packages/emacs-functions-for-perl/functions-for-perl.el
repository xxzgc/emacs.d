;;; functions-for-perl.el --- several useful functions for those who works with perl

;; Copyright (C) 2011-2012 Taras Yagniuk <truestyler@gmail.com>
;;

;; Author: Taras Yagniuk
;; URL: https://github.com/taryk/emacs-functions-for-perl
;; Version: 0.1.6
;; Keywords: perl

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Installation:

;; 1. Move functions-for-perl.el to emacs load-path directory. E.g: ~/.emacs.d/
;;    or specify path to functions-for-perl.el in your ~/.emacs file:
;;       (add-to-list 'load-path "/dir/to/functions-for-perl.el")
;;        
;; 2. In your .emacs file:
;;       (require 'functions-for-perl)
;;

;;; Code:

(defconst ffp-version "0.1.6"
  "Version number")

(require 'json)

;;
;; perl interpreter exec command
;;
(defcustom perlrun "/usr/bin/env perl"
  "Perl interpreter"
  :type 'string)

;;
;; Evaluates a selected region through a shell command  `perl -e '<code>'`
;;
(defun eval-marked-region(&optional extra-args code-prefix code-suffix)
  (let* ((code (buffer-substring (mark) (point))))
    (shell-command-to-string (concat perlrun (if extra-args extra-args)
                                             " -e '"
                                             (if code-prefix code-prefix)
                                             code
                                             (if code-suffix code-suffix)
                                             "'"))))
;;
;; Only evaluates a selected region without explicit result printing.
;;
(defun yta/eval-perl ()
  (interactive)
  (let ((result (eval-marked-region)))
    (message result)))

;;
;; Evaluates a selected region the same as `yta/eval-perl`. 
;; The result will be printed into echo area.
;;
(defun yta/print-eval-perl ()
  (interactive)
  (let ((result (eval-marked-region nil "print (" ")")))
    (message result)))

;;
;; Evaluates a selected region the same as `yta/eval-perl`. 
;; The result will be printed into current buffer at current position
;;
(defun yta/insert-eval-perl ()
  (interactive)
  (let ((result (eval-marked-region nil "print (" ")")))
    (with-current-buffer (current-buffer) (insert result))))

;;
;; Strictly to сheck up perl syntax on region
;;
(defun yta/check-syntax-on-region-strict ()
  (interactive)
  (let ((result (eval-marked-region " -Mstrict -W -c ")))
    (message result)))

;;
;; To check up perl syntax on region
;;
(defun yta/check-syntax-on-region ()
  (interactive)
  (let ((result (eval-marked-region " -c ")))
    (message result)))

;;
;; Adds temp buffer "*Perl installed modules*" with list of all installed 
;; modules on current perl version
;;
(defun yta/perl-installed-modules ()
  (interactive)
  (let ((installed-modules (shell-command-to-string 
                            (concat perlrun 
                                    " -MExtUtils::Installed -e "
                                    "'$,=\"\\n\";print(ExtUtils::Installed->new()->modules())'"))))
    (with-output-to-temp-buffer "*Perl installed modules*"
      (clear-rectangle (point-min) (point-max))
      (princ installed-modules))))

;; `curl -s 'https://metacpan.org/search/autocomplete?q=Mojo`

(defun cpan-modules-completion (inputstr)
  (let* ((inputstr (replace-regexp-in-string "::" "-" inputstr))
         (apiurl (format "curl -s 'http://api.metacpan.org/v0/release/_search?q=%s&fields=distribution&size=150'" 
                         (url-hexify-string inputstr)))
         (resjson (shell-command-to-string apiurl))
         (jsonh (json-read-from-string resjson)))
   (mapcar #'(lambda(data) 
        (replace-regexp-in-string "-" "::" (cdr (assoc 'distribution 
                                                       (assoc 'fields data)))))
    (cdr (assoc 'hits (assoc 'hits (cddr jsonh)))))))

;;
;; Autocomplete for the CPAN-modules in minibuffer (using MetaCPAN API)
;;
(defun yta/cpan-suggestion ()
  (interactive)
  (let ((keymap (copy-keymap minibuffer-local-map))
        (minibuffer-completion-table
         (completion-table-dynamic 'cpan-modules-completion)))
     (define-key keymap (kbd "<tab>") 'minibuffer-complete)
     (read-from-minibuffer "CPAN module: " nil keymap nil nil nil nil)))

;;
;; Checks if there is a perl-module and puts generated link into the minibuffer
;;
(defun yta/make-module-link (via module-name &optional module-url link-desc)
  ;; metacpan api for checking the module existence
  (let* ((metacpan-api-url 
          (concat "http://api.metacpan.org/v0/module/" 
                   module-name))
         ;; external curl for the API call
         (command-line 
          (concat "curl -s "
                  metacpan-api-url))
         (response (shell-command-to-string command-line)))
      ;; parsing json response
      (if (string-match "^{\[\s?\n\]+\"message\"\s*:\s*\"Not found\"" response)
        (error "Module %s not found" module-name)
        (let ((map (make-sparse-keymap)))
           ;; @note: lexical bindings there is in emacs 24 (lexical-binding ... )
           (lexical-let ((via via)
                         (arg (if module-url module-url module-name)))
             (define-key map (kbd "<down-mouse-1>") 
                 #'(lambda() 
                     (interactive) 
                     (funcall via arg)
                     (with-current-buffer (window-buffer (minibuffer-window))
                       ;; clear minibuffer
                       (clear-rectangle (point-at-bol) (point-at-eol))))))
           (with-current-buffer (window-buffer (minibuffer-window))
             ;; clear minibuffer
             (clear-rectangle (point-at-bol) (point-at-eol))
             ;; put propertized text into the minibuffer
             (insert (concat link-desc " " (propertize module-url
                                                       'mouse-face 'highlight
                                                       'keymap map))))))))

;;
;; Prints a perl-module link that opens in default browser
;;
(defun yta/module-url (module-name)
  "Prints a perl-module link that opens in default browser"
  (interactive "sModule name: ")
  (funcall 'yta/make-module-link 
           'browse-url-generic 
            module-name 
            (concat "http://metacpan.org/module/" 
                    module-name)
           "Open in default browser"))

;;
;; Prints a perl-module link that opens in emacs-w3m browser
;;
(defun yta/module-url-w3m (module-name)
  "Prints a perl-module link that opens in emacs-w3m browser"
  (interactive "sModule name: ")
  (funcall 'yta/make-module-link 
           'w3m-browse-url
            module-name 
            (concat "http://metacpan.org/module/" 
                    module-name)
           "Open in w3m"))
;;
;; Prints a perl-module link that opens in w3m-perldoc
;;
(defun yta/module-url-w3m-perldoc (module-name)
  "Prints a perl-module link that opens in w3m-perldoc"
  (interactive "sModule name: ")
  (funcall 'yta/make-module-link
           'w3m-perldoc 
            module-name
            module-name
           "Open in w3m-perldoc"))

;;
;; Function prints result of command:
;; /usr/bin/env perl -M'Module::Name' -e'print $INC{q{Module/Name.pm}}'
;; or error when it can't locate a perl module
;;
(defun yta/perl-module-path (module-name)
  "Prints perl module path by module name"
  (interactive "sModule name: ")
  (let* ((file-name
           (concat (replace-regexp-in-string "::" "/" module-name)
                   ".pm"))
          (command-line
           (concat perlrun " -M'"
                   module-name
                   "' -e 'print $INC{q{"
                   file-name
                   "}}'"))
          (path (shell-command-to-string command-line))
          (cant-locate (string-match "^Can't locate " path)))
     (if cant-locate
         (error (concat "Can't locate: " path))
         (message path))))

;;
;; Function prints result of command:
;; /usr/bin/env perl -M'Module::Name' -e'print $Module::Name::VERSION'
;; or error when it can't locate a perl module or can't get a perl module version
;; 
(defun yta/perl-module-version (module-name)
  "Prints version of a perl module"
  (interactive "sModule name: ")
  (let* ((command-line 
          (concat perlrun " -M'"
                  module-name
                  "' -e 'print $"
                  module-name
                  "::VERSION'"))
        (mod-version (shell-command-to-string command-line)))
    (progn
      (if (string-match "^Can't locate " mod-version)
          (error "Can't locate perl module %s. "
                 "Maybe it haven't installed or path haven't defined in @INC" 
                 module-name))
      (if mod-version
          (message "%s - %s" module-name mod-version)
          (error "Can't get '%s' version" module-name)))))


;;
;; Open a perl module in a buffer
;;
(defun yta/open-perl-module (module-name)
  "Open a perl module in a buffer"
  (interactive "sPerl module name: ")
  (let ((path (yta/perl-module-path module-name)))
    (if path
        ;; open file in a buffer
        (find-file path)
      (error "Module '%s' not found" module-name))))

;;
;; Creates a test script like ~/Development/perl/poligon/name/test_name.pl
;; and opens it in a buffer
;;
(defun yta/create-perl-poligon-script (script-name)
  "Creates a test script in poligon folder"
  (interactive "sPoligon script name (without ext): ")
  (let* ((poligon-path "~/Development/perl/poligon/")
         (poligon-name (replace-regexp-in-string "\\W+" "-" script-name))
         (path-to-folder (concat poligon-path
                                 poligon-name))
         (prefix-path (concat path-to-folder
                             "/poligon_"))
         (path-to-file prefix-path)
         (suffix 0))
    (progn
      (unless (file-exists-p path-to-folder)
        (mkdir path-to-folder))
      (while (file-exists-p 
              (setq path-to-file
               (concat prefix-path poligon-name 
                       (if (> suffix 0) (number-to-string suffix)) ".pl")))
        (message "File %s already exists" path-to-file)
        (setq suffix (+ suffix 1)))
      (switch-to-buffer
        (create-file-buffer path-to-file))
      (set-visited-file-name path-to-file)
      (cperl-mode)
      (save-buffer)
      (set-file-modes path-to-file 
                      (string-to-number "755" 8))
      (message "Created %s" path-to-file))))

;;
;; Retrieves http://www.perl.org/get.html and extracts a latest stable version 
;; of perl
;;
(defun yta/latest-perl-version ()
  "Prints latest version number of perl"
  (interactive)
  ;; external curl for retrieving html
  (let ((html (shell-command-to-string "curl -s http://www.perl.org/get.html"))
        (version 0))
    (progn
      ;; extracting a perl version from a link to tarball in html
      (string-match "perl\-\\([0-9\.]+\\)\.tar\.gz"
                    html)
      (setq version (match-string 1 html))
      (unless version
        (error "Can't detect the latest perl version"))
      (message "The Latest Perl v%s" version))))

;;
;; Replaces the subroutines calls
;;
(defun yta/gloablly-replace-sub-calls (subs &optional start end)
  (dolist (item subs)
    (goto-char start)
    ;; ignore variables $@%
    ;; for example, `fooBar;` will be replaced with `foo_bar;` but `$fooBar;` won't be affected
    (while (re-search-forward (concat "\\([^\\w$@%]\\|^\\)\\("
                                      (car item) 
                                      "\\)\\(\\W\\|$\\)") end t)
        (replace-match (concat (match-string-no-properties 1)
                               (cadr item)
                               (match-string-no-properties 3))))))

;;
;; Converts the definitions of camelCase subroutine names into
;; an underscore-separated style in selected region
;;
;; definitions
;; `sub camelCase { ... }`
;; replaces with
;; `sub camel_case { ... }`
;;
(defun yta/camelcase-subs-to-underscores-region (start end)
  "Converts the definitions of camelCase subroutine names into
   the underscore separated style in selected region"
  (interactive "*r")
  (save-excursion
    (let* ((end (copy-marker end))
           subs
           (case-fold-search nil))  ;; case sensitive !
      (progn
        (while
          (progn
            (goto-char start)
            ;; detects on the lowercase character followed by uppercase one
            (re-search-forward "sub\\s-+\\(\\<\\w*[a-z][A-Z]\\w*\\>\\)" end t))
          (let* ((sub (match-string-no-properties 1))
                 (subr (downcase 
                        (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)"
                                                  "\\1_\\2" 
                                                  sub))))
            (progn
              (push (list sub subr) subs)
              (replace-match
               ;; lowercase whole subroutine name
               (concat "sub " subr)))))
        (yta/gloablly-replace-sub-calls subs start end)))))

;;
;; Converts the definitions of camelCase subroutine names into
;; the underscore-separated style in current buffer
;;
(defun yta/camelcase-subs-to-underscores-buffer ()
  "Converts the definitions of camelCase subroutine names into
   the underscore separated style in current buffer"
  (interactive)
  (yta/camelcase-subs-to-underscores-region (point-min) (point-max)))



(provide 'functions-for-perl)

;;; functions-for-perl.el ends here
