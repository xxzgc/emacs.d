;;; defuns.el --- several useful functions which can be used in dialy work

;; Copyright (C) 2013 Free Software Foundation, Inc.
;;
;; Author: Taras Iagniuk <mrtaryk@gmail.com>
;; Maintainer: Taras Iagniuk <mrtaryk@gmail.com>
;; Created: 02 Jun 2013
;; Version: 0.01
;; Keywords

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'defuns)

;;; Code:

(eval-when-compile
  (require 'cl))

;;
;; perl interpreter exec command
;;
(defcustom perlrun "/usr/bin/env perl"
  "Perl interpreter"
  :type 'string)

(require 'json)

(defun package-name ()
  (let ((package-file (buffer-file-name)))
    (if (string-match "libs?\\/\\([a-zA-Z_0-9\\/]+\\)\\.pm$"
                      package-file)
      (replace-regexp-in-string "/" "::" 
        (match-string 1 package-file)))))

;;
;; Evaluates a selected region through a shell command  `perl -e '<code>'`
;;
(defun p5-eval-marked-region(&optional extra-args code-prefix code-suffix)
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
(defun p5-eval ()
  (interactive)
  (let ((result (p5-eval-marked-region)))
    (message result)))

;;
;; Evaluates a selected region the same as `eval-perl`. 
;; The result will be printed into echo area.
;;
(defun p5-eval-print ()
  (interactive)
  (let ((result (p5-eval-marked-region nil "print (" ")")))
    (message result)))

;;
;; Evaluates a selected region the same as `eval-perl`. 
;; The result will be printed into current buffer at current position
;;
(defun p5-eval-insert ()
  (interactive)
  (let ((result (p5-eval-marked-region nil "print (" ")")))
    (with-current-buffer (current-buffer) (insert result))))

;;
;; Strictly to сheck up perl syntax on region
;;
(defun p5-check-syntax-on-region-strict ()
  (interactive)
  (let ((result (p5-eval-marked-region " -Mstrict -W -c ")))
    (message result)))

;;
;; To check up perl syntax on region
;;
(defun p5-check-syntax-on-region ()
  (interactive)
  (let ((result (p5-eval-marked-region " -c ")))
    (message result)))

;;
;; Adds temp buffer "*Perl installed modules*" with list of all installed 
;; modules on current perl version
;;
(defun p5-installed-modules ()
  (interactive)
  (let ((installed-modules (shell-command-to-string 
                            (concat perlrun 
                                    " -MExtUtils::Installed -e "
                                    "'$,=\"\\n\";print(ExtUtils::Installed->new()->modules())'"))))
    (with-output-to-temp-buffer "*Perl installed modules*"
      (clear-rectangle (point-min) (point-max))
      (princ installed-modules))))

;; `curl -s 'https://metacpan.org/search/autocomplete?q=Mojo`

(defun p5-cpan-modules-completion (inputstr)
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
(defun p5-cpan-suggestion ()
  (interactive)
  (let ((keymap (copy-keymap minibuffer-local-map))
        (minibuffer-completion-table
         (completion-table-dynamic 'p5-cpan-modules-completion)))
     (define-key keymap (kbd "<tab>") 'minibuffer-complete)
     (read-from-minibuffer "CPAN module: " nil keymap nil nil nil nil)))

;;
;; Checks if there is a perl-module and puts generated link into the minibuffer
;;
(defun p5-make-module-link (via module-name &optional module-url link-desc)
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
(defun p5-module-url (module-name)
  "Prints a perl-module link that opens in default browser"
  (interactive "sModule name: ")
  (funcall 'p5-make-module-link 
           'browse-url-generic 
            module-name 
            (concat "http://metacpan.org/module/" 
                    module-name)
           "Open in default browser"))

;;
;; Prints a perl-module link that opens in emacs-w3m browser
;;
(defun p5-module-url-w3m (module-name)
  "Prints a perl-module link that opens in emacs-w3m browser"
  (interactive "sModule name: ")
  (funcall 'p5-make-module-link 
           'w3m-browse-url
            module-name 
            (concat "http://metacpan.org/module/" 
                    module-name)
           "Open in w3m"))
;;
;; Prints a perl-module link that opens in w3m-perldoc
;;
(defun p5-module-url-w3m-perldoc (module-name)
  "Prints a perl-module link that opens in w3m-perldoc"
  (interactive "sModule name: ")
  (funcall 'p5-make-module-link
           'w3m-perldoc 
            module-name
            module-name
           "Open in w3m-perldoc"))

;;
;; Function prints result of command:
;; /usr/bin/env perl -M'Module::Name' -e'print $INC{q{Module/Name.pm}}'
;; or error when it can't locate a perl module
;;
(defun p5-module-path (module-name)
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
(defun p5-module-version (module-name)
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
(defun p5-open-module (module-name)
  "Open a perl module in a buffer"
  (interactive "sPerl module name: ")
  (let ((path (p5-module-path module-name)))
    (if path
        ;; open file in a buffer
        (find-file path)
      (error "Module '%s' not found" module-name))))

;;
;; Creates a test script like ~/Development/perl/sandbox/name/test_name.pl
;; and opens it in a buffer
;;
(defun p5-create-sandbox-script (script-name)
  "Creates a test script in sandbox folder"
  (interactive "sSandbox script name (without ext): ")
  (let* ((sandbox-path "~/Development/perl/sandbox/")
         (sandbox-name (replace-regexp-in-string "\\W+" "-" script-name))
         (path-to-folder (concat sandbox-path
                                 sandbox-name))
         (prefix-path (concat path-to-folder
                             "/sandbox_"))
         (path-to-file prefix-path)
         (suffix 0))
    (progn
      (unless (file-exists-p path-to-folder)
        (mkdir path-to-folder))
      (while (file-exists-p 
              (setq path-to-file
               (concat prefix-path sandbox-name 
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
(defun p5-latest-version ()
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
(defun p5-gloablly-replace-sub-calls (subs &optional start end)
  (dolist (item subs)
    (goto-char start)
    ;; ignore variables $@%
    ;; for example, `fooBar;` will be replaced with `foo_bar;` 
    ;; but `$fooBar;` won't be affected
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
(defun p5-camelcase-subs-to-underscores-region (start end)
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
        (p5-gloablly-replace-sub-calls subs start end)))))

;;
;; Converts the definitions of camelCase subroutine names into
;; the underscore-separated style in current buffer
;;
(defun p5-camelcase-subs-to-underscores-buffer ()
  "Converts the definitions of camelCase subroutine names into
   the underscore separated style in current buffer"
  (interactive)
  (p5-camelcase-subs-to-underscores-region (point-min) (point-max)))

;;(defun get-used-modules (start end)
;;  (while
;;      (re-search-forward "\\(use\\|require\\)\\s-+\\([A-Za-z_0-9:]+\\)\\s-+;" end t)
;;)

(defun json-to-perl (start end)
  ;; replace `" : ` to `" => `
  (replace-regexp "\"\\s-*:\\s-*" "\" => " nil start end)
  ;; unquote left-hand side keys if it's possible
  (replace-regexp "\"\\([A-Za-z_0-9]+\\)\"\\s-*=>" "\\1 =>" nil start end))

(defun p5-json-to-perl-region (start end)
  "Convert JSON string into a Perl structure. Bool type convers into 1/0"
  (interactive "*r")
  ;; replace true to 1
  (replace-regexp "\"\\s-*:\\s-*true" "\" => 1" nil start end)
  ;; replace false to 2
  (replace-regexp "\"\\s-*:\\s-*false" "\" => 0" nil start end)
  (json-to-perl start end))

(defun p5-json-to-perl-bool-region (start end)
  "Convert JSON string into a Perl structure. Bool type convers into JSON::true/JSON::false"
  (interactive "*r")
  ;; replace true to JSON::true
  (replace-regexp "\"\\s-*:\\s-*true" "\" => JSON::true" nil start end)
  ;; replace false to JSON::false
  (replace-regexp "\"\\s-*:\\s-*false" "\" => JSON::false" nil start end)
  (json-to-perl start end))

(provide 'defuns)
;;; defuns.el ends here
