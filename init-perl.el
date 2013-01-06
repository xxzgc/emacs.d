;; {{
;; Perl
(defvar init-perl-module)

(defalias 'perl-mode 'cperl-mode)

;; (add-hook 'cperl-mode-hook 
;;           (lambda()
;;             (unless (boundp 'init-perl-module) (init-perl))))

;; xs-mode
(require 'xs-mode)
(autoload 'xs-mode "xs-mode" "Major mode for XS files" t)

;; Template Toolkit mode
(setq load-path (cons "~/.emacs.d/packages/tt-mode" load-path))
(require 'tt-mode)

;; SEPIA -  Simple Emacs-Perl Interface
(setq load-path (cons "~/.emacs.d/packages/sepia" load-path))
(require 'sepia)

;;
;; cpanm Devel::PerlySense
;;       Devel::CoverX::Covered
;;       File::Corresponding
;;       Project::Libs
;;       App::perlbrew

;; perlbrew-mini
(require 'perlbrew-mini)

(perlbrew-mini-set-perls-dir "/opt/perl5/perls/")

;; (defvar perlbrew-current "perl-5.16.0-threaded")
(defvar perlbrew-init-file "~/.perlbrew/init")

(defun perlbrew-detect ()
  (let ((file-content (file-string perlbrew-init-file)))
    (if (file-exists-p perlbrew-init-file)
        (when (string-match "export\\s-PERLBREW\_PERL\=\"\\(.+\\)\"" 
                            file-content)
           (match-string-no-properties 1 file-content)))))


(perlbrew-mini-use (perlbrew-detect))

;; ffap-perl-module
(eval-after-load "ffap" '(require 'ffap-perl-module))

;; PerlTidy
(require 'perltidy)

;; Perl::Critic
(require 'perlcritic)

;; flymake
(require 'flymake)
(set-face-attribute 'flymake-errline nil 
                    :underline "red" 
                    :background nil)
(set-face-attribute 'flymake-warnline nil
                    :underline "yellow"
                    :background nil)

(setq flymake-log-level 3)

(defun flymake-perl-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list (perlbrew-mini-get-current-perl-path)
          (list "-MProject::Libs" "-wc" local-file))))

(defun flymake-display-warning (warning) 
  "Display a warning to the user, using lwarn"
  (lwarn 'flymake :warning warning))

(setq cperl-indent-level 2)
(setq cperl-continued-statement-offset 0)
(setq cperl-extra-newline-before-brace nil)
(setq cperl-indent-parens-as-block t)
(setq cperl-electric-parens t)
(setq cperl-electric-keywords t)

(setq cperl-highlight-variables-indiscriminately t)
; (set-face-background 'cperl-array-face "wheat")
; (set-face-background 'cperl-hash-face "wheat")


(local-set-key (kbd "C-h f") 'cperl-perldoc)

;; flymake
(flymake-mode t)

;; man completion
(eval-after-load "man" '(require 'man-completion))

(setq man-completion-at-point-functions
       '(man-completion-transform-perl
         man-completion-transform-poco))

;; enable perl-completion
(require 'perl-completion)
(perl-completion-mode t)

(when (require 'auto-complete nil t) ; no error whatever auto-complete.el is not installed.
  (auto-complete-mode t)
  (make-variable-buffer-local 'ac-sources)
  (setq ac-sources
    '(ac-source-perl-completion)
  )
)

(custom-set-faces
  '(cperl-array-face ((t (:foreground "green" :weight bold))))
  ;; '(cperl-hash-face ((t (:foreground "Red" :slant italic :weight bold))))
  '(cperl-hash-face ((t (:foreground "orange red" :weight bold)))))

(setq cperl-electric-keywords nil)
;; (init-pde)
;; (init-perlysence)

(defun init-pde ()
  (interactive)
  ;; PDE
  (add-to-list 'load-path "~/.emacs.d/pde/")
  (load "pde-load"))

(defun init-perlysence ()
  (interactive)
  ;;
  ;; ** PerlySense **
  ;; The PerlySense prefix key (unset only if needed, like for \C-o)
  (global-unset-key "\C-o")
  (setq ps/key-prefix "\C-o")

  ;; ** Flymake **
  ;; Load flymake if t
  ;; Flymake must be installed.
  ;; It is included in Emacs 22
  ;;     (or http://flymake.sourceforge.net/, put flymake.el in your load-path)
  ;; (setq ps/load-flymake t)
  ;; Note: more flymake config below, after loading PerlySense


  ;; *** PerlySense load (don't touch) ***
  (setq ps/external-dir (shell-command-to-string "perly_sense external_dir"))
  (if (string-match "Devel.PerlySense.external" ps/external-dir)
      (progn
        (message
         "PerlySense elisp files  at (%s) according to perly_sense, loading..."
         ps/external-dir)
        (setq load-path (cons
                         (expand-file-name
                          (format "%s/%s" ps/external-dir "emacs")
                          ) load-path))
        (load "perly-sense")
        )
    (message "Could not identify PerlySense install dir.
  Is Devel::PerlySense installed properly?
  Does 'perly_sense external_dir' give you a proper directory? (%s)" ps/external-dir)
    )


  ;; ** Flymake Config **
  ;; If you only want syntax check whenever you save, not continously
  (setq flymake-no-changes-timeout 9999)
  (setq flymake-start-syntax-check-on-newline nil)

  ;; ** Code Coverage Visualization **
  ;; If you have a Devel::CoverX::Covered database handy and want to
  ;; display the sub coverage in the source, set this to t
  (setq ps/enable-test-coverage-visualization nil)

  ;; ** Color Config **
  ;; Emacs named colors: http://www.geocities.com/kensanata/colors.html
  ;; The following colors work fine with a white X11
  ;; background. They may not look that great on a console with the
  ;; default color scheme.
  ;;  (set-face-background 'flymake-errline "antique white")
  ;;  (set-face-background 'flymake-warnline "lavender")
  (set-face-attribute 'flymake-errline nil 
                      :underline "red" 
                      :background nil)
  (set-face-attribute 'flymake-warnline nil
                      :underline "yellow"
                      :background nil)
  (set-face-background 'dropdown-list-face "lightgrey")
  (set-face-background 'dropdown-list-selection-face "grey")

  ;; ** Misc Config **

  ;; Run calls to perly_sense as a prepared shell command. Experimental
  ;; optimization, please try it out.
  (setq ps/use-prepare-shell-command t))

;; }}

(defun package-name ()
  (let ((package-file (buffer-file-name)))
    (if (string-match "libs?\\/\\([a-zA-Z_0-9\\/]+\\)\\.pm$"
                      package-file)
      (replace-regexp-in-string "/" "::" 
        (match-string 1 package-file)))))

;;
;; perl interpreter exec command
;;
(defcustom perlrun "/usr/bin/env perl"
  "Perl interpreter"
  :type 'string)

(require 'json)

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

(provide 'init-perl)
