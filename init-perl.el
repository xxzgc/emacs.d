;;; {{
;;; Perl
;;
;; Used modules:
;; * Devel::PerlySense
;; * Devel::CoverX::Covered
;; * Emacs::PDE
;; * File::Corresponding
;; * Project::Libs
;; * App::perlbrew
;; * ExtUtils::Installed
;;
(defvar init-perl-module)

;; cperl-mode --
;; (add-to-list 'load-path "~/.emacs.d/packages/cperl-mode")
(require 'cperl-mode)
(defalias 'perl-mode 'cperl-mode)

; (set-face-background 'cperl-array-face "wheat")
; (set-face-background 'cperl-hash-face "wheat")

;; cperl-mode faces

(custom-set-faces
  '(cperl-array-face ((t (:foreground "green" :weight bold))))
  ;; '(cperl-hash-face ((t (:foreground "Red" :slant italic :weight bold))))
  '(cperl-hash-face ((t (:foreground "orange red" :weight bold)))))

(local-set-key (kbd "C-h f") 'cperl-perldoc)

(add-hook 'cperl-mode-hook 
           (lambda()
             (perl-completion-mode t)
             (yas/minor-mode-on)))

;; xs-mode
(require 'xs-mode)
(autoload 'xs-mode "xs-mode" "Major mode for XS files" t)

;; Template Toolkit mode
;; (setq load-path (cons "~/.emacs.d/packages/tt-mode" load-path))
(require 'tt-mode)

;; SEPIA -  Simple Emacs-Perl Interface
;; (setq load-path (cons "~/.emacs.d/packages/sepia" load-path))
(require 'sepia)

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

(eval-after-load "init-flymake" '(lambda()
  ;;; flymake-perlcritic
  ;; (add-to-list 'load-path "~/.emacs.d/packages/emacs-flymake-perlcritic")
  ;; If flymake_perlcritic isn't found correctly, specify the full path
  (setq flymake-perlcritic-command
        "~/.emacs.d/packages/emacs-flymake-perlcritic/bin/flymake_perlcritic")
  ;; Lets set it to be the most severe available.
  (setq flymake-perlcritic-severity 1)
  ;; If you don't want to use the default ~/.perlcriticrc
  ;; (setq flymake-perlcritic-profile "~/projects/big-project/perlcriticrc")
  (require 'flymake-perlcritic)))

;; man completion
(eval-after-load "man" '(require 'man-completion))

(setq man-completion-at-point-functions
       '(man-completion-transform-perl
         man-completion-transform-poco))

;; enable perl-completion
(require 'perl-completion)

(when (require 'auto-complete nil t) ; no error whatever auto-complete.el is not installed.
  (auto-complete-mode t)
  (make-variable-buffer-local 'ac-sources)
  (setq ac-sources '(ac-source-perl-completion))
  ;;(setq plcmp-extra-using-modules '("DBIx::Class::ResultSet" ("LWP::UserAgent" . "HTTP::Response"))
)

;; cd ~/.emacs.d/packages/emacs-pde/; perl Makefile.PL && make && make install
(defun init-pde ()
  (interactive)
  ;; PDE
  ;; (add-to-list 'load-path "~/.emacs.d/packages/emacs-pde/lisp/")
  (load "pde-load"))

(defun init-perlysense ()
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
  (setq ps/load-flymake nil)
  ;; Note: more flymake config below, after loading PerlySense

  ;; *** PerlySense load (don't touch) ***
  ;; (setq ps/external-dir (shell-command-to-string "perly_sense external_dir"))
  (setq ps/external-dir
        "~/.emacs.d/packages/Devel-PerlySense/source/lib/Devel/PerlySense/external")
  ;; (if (string-match "Devel.PerlySense.external" ps/external-dir)
  ;;     (progn
  ;;       (message
  ;;        "PerlySense elisp files  at (%s) according to perly_sense, loading..."
  ;;        ps/external-dir)
  ;;       (setq load-path (cons
  ;;                        (expand-file-name
  ;;                         (format "%s/%s" ps/external-dir "emacs")
  ;;                         ) load-path))
  ;;       (load "perly-sense")
  ;;       )
  ;;   (message "Could not identify PerlySense install dir.
  ;; Is Devel::PerlySense installed properly?
  ;; Does 'perly_sense external_dir' give you a proper directory? (%s)" ps/external-dir)
  ;;   )

  ;; hardcode external dir ;)
  (add-to-list 'load-path
               (expand-file-name (format "%s/%s" ps/external-dir "emacs")))
  (load "perly-sense")

  ;; ** Flymake Config **
  ;; If you only want syntax check whenever you save, not continously
  ;; (setq flymake-no-changes-timeout 9999)
  ;; (setq flymake-start-syntax-check-on-newline nil)

  ;; ** Code Coverage Visualization **
  ;; If you have a Devel::CoverX::Covered database handy and want to
  ;; display the sub coverage in the source, set this to t
  (setq ps/enable-test-coverage-visualization t)

  ;; ** Color Config **
  ;; Emacs named colors: http://www.geocities.com/kensanata/colors.html
  ;; The following colors work fine with a white X11
  ;; background. They may not look that great on a console with the
  ;; default color scheme.
  ;;  (set-face-background 'flymake-errline "antique white")
  ;;  (set-face-background 'flymake-warnline "lavender")

  (set-face-background 'dropdown-list-face "lightgrey")
  (set-face-background 'dropdown-list-selection-face "grey")

  ;; ** Misc Config **
  ;; Run calls to perly_sense as a prepared shell command. Experimental
  ;; optimization, please try it out.
  (setq ps/use-prepare-shell-command t))

;; }}

(load "~/.emacs.d/perl/defuns.el")

(init-pde)
(init-perlysense)

(setq cperl-auto-newline nil
      cperl-auto-newline-after-colon nil
      cperl-continued-statement-offset 0
      cperl-electric-keywords nil ;; DON'T ENABLE `electric-keywords`!!!
      cperl-electric-linefeed t
      cperl-electric-parens t
      cperl-extra-newline-before-brace nil
      cperl-highlight-variables-indiscriminately t
      cperl-indent-level 4
      cperl-indent-parens-as-block t
      cperl-regexp-scan nil)

(provide 'init-perl)
