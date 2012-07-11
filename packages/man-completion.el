;;; man-completion.el --- completion for M-x man

;; Copyright 2008, 2009, 2010 Kevin Ryde

;; Author: Kevin Ryde <user42@zip.com.au>
;; Version: 30
;; Keywords: data
;; URL: http://user42.tuxfamily.org/man-completion/index.html
;; EmacsWiki: ManMode

;; man-completion.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; man-completion.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This spot of code extends M-x man with completion of man page names and
;; filenames, and a new default page name at point which collapses hyphens
;; (like nroff uses in formatted pages), and optionally with transformations
;; of some Perl class names.
;;
;; Emacs 23.2 has incorporated some of this, but man-completion.el has more
;; features.
;;
;; iman.el does similar page name completion, and with info documents too,
;; but not filename completion.  As of rev 2.22 iman.el has the section
;; number like "cat(1)" but sets "require-match" so if you have cat(1) and
;; catdoc(1) then you must "cat(" to differentiate, which is a bit annoying.
;;
;; woman.el does a similar completion too, by looking through /usr/share/man
;; etc for a filename.  That's also what "bash_completion" does
;; (http://www.caliban.org/bash/index.shtml#completion).  But the strategy
;; for man-completion.el is to ask man what pages are available rather than
;; digging.  This may get aliases man knows from its lexgrog parse but which
;; don't have symlinks (or ".so" pages), and might cope with strange
;; /etc/manpath.conf setups.

;;; Install:

;; Put man-completion.el in one of your `load-path' directories and the
;; following in your .emacs
;;
;;     (eval-after-load "man" '(require 'man-completion))
;;
;; To use the smart perl module name at point defaulting, add also
;;
;;     (setq man-completion-at-point-functions
;;           '(man-completion-transform-perl
;;             man-completion-transform-poco))
;;
;; There's an autoload cookie for the `require' below, if you use
;; `update-file-autoloads' and friends.

;;; Emacsen:

;; Designed for Emacs 21 and up.  Works in XEmacs 21.
;; Probably works in Emacs 20 if you get a replace-regexp-in-string.

;;; History:

;; Version 1 - the first version
;; Version 2 - avoid truncation to tty width on very long page names
;; Version 3 - new page at point transform scheme
;; Version 4 - disallow "[" except for sole "[" alias of "test(1)"
;; Version 5 - cope with non-existent `default-directory'
;; Version 6 - fix perl package name pruning when a section number too
;; Version 7 - notice man-db index file changes to refresh page names
;; Version 8 - page name completion case-insensitive
;; Version 9 - disallow "/" for page name at point
;; Version 10 - disallow dash before hyphen like "X--\nY"
;; Version 11 - add emacs23 `boundaries' in completion
;; Version 12 - :: gets perl page name mangling
;; Version 13 - tie-in to completing-help.el for page descriptions
;; Version 14 - tie-in to icicles.el help, and some bug fixes
;; Version 15 - boundaries at perl "::" separator (emacs23)
;; Version 16 - oops, boundaries wrong, want ":" delim for partial completion
;; Version 17 - complete space after any -X option
;; Version 18 - case-sensitive -l option, remove filename `boundaries'
;; Version 19 - undo defadvice on unload-feature
;; Version 20 - speedup for "[" command at point
;; Version 21 - clean man-completion-at-point-functions on other package unload
;; Version 22 - perl page at point turn foo.pm into foo(3perl) or foo(3pm)
;;            - allow buffer-local man-completion-at-point-functions
;; Version 23 - express dependency on 'advice
;; Version 24 - remove thing-at-point on unload-feature
;; Version 25 - put customize group underneath `man'
;; Version 26 - page name at point exclude perl pod E<gt> and E<lt>
;; Version 27 - fix to recognising constant.pm etc perl-only pages
;; Version 28 - page at point for perl -MFoo::Bar and -MO=Debug
;; Version 29 - fix -k arg and output regexp for MacOS-X man as advised by
;;              Daniel Pittman
;; Version 30 - fix regexp no leading "." on page names

;;; Code:

;;;###autoload (eval-after-load "man" '(require 'man-completion))

(require 'man)

;; for `ad-find-advice' macro when running uncompiled
;; (don't unload 'advice before our -unload-function)
(require 'advice)


;;----------------------------------------------------------------------------
;; xemacs21 lacking

;; [same in ffap-makefile-vars.el]
;;
(eval-and-compile ;; quieten emacs byte compiler
  ;; no `eval-when-compile' on this fboundp because in xemacs 21.4.22
  ;; easy-mmode.el (which is define-minor-mode etc) rudely defines a
  ;; replace-regexp-in-string, so a compile-time test is unreliable
  (if (fboundp 'replace-regexp-in-string)
      ;; emacs (21 up)
      (defalias 'man-completion--replace-regexp-in-string
        'replace-regexp-in-string)

    ;; xemacs21
    (defun man-completion--replace-regexp-in-string
      (regexp rep string fixedcase literal)
      "`replace-regexp-in-string' made available in xemacs.
The FIXEDCASE argument is ignored, case is always fixed."
      (replace-in-string string regexp rep literal))))

;;----------------------------------------------------------------------------
;; emacs22 new stuff

;; `file-name-completion' gaining PRED argument in emacs22
(if (eval-when-compile
      (condition-case nil
          (progn
            (file-name-completion "nosuchfilename" "/" (lambda (x) t))
            t)
        (error nil)))

    ;; emacs22 up -- file-name-completion has PRED arg
    (eval-and-compile
      (defalias 'man-completion--file-name-completion
        'file-name-completion))

  ;; emacs21, xemacs21
  (defun man-completion--file-name-completion (file dir &optional pred)
    "`file-name-completion' with PRED argument like Emacs 22.
But PRED is ignored, which is not right, but might be close
enough for current uses."
    (file-name-completion file dir)))

(if (eval-when-compile (fboundp 'complete-with-action))
    ;; emacs22 (and in emacs23 recognising the "boundaries" thing)
    (eval-and-compile
      (defalias 'man-completion--complete-with-action
        'complete-with-action))

  ;; emacs21,xemacs21
  (defun man-completion--complete-with-action (action table string pred)
    (cond ((null action)
           (try-completion string table pred))
          ((eq action t)
           (all-completions string table pred))
          (t
           (eq t (try-completion string table pred))))))

;;----------------------------------------------------------------------------
;; variables

;;;###autoload
(defgroup man-completion nil "Man-Completion."
 :prefix "man-completion-"
 :group  'man
 :link   '(url-link :tag "man-completion.el home page"
                    "http://user42.tuxfamily.org/man-completion/index.html"))

(defcustom man-completion-index-files
  '("/var/cache/man/index.db")
  "Files used by \"man\" for its apropos database.
man-completion notes the file times and if they change it re-runs
\"man\" for a new set of page names.  Usually this is when the
system administrator installs new pages.

The default is a single file \"/var/cache/man/index.db\", which
is what man-db (URL `http://man-db.nongnu.org/') uses.  (Are the
language specific sub-trees like \"/var/cache/man/pt_BR/index.db\"
needed too?)

It's fine if some (or all) the files here don't exist.
man-completion monitors the modtimes of those which exist, and
watches for the non-existent ones being created."

  ;; there's no "options" scheme for `repeat', otherwise might put some
  ;; likely files for various systems
  :group 'man-completion
  :type  '(repeat file))

(defcustom man-completion-at-point-functions nil
  "List of functions to modify `man-completion-at-point'.
Each function is called (FUNC STR) with the apparent page name.
FUNC can transform STR and return a new string which goes on to
the further functions, or it can return either nil or STR
unchanged if it's got nothing to apply.

Caution: This is slightly experimental.  Maybe page name
transformations could be better applied to the user-entered
string too, or instead.

The supplied `man-completion-transform-perl' and
`man-completion-transform-poco' are both for Perl.
man-completion isn't Perl-specific, but Perl is one of only a few
big users of man pages."

  :group   'man-completion
  :type    'hook
  :options '(man-completion-transform-perl
             man-completion-transform-poco))
;; automatically `risky-local-variable-p' due to name `-functions'

;; ask unload-feature to remove stuff when a package is unloaded
(eval-after-load "loadhist"
  '(when (boundp 'unload-feature-special-hooks) ;; new in emacs22
     (add-to-list 'unload-feature-special-hooks
                  'man-completion-at-point-functions)))


;;----------------------------------------------------------------------------
;; generic bits

(defsubst man-completion-assoc-string-ignore-case (key alist)
  "Lookup a string KEY in ALIST, case insensitively."
  ;; cope with incompatible change in Emacs 22 removing the helpful function
  ;; `assoc-ignore-case'
  (if (eval-when-compile (fboundp 'assoc-string))
      (assoc-string key alist t)
    (assoc-ignore-case key alist)))

(defmacro man-completion-with-pcm-colon (&rest body)
  "Evaluate BODY with a \":\" added to `completion-pcm-word-delimiters'."
  ;; (declare (debug t))  ;; emacs22,xemacs21, or 'cl
  (if (eval-when-compile (boundp 'completion-pcm-word-delimiters))
      ;; emacs23 up
      ;;
      ;; `completion-pcm--prepare-delim-re' seems inflexible, so guard
      ;; against it perhaps disappearing in the future.  prepare-delim-re
      ;; sets `completion-pcm--delim-wild-regex'.  Could let-bind to
      ;; preserve that variable, but better not anticipate what variables
      ;; will be setup.
      ;;
      `(unwind-protect
           (let ((completion-pcm-word-delimiters
                  (concat completion-pcm-word-delimiters ":")))
             (when (fboundp 'completion-pcm--prepare-delim-re)
               (completion-pcm--prepare-delim-re completion-pcm-word-delimiters))
             ,@body)
         (when (fboundp 'completion-pcm--prepare-delim-re)
           (completion-pcm--prepare-delim-re completion-pcm-word-delimiters)))

    ;; older emacs
    `(progn ,@body)))

(defun man-completion-hook-to-list (var)
  "Return a list of functions in hook variable VAR.
VAR is a symbol.  If unbound then return an empty list."
  (and (boundp var)
       (let ((value (symbol-value var)))
         (if (functionp value)
             (list value)
           (apply 'append
                  (mapcar (lambda (elem)
                            (if (eq t elem)
                                (let ((value (default-value var)))
                                  (if (functionp value) (list value) value))
                              (list elem)))
                          value))))))


;;----------------------------------------------------------------------------
;; modifying the guessed page at point

(defun man-completion-transform-perl (str)
  "Expand or contract a Perl package name.
This function is designed for `man-completion-at-point-functions'.

* \"foo.pm\" is turned into \"foo(3pm)\" or \"foo(3perl)\", for
  when a single-word module is written with .pm to make it clear
  it's a module name.

* A Perl package name suffix like \"Xyzzy\" expands to say
  \"Some::Thing::Xyzzy\" if that's the only Xyzzy available.
  A multipart \"Xx::Yy\" can expand to \"Some::Thing::Xx::Yy\"
  too.  This is good when a long base part is omitted from
  documentation (or even from code in suitable contexts), eg. POE
  and Perl::Critic.

* A qualified Perl name like \"Foo::Bar::Quux::something\" is
  tried first in full then reduced to \"Foo::Bar::Quux\",
  \"Foo::Bar\" or even just \"Foo\".  This is good for getting
  the package name out of a full function name or a sub-package.
  Sometimes it's too aggressive, but it's only for the default,
  so you can see what it did before accepting.

The prune and expand are only applied if STR has an upper case
letter or a \"::\".  This means plain shell command pages go
through without loading up the full set of page name
completions."

  (let ((case-fold-search nil))
    (cond
     ;; foo.pm
     ((and (string-match "\\.pm\\'" str)
           ;; don't change if "foo.pm" itself exists, though that should be
           ;; unlikely
           (not (man-completion-assoc-string-ignore-case
                 str (man-completion-cache))))

      ;; If there's just one "foo" page then man-completion-cache has "foo"
      ;; and "3perl foo", but "foo(3perl)" is omitted, so must lookup under
      ;; "3perl foo".  Return "foo(3perl)" though, as it looks better in the
      ;; prompt without a space, and the position of the (3perl) or (3pm)
      ;; corresponds to the ".pm".
      ;;
      ;; 3perl is the module supplied with perl and 3pm is a local add-on.
      ;; Prefer the 3pm if both exist as it will normally be the one hit by
      ;; the default @INC path.
      ;;
      (let ((sans-suffix (substring str 0 -3)))
        (or (and (assoc (concat "3pm " sans-suffix)
                        (man-completion-cache))
                 (concat sans-suffix "(3pm)"))
            (and (assoc (concat "3perl " sans-suffix)
                        (man-completion-cache))
                 (concat sans-suffix "(3perl)")))))

     ;; Foo or foo::bar
     ((and (string-match "[A-Z]\\|::" str)
           ;; don't change if STR is already an exact match
           ;; this is a case-insensitive exact match as STR is only a guess,
           ;; in particular it means "Unicode" doesn't go to Encode::Unicode
           ;; when there's a plain lower-case "unicode" available
           (not (man-completion-assoc-string-ignore-case
                 str (man-completion-cache))))

      ;; prefix expansion first and if it produces a match then don't
      ;; need to prune
      (or (man-completion-perl-expand-prefix str)
          (man-completion-perl-prune-suffix str))))))

(defun man-completion-perl-expand-prefix (str)
  "Expand a Perl package name STR.
This is part of `man-completion-transform-perl'.
Return an expanded string, or nil if nothing found."

  (eval-and-compile (require 'cl)) ;; for `remove-duplicates'
  (let* ((case-fold-search nil)
         (matches (remove* (concat "\\(\\`\\|::\\)"
                                   (regexp-quote str) "\\'")
                           (man-completion-cache)
                           :key 'car
                           :test-not 'string-match)))
    (setq matches (mapcar (lambda (elem)
                            (man-completion-name-sans-section (car elem)))
                          matches))
    (setq matches (remove-duplicates matches :test 'string-equal))
    (and (= 1 (length matches))
         (car matches))))

(defun man-completion-perl-prune-suffix (str)
  "Prune a Perl package name STR.
This is part of `man-completion-transform-perl'.
Return a pruned string, or nil if nothing found."

  ;; only act when there's a :: and when the whole thing isn't already good
  (when (and (string-match "::" str)
             (not (assoc str (man-completion-cache))))

    ;; something like "AptPkg::Cache(3pm)" won't be in the cache with its
    ;; 3pm suffix since it's the sole manpage of that name, so try first
    ;; "AptPkg::Cache"
    (setq str (man-completion-name-sans-section str))

    ;; stop at pruned STR found or STR set to nil when no more "::"s
    (while (and (not (assoc str (man-completion-cache)))
                (setq str (and (string-match "\\(.*\\)::" str)
                               (match-string 1 str)))))
    str))

(defun man-completion-transform-poco (str)
  ;; checkdoc-params: (str)
  "Expand PoCo to POE::Component.
This function is designed for `man-completion-at-point-functions'.

\"PoCo\" is expanded to \"POE::Component\", either alone, or like
\"PoCo::Client::DNS\" to \"POE::Component::Client::DNS\".  PoCo
is used in various POE docs but man pages are under the full
name.

This is only of interest if you're using POE, but it does no harm
to have it turned on all the time."

  (let ((case-fold-search nil))
    (and (string-match "\\`PoCo\\(::\\|\\'\\)" str)
         (concat "POE::Component" (substring str (match-beginning 1))))))

(defun man-completion-name-sans-section (str)
  "Return man page name STR without section number.
A leading section like \"1 foo\" or trailing section like
\"foo(1)\" are both stripped to give \"foo\"."
  (if (string-match "\\`.* \\(.*\\)" str)
      (setq str (match-string 1 str)))
  (if (string-match "\\(.*\\)(.*)\\'" str)
      (setq str (match-string 1 str)))
  str)


;;----------------------------------------------------------------------------
;; page name at point

;; `man-completion-at-point-regexp' expresses what is a man page name.
;; man.el has `Man-name-regexp' and friends, but this pattern accepts
;; non-ascii, restricts punctuation, and does hyphenation better.

(defconst man-completion-hyphen-regexp
  (concat "[-"

          ;; xemacs21 doesn't have coding-system-p or decode-char, so use
          ;; coding-system-list and decode-coding-string (of course you need
          ;; mule-ucs to have utf-8 in xemacs21 at all though).
          ;;
          ;; other unicode like en-dash, em-dash or non-breaking hyphen
          ;; would be conceivable too, perhaps in rendered html output, but
          ;; leave them out until seen in practice.  Anything that's really
          ;; a hyphen probably ought to be U+2010 hyphen.
          ;;
          (and (memq 'utf-8 (coding-system-list))
               (let ((str (string 226 128 144))) ;; U+2010 HYPHEN
                 (if (eval-when-compile (fboundp 'string-make-unibyte))
                     (setq str (string-make-unibyte str)))
                 (decode-coding-string str 'utf-8)))

          ;; latin1 #xAD SOFT HYPHEN
          (decode-coding-string (string 173) 'iso-8859-1)

          "]" "\n[ \t]*")

  "Regexp for a hyphenated line break.
This is an internal part of `man-completion-at-point'.

groff spits out ascii, latin1 or unicode U+2010 dashes according
to its output mode.  groff breaks page names rather a lot in its
output, so recognising and collapsing them is important.")


;; In the following `chars' is the permitted characters.  The aim is to make
;; this tight to avoid punctuation in text, or operators in program code.
;;
;;   * The syntax table definition of a word char or symbol char etc is
;;     not used, so as to be mode-independent.  It's not unusual to find
;;     man page names in comments which are not directly related to the
;;     programming language at hand.
;;
;;   * [:alnum:] includes non-ascii chars, so a word at point with accented
;;     chars is taken whole, not just an ascii part of it.  Non-ascii page
;;     names will be unlikely, since there's so many ways for it to go wrong
;;     between filename coding, file content coding and emacs read coding.
;;
;;   * [:alnum:] in emacs23 only means word-syntax for non-ascii chars.
;;     That's close enough, though the intention is to be
;;     mode-independent, including syntax-table independent.
;;
;;   * [:alnum:] not available in xemacs21, hence the fallback to A-Z etc.
;;     What would be better for non-ascii there?
;;
;;   * "/" is disallowed since believe it doesn't normally occur, and
;;     disallowing it gets the the right page name from a perl POD
;;     L<pagename/section>.  The way man uses directories to hold pages will
;;     mean there can't be a "/" in the primary page name, though it might
;;     conceivably be in an alias lexgrog could pick out from the NAME part.
;;
;; `word' is setup so
;;
;;   * "." and "-" are allowed in the middle of a page name, but not as
;;     the first or last char.  Disallowing a final dot helps a page name
;;     at the end of a sentence.
;;
;;   * "-" is not allowed before a hyphen sequence "-\n", so "foo--\nbar"
;;     is not considered a hyphenation.  This helps when the dashes on the
;;     previous line are a separator like a "--cut-here--".  groff doesn't
;;     hyphenate after a dash, unless you explicitly give it permission
;;     with \%, so a double dash shouldn't normally occur.
;;
;;   * "+" is allowed in the middle or at the end, but not at the start.
;;     "+" at the end is needed for say "g++" and other C++ stuff.
;;
;;   * "[" and "]" are disallowed like other parentheses, but there's a
;;     special case in `man-completion-at-point' for "[" alone, the shell
;;     "test" command.
;;
;;   * [:digit:] digits are allowed at the start of a page name.  This is
;;     unusual, but found in things like the Debian "822-date" program.
;;
;; `man-completion-at-point-regexp' is then setup so
;;
;;   * "::" is allowed for perl module names like B::Lint etc.  Each part
;;     must be a `word' per the rule above.  Don't think ":" occurs in a
;;     page name apart from doubled for perl, but maybe that could be
;;     loosened (which would simplify the regexp too).  A ":" at the end
;;     is disallowed though, since that'd most often be punctuation.
;;
;;   * A "::" is deliberately not matched at the end, for the benefit of
;;     perl code using the double-colon bareword Glib::Object:: etc for a
;;     class name.  In that case just Glib::Object is the page name.
;;
;;   * A section suffix like "chmod(2)" is matched only as a digit "(2)"
;;     or digit and alnums like "(3X)" or "(3ncurses)".  The match is kept
;;     tight so that on program code like "exit(errors ? 0 : 1)" it won't
;;     get the arg in parens, just the "exit".  Clearly this can be
;;     tricked by say "exit(1)" as code, but a space is not matched so GNU
;;     style "exit (1)" is ok.  It's expected that in comments or a
;;     document you won't put a space before a section number.
;;
;;     Alpha-only section IDs were, maybe still are, found on some non-free
;;     systems.  One system of evil memory, to remain nameless, but which
;;     had letters S, C, and O in its name, had say "(S)" for system
;;     commands or something like that.  Really not too interested in doing
;;     anything for those.
;;
(let* ((alnum  (if (string-match "[[:alnum:]]" "A") "[:alnum:]" "A-Za-z0-9"))
       (digit  (if (string-match "[[:digit:]]" "0") "[:digit:]" "0-9"))
       (chars  (concat alnum "_"))
       (first  (concat "[" chars "]"))   ;; first and maybe only char
       (last   (concat "[" chars "+]"))  ;; last char
       (middle (concat "[" chars ".+-]"))
       (nodash (concat "[" chars ".+]")) ;; middle without dash
       (hyphen man-completion-hyphen-regexp)
       (word   (concat first "\\("
                       "\\(" hyphen "\\)?"
                       "\\(" middle "*" nodash hyphen "\\)*"
                       middle "*" last
                       "\\)?")))
  (defconst man-completion-at-point-regexp
    (concat word
            "\\(::" word "\\)*"              ;; perl "::" module name parts
            "\\(([" digit "][" alnum "_]*)\\)?") ;; optional section suffix
    "Regexp for a man page name at point.
This is an internal part of `man-completion-at-point'.")

  (defconst man-completion-at-point-regexp-M
    (concat "\\(\\(\\`\\|[^" chars "]\\)"
            "-M\\(O=\\)?\\)"
            "\\(" man-completion-at-point-regexp "\\)") ;; group 4
    "Regexp for Perl -MFoo::Bar or -MO=Concise at point.
This is an internal part of `man-completion-at-point'."))

(defun man-completion-looking-at-brack ()
  "Return non-nil if point is before or after a [ shell test command.
This is an internal part of `man-completion-at-point'.  There
must be whitespace or start or end of buffer before and after the
\[, since that's what /bin/sh demands for [ as the test command.

This used to be a simple `thing-at-point-looking-at', but that
ended up searching the whole buffer for a \"[\" in its workaround
for `re-search-backward' refusing to match across point, which
was slow in a big buffer without any \"[\"s."

  (save-excursion
    (if (equal ?\[ (char-before))
        (goto-char (1- (point))))
    (and (equal ?\[ (char-after))
         (or (looking-at "\\`\\[\\(\\s-\\|\\'\\)")
             (progn
               (goto-char (1- (point)))
               (looking-at "\\s-\\[\\(\\s-\\|\\'\\)"))))))

(defun man-completion-at-point ()
  "Return a man page name string at point, or nil if none.
There's no checking whether the page exists, this only picks out
something from the buffer that might be a page name.

A section number like \"(2)\" in \"chmod(2)\" is included if
present.  A hyphenated line break is recognised and collapsed
out, which is good for man output where nroff often hyphenates
words."

  (eval-and-compile
    (require 'thingatpt))

  (save-excursion
    ;; if point is just after a word then go back one char to let
    ;; thing-at-point-looking-at match on that previous word
    (and (not (bobp))
         (save-excursion
           (goto-char (1- (point)))
           (looking-at "\\S-\\(\\s-\\|\\'\\)"))
         (goto-char (1- (point))))

    (let ((str
           (if (man-completion-looking-at-brack) ;; special case for "["
               "["

             ;; Narrow to exclude a perl pod E<gt> in a marked-up class
             ;; method call like "C<Foo::Bar-E<gt>method>".  Want "Foo::Bar"
             ;; not "Foo::Bar-E".
             ;;
             ;; Exclude E<lt> too in case something like S<FOOE<lt>=BAR>,
             ;; though it'd be unlikely such a FOO was a page name.
             ;;
             ;; Perhaps alphanumeric E<> escapes like E<eacute> could be
             ;; matched by `man-completion-at-point-regexp' and crunched to
             ;; the equivalent formtted char.  But non-ascii in man page
             ;; names is unusual and wanting to visit it from pod markup
             ;; even more unusual.  New enough pod accepts actual non-ascii
             ;; chars too anyway, which need no special handling.
             ;;
             (save-restriction
               (save-excursion
                 (let ((case-fold-search nil)) ;; case sensitive E<>
                   (if (re-search-forward
                        "E<[gl]t>"
                        (save-excursion (forward-line 3) (point))
                        t)
                       (narrow-to-region (point-min) (match-beginning 0)))))

               (cond ((thing-at-point-looking-at
                       man-completion-at-point-regexp-M)
                      (if (match-beginning 3)
                          (concat "B::" (match-string 4))
                        (match-string 4)))
                     ((thing-at-point-looking-at
                       man-completion-at-point-regexp)
                      (match-string 0)))))))

      (when str
        ;; strip hyphens
        (setq str (man-completion--replace-regexp-in-string
                   man-completion-hyphen-regexp "" str t t))
        (dolist (func (man-completion-hook-to-list
                       'man-completion-at-point-functions))
          (setq str (or (funcall func str) str))
          (or (stringp str)
              (error "Not a string from %s" func))))
      str)))

;; Not really making direct use of thing-at-point here, but it doesn't hurt
;; to offer this "thing" through that mechanism too.  Could offer
;; bounds-of-thing-at-point etc too, except that wouldn't get hyphens
;; stripped or the man-completion-at-point-functions transforms.
;;
(put 'man-completion-default 'thing-at-point 'man-completion-at-point)


;;----------------------------------------------------------------------------
;; man page name cache

(defvar man-completion-cache 'uninitialized
  "An alist of available man pages for completion.
When not yet initialized it's the symbol `uninitialized' instead
of a list.  Function `man-completion-cache' initializes if not
already done.  Each entry is

    (NAME-STRING . nil)

or if descriptions have been requested then

    (NAME-STRING . DESCRIPTION-STRING)

For a plain page like \"cat\" there's two entries, the plain name
and with its section

    \"cat\"
    \"1 cat\"

If there's two pages with the same name in different sections
then parenthesized section number forms are included too, so as
to present those alternatives in completion.  The plain name is
also present so completion stops before the \"(\".  Eg.

    \"chmod\"
    \"chmod(1)\"
    \"chmod(2)\"
    \"1 chmod\"
    \"2 chmod\"

Emacs 22 allows a list of strings for completion as well as an
alist, but in Emacs 21 it must be an alist.  Making
man-completion-cache vary would be too tedious for
`man-completion-at-point-functions' which consult it, so it's
always an alist.")

(defvar man-completion-index-files-seen nil
  "Alist of modification times of `man-completion-index-files'.
This is the last seen times.  Currently each element is
    (FILENAME . MODTIME)
where MODTIME is a two-element list per `file-attributes'.")

(defun man-completion-index-files-changed-p ()
  "Check whether any `man-completion-index-files' have changed.
Return true if the files have changed.  The last noticed file
times are updated so this function returns true just once until
the files change again.

If a file has been added to `man-completion-index-files' since
the last check then currently it's treated as unchanged.
Normally you should set interesting `man-completion-index-files'
before doing things with `man-completion-cache'."

  (let (changed)
    (setq man-completion-index-files-seen
          (mapcar (lambda (filename)
                    (let ((modtime (nth 5 (file-attributes filename)))
                          (seen    (assoc filename
                                          man-completion-index-files-seen)))
                      (when (and seen
                                 (not (equal modtime (cdr seen))))
                        (setq changed t))
                      (cons filename modtime)))
                  man-completion-index-files))
    changed))

(defun man-completion-reset ()
  "Discard data cached for `man-completion-read'.
This can be used to get newly installed man pages recognised,
though if the `man-completion-index-files' scheme is working this
should happen automatically."
  (interactive)
  (setq man-completion-cache 'uninitialized))

(defun man-completion-cache (&optional want-desc)
  "Generate and return the `man-completion-cache' list.
If WANT-DESC is t then the list includes page descriptions (by is
re-loading if it doesn't already have descriptions)."

  ;; This code is disappointingly slow if you've got a lot of man pages.
  ;; About half the time goes in "man -k" and reading its output, and the
  ;; rest in crunching the buffer into strings.
  ;;
  ;; The man-db system also offers its page database with the "accessdb"
  ;; program.  accessdb seems a touch quicker than "man -k", but the latter
  ;; should be adequate for now.  The cutest thing might be direct dbm
  ;; access to the man-db database, if dbm can do partial key searches, and
  ;; if you didn't mind getting intimate with stuff normally internal to
  ;; man-db.
  ;;
  ;; In emacs23 it seems a touch quicker to send man -k to a file then read
  ;; and crunch that.  File and pipe reading are the same speed, but getting
  ;; the buffer in a block read of a file seems to help the crunching.
  ;; Maybe it avoids fragmentation or something.  The difference is only
  ;; about 10-15%.
  ;;
  ;; Caching into a file and just `read' of a lisp form to restore is a good
  ;; 5 times faster, if it was worth the trouble to maintain that (comparing
  ;; dates on the man-db index files for updates, as done now).
  ;;

  ;; call `man-completion-index-files-changed-p' even when 'uninitialized,
  ;; so it records the current times of the files, for future comparison
  (when (or (man-completion-index-files-changed-p)
            (eq 'uninitialized man-completion-cache)
            (and want-desc
                 (not (cdar man-completion-cache))))
    (message "Building man page completions ...")
    (with-temp-buffer
      (setq default-directory "/") ;; in case inherited doesn't exist

      ;; "man -k" truncates long page names to the tty width, so set COLUMNS
      ;; to avoid that; and force process-connection-type to a pipe so man
      ;; can't maybe get the width on the pseudo-tty TIOCGWINSZ instead of
      ;; COLUMNS.
      ;;
      ;; Old man.el in XEmacs has `Manual-program' in man.el as well as
      ;; `manual-program' in paths.el.  Which has greater virtue?
      ;; Just `manual-program' is easier because that's the one in Emacs.
      ;;
      ;; man.el runs `manual-program' as a shell command, do the same here,
      ;; allowing maybe environment variables in it or whatnot.
      ;;
      ;; POSIX man -k argument is supposed to be an ERE regexp, but rumour
      ;; has it on MacOS-X it's a BRE or some such, and in particular on
      ;; OS-X an empty string matches nothing.  In man-db the match seems to
      ;; be on the page name and then on the description, so a space " "
      ;; doesn't match if there's no spaces in the description, it seems.
      ;; Try a "^" as an always-match regexp, the same as in Emacs 23.2.
      ;;
      (let ((process-environment (copy-sequence process-environment))
            (process-connection-type nil))
        (setenv "COLUMNS" "999")
        (call-process shell-file-name
                      nil  ;; stdin /dev/null
                      t    ;; stdout+stderr to current-buffer
                      nil  ;; no display
                      shell-command-switch
                      (concat manual-program " -k \"^\"")))

      (let (ret prev prevsect prevdesc)

        ;; "man -k" gives lines like
        ;;     cat (1)      - concatenate files and print ...
        ;; or on MacOS-X without the space,
        ;;     ls(1)        - list directory contents
        ;; or for an "alias", in some past man-db's perhaps
        ;;     boot-scripts (7) [boot] - General description of boot sequence
        ;;
        ;; ".*?" is the page name, the first parens "(foo)" is the section,
        ;; then skip "[foo]" real page and " - " to get to the description
        ;; part.
        ;;
        (goto-char (point-min))
        (while (re-search-forward "\
^\\(.*?\\)\
 ?(\\([^)]+?\\))\
\\( \\[[^]*]\\)?\
\\( +- \\)?\
\\(.*\\)" nil t)
          ;; one \\( \\) group per line
          (let ((name (match-string 1))
                (sect (match-string 2))
                (desc (and want-desc (match-string 5))))

            ;; If previous name was the same then put it with its prevsect
            ;; like "chmod(1)" and also the present one like "chmod(2)".
            (if (equal name prev)
                (progn
                  (setq ret (cons (cons (concat name "(" sect ")")
                                        desc)
                                  ret))
                  (when prevsect
                    (setq ret (cons (cons (concat prev "(" prevsect ")")
                                          prevdesc)
                                    ret))
                    (setq prevsect nil)))

              ;; If previous name different then just a solitary entry for
              ;; this one, like "chmod".
              (setq ret      (cons (cons name desc) ret)
                    prev     name
                    prevsect sect
                    prevdesc desc))

            ;; always an entry like "1 chmod" or "2 chmod" with the sect
            (setq ret (cons (cons (concat sect " " name) desc)
                            ret))))

        (setq man-completion-cache (nreverse ret))))
    (message ""))
  man-completion-cache)


;;----------------------------------------------------------------------------
;; page name completion

(defun man-completion-match-filename (str)
  ;; checkdoc-params: (str)
  "An internal part of `man-completion-handler'.
Match a filename type entered, which means starting Currently
\"/\" or \".\" or \"-l \".  Match group 1 is the \"-l \", or nil
if a plain filename."
  (let ((case-fold-search nil))
    (string-match "\\`\\(-l \\)\\|\\`[./]" str)))

(defun man-completion-handler (str pred action)
  ;; checkdoc-params: (str pred action)
  "Perform completion for `man-completion-read'."
  (cond ((man-completion-match-filename str)
         ;; filename
         (let* ((before (or (match-string 1 str) ""))
                (fname  (substring str (or (match-end 1) 0)))
                (dir    (or (file-name-directory fname) default-directory))
                (part   (file-name-nondirectory fname))
                
                (completion-ignore-case
                 ;; xemacs21 no `read-file-name-completion-ignore-case'
                 (if (boundp 'read-file-name-completion-ignore-case)
                     read-file-name-completion-ignore-case
                   completion-ignore-case)))
           
           (cond ((null action) ;; try-completion
                  (let ((ret (man-completion--file-name-completion part
                                                                   dir pred)))
                    (if (stringp ret)
                        (concat before dir ret)
                      ret)))
                 ((eq action t) ;; all-completions
                  (let ((ret (mapcar (lambda (s) (concat before dir s))
                                     (file-name-all-completions part dir))))
                    (if pred
                        (delq nil (mapcar (lambda (s) (if (funcall pred s) s))
                                          ret))
                      ret)))
                 (t ;; `lambda' for test-completion
                  (file-exists-p fname)))))
        
        ((string-match "\\`-\\(.\\)?" str)
         ;; "-" or "-X" -- alone and with a space so you don't have to type
         ;; C-q space after an option.  "-l" or "-k" are shown as standard
         ;; options, but then add anything else so other options can be
         ;; typed.
         ;;
         ;; For formatting options like "-Tascii" could think about applying
         ;; the pagename or filename completion to the next arg, but
         ;; normally such options will go in `Man-switches'.
         ;;
         (let ((table '(("-l " . "run man on a file")
                        ("-k " . "search apropos database"))))
           (unless (member str '("-k" "-l" "-k " "-l "))
             (push (cons str "option") table)
             (push (cons (concat str " ") "option") table))
           (man-completion--complete-with-action action table str pred)))
        
        (t
         ;; pagename
         (man-completion--complete-with-action action (man-completion-cache)
                                               str pred))))

(defvar icicle-candidate-help-fn) ;; quieten the byte compiler

(defun man-completion-read ()
  "Read the name of a man page.
A page name at point is offered as a default, and completions
come from the pages reported by \"man -k\" (its \"apropos\"
database).

A filename can also be entered as any of the following, with
filename completion.

    ./foo
    /some/dir/foo
    -l foo

An apropos query can be entered as on the \"man\" command line.

    -k foo

This is a normal feature of `M-x man', though before Emacs 23.2
if there's no matches then the error is \"Can't find the -k foo
manpage\", which might make you think it tried to get a page
called \"-k\" or that man-completion has upset it, but it just
means no matches.

Page name completion is case-insensitive because \"man\" is
case-insensitive and because it helps odd combinations of
upper/lower in page names.  Filename case sensitivity follows
`read-file-name-completion-ignore-case' the same as
`read-file-name' does.

If the default page name offered is not quite right remember
the usual `\\<minibuffer-local-map>\\[next-history-element]' puts it in the minibuffer for you to edit.

If `completing-help-mode' is enabled then page descriptions are
shown in the completions list (with `\\<minibuffer-local-completion-map>\\[minibuffer-completion-help]' etc).

If `icicle-mode' is enabled then page descriptions and filename
descriptions are available with the usual icicles help.

The man-completion home page is
URL `http://user42.tuxfamily.org/man-completion/index.html'"

  ;; completion-ignore-case is set here rather than within the handler with
  ;; the idea a C-t could toggle sensitivity by changing the variable.  But
  ;; perhaps it would better operate on a man-completion-ignore-case than
  ;; the global.
  ;;
  ;; completion-pcm-word-delimiters new in emacs23 is pre-loaded, even under
  ;; -batch, so can safely let-bind without (require 'minibuffer), avoiding
  ;; a conditional for minibuffer.el not existing in xemacs21
  ;;
  ;; must update `completion-pcm--delim-wild-regex' for temporary value of
  ;; `completion-pcm-word-delimiters', like the customize :set of that
  ;; variable does
  ;;
  (let* ((completion-ignore-case t)
         (default  (man-completion-at-point))
         (icicle-candidate-help-fn 'man-completion-icicle-help))

    (man-completion-with-pcm-colon
     (or (completing-read (if default
                              (format "Manual entry (default %s): " default)
                            "Manual entry: ")
                          'man-completion-handler
                          nil  ;; predicate
                          nil  ;; require-match
                          nil  ;; initial-input
                          (if (eval-when-compile (boundp 'Man-topic-history))
                              'Man-topic-history             ;; emacs
                            'Manual-page-minibuffer-history) ;; xemacs
                          default)
         default ""))))

;; emacs
(defadvice man (before man-completion activate)
  "Completion and default pagename from `man-completion-read'."
  (interactive (list (man-completion-read))))

;; xemacs
(defadvice manual-entry (before man-completion activate)
  "Completion and default pagename from `man-completion-read'."
  (interactive (list (man-completion-read))))

;;----------------------------------------------------------------------------
;; completing-help.el tie-in

(defvar completing-help-groups) ;; quieten the byte compiler

(defvar man-completion-completing-help-group
  '(:predicate man-completion-completing-help-p
    :get       man-completion-completing-help-get
    :info-head " - "
    :info-tail "")
  "`completing-help-mode' configuration for `man-completion-read'.")

(defun man-completion-completing-help-p ()
  "Return non-nil if within a `man-completion-read'."
  (eq minibuffer-completion-table 'man-completion-handler))

(defun man-completion-completing-help-get (str)
  "Return man page description for STR, or nil if unknown.
If STR is one of the filename forms supported by
`man-completion-read' then the return is always nil."
  (and (not (man-completion-match-filename str))
       (cdr (assoc str (man-completion-cache t)))))

(eval-after-load "completing-help"
  '(if (boundp 'man-completion-completing-help-group) ;; in case we're unloaded
       (add-to-list 'completing-help-groups
                    'man-completion-completing-help-group)))

;;----------------------------------------------------------------------------
;; icicles.el tie-in

(defun man-completion-icicle-help (str)
  "Display help for STR in `icicles-mode'.
Page descriptions are shown with `icicle-msg-maybe-in-minibuffer'
because they're just a single line.  Files are shown with
`icicle-describe-file'."
  (if (man-completion-match-filename str)
      (let ((filename (substring str (or (match-end 1) 0))))
        (unless (equal "" filename)
          ;; not sure this is terrific on a non-existent file, but leave
          ;; that to be handled by icicles
          (icicle-describe-file filename)))
    (let ((desc (cdr (assoc str (man-completion-cache t)))))
      (if desc
          (icicle-msg-maybe-in-minibuffer "%s" desc)))))

;;----------------------------------------------------------------------------

(defun man-completion-unload-function ()
  "Undo man-completion setups.
This is called by `unload-feature'."
  (when (ad-find-advice 'man          'before 'man-completion)
    (ad-remove-advice   'man          'before 'man-completion)
    (ad-activate        'man))
  (when (ad-find-advice 'manual-entry 'before 'man-completion)
    (ad-remove-advice   'manual-entry 'before 'man-completion)
    (ad-activate        'manual-entry))

  (put 'man-completion-default 'thing-at-point nil)

  ;; mustn't leave an unbound var in `completing-help-groups' or it will
  ;; error out as of completing-help.el 3.13
  (if (boundp 'completing-help-groups)
      (setq completing-help-groups
            (remove 'man-completion-completing-help-group
                    completing-help-groups)))

  ;; and do normal unload-feature actions too
  nil)

;; LocalWords: catdoc usr lexgrog symlinks manpath conf db modtimes groff latin initializes initialized parenthesized DESC manpage minibuffer pagename Xyzzy multipart Yy Quux

(provide 'man-completion)

;;; man-completion.el ends here
