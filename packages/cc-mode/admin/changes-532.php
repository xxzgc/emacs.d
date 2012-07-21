<!-- -*- html -*- -->
<?php
  $title = "Changes for CC Mode 5.32";
  $menufiles = array ("links.h", "changelinks.h");
  include ("header.h");
?>

<p>See also the <a href="changes-531.php">user visible changes for
5.31</a>.

<p><a
href="http://prdownloads.sourceforge.net/cc-mode/cc-mode-5.32.tar.gz">Download</a>
this CC Mode version.</p>

<p>This version contains few, but big, new features, and significant internal
improvements.

<ul>

   <p><li>CC Mode is now licensed under the GPL version 3 or any later version.

   <p><li>Emacs 21 is no longer supported, although CC Mode might well still
   work with it.
       <br>The minimum versions supported are Emacs 22 and XEmacs 21.4.

   <p><li>It is now possible for CC Mode to "guess" an existing buffer's
   style.
       <br>This style can then be used in other buffers.  Contributed by
       Masatake YAMATO, after original code by Barry Warsaw.

   <p><li>Java Mode has been enhanced to support Java 5.0 (Tiger) and Java 6
   (Mustang).
       <br>Contributed by Nathaniel Flath.

   <p><li><code>c-beginning-of-defun</code> and <code>c-end-of-defun</code>
   now respect nested scopes.
       <br>Thus <code>C-M-a</code> will, by default, go to the beginning of
       the immediate function, not the top level.

   <p><li>"Macros with semicolons" can be registered, for correct indentation.
       <br>Where such a macro ends a line (no semicolon), the next statement
       is no longer parsed as a statement continuation.

   <p><li>Many bugs have been fixed, font locking has become more accurate,
   angle brackets are better handled, and sluggish code has been optimised.

<?php include ("footer.h"); ?>
