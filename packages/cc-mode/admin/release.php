<!-- -*- html -*- -->
<?php
  $title = "Current Release";
  include ("header.h");
?>

<p>The current release is 5.32. <a
href="http://prdownloads.sourceforge.net/cc-mode/cc-mode-5.32.tar.gz">Download</a>
the source package to upgrade the version that came with your Emacs or
XEmacs dist.  Installation instructions are available in the <a
href="src/README">README</a> file in the tarball and <a
href="installation.php">on-line</a>.  Older releases can be found <a
href="http://prdownloads.sourceforge.net/cc-mode/">here</a>.

<p>You can also browse the <a href="src/">individual files</a>.

<p>The documentation is also available in several forms.  You can
either <a href="html-manual/index.html">browse</a> it on-line or grab
one of the <a href="manual/">pre-formatted</a> documents generated
from the texinfo source.

<p>See the list of <a href="changes-532.php">user visible changes</a>
since 5.31.3 and earlier versions, and the <a
href="src/ChangeLog">ChangeLog file</a> for details about the bugs
fixed in any patch releases made since the first 5.32 release.

<h3>Notes</h3>

<p>Since version 5.26, CC Mode has supported the use of Filladapt mode to fill
text in comments and string literals.  There are however some <a
href="filladapt.php">issues</a> with this that you should be aware of if you
want to use Filladapt in CC Mode.

<?php include ("footer.h"); ?>
