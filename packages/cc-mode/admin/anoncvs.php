<!-- -*- html -*- -->
<?php
  $title = "Anonymous CVS";
  include ("header.h");
?>

<p>CC Mode used to be developed under CVS.  You can get the last CVS
development version, just before the conversion to Mercurial in Febrary 2012.
You can also browse this historical CVS
repository <a href="http://cc-mode.cvs.sourceforge.net/cc-mode/cc-mode">here</a>.

<p>To get the final release branch with only bug fixes and no new features,
check out the patch branch, which is called <i>Branch_5_32</i>:

<pre>
cvs -d:pserver:anonymous@cc-mode.cvs.sourceforge.net:/cvsroot/cc-mode login</pre>

<p>Just press Enter at the password prompt. Then:

<pre>
cvs -z3 -d:pserver:anonymous@cc-mode.cvs.sourceforge.net:/cvsroot/cc-mode co -rBranch_5_32 cc-mode</pre>

<h3>Checking out the final development version</h3>

<p>The development version was on the main trunk in cvs:

<pre>
cvs -z3 -d:pserver:anonymous@cc-mode.cvs.sourceforge.net:/cvsroot/cc-mode co cc-mode</pre>

<p>Please don't report any bugs you might find in these versions.  Instead,
clone the latest Mercurial <a href="hgaccess.php">repository</a> and report
the bugs which are there.  :-)

<h3>The source tree</h3>

<p>The CC Mode source would have been in the root of the checked out tree.
It's the same thing you'd find in a dist tarball (with a couple of extra
files around it), so it could have been byte compiled and used straight away.
You'd have found the regression test suite in the <code>tests</code>
directory; look in <code>000tests.el</code> to figure out how to use it.
The <code>admin</code> directory probably wasn't very interesting; it just
contained the source for this web site.

<?php include ("footer.h"); ?>
