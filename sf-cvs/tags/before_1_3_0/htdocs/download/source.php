<?php
/*
 * Copyright 2004 Matt Brubeck
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  require_once "../latest/versions.inc.php";
  $pageId = "source";
  $pageTitle = _("Source Code");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>

<h3><?=_("Recommended Download")?></h3>

<ul>
  <li><p><?php printf(_('<a href="%s">Audacity %s source tarball</a> (%.1lf MB)'), "../latest/".$src_url, src_version, src_size)?></p></li>
</ul>

<h3><?=_("Optional Downloads")?></h3>
<ul>
  <li><p><?=_('You can also get the latest development code from <a href="../community/developers#cvs">CVS</a>.')?></p>
  <li><p><?php printf(_('If you have trouble with your download, or need an older version of Audacity, try our <a href="%s">alternate download links</a>.'), "http://sourceforge.net/project/showfiles.php?group_id=6235")?></p></li>
</ul>

<h3 id="instructions"><?=_("How to Build Audacity")?></h3>

<h4><?=_("Dependencies")?></h4>
<p><?=_('The <a href="http://wxwidgets.org">wxWidgets</a> library is <b>required</b>.  Audacity 1.2 needs wxGTK 2.4, compiled <em>without</em> the gtk2 or unicode options.  (Future versions of Audacity will support newer wxWidgets and GTK libraries.)')?></p>
<p><?=_("You may also choose to install the following <b>optional libraries</b>, or get them by checking out Audacity from CVS.")?></p>
<ul>
  <li><a href="http://www.underbit.com/products/mad/">libmad</a></li>
  <li><a href="http://www.mega-nerd.com/libsndfile/">libsndfile</a></li>
  <li><a href="http://vorbis.com/">Ogg Vorbis</a></li>
</ul>
<p><?=_("If you install libraries using a package management system like Apt or RPM, make sure to install the \"dev\" (development) packages for each library.")?></p>

<h4><?=_("Compilation")?></h4>
<p><?=_("To build Audacity, run the following command in the Audacity source directory:")?></p>
<blockquote>
<pre>
./configure &amp;&amp; make
</pre>
</blockquote>
<p><?=_("You can type <kbd>./configure --help</kbd> to see a list of compilation options.  After Audacity is compiled, run <kbd>make install</kbd> as root to install it.")?></p>

<h4><?=_("Notes")?></h4>
<p><?=_("For additional information about building Audacity on Windows or Mac OS X, see the files <b>win/compile.txt</b> and <b>mac/compile.txt</b> in the Audacity source code directory.")?></p>

<?php
  include "../include/footer.inc.php";
?>
