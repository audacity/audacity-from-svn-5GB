<?php
/*
 * Copyright 2004 Matt Brubeck
 * Copyright 2005 - 8 Dominic Mazzoni, Gale Andrews
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
 */
  require_once "main.inc.php";
  require_once "../beta/versions.inc.php";
  $pageId = "beta_source";
  $pageTitle = _("Source Code");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>

<?php include "betawarning.inc.php" ?>

<h3 id="recdown"><?=_("Recommended Download")?></h3>

<ul>
  <li><p><?php printf(_('<a href="%s">Audacity %s release (source tarball)</a> (.tar.bz2 file, %.1lf MB)'), "../beta/".$src_url, src_version, src_size)?></p></li>
</ul>

<h3><?=_("Optional Downloads")?></h3>
<ul>
  <li><p><?=_('Get the latest development code (incorporating changes since the release tarball) from <a href="../community/developers#cvs">CVS</a>. This is the best choice if you are compiling on Windows.')?></p></li>
  <li><p><?php printf(_('If you have trouble with your download, or need an older version of Audacity, try our <a href="%s">alternate download links</a>.'), "http://sourceforge.net/project/showfiles.php?group_id=6235")?></p></li>
</ul>

<h3 id="sysreq"><?=_("System Requirements")?></h3>
 <ul>
  <li><?=_('Audacity runs best with at least 64 MB RAM and a 300 MHz processor. Please review our operating system support for')?> <a href="./beta_windows#sysreq">Windows</a>, <a href="./beta_mac#sysreq">Mac</a> <?=_('or')?> <a href="./beta_linux#sysreq">Linux</a>.</li>
 </ul>


<h3 id="instructions"><?=_("How to Compile Audacity")?></h3>

<h4><?=_("Dependencies")?></h4>
<p><?=_('The <a href="http://wxwidgets.org">wxWidgets</a> library is <b>required</b>.  Audacity 1.3.6 requires wxGTK 2.8.9.')?></p>
<p><?=_('Installation of the following libraries is <b>optional</b> - they are included in Audacity obtained from <a href="../community/developers#cvs">CVS</a>.')?></p>
<ul>
  <li><a href="http://www.underbit.com/products/mad/">libmad</a></li>
  <li><a href="http://www.mega-nerd.com/libsndfile/">libsndfile</a></li>
  <li><a href="http://vorbis.com/">Ogg Vorbis</a></li>
</ul>
<p><?=_('If you install libraries using a package management system like Apt or RPM, you need to install the "dev" (development) packages for each library.')?></p>

<h4><?=_("Compilation")?></h4>
<p><?=_("To compile Audacity, run the following command in the Audacity source directory:")?></p>
<blockquote>
<i>./configure &amp;&amp; make</i>
</blockquote>
<p><?=_("You can type <i>./configure --help</i> to see a list of compilation options. After Audacity is compiled, run <i>make install</i> as root to install it.")?></p>

<h4><?=_("Further Help")?></h4>
<p><?=_('
<ul>
 <li>On Windows, see the file "compile.txt" inside the "Win" folder in the source code. For OS X, see "compile.txt" inside the "Mac" folder in the code.</li>
 <li>See our guides to compiling Audacity for <a href="http://audacityteam.org/wiki/index.php?title=Developing_On_Windows">Windows</a>, <a href="http://audacityteam.org/wiki/index.php?title=Developing_On_Mac">Mac</a> and <a href="http://audacityteam.org/wiki/index.php?title=Developing_On_Linux">Linux/Unix</a> on the <a href="http://audacityteam.org/wiki/">Wiki</a>.</li>
 <li>If you are still having difficulties, we want to help! Please join <a href="http://lists.sourceforge.net/lists/listinfo/audacity-devel">audacity-devel</a>, our developers\' mailing list, then send us an e-mail.</li>
</ul>
')?></p>

<p>&nbsp;</p>

<?php
  include "../include/footer.inc.php";
?>
