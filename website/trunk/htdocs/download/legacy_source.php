<?php
/*
 * Copyright 2004 Matt Brubeck
 * 2006 - 2012 Richard Ash, Gale Andrews
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
 */
  require_once "main.inc.php";
  require_once "../legacy/versions.inc.php";
  require_once "../legacy/mirror.inc.php";
  $pageId = "source";
  $pageTitle = _("Legacy Source Code");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>

<h3><?=_("Download")?></h3>

<ul>
  <li><p><?php printf(_('<a href="%s">Audacity %s release (source tarball)</a> (.tar.gz file, %.1lf MB)'), download_url($src_url), src_version, src_size)?></p></li>
</ul>

<h3><?=_("Optional Downloads")?></h3>
<ul>
  <li><p><?=_('Get the source code from <a href="../community/developers#svn">SVN</a>.')?></p></li>
<li><p><?php printf(_('If you have trouble with your download, or need an older version of Audacity, try our alternate download links:')) ?></p>
   <ul><li><?php printf(_('<a href="%s">SourceForge</a> (older versions can be viewed by clicking to enter the required folder under "Name")'), "http://sourceforge.net/project/showfiles.php?group_id=6235")?></li>
       <li><?php printf(_('<a href="%s">Google Code</a> (click on the headings to sort the list)'), "http://code.google.com/p/audacity/downloads/list")?></li>
   </ul>
</li>
</ul>

<h3 id="sysreq"><?=_("System Requirements")?></h3>
 <ul>
  <li><?=_('Audacity runs best with at least 64 MB RAM and a 300 MHz processor. Please review our operating system support for')?> <a href="./windows#sysreq">Windows</a>, <a href="./mac#sysreq">Mac</a> <?=_('or')?> <a href="./linux#sysreq">Linux</a>.</li>
 </ul>


<h3 id="instructions"><?=_("How to Compile Audacity")?></h3>

<h4><?=_("Dependencies")?></h4>
<p><?=_('The <a href="http://wxwidgets.org">wxWidgets</a> library is <b>required</b>.  Audacity 1.2 needs wxGTK 2.4, compiled <em>without</em> the unicode options. The next version of Audacity will support newer wxWidgets and GTK libraries.')?></p>
<p><?=_('Installation of the following libraries is <b>optional</b> - they are included in Audacity obtained from <a href="../community/developers#svn">SVN</a>.')?></p>
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
<p><? printf('<ul><li>%s</li>',
	_('On Windows, see the file "compile.txt" inside the "Win" folder in the source code. For OS X, see "compile.txt" inside the "Mac" folder in the code.'));
  printf(_('%sSee our guides to compiling Audacity for %sWindows%s, %sMac%s and %sGNU/Linux%s on the %sWiki%s.%s'),
   '<li>',
   '<a href="http://wiki.audacityteam.org/wiki/Developing_On_Windows">',
   '</a>',
   '<a href="http://wiki.audacityteam.org/wiki/Developing_On_Mac">',
   '</a>',
   '<a href="http://wiki.audacityteam.org/wiki/Developing_On_Linux">',
   '</a>',
   '<a href="http://wiki.audacityteam.org/">',
   '</a>',
   '</li>');
  printf(_('%sIf you are still having difficulties, we want to help! Please ask on the %sCompiling Audacity%s board on our %sForum%s.%s'),
  '<li>',
  '<a href="http://forum.audacityteam.org/viewforum.php?f=19">',
  '</a>',
  '<a href="http://forum.audacityteam.org/">',
  '</a>',
  '</li></ul>');
?></p>

<p>&nbsp;</p>



<?php
  include "../include/footer.inc.php";
?>
