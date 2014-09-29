<?php
/*
 * Copyright 2004 Matt Brubeck
 * 2007-2014 Vaughan Johnson, Gale Andrews
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
 */
  require_once "main.inc.php";
  $pageId = "developers";
  $pageTitle = _("Developers");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>

<p><?=_('<a href="https://www.openhub.net/p/audacity">Open HUB</a> (formerly known as "Ohloh") has statistics on the value of Audacity development. The Open HUB &quot;badge&quot; at the bottom of each page on this site shows updated summary values. The statistics are set up by us to show only the values of Audacity-specific development, excluding the third-party code libraries Audacity uses. Searchable <a href="https://www.openhub.net/p/audacity/commits">commit listings</a> are produced every 10 days.')?></p>

<h3><?=_('Mailing Lists')?></h3>
<p><?=_('Join the <a href="http://lists.sourceforge.net/lists/listinfo/audacity-devel">audacity-devel</a> mailing list to discuss current and future development, or to get help modifying the Audacity source code.')?></p>

<h3 id="svn"><?=_('SVN')?></h3>
<p><?=_('You can get the latest Audacity code from our <a href="http://code.google.com/p/audacity/source/browse/audacity-src/trunk">SVN repository</a>.')?>

<ul>
  <li><p><?=_('Type this at the command line to check out the latest source code from SVN:')?></p>
  <p><kbd>svn checkout http://audacity.googlecode.com/svn/audacity-src/trunk/ audacity-read-only</kbd></p>
</ul>
</p>

<p><?php printf(_('You can also check out a specific <a href="%s">tagged release</a> from SVN.'), "http://code.google.com/p/audacity/source/browse/#svn%2Faudacity-src%2Ftags")?>
<ul> 
  <li><p><?=_('Use the following syntax at the command line to check out a specific tagged release from SVN, for example:')?></p>
  <p><kbd>svn checkout http://audacity.googlecode.com/svn/audacity-src/tags/Audacity_2_0_3 audacity-read-only</kbd></p>
</ul>
</p>

<p><?=_('The SVN repository may also be accessed using GUI and IDE <a href="http://wiki.audacityteam.org/wiki/SVN_Clients">clients or plug-ins</a>. If using GUI or IDE tools, please use the commands indicated in the tool\'s documentation.')?></p>

<h3><?=_("Compiling Audacity")?></h3>
<p><? printf('<ul><li>%s</li>',
	_('On Windows, see the file "compile.txt" inside the "Win" folder in the source code. For OS X, see "compile.txt" inside the "Mac" folder in the code.'));
  printf(_('%s Further information can be found in <a href="%s">Developer Guide</a> and in our more detailed guides on compiling Audacity for %sWindows%s, %sMac%s and %sGNU/Linux%s.%s'),
   '<li>',
   'http://wiki.audacityteam.org/wiki/Developer_Guide',
   '<a href="http://wiki.audacityteam.org/wiki/Developing_On_Windows">',
   '</a>',
   '<a href="http://wiki.audacityteam.org/wiki/Building_On_Mac">',
   '</a>',
   '<a href="http://wiki.audacityteam.org/wiki/Developing_On_Linux">',
   '</a>',
   '</li>');
  printf(_('%sIf you are still having difficulties, please ask on the %sCompiling Audacity%s board on the %sAudacity Forum%s.%s'),
  '<li>',
  '<a href="http://forum.audacityteam.org/viewforum.php?f=19">',
  '</a>',
  '<a href="http://forum.audacityteam.org">',
  '</a>',
  '</li></ul>');
?></p>

<h3 id="bugs"><?=_('Bug Tracking')?></h3>
<p><?=_('The Audacity developers use bugzilla to track bugs and enhancements. To find issues to work on, please view our categorized <a href="http://wiki.audacityteam.org/wiki/Bug_Lists">Bug Lists</a>.')?></p>
<p><?=_('If you are a user reporting a new bug, please e-mail our <a href="../contact#feedback">feedback address</a>.')?></p>

<h3><?=_('Submitting Patches')?></h3>
<p><?=_('We welcome patches from developers. Please see our <a href="http://wiki.audacityteam.org/wiki/SubmittingPatches">Submitting Patches</a> page on the <a href="http://wiki.audacityteam.org/wiki">Audacity Wiki</a>.')?></p>

<?php
  include "../include/footer.inc.php";
?>
