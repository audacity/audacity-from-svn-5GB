<?php
/*
 * Copyright 2004 Matt Brubeck
 * 2007-2013 Vaughan Johnson, Gale Andrews
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
 */
  require_once "main.inc.php";
  $pageId = "developers";
  $pageTitle = _("Developers");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>

<p><?=_('<a href="http://www.ohloh.net/projects/59">Ohloh</a> has statistics on the value of Audacity development. The Ohloh &quot;badge&quot; at the bottom of each page on this site shows updated summary values. We set up the Ohloh pages about Audacity so the statistics show only the values of Audacity-specific development, excluding the third-party code libraries Audacity uses. The <a href="http://www.ohloh.net/projects/59/analyses/latest">Code tab</a> on the Ohloh site shows the progress of code development.')?></p>

<h3><?=_('Mailing Lists')?></h3>
<p><?=_('Join the <a href="http://lists.sourceforge.net/lists/listinfo/audacity-devel">audacity-devel</a> mailing list to discuss current and future development, or to get help modifying the Audacity source code.')?></p>

<h3 id="svn"><?=_('SVN')?></h3>
<p><?=_('You can get the latest Audacity code from our <a href="http://code.google.com/p/audacity/source/browse/audacity-src/trunk">SVN repository</a>.  We aren\'t currently working on any other version.')?>

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

<p><?=_('See our development source code page for <a href="../download/source#instructions">compilation instructions</a>.')?></p>

<p><?=_('The SVN repository may also be accessed using GUI and IDE <a href="http://wiki.audacityteam.org/wiki/SVN_Clients">clients or plug-ins</a>. If using GUI or IDE tools, please use the commands indicated in the tool\'s documentation.')?></p>

<h3 id="bugs"><?=_('Bug Tracking')?></h3>
<p><?=_('The Audacity developers use bugzilla to track bugs and enhancements. To find issues to work on, please view our categorized <a href="http://wiki.audacityteam.org/wiki/Bug_Lists">Bug Lists</a>.')?></p>
<p><?=_('If you are a user reporting a new bug, please e-mail our <a href="../contact#feedback">feedback address</a>.')?></p>

<h3><?=_('Submitting Patches')?></h3>
<p><?=_('We welcome patches from developers. Please see our <a href="http://wiki.audacityteam.org/wiki/SubmittingPatches">Submitting Patches</a> page on the <a href="http://wiki.audacityteam.org/wiki">Wiki</a>.')?></p>

<?php
  include "../include/footer.inc.php";
?>
