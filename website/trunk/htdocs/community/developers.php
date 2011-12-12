<?php
/*
 * Copyright 2004 Matt Brubeck
 * 2007-2011 Vaughan Johnson, Gale Andrews
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "developers";
  $pageTitle = _("Developers");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>

<p><?=_('<a href="http://www.ohloh.net/projects/59">Ohloh</a> has statistics on the value of Audacity development. The Ohloh &quot;badge&quot; at the bottom of each page on this site shows updated summary values. We set up the Ohloh pages about Audacity so the statistics show only the values of Audacity-specific development, excluding the third-party code libraries Audacity uses. The <a href="http://www.ohloh.net/projects/59/analyses/latest">Code tab</a> on the Ohloh site shows the progress of code development.')?></p>

<h3><?=_('Mailing Lists')?></h3>
<p><?=_('Join the <a href="http://lists.sourceforge.net/lists/listinfo/audacity-devel">audacity-devel</a> mailing list to discuss current and future development, or to get help building and modifying the Audacity source code.')?></p>

<h3 id="svn"><?=_('SVN')?></h3>
<p><?=_('You can now get both the Audacity Stable and latest Beta development code from our <a href="http://code.google.com/p/audacity/source/browse/audacity-src/trunk">SVN repository</a>.')?>

<ul>
  <li><p><?=_('Type this at the command line to check out the latest Stable source code from SVN:')?> (1.2)</p>
  <p><kbd>svn checkout http://audacity.googlecode.com/svn/audacity-src/branches/AUDACITY_1_2/  audacity-read-only</kbd></p>
  <p><?=_('See our Stable source code page for <a href="../download/source#instructions">compilation instructions</a>.')?></p>
  </li>
</ul>

<ul>
  <li><p><?=_('Type this at the command line to check out the latest Beta development source code from SVN:')?> (1.3)</p>
  <p><kbd>svn checkout http://audacity.googlecode.com/svn/audacity-src/trunk/ audacity-read-only</kbd></p>
  <p><?=_('See our development source code page for <a href="../download/beta_source#instructions">compilation instructions</a>.')?></p>
</li>
</ul>

<p><?=_('The SVN repository may also be accessed using many different GUI and IDE <a href="http://subversion.tigris.org/links.html#clients">clients and plug-ins</a>. If using GUI or IDE tools, please use the commands indicated in the tool\'s documentation.')?></p>

<h3 id="bugs"><?=_('Bug Tracking')?></h3>
<p><?=_('The Audacity developers use bugzilla to track bugs and enhancements. To find issues to work on, please view our categorized <a href="http://wiki.audacityteam.org/wiki/Bug_Lists">Bug Lists</a>.')?></p>
<p><?=_('If you are a user reporting a new bug, please e-mail our <a href="../contact#feedback">feedback address</a>.')?></p>

<h3><?=_('Submitting Patches')?></h3>
<p><?=_('We welcome patches from developers. Please see our <a href="http://wiki.audacityteam.org/wiki/SubmittingPatches">Submitting Patches</a> page on the <a href="http://wiki.audacityteam.org/wiki">Wiki</a>.')?></p>

<?php
  include "../include/footer.inc.php";
?>
