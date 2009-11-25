<?php
/*
 * Copyright 2004 Matt Brubeck
 * 2007-2008 Vaughan Johnson, Gale Andrews
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "developers";
  $pageTitle = _("Developers");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>

<p><?=_{'<a href="http://www.ohloh.net/projects/59">Ohloh</a> has statistics on the value of Audacity development. The Ohloh &quot;badge&quot; at the bottom of each page on this site shows updated summary values. We set up the Ohloh pages about Audacity so the statistics show only the values of Audacity-specific development, excluding the third-party code libraries Audacity uses. The <a href="http://www.ohloh.net/projects/59/analyses/latest">Code tab</a> on the Ohloh site shows the progress of code development.")?></p>

<h3><?=_('Mailing Lists')?></h3>
<p><?=_('Join the <a href="http://lists.sourceforge.net/lists/listinfo/audacity-devel">audacity-devel</a> mailing list to discuss current and future development, or to get help building and modifying the Audacity source code.')?></p>

<h3 id="cvs"><?=_('CVS')?></h3>
<p><?=_('You can get the latest Audacity stable or development code from our CVS repository.')?>

<ul>
  <li><p><?=_('Browse the source code online sorted by <a href="http://audacity.cvs.sourceforge.net/audacity/audacity-src/">file name</a> or <a href="http://audacity.cvs.sourceforge.net/audacity/audacity-src/?sortby=date#dirlist">date</a>.')?></p></li>

  <li><p><?=_('Type this at the command line to check out the latest stable source code from CVS:')?> (1.2)</p>
  <p><kbd>cvs -d:pserver:anonymous@audacity.cvs.sourceforge.net:/cvsroot/audacity checkout -r AUDACITY_1_2 audacity</kbd></p></li>

  <li><p><?=_('Or type this at the command line to check out the latest beta/development source code from CVS:')?> (1.3)</p>
  <p><kbd>cvs -d:pserver:anonymous@audacity.cvs.sourceforge.net:/cvsroot/audacity checkout audacity</kbd></p></li>
</ul>
<p><?=_('For more information, read the <a href="http://sourceforge.net/cvs/?group_id=6235">SourceForge CVS instructions</a>. If using an interface CVS tool such as TortoiseCVS or WinCVS, use the command instructed in the tool\'s documentation.')?></p>
<p><?=_('See the source code page for <a href="../download/source#instructions">compilation instructions</a>.')?></p>

<h3 id="bugs"><?=_('Bug Tracking')?></h3>
<p><?=_('The Audacity developers now use <a href="http://audacityteam.org/wiki/index.php?title=Release_Checklist">Release Checklist</a> on the <a href="http://audacityteam.org/wiki/index.php">Wiki</a> to keep track of confirmed bugs. Check the list of <a href="http://audacityteam.org/wiki/index.php?title=Release_Checklist#Release_Issues">Release Issues</a> on that page if you are looking for something to work on.')?></p>
<p><?=_('<strong>Note:</strong> This Checklist is for development use only. If you are a user reporting new bugs, please <a href="../contact/">use our Forum or e-mail us</a> instead.')?></p>

<h3><?=_('Submitting Patches')?></h3>
<p><?=_('We welcome patches from developers. Please see our <a href="http://audacityteam.org/wiki/index.php?title=SubmittingPatches">Submitting Patches</a> page on the <a href="http://audacityteam.org/wiki/index.php?title=Audacity_Wiki_Home_Page">Wiki</a>.')?></p>

<?php
  include "../include/footer.inc.php";
?>