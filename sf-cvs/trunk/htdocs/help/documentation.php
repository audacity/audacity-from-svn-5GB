<?php
/*
 * Copyright 2004 Matt Brubeck
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "documentation";
  $pageTitle = _("Documentation");
  include "../include/header.inc.php";

	echo "<h2>$pageTitle</h2>";

	// i18n-hint:  Please add a note that the manuals below are in English.  If
	// there is documentation available in your language, link to it here.
	echo "<p>"._('This page contains manuals and user’s guides for the free Audacity sound editor.  For additional help, see the <a href="faq">Frequently Asked Questions</a> and <a href="tutorials">Tutorials</a>.')."</p>";
?>

<h3><?=_('Quick Reference Guide')?></h3>
<ul>
	<li><a href="../onlinehelp-1.2/"><?=_('Read the Audacity reference online.')?></a> <?=_('(You can also access this reference though the "Help" menu in the Audacity program.)')?></li>
	<li><a href="../onlinehelp-1.2/reference.html"><?=_('Printable version of the Audacity reference.')?></a></li>
</ul>

<h3><?=_('Audacity User’s Manual')?></h3>
<ul>
	<li><a href="../manual-1.2/"><?=_('Read the Audacity manual online.')?></a></li>
	<li><a href="../audacity-manual-1.2.zip"><?=_('Download the Audacity manual.')?></a> <?=_('(To use the downloaded manual, unzip it and then open the "index.html" file.)')?></li>
</ul>

<h3><?=_('Other Resources')?></h3>
<ul>
	<li><?=_('<a href="http://audacityteam.org/wiki/">The Audacity Wiki</a> contains tips, tricks, and documentation that anyone can edit.')?></li>
</ul>

<?php
  include "../include/footer.inc.php";
?>
