<?php
/*
 * Copyright 2004 - 2008 Matt Brubeck, Gale Andrews
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "documentation";
  $pageTitle = _("Documentation");
  include "../include/header.inc.php";

	echo "<h2>$pageTitle</h2>";

     // i18n-hint:  Please add a note that the manuals below are in English.
     // If there is documentation available in your language not already
     // linked to at the bottom of this page, please add the links.
	echo "<p>"._('This page contains manuals and quick reference guides for the free Audacity sound editor.  For additional help, see the <a href="faq">Frequently Asked Questions</a> and our <a href="http://audacityteam.org/wiki/index.php?title=Tutorials">Tutorials</a> and <a href="http://audacityteam.org/wiki/index.php?title=Tips">Tips</a> on the Audacity <a href="http://audacityteam.org/wiki/">Wiki</a>.')."</p>";
?>

<h3><?=_('Quick Reference Guide')?></h3>
<ul>
	<li><a href="../onlinehelp-1.2/"><?=_('Read the Audacity reference online.')?></a> <?=_('(You can also access this reference through the "Help" menu in the Audacity program.)')?></li>
	<li><a href="../onlinehelp-1.2/reference.html"><?=_('Printable version of the Audacity reference.')?></a> <?=_('(This can be printed as a single page using the print function of your browser.)')?></li>
 </ul>

<h3><?=_('Audacity User\'s Manual')?></h3>
<ul>
	<li><a href="../manual-1.2/"><?=_('Read the Audacity manual online.')?></a></li>
	<li><a href="../audacity-manual-1.2.zip"><?=_('Download the Audacity manual in HTML format.')?></a> <?=_('(To use the manual, unzip it and open the "index.html" file.)')?></li>
    	<li><a href="../audacity-manual-1.2.pdf"><?=_('Download the Audacity manual in PDF format.')?></a></li>
</ul>

<div lang="fr">
  <h3>Aide en Français</h3>
  <ul>
    <li><a href="../localhelp/fr/audacity-1.2-help.htb">Menu d'aide d'Audacity en français</a> (Remplacer le fichier qui porte comme nom, audacity-1.2-help.htb par celui qu'on vous propose. <a href="http://zonelibre.grics.qc.ca/libre/article.php3?id_article=100">Aide</a>)</li>
    <li><a href="../audacity-mode-d'emploi.pdf">Mode d'emploi en français</a> (PDF, 700 ko.  Ce fichier peut prendre un bon moment de télécharger.)</li>
 </ul>
</div>

<div lang="de">
  <h3>Hilfe auf Deutsch</h3>
  <ul>
    <li><a href="../localhelp/de/audacity-1.2-help.htb">Deutsche Hilfedatei (htb)</a> (Ersetzen Sie audacity-1.2-help.htb im Audacity Installation-Heft mit diesem Download.)</li>
    <li><a href="../localhelp/de/audacity-deutsch-handbuch-23-Jan-2005.pdf">Deutsches Handbuch (PDF)</a>
 </ul>
</div>



<?php
  include "../include/footer.inc.php";
?>
