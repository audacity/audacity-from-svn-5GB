<?php
/*
 * Copyright 2004 - 2011 Matt Brubeck, Gale Andrews
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
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
	<li><a href="../onlinehelp-1.2/contents.htm"><?=_('Read the Audacity 1.2 reference online.')?></a> <?=_('(You can also access this reference through the "Help" menu in the Audacity program.)')?></li>
	<li><a href="../onlinehelp-1.2/reference.html"><?=_('Printable version of the Audacity 1.2 reference.')?></a> <?=_('(This can be printed in one step using the print function of your browser.)')?></li>
 </ul>

<h3><?=_('Audacity User\'s Manuals')?></h3>
<ul>
	<li><a href="../manual-1.2/"><?=_('Read the Audacity 1.2 manual online.')?></a></li>
	<li><a href="../audacity-manual-1.2.zip"><?=_('Download the Audacity 1.2 manual in HTML format.')?></a> <?=_('(To use the manual, unzip it and open the "index.html" file.)')?></li>
	<li><a href="audacity-manual-1.2.pdf"><?=_('Download the Audacity 1.2 manual in PDF format.')?></a></li>
	<li><a href="http://manual.audacityteam.org/man/Unzipping_the_Manual"><?=_('Download the latest Audacity 1.3 Beta manual in HTML format')?></a> <?=_('(The Audacity Beta installers for Windows and Mac already include a built-in copy of the Manual accessed through the "Help" menu in the Audacity program.)')?></li>
	<li><a href="http://manual.audacityteam.org/index.php?title=Main_Page"><?=_('Read the latest Audacity 1.3 alpha manual online.</a>')?> <?=_('(The latest Audacity 1.3 alpha Nightly Builds for testing purposes can be downloaded <a href="http://wiki.audacityteam.org/wiki/Nightly_Builds">here</a>.)')?></li>
</ul>

<?php
	echo
     // i18n-hint: Text below this hint is local help for
     // specific languages and should be left untranslated.
	_(' ');
?>

<div id="fr">
  <h3>Aide en Français</h3>
  <ul>
    <li><a href="../localhelp/fr/audacity-1.2-help.htb">Menu d'aide d'Audacity en français</a> (Remplacer le fichier qui porte comme nom, audacity-1.2-help.htb par celui qu'on vous propose. <a href="http://zonelibre.grics.qc.ca/spip.php?article100">Aide</a>)</li>
    <li><a href="../localhelp/fr/audacity-fr-manuel-1.2.zip">Manuel d'Audacity en français</a> (zip, 700 ko.)
    <li><a href="../localhelp/fr/audacity-mode-d'emploi.pdf">Mode d'emploi en français</a> (PDF, 700 ko.  Ce fichier peut prendre un bon moment de télécharger.)</li>
    <li><a href="http://www.pearson.fr/livre/?GCOI=27440100057910">Christian Brochec - Audacity 2</a> Une présentation complète d'Audacity, accompagnée d'exercices pour faciliter la prise en main et aller plus loin dans l'utilisation. <b>Attention:</b> En raison du retard pris par le développement d'Audacity 2.0, ce livre est basé sur la version 1.3.11 qui préfigure largement la version finale.</li>
  </ul>
</div>

<div id="de">
  <h3>Hilfe auf Deutsch</h3>
  <ul>
    <li><a href="../localhelp/de/audacity-1.2-help.htb">Deutsche Hilfedatei (htb)</a> (Ersetzen Sie audacity-1.2-help.htb im Audacity Installation-Heft mit diesem Download.)</li>
    <li><a href="../localhelp/de/audacity-deutsch-handbuch-23-Jan-2005.pdf">Deutsches Handbuch (PDF)</a></li>
    <li><a href="http://www.brain-media.de/index.php/catalogsearch/result/?q=audacity">Markus Priemer - Audacity kompakt</a> Das deutschsprachige Anwenderhandbuch zu Audacity. 
Herausgegeben vom brain-media.de. Mit Unterst&uuml;tzung und einem Vorwort von Audacity-Entwickler Markus Meyer.</li>
 </ul>
</div>



<?php
  include "../include/footer.inc.php";
?>
