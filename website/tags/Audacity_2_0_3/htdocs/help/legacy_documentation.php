<?php
/*
 * Copyright 2012 Gale Andrews
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
 */
  require_once "main.inc.php";
  $pageId = "legacy_documentation";
  $pageTitle = _("Legacy Documentation");
  include "../include/header.inc.php";


        echo "<h2>$pageTitle</h2>";
?>

<p><?=_('This is old documentation for legacy versions of Audacity. Please see <a href="/help/documentation">Documentation</a> for information about the current Audacity Manuals.')?></p>

<?php
     echo 
     // i18n-hint:  Please add a note that the guides or manuals below are 
     // in English. This is legacy documentation, so it is not a priority to 
     // add links to other localised documentation.
	_(' ');
?>

<h3><?=_('Audacity 1.2 Quick Reference Guide')?></h3>
<ul>
<li><a href="../onlinehelp-1.2/contents.htm"><?=_('Read the Audacity 1.2 reference online.')?></a> <?=_('(You can also access this reference through the "Help" menu in the Audacity program.)')?></li>
<li><a href="../onlinehelp-1.2/reference.html"><?=_('Printable version of the Audacity 1.2 reference.')?></a> <?=_('(This can be printed in one step using the print function of your browser.)')?></li>
</ul>

<h3><?=_('Audacity 1.2 Manual (including Tutorials)')?></h3>
<ul>
	<li><a href="../manual-1.2/"><?=_('Read the Audacity 1.2 manual online.')?></a></li>
	<li><a href="../audacity-manual-1.2.zip"><?=_('Download the Audacity 1.2 manual in HTML format.')?></a> <?=_('(To use the manual, unzip it and open the "index.html" file.)')?></li>
	<li><a href="audacity-manual-1.2.pdf"><?=_('Download the Audacity 1.2 manual in PDF format.')?></a></li>
</ul>

<?php
	echo
     // i18n-hint: Text below this hint is local help for
     // specific languages and should be left untranslated.
	_(' ');
?>

<div id="fr">
  <h3>Aide pour 1.2 en Français</h3>
  <ul>
    <li><a href="../localhelp/fr/audacity-1.2-help.htb">Menu d'aide d'Audacity en français</a> (Remplacer le fichier qui porte comme nom, audacity-1.2-help.htb par celui qu'on vous propose. <a href="http://zonelibre.grics.qc.ca/spip.php?article100">Aide</a>)</li>
    <li><a href="../localhelp/fr/audacity-fr-manuel-1.2.zip">Manuel d'Audacity en français</a> (zip, 700 ko.)
    <li><a href="../localhelp/fr/audacity-mode-d'emploi.pdf">Mode d'emploi en français</a> (PDF, 700 ko.  Ce fichier peut prendre un bon moment de télécharger.)</li>
  </ul>
</div>

<div id="de">
  <h3>Hilfe f&uuml;r 1.2 auf Deutsch </h3>
  <ul>
    <li><a href="../localhelp/de/audacity-1.2-help.htb">Deutsche Hilfedatei (htb)</a> (Ersetzen Sie audacity-1.2-help.htb im Audacity Installation-Heft mit diesem Download.)</li>
    <li><a href="../localhelp/de/audacity-deutsch-handbuch-23-Jan-2005.pdf">Deutsches Handbuch (PDF)</a></li>
  </ul> 
</div>

<?php
  include "../include/footer.inc.php";
?>


