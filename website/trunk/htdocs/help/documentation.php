<?php
/*
 * Copyright 2004 - 2013 Matt Brubeck, Gale Andrews
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
 */
  require_once "main.inc.php";
  require_once "../latest/versions.inc.php";
  $pageId = "documentation";
  $pageTitle = _("Manuals and Documentation");
  include "../include/header.inc.php";

	echo "<h2>$pageTitle</h2>";

     // i18n-hint:  Please add a note that the manuals below are in English.
     // If there is documentation for Audacity 2.x available in your language
     // but not already linked to at the bottom of this page, please add the links.
	echo "<p>"._('This page contains links to Manuals (with Tutorials) for the free Audacity audio editor. For additional help, see the <a href="/faq">Frequently Asked Questions</a> and additional <a href="http://wiki.audacityteam.org/wiki/Category:Tutorial">Tutorials</a> and <a href="http://wiki.audacityteam.org/wiki/Category:Tips">Tips</a> on the <a href="http://wiki.audacityteam.org/">Audacity Wiki</a>.')."</p>";
?>

<p><?=_('Legacy documentation for <a href="../download/#legacy">old versions of Audacity</a> can be found <a href="/help/legacy_documentation">here</a>.')?></p>

<h3><?=_('Audacity Manuals (with Tutorials)')?></h3>

<p><?=_('The <a href="/download/windows">Windows .exe</a> and <a href="/download/mac">Mac OS X .dmg</a> installers already include a built-in copy of the Manual. To access it, click <b>Help > Manual (in web browser)</b> in the Audacity program.')?></p>

<p>
<ul>
<li><a href="http://manual.audacityteam.org/o/
"><?php printf(_('View the latest Audacity %s Manual online'), src_version)?></a>.</li>
<li><a href="http://manual.audacityteam.org/o/man/unzipping_the_manual.html"><?php printf(_('Download and install the latest Audacity %s Manual in HTML format'), src_version)?></a>  <?=_('(Most users on <a href="/download/linux">GNU/Linux</a> will need this if they want a built-in Manual).')?></li>
<li><a href="http://manual.audacityteam.org/index.php?title=Main_Page"><?=_('View the latest Audacity alpha development Manual online.</a>')?> <?=_('(This Manual is only for the latest Audacity alpha <a href="http://wiki.audacityteam.org/wiki/Nightly_Builds">Nightly Builds</a>).')?></li>
</ul>
</p>

<?php
	echo
     // i18n-hint: Text below this hint is local help for
     // specific languages and should be left untranslated.
	_(' ');
?>
<div id="fr">
  <h3>Aide en Français</h3>
  <ul>
    <li><a href="http://www.pearson.fr/livre/?GCOI=27440100057910">Christian Brochec - Audacity 2</a> Une présentation complète d'Audacity, accompagnée d'exercices pour faciliter la prise en main et aller plus loin dans l'utilisation. <b>Attention:</b> En raison du retard pris par le développement d'Audacity 2.0, ce livre est basé sur la version 1.3.11 qui préfigure largement la version finale.</li>
  </ul>
</div>

<div id="de">
  <h3>Hilfe auf Deutsch</h3>
  <ul>
    <li><a href="http://www.brain-media.de/index.php/catalogsearch/result/?q=audacity">Markus Priemer - Audacity kompakt</a> Das deutschsprachige Anwenderhandbuch zu Audacity, basierend auf 1.3.7, die etwas &auml;hnelt Audacity 2.0. Herausgegeben vom brain-media.de. Mit Unterst&uuml;tzung und einem Vorwort von Audacity-Entwickler Markus Meyer.</li>
 </ul>
</div>


<?php
  include "../include/footer.inc.php";
?>
