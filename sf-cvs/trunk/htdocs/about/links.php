<?php
/*
 * Copyright 2004 Matt Brubeck
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "links";
  $pageTitle = _("Links");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>
<h3><?=_("Other Things Named “Audacity” (not related to us)")?></h3>
<ul>
  <li><?=_('<a href="http://www.audiost.com/advr.htm">Audacity DVR</a> is a digital voice recorder for Palm OS and Treo handheld computers')?></li>
  <li><?=_('<a href="http://www.audacity.org">Audacity.org</a> is a research company for construction industry professionals.')?></li>
  <li><?=_('<a href="http://kotinetti.suomi.net/jakke/audacity/">Audacity</a> is a street-rock band from Finland.')?></li>
  <li><?=_('<a href="http://www.audacity.com/">Audacity, Inc.</a> builds custom enterprise and departmental Windows applications.')?></li>
</ul>

<h3><?=_("Other Audio Software")?></h3>
<p><?=_('<a href="http://ardour.org/">Ardour</a> is a powerful digital audio workstation that runs on GNU/Linux.')?></p>
<p><?=_('<a href="http://cdexos.sourceforge.net/">CDex</a> is a free CD ripper for Microsoft Windows.')?></p>
<p><?=_('<a href="http://www.opensourcepartners.nl/~costar/gramofile/">GramoFile</a> is a free tool for capturing and cleaning recordings from vinyl records.')?></p>
<p><?=_('More free software at SourceForge.net:')?></p>
<ul>
  <li><a href="http://sourceforge.net/softwaremap/trove_list.php?form_cat=120"><?=_("Audio Editors")?></a>
  <li><a href="http://sourceforge.net/softwaremap/trove_list.php?form_cat=115"><?=_("Audio Capture/Recording")?></a>
  <li><a href="http://sourceforge.net/softwaremap/trove_list.php?form_cat=114"><?=_("Audio Analysis")?></a>
</ul>

<?php
  include "../include/footer.inc.php";
?>
