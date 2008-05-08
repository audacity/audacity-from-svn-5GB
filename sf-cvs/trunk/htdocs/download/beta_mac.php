<?php
/*
 * Copyright 2004 Matt Brubeck
 * Copyright 2005 Dominic Mazzoni
 * Copyright 2008 Gale Andrews
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  require_once "../beta/versions.inc.php";
  $pageId = "beta_mac";
  $pageTitle = _("Mac OS X");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>

<?php include "betawarning.inc.php" ?>

<p><?=$downloadTagline?></p>

<h3 id="recdown"><?=_("Recommended Download")?></h3>
<?php include "recommended.inc.php"?>
<ul>
  <li><p>Universal Binary: <?php printf(_('<a href="%s">Audacity %s</a> (.dmg file, %.1lf MB)'), "../beta/".$macosx_ub_url, macosx_ub_version, macosx_ub_size)?></p></li>
</ul>

		<p> <b>Installation instructions:</b>
 <ol>
   <li>Inside your Applications folder, create a folder called "Audacity"</li>
   <li>Double-click the downloaded .dmg to mount it</li>
   <li>Option-drag the whole of the .dmg contents (not the .dmg itself) into the "Audacity" folder you created</li>
   <li>Double-click Audacity.app inside the Applications folder to launch it</li>
 </ol>
</p>

<h3 id="optional"><?=_("Optional Downloads")?></h3>
<ul>
  <li><p><a href="http://ardour.org/files/releases/swh-plugins-0.4.15.dmg">LADSPA plugins installer</a> - over 90 plug-ins.</p></li>
<?php include "common.inc.php"; ?>
</ul>

<h3 id="sysreq"><?=_("System Requirements")?></h3>
<ul>
  <li><?=_("Audacity 1.3.5 requires Mac OS X 10.4 or later. Users on OS X 10.3 who wish to use a Beta version of Audacity should")?> <a href="http://downloads.sourceforge.net/audacity/audacity-macosx-ub-1.3.3.dmg?modtime=1179456290&big_mirror=0"><?=_("download the previous 1.3.3 version")?></a>.<?=_(" Audacity 1.3 does not support OS X 10.2 or earlier.")?></li>
  <li><?=_("Audacity runs best with at least 64 MB RAM and a 300 MHz processor.")?></li>
</ul>


<?php
  include "../include/footer.inc.php";
?>