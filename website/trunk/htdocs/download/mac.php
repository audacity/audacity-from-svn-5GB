<?php
/*
 * Copyright 2004 Matt Brubeck
 * Richard Ash 2006
 * Gale Andrews 2009-2011
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
 */
  require_once "main.inc.php";
  require_once "../latest/versions.inc.php";
  require_once "../latest/mirror.inc.php";  
  $pageId = "mac";
  $pageTitle = _("Mac OS 9 / X");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>
<p><?=$downloadTagline?></p>

<h3><?=_("Recommended Download")?></h3>
<p><a href="#sysreq">System Requirements</a></p>
<ul>
  <li><p><?php printf(_('For Mac OS X (<b>Intel</b>): <a href="%s">Audacity %s</a> (.dmg file, %.1lf MB) - The latest version of the free Audacity audio editor.'), download_url($macosx_intel_url), macosx_intel_version, macosx_intel_size)?></p></li>
  <li><p><?php printf(_('For Mac OS X (<b>PPC</b>): <a href="%s">Audacity %s</a> (.dmg file, %.1lf MB) - The latest version of the free Audacity audio editor.'), download_url ($macosx_url), macosx_version, macosx_size)?></p></li>
  <li><p><?php printf(_('For Mac OS 9: <a href="%s">Audacity %s</a> (.sit file, %.1lf MB)'), download_url($mac_classic_url), mac_classic_version, mac_classic_size)?></p></li>
  <p><?=_("(Unfortunately, we no longer have the resources to provide new versions of Audacity for Mac OS 9.)")?></p></li>
</ul>

<p> <b><?=_("Installation instructions (OS X .dmg files)")?>:</b>
 <ol>
   <li><?=_("Inside your Applications folder, create a folder called \"Audacity\"")?></li>
   <li><?=_("Double-click the downloaded .dmg to mount it")?></li>
   <li><?=_("Option-drag the whole of the .dmg contents (not the .dmg itself) into the \"Audacity\" folder you created")?></li>
   <li><?=_("Double-click the Audacity icon inside the \"Audacity\" folder to launch the program")?></li>
 </ol>
</p>

<h3 id="optional"><?=_("Optional Downloads")?></h3>

<h4><?=_("Plug-ins and Libraries")?></h4>
<ul>
  <li><p><a href="http://ardour.org/files/releases/swh-plugins-0.4.15.dmg"><?=_("LADSPA plug-ins installer</a> - over 90 plug-ins.")?></p></li>
<?php include "common.inc.php"; ?>

<h3 id="sysreq"><?=_("System Requirements")?></h3>
<ul>
  <li><?=_("Audacity 1.2 requires Mac OS X 10.1 or later.")?></li>
  <li><?=_("Audacity 1.0 requires Mac OS 9.0 or later.")?></li>
  <li><?=_("Audacity runs best with at least 64 MB RAM and a 300 MHz processor.")?></li>
</ul>


<?php
  include "../include/footer.inc.php";
?>
