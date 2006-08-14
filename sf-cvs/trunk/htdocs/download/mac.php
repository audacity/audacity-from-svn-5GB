<?php
/*
 * Copyright 2004 Matt Brubeck
 * Richard Ash 2006
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  require_once "../latest/versions.inc.php";
  $pageId = "mac";
  $pageTitle = _("Mac OS 9 / X");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>
<p><?=$downloadTagline?></p>

<p><a href="http://audacityteam.org/mac"><?php printf(_('Using an Intel Mac?  Click Here!'))?></a></p>

<h3><?=_("Recommended Download")?></h3>
<?php include "recommended.inc.php" ?>
<ul>
  <li><p><?php printf(_('For Mac OS X: <a href="%s">Audacity %s</a> (.dmg file, %.1lf MB) - The latest version of the free Audacity audio editor.'), "../latest/".$macosx_url, macosx_version, macosx_size)?></p></li>
  <!-- TODO: Installation instructions? -->
  <li><p><?php echo _('The above version of Audacity is not suitable for 
  Intel-based Macs due to issues with Rosetta. Pre-release Native Intel
  binaries can be found at ');?><a href="http://audacityteam.org/mac/">
  http://audacityteam.org/mac/</a>.</p></li>

  <li><p><?php printf(_('For Mac OS 9: <a href="%s">Audacity %s</a> (.sit file, %.1lf MB)'), "../latest/".$mac_classic_url, mac_classic_version, mac_classic_size)?></p></li>
  <p><?=_("(Unfortunately, we no longer have the resources to provide new versions of Audacity for Mac OS 9.)")?></p></li>
</ul>

<h3 id="optional"><?=_("Optional Downloads")?></h3>
<ul>
  <?php include "common.inc.php"; ?>
</ul>

<h3><?=_("System Requirements")?></h3>
<ul>
  <li><?=_("Audacity 1.2 requires Mac OS X 10.1 or later.")?></li>
  <li><?=_("Audacity 1.0 requires Mac OS 9.0 or later.")?></li>
  <li><?=_("Audacity runs best with at least 64 MB RAM and a 300 MHz processor.")?></li>
</ul>


<?php
  include "../include/footer.inc.php";
?>
