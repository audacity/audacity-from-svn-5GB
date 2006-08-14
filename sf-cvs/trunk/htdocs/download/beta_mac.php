<?php
/*
 * Copyright 2005 Dominic Mazzoni
 * Copyright 2004 Matt Brubeck
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  require_once "../beta/versions.inc.php";
  $pageId = "beta_mac";
  $pageTitle = _("Mac OS 9 / X");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>

<?php include "betawarning.inc.php" ?>
<?php include "betafeatures.inc.php" ?>

<p><a href="http://audacityteam.org/mac"><?php printf(_('Using an Intel Mac?  Click Here!'))?></a></p>

<p><?=$downloadTagline?></p>

<h3><?=_("Recommended Download")?></h3>
<?php include "recommended.inc.php"?>
<ul>
  <li><p><?php printf(_('For Mac OS X: <a href="%s">Audacity %s</a> (.dmg file, %.1lf MB) - The latest version of the free Audacity audio editor.'), "../beta/".$macosx_url, macosx_version, macosx_size)?></p></li>
  <!-- TODO: Installation instructions? -->
</ul>

<h3 id="optional"><?=_("Optional Downloads")?></h3>
<ul>
  <?php include "common.inc.php"; ?>
</ul>

<h3><?=_("System Requirements")?></h3>
<ul>
  <li><?=_("Audacity 1.3 requires Mac OS X 10.3 or later.")?></li>
  <li><?=_("Audacity runs best with at least 64 MB RAM and a 300 MHz processor.")?></li>
</ul>


<?php
  include "../include/footer.inc.php";
?>
